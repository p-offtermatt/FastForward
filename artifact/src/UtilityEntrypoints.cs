using System;
using System.Collections.Generic;
using Petri;
using System.Linq;
using System.Diagnostics;
using System.IO;
using Benchmark;
using Statistics = MathNet.Numerics.Statistics.Statistics;
using Utils;
#if GUROBI
using static Petri.GurobiHeuristics;
#endif
using Microsoft.Z3;

namespace PetriTool
{
    class UtilityEntrypoints
    {
        public static void TransformToWFNet(WFTransformationOptions options)
        {
            NetParser parser = ParserPicker.ChooseNetParser(options.netFilePath);
            (PetriNet net, Marking initialMarking) = parser.ReadNet(options.netFilePath);

            try
            {
                WorkflowUtils.TransformToWorkflowNet(net, initialMarking);
                Console.WriteLine("Transformed net!");
            }
            catch (Petri.WorkflowException e)
            {
                // If tranformation failed, it should have had no side effects, so just move on.
                Console.WriteLine("Could not transform into workflow net for the following reason:");
                Console.WriteLine("------------------------------------------------------------------");
                Console.WriteLine(e.ToString());
                Console.WriteLine("------------------------------------------------------------------");
            }

            (bool, IEnumerable<Place>, IEnumerable<Place>) wfCheck = net.IsWorkflowNet();
            if (!wfCheck.Item1)
            {
                Console.WriteLine(@"Net is not a workflow net after transformation, but KHA ran through, so it was not enough to transform.");
            }
            else
            {
                Console.WriteLine("Net is a workflow net!");
            }

            string lolaOutput = net.ToLola(initialMarking);
            using (StreamWriter file = new StreamWriter(options.outputFilePath + ".lola", append: false))
            {
                file.Write(lolaOutput);
            }
        }

        public static void TranslateWFNet(TranslateWFOptions options)
        {
            NetParser parser = ParserPicker.ChooseNetParser(options.netFilePath);
            (PetriNet net, _) = parser.ReadNet(options.netFilePath);

            (bool isWF, IEnumerable<Place> sources, IEnumerable<Place> sinks) = net.IsWorkflowNet();

            if (!isWF)
            {
                throw new WorkflowException("Not a workflow net! Sources: " + String.Join(", ", sources) + "; Sinks: " + String.Join(", ", sinks));
            }

            Place initial = sources.First();
            Place final = sinks.First();

            List<MarkingWithConstraints> resultTargetMarkings = null;
            PetriNet resultNet = null;

            switch (options.translationMode)
            {
                case WorkflowTranslation.Soundness:
                    if (options.outputFormat != OutputFormat.Lola)
                    {
                        throw new NotSupportedException("Only LoLA format supports checking soundness!");
                    }
                    // cannot fall through, so simply goto right case label after doing additional check.
                    goto case WorkflowTranslation.Reachability;
                case WorkflowTranslation.Coverability:
                case WorkflowTranslation.Reachability:
                    {
                        Marking initialMarking = new Marking();
                        initialMarking[initial] = 1;

                        Marking finalMarking = new Marking();
                        finalMarking[final] = 1;

                        MarkingWithConstraints finalMarkingWithConstraints = options.translationMode switch
                        {
                            // just wraps finalMarking in marking with constraints; constraints will be ignored for soundness later
                            PetriTool.WorkflowTranslation.Soundness => MarkingWithConstraints.AsCoverability(finalMarking),
                            PetriTool.WorkflowTranslation.Coverability => MarkingWithConstraints.AsCoverability(finalMarking),
                            PetriTool.WorkflowTranslation.Reachability => MarkingWithConstraints.AsReachability(finalMarking, net)
                        };

                        resultTargetMarkings = new List<MarkingWithConstraints>() { finalMarkingWithConstraints };
                        // formulaString = options.translationMode switch
                        // {
                        //     PetriTool.WorkflowTranslation.Soundness => finalMarking.ToLolaLivenessPredicate(net),
                        //     PetriTool.WorkflowTranslation.Coverability => "EF (" + MarkingWithConstraints.AsCoverability(finalMarking).ToLola() + ")",
                        //     PetriTool.WorkflowTranslation.Reachability => "EF (" + MarkingWithConstraints.AsReachability(finalMarking, net).ToLola() + ")"
                        // };

                        resultNet = net;
                        break;
                    }

                case WorkflowTranslation.StructuralReachability:
                    {
                        // reduces structural reachability/coverability to Petri net reachability/coverability
                        // by adding auxiliary places; see Fig 1 of Structural soundness of workflow nets is decidable;
                        // e.g. http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.66.1432&rep=rep1&type=pdf

                        Place p1 = net.AddNewPlace("aux_p1");
                        Place p2 = net.AddNewPlace("aux_p2");
                        Place p3 = net.AddNewPlace("aux_p3");

                        UpdateTransition r1 = new UpdateTransition("r1");
                        r1.AddPlaceToPre(p1, 1);

                        r1.AddPlaceToPost(initial, 1);
                        r1.AddPlaceToPost(p1, 1);
                        r1.AddPlaceToPost(p2, 1);

                        net.AddTransition(r1);


                        UpdateTransition r2 = new UpdateTransition("r2");
                        r2.AddPlaceToPre(p1, 1);
                        r2.AddPlaceToPre(p2, 1);

                        r2.AddPlaceToPost(p2, 1);
                        r2.AddPlaceToPost(p3, 1);

                        net.AddTransition(r2);


                        UpdateTransition r3 = new UpdateTransition("r3");
                        r3.AddPlaceToPre(p2, 1);
                        r3.AddPlaceToPre(final, 1);
                        r3.AddPlaceToPre(p3, 1);

                        r3.AddPlaceToPost(p3, 1);

                        net.AddTransition(r3);

                        Marking initialMarking = new Marking();
                        initialMarking[p1] = 1;

                        resultNet = net;

                        Marking finalMarking = new Marking();
                        finalMarking[p3] = 1;

                        resultTargetMarkings = new List<MarkingWithConstraints>() { MarkingWithConstraints.AsReachability(finalMarking, net) };
                        break;
                    }
            }


            using (StreamWriter file = new StreamWriter(options.outputFilePath + ".lola", append: false))
            {
                file.Write(netString);
            }
            using (StreamWriter file = new StreamWriter(options.outputFilePath + ".formula", append: false))
            {
                file.Write(formulaString);
            }
        }

        public static void Translate(TranslationOptions options)
        {
            SearchBenchmarkEntry entry = new SearchBenchmarkEntry();
            FullParser parser = ParserPicker.ChooseFullParser(options.netFilePath, options.formulaFilePath);
            (PetriNet net, Marking initialMarking) = parser.ReadNet(options.netFilePath);

            List<MarkingWithConstraints> targetMarkings = null;
            targetMarkings = parser.ReadFormula(options.formulaFilePath);

            if (options.backwardPrune || options.forwardPrune)
            {
                Pruning.Prune(null, ref net, ref initialMarking, ref targetMarkings, options.forwardPrune, options.backwardPrune);
            }

            if (net.Places.Count == 0 || net.Transitions.Count == 0 || initialMarking.Count == 0 || targetMarkings.Count == 0)
            {
                Console.WriteLine("Pruning solved the instances, not doing any output...");
                return;
            }

            switch (options.outputFormat)
            {
                case (OutputFormat.Dotspec):
                    TranslateNetToDotspec(net, initialMarking, targetMarkings, options.outputFilePath + ".spec");
                    break;
                case (OutputFormat.Lola):
                    string lolaOutput = net.ToLola(initialMarking);
                    using (StreamWriter file = new StreamWriter(options.outputFilePath + ".lola", append: false))
                    {
                        file.Write(lolaOutput);
                    }
                    using (StreamWriter file = new StreamWriter(options.outputFilePath + ".formula", append: false))
                    {
                        file.Write(MarkingWithConstraints.ListToLola(targetMarkings));
                    }
                    break;
                case (OutputFormat.TTS):
                    using (StreamWriter writer = new StreamWriter(options.outputFilePath + ".tts"))
                    {
                        writer.Write(net.ToTTS_PN());
                    }
                    using (StreamWriter writer = new StreamWriter(options.outputFilePath + ".prop"))
                    {
                        writer.Write(targetMarkings.First().ToTTS_PN(net.GetPlaceToCounterNumDict(), initialMarking: false));
                    }
                    using (StreamWriter writer = new StreamWriter(options.outputFilePath + ".init"))
                    {
                        writer.Write(initialMarking.ToTTS_PN(net.GetPlaceToCounterNumDict(), initialMarking: true));
                    }
                    break;
                default:
                    Console.WriteLine("Could not understand file format \"" + options.outputFormat + "\"");
                    System.Environment.Exit(3);
                    break;
            }
        }

        public static void RemovePlace(RemovePlaceOptions options)
        {
            SearchBenchmarkEntry entry = new SearchBenchmarkEntry();
            String lolaFileName = options.netFilePath.Split("/").Last();
            String formulaFileName = options.formulaFilePath.Split("/").Last();
            FullParser parser = ParserPicker.ChooseFullParser(options.netFilePath, options.formulaFilePath);

            (PetriNet net, Marking initialMarking) = parser.ReadNet(options.netFilePath);
            Place placeToRemove = new Place(options.place);


            List<MarkingWithConstraints> targetMarkings = null;
            targetMarkings = parser.ReadFormula(options.formulaFilePath);

            foreach (MarkingWithConstraints target in targetMarkings)
            {
                target.Marking.Remove(placeToRemove);
                target.Constraints.Remove(placeToRemove);
            }

            net.RemovePlace(placeToRemove, keepTransitions: true);

            initialMarking.Remove(placeToRemove);

            String formulaString = "EF ((" +
                String.Join(") OR (", targetMarkings.Select(m => m.ToLola())
                ) + "))";

            using (StreamWriter lola_file = new StreamWriter(options.outputDir + lolaFileName, append: false))
            using (StreamWriter formula_file = new StreamWriter(options.outputDir + formulaFileName, append: false))
            {
                lola_file.Write(net.ToLola(initialMarking));
                formula_file.Write(formulaString);
            }
        }

        public static void AddIndicatorPlacesSypet(AddIndicatorPlacesOptions options)
        {
            SearchBenchmarkEntry entry = new SearchBenchmarkEntry();
            String lolaFileName = options.netFilePath.Split("/").Last();
            String formulaFileName = options.formulaFilePath.Split("/").Last();

            FullParser parser = ParserPicker.ChooseFullParser(options.netFilePath, options.formulaFilePath);
            (PetriNet net, Marking initialMarking) = parser.ReadNet(options.netFilePath);


            List<MarkingWithConstraints> targetMarkings = null;
            targetMarkings = parser.ReadFormula(options.formulaFilePath);

            foreach (Place place in initialMarking.GetMarkedPlaces())
            {
                Place indicatorPlace = new Place("indicator_place_" + place.Name);
                net.Places.Add(indicatorPlace);
                foreach (UpdateTransition transition in net.Transitions)
                {
                    if (transition.Pre.GetValueOrDefault(place) > 0)
                    {
                        transition.Post.Add(indicatorPlace, 1);
                    }
                }

                foreach (MarkingWithConstraints targetMarking in targetMarkings)
                {
                    targetMarking.Marking.Add(indicatorPlace, 1);
                    targetMarking.Constraints.Add(indicatorPlace, ConstraintOperators.GreaterEqual);
                }
            }

            String formulaString = "EF ((" +
                String.Join(") OR (", targetMarkings.Select(m => m.ToLola())
                ) + "))";

            using (StreamWriter lola_file = new StreamWriter(options.outputDir + lolaFileName, append: false))
            using (StreamWriter formula_file = new StreamWriter(options.outputDir + formulaFileName, append: false))
            {
                lola_file.Write(net.ToLola(initialMarking));
                formula_file.Write(formulaString);
            }
        }

        public static void TranslateNetToDotspec(PetriNet net, Marking initialMarking, List<MarkingWithConstraints> targetMarkings, string outfilePath)
        {
            using (System.IO.StreamWriter file =
            new System.IO.StreamWriter(outfilePath, append: false))
            {
                file.Write(net.ToDotspec(initialMarking, targetMarkings));

                Console.WriteLine("Successfully wrote output to " + outfilePath);
            }
        }


        public static void CalculateHeuristicDistance(CalculateHeuristicOptions options)
        {
            SearchBenchmarkEntry entry = new SearchBenchmarkEntry();
            FullParser parser = ParserPicker.ChooseFullParser(options.netFilePath, options.formulaFilePath);

            (PetriNet net, Marking initialMarking) = parser.ReadNet(options.netFilePath);
            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(options.formulaFilePath);

            Pruning.Prune(entry, ref net, ref initialMarking, ref targetMarkings);

            Func<Marking, float?> heuristic = HeuristicPicker.ChooseForwardHeuristic(entry, options, net, initialMarking, targetMarkings);

            var initialScore = heuristic(initialMarking);
            Console.WriteLine("{ \"heuristic\": " + (initialScore.HasValue ? initialScore.Value.ToString() : "\"unreachable\"") + "}");
        }


        public static void CalculateHeuristicSupport(CalculateHeuristicSupportOptions options)
        {
#if GUROBI
            SearchBenchmarkEntry entry = new SearchBenchmarkEntry();
            FullParser parser = ParserPicker.ChooseFullParser(options.netFilePath, options.formulaFilePath);

            (PetriNet net, Marking initialMarking) = parser.ReadNet(options.netFilePath);
            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(options.formulaFilePath);

            Pruning.Prune(entry, ref net, ref initialMarking, ref targetMarkings);

            // Func<Marking, float?> heuristic = InitializeMarkingEquationHeuristic(net, targetMarkings, domain);
            Func<Marking, Tuple<IEnumerable<Transition>, float?>> heuristic =
                InitializeMarkingEquationTransitionSupportComputation(net.Places, net.Transitions, targetMarkings, GurobiConsts.Domains.N);

            (IEnumerable<Transition> support, float? initialHeuristicValue) = heuristic(initialMarking);
            if (support == null)
            {
                Console.WriteLine("{ \"transitionSupport\": \"unreachable\" }");
            }
            else
            {

                Console.WriteLine("{ \"transitionSupport\": \"" + String.Join(", ", support.Select(transition => transition.Name)) + "\"}");
            }
# else
            Console.WriteLine("Gurobi needs to be installed, and the compile flag set, to use this method! See the README for more information.");
            System.Environment.Exit(5);
#endif
        }

        public static void WitnessCheck(WitnessCheckOptions options)
        {
            SearchBenchmarkEntry entry = new SearchBenchmarkEntry();
            Console.WriteLine("Running witness check with net " + options.netFilePath + ", formula " + options.formulaFilePath == null ? "null" : options.formulaFilePath + ", witness: " + options.witness);

            NetParser netParser = null;

            List<MarkingWithConstraints> targetMarkings = null;

            if (options.formulaFilePath != null)
            {
                FullParser parser = ParserPicker.ChooseFullParser(options.netFilePath, options.formulaFilePath);
                targetMarkings = parser.ReadFormula(options.formulaFilePath);
                netParser = parser;
            }
            else
            {
                NetParser parser = ParserPicker.ChooseNetParser(options.netFilePath);
                targetMarkings = new List<MarkingWithConstraints>();
                netParser = parser;
            }

            (PetriNet net, Marking initialMarking) = netParser.ReadNet(options.netFilePath);

            Console.WriteLine("Initial Marking is " + initialMarking.ToString());

            List<Transition> transitions = new List<Transition>();

            foreach (string transitionName in options.witness.Split(", "))
            {
                Transition transition = net.Transitions.Where(transition => transition.Name == transitionName).First();
                transitions.Add(transition);
            }

            HashSet<Marking> currentMarkings = new HashSet<Marking> { initialMarking };
            foreach (Transition transition in transitions)
            {
                Console.WriteLine("Current marking: " + currentMarkings.First().ToString());
                Console.WriteLine("Current transition: " + transition.ToString());
                HashSet<Marking> successors = currentMarkings.Aggregate(new HashSet<Marking>(), (acc, marking) => acc.Union(marking.Fire(transition)).ToHashSet());
                if (successors.Count == 0)
                {
                    Console.WriteLine("ERROR! COULD NOT FIRE TRANSITION!");
                    Console.WriteLine("Stopped at transition " + transition.Name);
                    Console.WriteLine("Final Marking was " + initialMarking.ToString());
                    return;
                }
                currentMarkings = successors;
            }
            Console.WriteLine("Fired all transitions, reached marking: " + currentMarkings.First().ToString());
            bool isTargetMarking = targetMarkings.Any(target => currentMarkings.Any(curMarking => target.SatisfiedByMarking(curMarking)));
            Console.WriteLine("Reached target marking: " + isTargetMarking.ToString());
        }

        private static List<Place> ComputeMonotonicPlaceOrder(PetriNet net)
        {
            Dictionary<Place, HashSet<Place>> predecessors =
                new Dictionary<Place, HashSet<Place>>(net.Places.Count);

            foreach (UpdateTransition transition in net.Transitions)
            {
                foreach (Place succ in transition.Post.Keys)
                {
                    HashSet<Place> tmp = predecessors.GetValueOrDefault(succ, new HashSet<Place>());
                    foreach (Place pre in transition.Pre.Keys)
                    {
                        if (!pre.Equals(succ))
                            tmp.Add(pre);
                    }
                    predecessors[succ] = tmp;
                }
            }

            List<Place> sortedPlaces = new List<Place>();

            Stack<Place> prelessPlaces = new Stack<Place>();

            foreach (Place place in net.Places)
            {
                if (!predecessors.ContainsKey(place))
                {
                    prelessPlaces.Push(place);
                }

            }

            while (prelessPlaces.Count != 0)
            {
                Place cur = prelessPlaces.Pop();
                sortedPlaces.Add(cur);
                foreach ((Place successor, HashSet<Place> preds) in predecessors)
                {
                    if (preds.Contains(cur))
                    {
                        preds.Remove(cur);
                        if (preds.Count == 0)
                        {
                            prelessPlaces.Push(successor);
                            predecessors.Remove(successor);
                        }
                    }
                }
            }

            if (predecessors.Count != 0)
            {
                return null;
            }
            else
            {
                return sortedPlaces;
            }
        }

        private static Dictionary<Transition, Double> GetIntegerBoundednessCounterexample(PetriNet net)
        {
            return GurobiHeuristics.CheckIntegerUnboundedness(net);
        }

        private static List<Place> ComputeMonotonicPlaceOrderViaSMT(PetriNet net, int degree)
        {
            using (Context ctx = new Context(new Dictionary<string, string>
            {
                ["timeout"] = Z3Heuristics.z3TimeOut.ToString(),
            }))
            {
                Solver solver = ctx.MkSolver();
                // Place orders should be sequential, starting with 1 => all distinct, all >0, sum up to 1 + 2 + ... + n = n(n+1)/2
                IntExpr[] placeExprs = net.Places.Select(p => ctx.MkIntConst(p.Name)).ToArray();
                solver.Add(ctx.MkDistinct(placeExprs));

                int target = (net.Places.Count * (net.Places.Count + 1)) / 2;

                solver.Add(ctx.MkEq(ctx.MkAdd(placeExprs), ctx.MkInt(target)));

                foreach (Place p in net.Places)
                {
                    BoolExpr lt = ctx.MkInt(0) < ctx.MkIntConst(p.Name);
                    solver.Add(lt);
                }

                // Each transition locally satisfies order

                foreach (Transition t in net.Transitions)
                {
                    foreach (Place pre in t.GetPrePlaces())
                    {
                        foreach (Place post in t.GetPostPlaces())
                        {
                            solver.Add(ctx.MkIntConst(pre.Name) + ctx.MkInt(degree) <= ctx.MkIntConst(post.Name));
                        }
                    }
                }

                Status status = solver.Check();

                File.AppendAllText("log.txt", solver.ToString());
                File.AppendAllText("log.txt", "------------------------------------");



                if (status == Status.UNSATISFIABLE || status == Status.UNKNOWN)
                {
                    return null;
                }
                else
                {
                    Model model = solver.Model;
                    Dictionary<Place, int> placesWithOrderNumbers = new Dictionary<Place, int>();

                    File.AppendAllText("log.txt", model.ToString());
                    File.AppendAllText("log.txt", "====================================");

                    placesWithOrderNumbers = net.Places.ToDictionary(
                        place => place,
                        place => Int32.Parse(model.Eval(ctx.MkIntConst(place.Name)).ToString())
                    );

                    List<Place> orderedPlaces = placesWithOrderNumbers.OrderBy(kvPair => kvPair.Value).Select(kvPair => kvPair.Key).ToList();
                    return orderedPlaces;
                }

            }
        }

        public static bool CheckNetAgainstPlaceOrder(PetriNet net, List<Place> placeOrder, int degree)
        {
            foreach (UpdateTransition transition in net.Transitions)
            {
                if (transition.Pre.Count == 0 || transition.Post.Count == 0)
                {
                    continue;
                }
                int preMax = transition.Pre.Keys.Max(place => placeOrder.IndexOf(place));
                int postMin = transition.Post.Keys.Min(place => placeOrder.IndexOf(place));

                if (preMax + degree > postMin)
                {
                    return false;
                }

                if (degree == 1)
                {
                    if (transition.Pre.Any(kvPair => transition.Post.ContainsKey(kvPair.Key)))
                    {
                        return false;
                    }
                }
            }

            return true;
        }

        public static void ComputeNetStatistics(ComputeNetStatisticsOptions options)
        {
            NetParser netParser = ParserPicker.ChooseNetParser(options.netFilePath);

            (PetriNet net, Marking initialMarking) = netParser.ReadNet(options.netFilePath);
            NetStatisticsEntry dataEntry = new NetStatisticsEntry();

            List<MarkingWithConstraints> targetMarkings;
            List<MarkingWithConstraints> targetMarkingsCopy;
            List<MarkingWithConstraints> targetMarkingsCopy2;
            try
            {
                FormulaParser formulaParser = ParserPicker.ChooseFormulaParser(options.formulaFilePath);
                targetMarkings = formulaParser.ReadFormula(options.formulaFilePath);

                targetMarkingsCopy = formulaParser.ReadFormula(options.formulaFilePath);
                targetMarkingsCopy2 = formulaParser.ReadFormula(options.formulaFilePath);
            }
            catch (System.IO.FileNotFoundException)
            {
                targetMarkings = null;
                targetMarkingsCopy = null;
                targetMarkingsCopy2 = null;
            }


            dataEntry.netFile = options.netFilePath;
            dataEntry.formulaFile = options.formulaFilePath;

            dataEntry.places = net.Places.Count;
            dataEntry.transitions = net.Transitions.Count;


            // if (options.prune)
            // {
            //     PetriNet netCopy = new PetriNet(net);
            //     Marking initialMarkingCopy = new Marking(initialMarking);

            //     PetriNet netCopy2 = new PetriNet(net);
            //     Marking initialMarkingCopy2 = new Marking(initialMarking);
            //     Stopwatch forwardPruningWatch = Stopwatch.StartNew();
            //     Pruning.Prune(null, ref netCopy, ref initialMarkingCopy, ref targetMarkingsCopy, forward: true, backward: false);
            //     forwardPruningWatch.Stop();
            //     dataEntry.timeTakenForwardPruning = forwardPruningWatch.ElapsedMilliseconds;
            //     dataEntry.placesAfterForwardPruning = netCopy.Places.Count;
            //     dataEntry.transitionsAfterForwardPruning = netCopy.Transitions.Count;

            //     Stopwatch backwardPruningWatch = Stopwatch.StartNew();
            //     if (!(targetMarkingsCopy2 is null))
            //     {
            //         Pruning.Prune(null, ref netCopy2, ref initialMarkingCopy2, ref targetMarkingsCopy2, forward: false, backward: true);
            //     }
            //     backwardPruningWatch.Stop();

            //     dataEntry.timeTakenBackwardPruning = backwardPruningWatch.ElapsedMilliseconds;
            //     dataEntry.placesAfterBackwardPruning = netCopy2.Places.Count;
            //     dataEntry.transitionsAfterBackwardPruning = netCopy2.Transitions.Count;

            //     Stopwatch pruningWatch = Stopwatch.StartNew();
            //     if (targetMarkings is null)
            //     {
            //         Pruning.Prune(null, ref net, ref initialMarking, ref targetMarkings, forward: true, backward: false);
            //     }
            //     else
            //     {
            //         Pruning.Prune(null, ref net, ref initialMarking, ref targetMarkings, forward: true, backward: true);
            //     }
            //     pruningWatch.Stop();
            //     dataEntry.timeTakenPruning = pruningWatch.ElapsedMilliseconds;

            //     dataEntry.placesAfterPruning = net.Places.Count();
            //     dataEntry.transitionsAfterPruning = net.Transitions.Count();

            //     dataEntry.fractionOfPlacesPruned = 1 - (double)dataEntry.placesAfterPruning / (double)dataEntry.places;
            //     dataEntry.fractionOfTransitionsPruned = 1 - (double)dataEntry.transitionsAfterPruning / (double)dataEntry.transitions;
            // }

            // dataEntry.numberOfSelfLoops = net.Transitions.Where(
            //     transition => transition.HasSelfLoop()
            // ).Count();

            // dataEntry.numberOfNiceSelfLoops = net.Transitions.Where(
            //     transition => transition.HasNiceSelfLoop()
            // ).Count();

            // dataEntry.bioTransitions = net.Transitions.Where(
            //     transition => transition.IsBio()
            // ).Count();

            // dataEntry.fractionOfBioTransitions = (double)dataEntry.bioTransitions / (double)dataEntry.transitionsAfterPruning;

            IEnumerable<double> preSizes = net.Transitions.Select(transition => (double)transition.GetPrePlaces().Count);

            dataEntry.meanPre = Statistics.Mean(preSizes);

            // TODO there is also Statistics.FiveNumberSummary which gives these values,
            // but appearantly deconstruction does not work with arrays, so use this
            // more descriptive way now. this could be changed later
            dataEntry.medianPre = Statistics.Median(preSizes);
            dataEntry.minimalPre = Statistics.Minimum(preSizes);
            dataEntry.maximalPre = Statistics.Maximum(preSizes);
            dataEntry.firstQuartilePre = Statistics.LowerQuartile(preSizes);
            dataEntry.thirdQuartilePre = Statistics.UpperQuartile(preSizes);

            IEnumerable<double> postSizes = net.Transitions.Select(transition => (double)transition.GetPostPlaces().Count);

            dataEntry.meanPost = Statistics.Mean(postSizes);

            dataEntry.medianPost = Statistics.Median(postSizes);
            dataEntry.minimalPost = Statistics.Minimum(postSizes);
            dataEntry.maximalPost = Statistics.Maximum(postSizes);
            dataEntry.firstQuartilePost = Statistics.LowerQuartile(postSizes);
            dataEntry.thirdQuartilePost = Statistics.UpperQuartile(postSizes);

            // IEnumerable<double> inDegs = net.Places.Select(place => (double)net.GetInDegree(place));

            // dataEntry.meanInDeg = Statistics.Mean(inDegs);

            // dataEntry.medianInDeg = Statistics.Median(inDegs);
            // dataEntry.minimalInDeg = Statistics.Minimum(inDegs);
            // dataEntry.maximalInDeg = Statistics.Maximum(inDegs);
            // dataEntry.firstQuartileInDeg = Statistics.LowerQuartile(inDegs);
            // dataEntry.thirdQuartileInDeg = Statistics.UpperQuartile(inDegs);

            // IEnumerable<double> outDeg = net.Places.Select(place => (double)net.GetOutDegree(place));

            // dataEntry.meanOutDeg = Statistics.Mean(outDeg);

            // dataEntry.medianOutDeg = Statistics.Median(outDeg);
            // dataEntry.minimalOutDeg = Statistics.Minimum(outDeg);
            // dataEntry.maximalOutDeg = Statistics.Maximum(outDeg);
            // dataEntry.firstQuartileOutDeg = Statistics.LowerQuartile(outDeg);
            // dataEntry.thirdQuartileOutDeg = Statistics.UpperQuartile(outDeg);

            // Arc weights
            IEnumerable<double> weights = net.GetArcWeights().Select(num => (double)num);
            dataEntry.meanWeight = Statistics.Mean(weights);
            dataEntry.medianWeight = Statistics.Median(weights);
            dataEntry.minimalWeight = Statistics.Minimum(weights);
            dataEntry.maximalWeight = Statistics.Maximum(weights);
            dataEntry.firstQuartileWeight = Statistics.LowerQuartile(weights);
            dataEntry.thirdQuartileWeight = Statistics.UpperQuartile(weights);

            // // Monotonicity

            // if (options.monotonicityDegree != Int32.MinValue)
            // {
            //     dataEntry.checkedMonotonicityDegree = options.monotonicityDegree.ToString();
            //     List<Place> orderedPlaces = ComputeMonotonicPlaceOrderViaSMT(net, options.monotonicityDegree);
            //     if (orderedPlaces is null)
            //     {
            //         dataEntry.monotonicPlaceOrder = "Not monotonic";
            //         dataEntry.isMonotonic = false;
            //     }
            //     else
            //     {
            //         dataEntry.monotonicPlaceOrder =
            //             "Monotonic:\n" +
            //             String.Join("\n", orderedPlaces.
            //                 Select((Place place, int index) => place.Name + ": " + index.ToString())
            //             );

            //         dataEntry.isMonotonic = true;

            //         if (!CheckNetAgainstPlaceOrder(net, orderedPlaces, options.monotonicityDegree))
            //         {
            //             throw new Exception("Got a place order, but net is not monotonic!");
            //         }
            //     }
            // }
            // else
            // {
            //     dataEntry.checkedMonotonicityDegree = "Not checked";
            // }

            // // Checking for Marked Graph

            // int markedGraphPlaces = net.ComputeNumberOfMarkedGraphPlaces();
            // dataEntry.numberOfMarkedGraphPlaces = markedGraphPlaces;

            // dataEntry.fractionOfMarkedGraphPlaces = net.Places.Count == 0 ? 1 : (double)markedGraphPlaces / (double)net.Places.Count;

            // dataEntry.isMarkedGraph = markedGraphPlaces == net.Places.Count;

            // // Checking for State Machine

            // int stateMachineTransitions = net.ComputeNumberOfStateMachineTransitions();
            // dataEntry.numberOfStateMachineTransitions = stateMachineTransitions;

            // dataEntry.fractionOfStateMachineTransitions = net.Transitions.Count == 0 ? 1 : (double)stateMachineTransitions / (double)net.Transitions.Count;
            // dataEntry.isStateMachineNet = stateMachineTransitions == net.Transitions.Count;

            // Checking for workflow net

            var (isWF, inputPlaceChoices, outputPlaceChoices) = net.IsWorkflowNet();
            dataEntry.isWorkflowNet = isWF;

            dataEntry.sourcePlaces = inputPlaceChoices == null ? "None" : String.Join(", ", inputPlaceChoices.Select(o => o.ToString()));
            dataEntry.sinkPlaces = outputPlaceChoices == null ? "None" : String.Join(", ", outputPlaceChoices.Select(o => o.ToString()));

            // Checking for free-choice

            dataEntry.isFreeChoice = net.IsFreeChoice();

            // // Checking for integer boundedness

            // var counterexample = GetIntegerBoundednessCounterexample(net);

            // dataEntry.integerBoundednessCounterexample = counterexample == null ? "None" : String.Join(";", counterexample);

            if (isWF)
            {
                // Checking for integer boundedness in WF nets

                Stopwatch watch = Stopwatch.StartNew();

                var wfBoundCounterexample = GetIntegerBoundednessCounterexample(net.ShortCircuit(inputPlaceChoices.First(), outputPlaceChoices.First()));

                watch.Stop();

                dataEntry.timeForWFIntegerBoundednessCounterexample = watch.ElapsedMilliseconds;

                dataEntry.wfIntegerBoundednessCounterexample = wfBoundCounterexample == null ? "None" : String.Join(";", wfBoundCounterexample);
                dataEntry.wfIntegerBoundednessCounterexampleSupportSize = wfBoundCounterexample == null ? 0 : wfBoundCounterexample.Where(pair => pair.Value > 0).Count();
                dataEntry.wfIntegerBoundednessCounterexampleImageSize = wfBoundCounterexample == null ? 0 : wfBoundCounterexample.Sum(pair => pair.Value);

                if (wfBoundCounterexample == null && options.checkContinuousSoundness)
                {
                    // Checking for continuous soundness
                    watch = Stopwatch.StartNew();
                    var (isSound, counterexample) = Z3Heuristics.IsContinuousSound_ViaContinuousReach(net, initialMarking);
                    watch.Stop();
                    dataEntry.timeForContinuousSoundness = watch.ElapsedMilliseconds;

                    dataEntry.isContinuousSound = isSound;
                    dataEntry.continuousSoundnessCounterexample = counterexample == null ? "None" : String.Join(";", counterexample.Where(pair => pair.Value > 0));
                }

                if (wfBoundCounterexample == null && options.checkIntegerSoundness)
                {
                    // Checking for integer soundness
                    watch = Stopwatch.StartNew();

                    // (in/out)putPlaceChoices.First() guaranteed to return unique place since net is a WF net
                    var counterexample = GurobiHeuristics.CheckMarkingEquationUnsoundness(net, inputPlaceChoices.First(), outputPlaceChoices.First());
                    watch.Stop();
                    dataEntry.timeForIntegerSoundness = watch.ElapsedMilliseconds;

                    dataEntry.isIntegerSound = counterexample == null;
                    dataEntry.integerSoundnessCounterexample = counterexample == null ? "None" : String.Join(";", counterexample.Where(pair => pair.Value > 0));
                }
            }

            // Writing output
            Console.WriteLine(dataEntry.ToJSON());
            System.Environment.Exit(0);
        }

        public static void GenerateInstance(GenerateInstanceOptions options)
        {
            NetParser parser = ParserPicker.ChooseNetParser(options.netFilePath);
            (PetriNet net, Marking initialMarking) = parser.ReadNet(options.netFilePath);

            int finalLength = options.length;

            int curLength = 0;

            Random rng;

            if (options.seed.HasValue)
            {
                rng = new Random(options.seed.Value);
            }
            else
            {
                rng = new Random();
            }

            Marking curMarking = initialMarking;

            HashSet<UpdateTransition> loopTransitions = PetriNetUtils.IdentifyArtificalLoopTransitions(net);

            // start with a marking with at most 0.2 * length many tokens which are added now.
            // in the real net, this will be done via transitions, but we don't count those against the final length
            int initialTokensAddNum = loopTransitions.Count != 0 ? rng.Next((int)(0.2 * finalLength)) : 0;

            Console.WriteLine(loopTransitions.Count != 0 ? "Adding " + initialTokensAddNum + " initial tokens..." : "No loop transitions in net, taking initial marking as given...");
            for (int i = 0; i < initialTokensAddNum; i++)
            {
                UpdateTransition transition = loopTransitions.RandomElement(rng);
                Marking successorMarking = transition.FireOn(curMarking).First();
                curMarking = successorMarking;
            }

            IEnumerable<Transition> nonArtificialTransitions = net.Transitions.Where(transition => !loopTransitions.Contains(transition));


            while (curLength < finalLength)
            {
                IEnumerable<Transition> enabledTransitions = nonArtificialTransitions.Where(transition => transition.IsEnabledIn(curMarking));

                if (!enabledTransitions.Any())
                {
                    // no transition enabled
                    break;
                }

                Transition transition = enabledTransitions.RandomElement(rng);

                Marking successor = transition.FireOn(curMarking).RandomElement(rng);

                curMarking = successor;
                curLength++;
            }

            curMarking.AddPlaces(net.Places);

            Constraints constraints =
                new Constraints(curMarking.ToDictionary(
                        kvPair => kvPair.Key,
                        kvPair => options.generationMode == GenerationMode.Coverability ? ConstraintOperators.GreaterEqual : ConstraintOperators.Equal)
                );

            MarkingWithConstraints targetMarking = new MarkingWithConstraints(curMarking, constraints);

            List<MarkingWithConstraints> targetMarkings = new List<MarkingWithConstraints> { targetMarking };

            Pruning.Prune(null, ref net, ref initialMarking, ref targetMarkings, forward: options.forwardPrune, backward: options.backwardPrune);

            if (net.Places.Count == 0 || net.Transitions.Count == 0 || initialMarking.Count == 0 || targetMarking.Marking.Count == 0)
            {
                Console.WriteLine("Query is solved after pruning, not writing any net...");
                return;
            }

            switch (options.outputFormat)
            {
                case (OutputFormat.TTS):
                    using (StreamWriter writer = new StreamWriter(options.outputFilePath + ".tts"))
                    {
                        writer.Write(net.ToTTS_PN());
                    }
                    using (StreamWriter writer = new StreamWriter(options.outputFilePath + ".prop"))
                    {
                        writer.Write(targetMarkings.First().ToTTS_PN(net.GetPlaceToCounterNumDict(), initialMarking: false));
                    }
                    using (StreamWriter writer = new StreamWriter(options.outputFilePath + ".init"))
                    {
                        writer.Write(initialMarking.ToTTS_PN(net.GetPlaceToCounterNumDict(), initialMarking: true));
                    }
                    break;
                case (OutputFormat.Dotspec):
                    throw new NotImplementedException();
                case (OutputFormat.Lola):
                    using (StreamWriter writer = new StreamWriter(options.outputFilePath + ".formula"))
                    {
                        writer.Write(MarkingWithConstraints.ListToLola(targetMarkings));
                    }
                    using (StreamWriter writer = new StreamWriter(options.outputFilePath + ".lola"))
                    {
                        writer.Write(net.ToLola(initialMarking));
                    }
                    break;
            }
        }
    }
}