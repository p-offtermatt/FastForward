using System;
using System.Collections.Generic;
using Petri;
using System.Linq;
using System.Diagnostics;
using System.IO;
using Benchmark;
using Statistics = MathNet.Numerics.Statistics.Statistics;
using Utils;
using System.Text.RegularExpressions;
using Newtonsoft.Json;
using System.Threading.Tasks;
#if GUROBI
using static Petri.GurobiHeuristics;
#endif
using Microsoft.Z3;

namespace PetriTool
{
    public class UtilityEntrypoints
    {
        public static void CalculateHeuristicDistance(CalculateHeuristicOptions options)
        {
            BenchmarkEntryWithHeuristics entry = new BenchmarkEntryWithHeuristics();
            Stopwatch queryWatch = Stopwatch.StartNew();

            FullParser parser = ParserPicker.ChooseFullParser(options.netFilePath, options.formulaFilePath);

            (PetriNet net, Marking initialMarking) = parser.ReadNet(options.netFilePath);
            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(options.formulaFilePath);

            entry.numberOfPlaces = net.Places.Count;
            entry.numberOfTransitions = net.Transitions.Count;

            if (options.prune)
            {
                // heuristic benchmark entry does not have fields for pruning, so don't pass it in
                Pruning.Prune(null, ref net, ref initialMarking, ref targetMarkings);
            }

            Func<Marking, float?> heuristic = HeuristicPicker.ChooseForwardHeuristic(entry, options, net, initialMarking, targetMarkings);

            Stopwatch watch = Stopwatch.StartNew();
            var initialScore = heuristic(initialMarking);
            entry.timeInHeuristicCalculation = watch.ElapsedMilliseconds;
            entry.timeInQuery = queryWatch.ElapsedMilliseconds;

            string scoreString = "\"heuristic\": " + (initialScore.HasValue ? initialScore.Value.ToString() : "\"unreachable\"") + ",";

            string entryAsJson = entry.ToJSON();

            // insert the score string right after the opening bracket of the json
            entryAsJson = entryAsJson.Trim();
            entryAsJson = entryAsJson.Insert(1, scoreString);
            Console.WriteLine(entryAsJson);
        }

        public static void CalculateMinimalKMarkingEquation(CalculateMarkingEquationParikhImageOptions options)
        {
            BenchmarkEntryWithHeuristics entry = new BenchmarkEntryWithHeuristics();
            Stopwatch queryWatch = Stopwatch.StartNew();

            FullParser parser = ParserPicker.ChooseFullParser(options.netFilePath, options.formulaFilePath);

            (PetriNet net, Marking initialMarking) = parser.ReadNet(options.netFilePath);

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(options.formulaFilePath);

            entry.numberOfPlaces = net.Places.Count;
            entry.numberOfTransitions = net.Transitions.Count;

            if (targetMarkings.Count > 1)
            {
                throw new ArgumentException("This method supports only a single target marking!");
            }
            Marking targetMarking = targetMarkings[0].Marking;

            Stopwatch watch = Stopwatch.StartNew();
            int? minimal_k = Z3Heuristics.CalculateMinimalKMarkingEquation(net, initialMarking, targetMarking, doMarkingEQOverN: true);
            entry.timeInHeuristicCalculation = watch.ElapsedMilliseconds;
            entry.timeInQuery = queryWatch.ElapsedMilliseconds;


            string parikhImageString = "\"k\": " + (minimal_k != null ? minimal_k.ToString() : "\"unreachable\"") + ",";

            string entryAsJson = entry.ToJSON();

            // insert the score string right after the opening bracket of the json
            entryAsJson = entryAsJson.Trim();
            entryAsJson = entryAsJson.Insert(1, parikhImageString);
            Console.WriteLine(entryAsJson);
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
#else
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

        private static (bool isBounded, Dictionary<Transition, Double> counterexample) GetIntegerBoundednessCounterexample(PetriNet net)
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

            var (_, intBoundCounterexample) = GetIntegerBoundednessCounterexample(net);

            dataEntry.integerBoundednessCounterexample = intBoundCounterexample == null ? "None" : String.Join(";", intBoundCounterexample);

            if (isWF)
            {
                // Checking for integer boundedness in WF nets

                Stopwatch watch = Stopwatch.StartNew();

                Place initialPlace = inputPlaceChoices.First();
                Place finalPlace = outputPlaceChoices.First();

                (var isZBounded, var wfBoundCounterexample) = GetIntegerBoundednessCounterexample(net.ShortCircuit(initialPlace, finalPlace));

                watch.Stop();

                dataEntry.timeForWFIntegerBoundednessCounterexample = watch.ElapsedMilliseconds;

                dataEntry.wfIntegerBoundednessCounterexample = isZBounded ? "None" : String.Join(";", wfBoundCounterexample);
                dataEntry.wfIntegerBoundednessCounterexampleSupportSize = isZBounded ? 0 : wfBoundCounterexample.Where(pair => pair.Value > 0).Count();
                dataEntry.wfIntegerBoundednessCounterexampleImageSize = isZBounded ? 0 : wfBoundCounterexample.Sum(pair => pair.Value);

                if (isZBounded && options.checkContinuousSoundness)
                {
                    // Checking for continuous soundness
                    watch = Stopwatch.StartNew();
                    var (isSound, counterexample) = Z3Heuristics.IsContinuousSound_ViaContinuousReach(net, initialMarking);
                    watch.Stop();
                    dataEntry.timeForContinuousSoundness = watch.ElapsedMilliseconds;

                    dataEntry.isContinuousSound = isSound;
                    dataEntry.continuousSoundnessCounterexample = counterexample == null ? "None" : String.Join(";", counterexample.Where(pair => pair.Value > 0));
                }

                if (isZBounded && options.checkIntegerSoundness)
                {
                    // Checking for integer soundness
                    watch = Stopwatch.StartNew();

                    // (in/out)putPlaceChoices.First() guaranteed to return unique place since net is a WF net
                    var counterexample = GurobiHeuristics.CheckMarkingEquationUnsoundness(net, initialPlace, finalPlace);
                    watch.Stop();
                    dataEntry.timeForIntegerSoundness = watch.ElapsedMilliseconds;

                    dataEntry.isIntegerSound = counterexample == null;
                    dataEntry.integerSoundnessCounterexample = counterexample == null ? "None" : String.Join(";", counterexample.Where(pair => pair.Value > 0));
                }

                var netName = dataEntry.netFile.Split("/").Last().Split(".")[0];

                if (options.checkIntegerDeadlock)
                {
                    // check for integer deadlocks
                    watch = Stopwatch.StartNew();

                    var counterexample = GurobiHeuristics.CheckIntegerDeadlock(net, initialPlace, finalPlace,
                    true, netName);
                    watch.Stop();

                    dataEntry.hasIntegerDeadlock = counterexample != null;
                    dataEntry.integerDeadlockExample = counterexample == null ? "None" : String.Join(";", counterexample.Where(pair => pair.Value > 0));
                    dataEntry.timeForIntegerDeadlock = watch.ElapsedMilliseconds;
                }

                if (options.checkContinuousDeadlock)
                {
                    // check for integer deadlocks
                    watch = Stopwatch.StartNew();

                    var counterexample = GurobiHeuristics.CheckIntegerDeadlock(net, initialPlace, finalPlace,
                    false, netName);
                    watch.Stop();

                    dataEntry.hasContinuousDeadlock = counterexample != null;
                    dataEntry.continuousDeadlockExample = counterexample == null ? "None" : String.Join(";", counterexample.Where(pair => pair.Value > 0));
                    dataEntry.timeForContinuousDeadlock = watch.ElapsedMilliseconds;
                }

                // {
                //     // check transition multiplicities
                //     var checkNumbers = new HashSet<int>() { 1, 2, 3, 4, 5, 10, 50, 200 };

                //     TransitionMultResult[] results = new TransitionMultResult[checkNumbers.Count];

                //     // Iterate over the set using a foreach loop
                //     int i = 0;
                //     foreach (int number in checkNumbers)
                //     {
                //         watch = Stopwatch.StartNew();
                //         var (exceeded, example) = GurobiHeuristics.CheckTransitionMults(net, initialPlace, number);
                //         watch.Stop();

                //         var checkResult = new TransitionMultResult();
                //         checkResult.bound = number;
                //         checkResult.boundExceeded = exceeded;
                //         checkResult.parikhImage = example == null ? "None" : String.Join(";", example.Where(pair => pair.Value > 0));
                //         checkResult.timeTaken = watch.ElapsedMilliseconds;
                //         results[i] = checkResult;
                //         i += 1;
                //     }

                //     dataEntry.transitionMultResults = results;
                // }
                if (options.checkTransitionBottlenecks)
                {
                    // check transition bottlenecks
                    watch = Stopwatch.StartNew();

                    var transitionBottlenecks = GurobiHeuristics.ComputeTransitionBottlenecks(net, initialPlace, finalPlace);
                    watch.Stop();

                    dataEntry.transitionBottlenecks = "{" +
                    String.Join("};;;{", transitionBottlenecks.Select((kvpair) => kvpair.Key.Name + ": " + String.Join(";", kvpair.Value)))
                     + "}";
                    dataEntry.transitionBottleneckNums = transitionBottlenecks.ToDictionary(kvpair => kvpair.Key.Name, kvpair => kvpair.Value.GetValueOrDefault(kvpair.Key, -1));
                    dataEntry.timeForTransitionBottlenecks = watch.ElapsedMilliseconds;
                }
                if (options.checkSmallBoundProperties)
                {
                    Dictionary<String, object> smallBoundProperties = new Dictionary<string, object>();
                    watch = Stopwatch.StartNew();

                    // computing these only works properly if the net has a transition that consumes exactly one token from the input place, so ensure this here
                    bool hasStartTransition = net.Transitions.Any(transition =>
                    {
                        HashSet<Place> prePlaces = transition.GetPrePlaces();
                        return prePlaces.Count == 1 && prePlaces.First().Equals(initialPlace) && transition.GetGuard()[initialPlace] == 1;
                    });

                    if (!hasStartTransition)
                    {
                        smallBoundProperties.Add("error", "net has no transition consuming only 1 token from the initial place");
                    }
                    else
                    {
                        var task = Task.Run(() => GurobiHeuristics.Compute_A_n(net, initialPlace, finalPlace));
                        double an = 0;
                        if (task.Wait(TimeSpan.FromSeconds(60)))
                        {
                            smallBoundProperties["timeForComputingA_n"] = watch.ElapsedMilliseconds;
                            an = task.Result;
                        }
                        else
                        {
                            smallBoundProperties["timeForComputingA_n"] = -1;
                            an = -1.0;
                        }
                        smallBoundProperties["A_n"] = an;
                        if (an != -1.0) // check the net is linear
                        {
                            watch = Stopwatch.StartNew();
                            var minTimeTask = Task.Run(() => GurobiHeuristics.Unroll_ComputeMinTimeWithBound(net,
                                initialPlace,
                                finalPlace,
                                1,
                                (int)an));
                            if (minTimeTask.Wait(TimeSpan.FromSeconds(15)))
                            {
                                smallBoundProperties["timeForComputingMinTime"] = watch.ElapsedMilliseconds;
                                smallBoundProperties["minTime"] = minTimeTask.Result;
                            }
                            else
                            {
                                smallBoundProperties["timeForComputingMinTime"] = "timeout";
                                smallBoundProperties["minTime"] = "timeout";
                            }

                            watch = Stopwatch.StartNew();
                            var maxTimeTask = Task.Run(() => GurobiHeuristics.Unroll_ComputeL(net,
                                initialPlace,
                                finalPlace,
                                1,
                                (int)an));
                            if (maxTimeTask.Wait(TimeSpan.FromSeconds(15)))
                            {
                                smallBoundProperties["timeForComputingMaxTime"] = watch.ElapsedMilliseconds;
                                smallBoundProperties["maxTime"] = maxTimeTask.Result;
                            }
                            else
                            {
                                smallBoundProperties["timeForComputingMaxTime"] = "timeout";
                                smallBoundProperties["maxTime"] = "timeout";
                            }

                            watch = Stopwatch.StartNew();
                            var soundnessTask = Task.Run(() => GurobiHeuristics.Unroll_CheckSoundness(net,
                                initialPlace,
                                finalPlace,
                                1,
                                (int)an));
                            if (soundnessTask.Wait(TimeSpan.FromSeconds(15)))
                            {
                                smallBoundProperties["timeForComputingSoundnessViaUnrolling"] = watch.ElapsedMilliseconds;
                                smallBoundProperties["isSound"] = soundnessTask.Result;
                            }
                            else
                            {
                                smallBoundProperties["timeForComputingSoundnessViaUnrolling"] = "timeout";
                                smallBoundProperties["isSound"] = "timeout";
                            }
                        }
                    }

                    dataEntry.smallBoundProperties = smallBoundProperties;
                }
            }
            {
                // check for unboundedness
                var watch = Stopwatch.StartNew();

                var (hasBoundedRuns, counterexample) = GurobiHeuristics.CheckForNonnegativeCycle(net);
                watch.Stop();

                dataEntry.hasFastTermination = hasBoundedRuns;
                dataEntry.fastTerminationCounterexample = counterexample == null ? "None" : String.Join(";", counterexample.Where(pair => pair.Value > 0));
                dataEntry.timeForFastTerminationCheck = watch.ElapsedMilliseconds;
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