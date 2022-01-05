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
#if GUROBI
using static Petri.GurobiHeuristics;
#endif
using Microsoft.Z3;

namespace PetriTool
{
    public class TransformationEntryPoints
    {
        public static void ChainNetsEntryPoint(ChainNetsOptions options)
        {
            // holds tuples of <net, initialPlace, finalPlace> in order to facilitate chaining
            List<Tuple<PetriNet, Place, Place>> netTuples = new List<Tuple<PetriNet, Place, Place>>();
            Console.WriteLine("Reading nets to chain");
            foreach (string path in options.nets)
            {
                Console.WriteLine(path);
                NetParser netParser = ParserPicker.ChooseNetParser(path);
                (PetriNet net, Marking initialMarking) = netParser.ReadNet(path);
                (bool isWF, IEnumerable<Place> sources, IEnumerable<Place> sinks) = net.IsWorkflowNet();
                if (!isWF)
                {
                    throw new WorkflowException("Net is not a workflow net!");
                }
                Place initial = sources.First();
                Place final = sinks.First();

                netTuples.Add(new Tuple<PetriNet, Place, Place>(net, initial, final));
            }

            (PetriNet chainedNet, Marking firstInitialMarking) = ChainNets(netTuples);

            WriteNet(options.outputFormat, options.outputFilePath, chainedNet, firstInitialMarking, new List<MarkingWithConstraints>());
        }

        private static (PetriNet net, Marking initialMarking) ChainNets(List<Tuple<PetriNet, Place, Place>> netTuples)
        {
            (PetriNet curNet, Place curInitial, Place curFinal) = netTuples[0];
            // copy net to avoid modifying the input net
            curNet = new PetriNet(curNet);

            for (int i = 1; i < netTuples.Count(); i++)
            {
                (PetriNet nextNet, Place nextInitial, Place nextFinal) = netTuples[i];
                Dictionary<Place, Place> placeMapping = curNet.Join(nextNet);
                UpdateTransition chainTransition = curNet.AddNewTransition("net_" + (i - 1).ToString() + "_to_net_" + i.ToString());
                chainTransition.AddPlaceToPre(curFinal, 1);
                chainTransition.AddPlaceToPost(placeMapping[nextInitial], 1);

                curFinal = placeMapping[nextFinal];
            }

            Marking resultInitialMarking = new Marking();
            resultInitialMarking[curInitial] = 1;

            return (curNet, resultInitialMarking);
        }

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

        /// <summary>
        /// If the options specify, then this methods 
        /// removes transitions whose postsets can never be enabled from the initial marking.
        /// This is achieved by checking coverability using a-star plus the marking equation over Q in Gurobi.
        /// If options do no specify that, then the method simply returns the input net.
        /// Does not modify the input net, but rather returns a copy of it.
        /// </summary>
        /// <returns></returns>
        public static PetriNet RemoveUncoverableTransitions(RemoveUncoverableTransitionsOptions options, PetriNet net, Marking initialMarking)
        {
            if (!options.removeUncoverableTransitions)
            {
                return net;
            }

            return RemoveUncoverableTransitions(net, initialMarking);
        }

        public static PetriNet RemoveUncoverableTransitions(PetriNet original_net, Marking initialMarking)
        {
            // copy net to avoid modifying the input net
            PetriNet net = new PetriNet(original_net);

            HashSet<Transition> uncheckedTransitions = net.Transitions.ToHashSet();
            UpdateTransition transitionToCheck;

            while ((transitionToCheck = (UpdateTransition)uncheckedTransitions.FirstOrDefault()) != null)
            {
                (bool isCoverable, IEnumerable<Transition> usedTransitions) = PetriNetUtils.CheckTransitionCoverable(net, initialMarking, transitionToCheck);
                if (!isCoverable)
                {
                    net.RemoveTransition(transitionToCheck);
                }
                else
                {
                    foreach (Transition checkedTransition in usedTransitions)
                    {
                        uncheckedTransitions.Remove(checkedTransition);
                    }
                    uncheckedTransitions.Remove(transitionToCheck);
                }
            }

            return net;
        }

        public static void TranslateWFNet(TranslateWFOptions options)
        {
            if (options.k <= 0)
            {
                throw new ArgumentException("k needs to be a positive integer, but input was k=" + options.k.ToString());
            }

            if (options.k > 1 && options.translationMode == PetriTool.WorkflowTranslation.StructuralReachability)
            {
                throw new ArgumentException("k < 1 is not compatible with mode StructuralReachability!");
            }

            NetParser parser = ParserPicker.ChooseNetParser(options.netFilePath);
            (PetriNet net, _) = parser.ReadNet(options.netFilePath);

            (bool isWF, IEnumerable<Place> sources, IEnumerable<Place> sinks) = net.IsWorkflowNet();

            if (!isWF)
            {
                throw new WorkflowException("Not a workflow net! Sources: "
                + (sources == null ? "null" : String.Join(", ", sources)) + "; Sinks: "
                + (sinks == null ? "null" : String.Join(", ", sinks)));
            }

            Place initial = sources.First();
            Place final = sinks.First();

            if (options.k > 1)
            {
                Place newInitial = net.AddNewPlace("pAuxInit");
                Place newFinal = net.AddNewPlace("pAuxFinal");

                Transition auxInitTransition = net.AddNewTransition("tAuxInit");
                Transition auxFinalTransition = net.AddNewTransition("tAuxFinal");

                auxInitTransition.AddPlaceToPre(newInitial, 1);
                auxInitTransition.AddPlaceToPost(initial, options.k);

                auxFinalTransition.AddPlaceToPre(final, options.k);
                auxFinalTransition.AddPlaceToPost(newFinal, 1);
            }

            List<MarkingWithConstraints> resultTargetMarkings = null;
            PetriNet resultNet = null;

            Marking initialMarking = null;

            switch (options.translationMode)
            {
                case WorkflowTranslation.Soundness:
                case WorkflowTranslation.Coverability:
                case WorkflowTranslation.Reachability:
                    {
                        initialMarking = new Marking();
                        initialMarking[initial] = 1;

                        Marking finalMarking = new Marking();
                        finalMarking[final] = 1;

                        MarkingWithConstraints finalMarkingWithConstraints = options.translationMode switch
                        {
                            // just wraps finalMarking in marking with constraints; constraints will be ignored for soundness later
                            PetriTool.WorkflowTranslation.Soundness => MarkingWithConstraints.AsReachability(finalMarking, net),
                            PetriTool.WorkflowTranslation.Coverability => MarkingWithConstraints.AsCoverability(finalMarking),
                            PetriTool.WorkflowTranslation.Reachability => MarkingWithConstraints.AsReachability(finalMarking, net)
                        };

                        resultTargetMarkings = new List<MarkingWithConstraints>() { finalMarkingWithConstraints };

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

                        initialMarking = new Marking();
                        initialMarking[p1] = 1;

                        resultNet = net;

                        Marking finalMarking = new Marking();
                        finalMarking[p3] = 1;

                        resultTargetMarkings = new List<MarkingWithConstraints>() { MarkingWithConstraints.AsReachability(finalMarking, net) };
                        break;
                    }
            }


            FileInfo fi = new FileInfo(options.outputFilePath);
            if (!fi.Directory.Exists)
            {
                System.IO.Directory.CreateDirectory(fi.DirectoryName);
            }

            switch (options.outputFormat)
            {
                case OutputFormat.Lola:
                    {
                        using (StreamWriter file = new StreamWriter(options.outputFilePath + ".lola", append: false))
                        {
                            file.Write(resultNet.ToLola(initialMarking));
                        }
                        using (StreamWriter file = new StreamWriter(options.outputFilePath + ".formula", append: false))
                        {
                            // For soundness, transforms EG (o = 1) to AFEG (o = 1)
                            file.Write((options.translationMode == WorkflowTranslation.Soundness ? "AG" : "") + MarkingWithConstraints.ListToLola(resultTargetMarkings));
                        }
                        return;
                    }
                case OutputFormat.Dotspec:
                    {
                        using (StreamWriter file = new StreamWriter(options.outputFilePath + ".spec", append: false))
                        {
                            file.Write(resultNet.ToDotspec(initialMarking, resultTargetMarkings));
                        }
                        return;
                    }
                case OutputFormat.TTS:
                    {
                        if (resultTargetMarkings.Count > 1)
                        {
                            throw new ArgumentException(".tts format expects a single target marking, but the translation resulted in multiple!");
                        }
                        using (StreamWriter file = new StreamWriter(options.outputFilePath + ".tts", append: false))
                        {
                            file.Write(resultNet.ToTTS_PN());
                        }
                        using (StreamWriter writer = new StreamWriter(options.outputFilePath + ".prop", append: false))
                        {
                            writer.Write(resultTargetMarkings.First().ToTTS_PN(net.GetPlaceToCounterNumDict(), initialMarking: false));
                        }
                        using (StreamWriter writer = new StreamWriter(options.outputFilePath + ".init", append: false))
                        {
                            writer.Write(initialMarking.ToTTS_PN(net.GetPlaceToCounterNumDict(), initialMarking: true));
                        }
                        return;
                    }
                case OutputFormat.PNML:
                    WritePNMLToFile(options.outputFilePath + ".pnml", net, initialMarking);
                    return;
                case OutputFormat.CGraph:
                    WriteCGraphToFile(options.outputFilePath + ".xml", net, initialMarking);
                    return;
                default:
                    throw new ArgumentException("Did not understand output format: " + options.outputFormat);
            }
        }

        public static void Translate(TranslationOptions options)
        {
            SearchBenchmarkEntry entry = new SearchBenchmarkEntry();

            NetParser parser = ParserPicker.ChooseNetParser(options.netFilePath);
            (PetriNet net, Marking initialMarking) = parser.ReadNet(options.netFilePath);

            List<MarkingWithConstraints> targetMarkings = null;

            if (options.formulaFilePath != null)
            {
                FormulaParser formulaParser = ParserPicker.ChooseFormulaParser(options.formulaFilePath);
                targetMarkings = formulaParser.ReadFormula(options.formulaFilePath);
            }

            if (options.backwardPrune || options.forwardPrune)
            {
                Console.WriteLine("Pruning instance");
                Pruning.Prune(null, ref net, ref initialMarking, ref targetMarkings, options.forwardPrune, options.backwardPrune);
            }

            if (net.Places.Count == 0 || net.Transitions.Count == 0 ||
            (targetMarkings != null && targetMarkings.Count == 0))
            {
                Console.WriteLine("Instance has no places or transitions left, or final marking is empty. Not doing any output...");
                return;
            }

            WriteNet(options.outputFormat, options.outputFilePath, net, initialMarking, targetMarkings);
            WriteFormula(options.outputFormat, options.outputFilePath, net, initialMarking, targetMarkings);
        }

        public static void ReplaceArcWeights(ReplaceArcWeightOptions options)
        {
            NetParser parser = ParserPicker.ChooseNetParser(options.netFilePath);

            (PetriNet net, Marking initialMarking) = parser.ReadNet(options.netFilePath);
            PetriNet newNet = net.ReplaceArcWeights();

            WriteNet(options.outputFormat, options.outputFilePath, newNet, initialMarking, null);
        }

        private static void WriteFormula(OutputFormat outputFormat, string outputFilePath, PetriNet net, Marking initialMarking, List<MarkingWithConstraints> targetMarkings)
        {
            switch (outputFormat)
            {
                case (OutputFormat.Lola):
                    using (StreamWriter file = new StreamWriter(outputFilePath + ".formula", append: false))
                    {
                        file.Write(MarkingWithConstraints.ListToLola(targetMarkings));
                    }
                    break;
                case (OutputFormat.TTS):
                    using (StreamWriter writer = new StreamWriter(outputFilePath + ".prop"))
                    {
                        writer.Write(targetMarkings.First().ToTTS_PN(net.GetPlaceToCounterNumDict(), initialMarking: false));
                    }
                    using (StreamWriter writer = new StreamWriter(outputFilePath + ".init"))
                    {
                        writer.Write(initialMarking.ToTTS_PN(net.GetPlaceToCounterNumDict(), initialMarking: true));
                    }
                    break;
                default:
                    Console.WriteLine("Could not write target file for output format \"" + outputFormat + "\"");
                    Console.WriteLine("It may be the case that the target file is already in the net file for the chosen format.");
                    break;
            }
        }

        private static void WriteNet(OutputFormat outputFormat,
                                     string outputFilePath,
                                     PetriNet net,
                                     Marking initialMarking,
                                     List<MarkingWithConstraints> targetMarkings)
        {
            switch (outputFormat)
            {
                case (OutputFormat.Dotspec):
                    TranslateNetToDotspec(net, initialMarking, targetMarkings, outputFilePath + ".spec");
                    break;
                case (OutputFormat.Lola):
                    using (StreamWriter file = new StreamWriter(outputFilePath + ".lola", append: false))
                    {
                        file.Write(net.ToLola(initialMarking));
                    }
                    break;
                case (OutputFormat.TTS):
                    using (StreamWriter writer = new StreamWriter(outputFilePath + ".tts"))
                    {
                        writer.Write(net.ToTTS_PN());
                    }
                    break;
                case (OutputFormat.PNML):
                    WritePNMLToFile(outputFilePath + ".pnml", net, initialMarking);
                    break;
                case (OutputFormat.CGraph):
                    WriteCGraphToFile(outputFilePath + ".xml", net, initialMarking);
                    break;
                default:
                    Console.WriteLine("Could not understand file format \"" + outputFormat + "\"");
                    System.Environment.Exit(3);
                    break;
            }
        }

        public static void WritePNMLToFile(string filepath, PetriNet net, Marking initialMarking)
        {
            string netName = filepath.Split("/").Last();
            Regex discardNonAlphanums = new Regex("[^a-zA-Z0-9 -]");
            netName = discardNonAlphanums.Replace(netName, "");

            net.ToPNML(initialMarking, netName, filepath);
        }

        public static void WriteCGraphToFile(string filepath, PetriNet net, Marking initialMarking)
        {
            string netName = filepath.Split("/").Last();
            Regex discardNonAlphanums = new Regex("[^a-zA-Z0-9 -]");
            netName = discardNonAlphanums.Replace(netName, "");

            net.ToCGraph(netName, filepath);
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


    }
}