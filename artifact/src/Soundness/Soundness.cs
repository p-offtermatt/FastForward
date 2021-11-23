using PetriTool;
using Petri;
using System;
using System.Diagnostics;
using Benchmark;
using System.Linq;
using System.Collections.Generic;

namespace Soundness
{
    public static class SoundnessChecker
    {
        /// <summary>
        ///  Verifies soundness by checking whether for each transition t fireable from i: k,
        /// it holds that t^-1 can be expressed by the other transitions of the net (including the short circuit transition)-
        /// Utilizes Gurobi.
        /// </summary>
        public static void VerifySoundnessViaTransition(SoundnessReverseTransitionOptions options)
        {
            SoundnessViaTransitionBenchmarkEntry entry = new SoundnessViaTransitionBenchmarkEntry();
            entry.checkedIndex = options.index;
            Stopwatch queryWatch = Stopwatch.StartNew();

            NetParser parser = ParserPicker.ChooseNetParser(options.netFilePath);
            (PetriNet net, Marking marking) = parser.ReadNet(options.netFilePath);

            entry.numberOfPlaces = net.Places.Count();
            entry.numberOfTransitions = net.Transitions.Count();

            (bool isWF, IEnumerable<Place> sources, IEnumerable<Place> sinks) = net.IsWorkflowNet();
            if (!isWF)
            {
                throw new NotAWorkflowNetException("Net is not a workflow net! Sources: "
                + String.Join("; ", sources) + "\n Sinks: " + String.Join("; ", sinks));
            }

            Place initial = sources.First();
            Marking initialMarking = new Marking();
            initialMarking[initial] = options.index;

            // will be updated later if a counterexample is encountered
            entry.allTransitionsExpressible = true;

            HashSet<Transition> coverableTransitions = new HashSet<Transition>();

            Func<UpdateTransition, bool> expressibilityChecker = GurobiHeuristics.InitializeTransitionExpressibilityChecker(net);
            foreach (UpdateTransition transitionToCheck in net.Transitions)
            {
                bool expressible = expressibilityChecker(transitionToCheck);

                if (!expressible)
                {
                    (bool isCoverable, IEnumerable<Transition> usedTransitions) = UtilityEntrypoints.CheckTransitionCoverable(net, initialMarking, transitionToCheck);
                    if (isCoverable)
                    {
                        entry.allTransitionsExpressible = false;

                        queryWatch.Stop();
                        entry.counterexampleTransition = transitionToCheck.Name;
                        break;
                    }
                }
            }
            entry.timeInQuery = queryWatch.ElapsedMilliseconds;

            Console.WriteLine(entry.ToJSON());
        }

        public static (bool, Transition) CheckTransitionExpressibility(PetriNet net)
        {
            Func<UpdateTransition, bool> transitionCheck = GurobiHeuristics.InitializeTransitionExpressibilityChecker(net);

            foreach (UpdateTransition transition in net.Transitions)
            {
                if (!transitionCheck(transition))
                {
                    return (false, transition);
                }
            }
            return (true, null);
        }

        public static void VerifyContinuousSoundness(ContinuousSoundnessOptions options)
        {
            SoundnessBenchmarkEntry benchmarkEntry = new SoundnessBenchmarkEntry();

            NetParser parser = ParserPicker.ChooseNetParser(options.netFilePath);
            (PetriNet net, Marking initialMarking) = parser.ReadNet(options.netFilePath);

            benchmarkEntry.numberOfPlaces = net.Places.Count;
            benchmarkEntry.numberOfTransitions = net.Transitions.Count;

            Stopwatch queryWatch = Stopwatch.StartNew();
            var (isSound, counterexample) = Z3Heuristics.IsContinuousSound_ViaContinuousReach(net, initialMarking);
            queryWatch.Stop();
            benchmarkEntry.timeInQuery = queryWatch.ElapsedMilliseconds;

            benchmarkEntry.isContinuousSound = isSound;
            if (!isSound)
            {
                benchmarkEntry.counterexampleMarking = String.Join(", ", counterexample.Where(pair => pair.Value > 0));
            }
            Console.WriteLine(benchmarkEntry.ToJSON());
        }

        public static void VerifySoundness(SoundnessOptions options)
        {
            SoundnessBenchmarkEntry benchmarkEntry = new SoundnessBenchmarkEntry();

            NetParser parser = ParserPicker.ChooseNetParser(options.netFilePath);
            (PetriNet net, Marking initialMarking) = parser.ReadNet(options.netFilePath);

            benchmarkEntry.numberOfPlaces = net.Places.Count;
            benchmarkEntry.numberOfTransitions = net.Transitions.Count;

            (bool isWF, IEnumerable<Place> initial, IEnumerable<Place> final) = net.IsWorkflowNet();

            // net is not a workflow net: return error
            if (!isWF)
            {
                throw new NotAWorkflowNetException(initial, final);
            }

            Stopwatch queryWatch = Stopwatch.StartNew();
            (bool isSound, int k) = CheckAnyInRangeSound(net, initial.First(), final.First(), options.startIndex, options.stopIndex);
            queryWatch.Stop();
            benchmarkEntry.timeInQuery = queryWatch.ElapsedMilliseconds;
            benchmarkEntry.isSound = isSound;
            benchmarkEntry.soundNumber = k;

            Console.WriteLine(benchmarkEntry.ToJSON());
        }

        public static Tuple<bool, int> CheckAnyInRangeSound(PetriNet net, Place initialPlace, Place finalPlace, int startIndex, int stopIndex)
        {
            for (int i = startIndex; i <= stopIndex; i++)
            {
                Marking initialMarking = new Marking();
                initialMarking[initialPlace] = i;

                Marking finalMarking = new Marking();
                finalMarking[finalPlace] = i;

                Func<Marking, float?> distanceHeuristic = Z3Heuristics.InitializeQReachabilityHeuristic(net,
                    new List<MarkingWithConstraints> { MarkingWithConstraints.AsReachability(finalMarking, net) });

                Func<Marking, bool> targetEvaluation = marking => !distanceHeuristic(marking).HasValue;

                IEnumerable<Transition> actions = net.GetTransitions();
                Func<Marking, Transition, Tuple<Marking, float>[]> successorFunction = PetriNetUtils.DistanceSuccessorFunction;

                (List<Marking> markingPath, List<Transition> transitionPath) = SearchAlgorithms.AStarAlgorithm.FindShortestPath(
                                    initialMarking,
                                    targetEvaluation,
                                    actions,
                                    successorFunction,
                                    heuristicFunction: marking => 1);
                if (markingPath == null)
                {
                    return new Tuple<bool, int>(true, i);
                }

            }
            return new Tuple<bool, int>(false, 0);
        }
    }
}