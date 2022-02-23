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
#if GUROBI
            SoundnessViaTransitionBenchmarkEntry entry = new SoundnessViaTransitionBenchmarkEntry();
            entry.checkedIndex = options.index;

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

            Place final = sinks.First();

            net = net.ShortCircuit(initial, final);

            // will be updated later if a counterexample is encountered
            entry.allTransitionsExpressible = true;

            Stopwatch queryWatch = Stopwatch.StartNew();
            (bool result, Transition counterexample) = CheckAllCoverableTransitionsExpressible(net, initialMarking);
            queryWatch.Stop();

            entry.timeInQuery = queryWatch.ElapsedMilliseconds;
            entry.allTransitionsExpressible = result;
            entry.counterexampleTransition = counterexample == null ? null : counterexample.Name;

            Console.WriteLine(entry.ToJSON());
#else
            Console.WriteLine("Gurobi needs to be installed, and the compile flag set, to use this heuristic! See the README for more information.");
            System.Environment.Exit(5);

#endif
        }

#if GUROBI
        /// <summary>
        /// Checks whether all transitions in the input net that are *not* reverse-expressible are also *not* coverable from the initial marking.
        /// </summary>
        /// <returns>A tuple containing the analysis results, and if it is false, which transition is the counterexample.</returns>
        public static (bool result, Transition counterexample) CheckAllCoverableTransitionsExpressible(PetriNet net, Marking initialMarking)
        {
            HashSet<Transition> coverableTransitions = new HashSet<Transition>();

            Func<UpdateTransition, bool> expressibilityChecker = GurobiHeuristics.InitializeTransitionExpressibilityChecker(net);
            foreach (UpdateTransition transitionToCheck in net.Transitions)
            {
                bool expressible = expressibilityChecker(transitionToCheck);

                if (!expressible)
                {
                    (bool isCoverable, IEnumerable<Transition> usedTransitions) = PetriNetUtils.CheckTransitionCoverable(net, initialMarking, transitionToCheck);
                    if (isCoverable)
                    {
                        return (false, transitionToCheck);

                    }
                }
            }
            return (true, null);
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
#endif

        public static void VerifyContinuousSoundness(ContinuousSoundnessOptions options)
        {
            ContinuousSoundnessBenchmarkEntry benchmarkEntry = new ContinuousSoundnessBenchmarkEntry();

            NetParser parser = ParserPicker.ChooseNetParser(options.netFilePath);
            (PetriNet net, Marking initialMarking) = parser.ReadNet(options.netFilePath);

            benchmarkEntry.numberOfPlaces = net.Places.Count;
            benchmarkEntry.numberOfTransitions = net.Transitions.Count;

            Stopwatch queryWatch = Stopwatch.StartNew();
#if GUROBI
            Stopwatch zBoundednessWatch = Stopwatch.StartNew();
            var (isZBounded, zBoundednessCounterexample) = GurobiHeuristics.CheckIntegerUnboundedness(net);
            zBoundednessWatch.Stop();
            benchmarkEntry.timeForZBoundedness = zBoundednessWatch.ElapsedMilliseconds;
            benchmarkEntry.isZBounded = isZBounded;
            if (!isZBounded)
            {
                queryWatch.Stop();
                benchmarkEntry.timeInQuery = queryWatch.ElapsedMilliseconds;
                benchmarkEntry.ZBoundednessCounterexample = String.Join(", ", zBoundednessCounterexample.Where(pair => pair.Value > 0));
                benchmarkEntry.isContinuousSound = false;
                Console.WriteLine(benchmarkEntry.ToJSON());
                return;
            }
#else
            Stopwatch zBoundednessWatch = new Stopwatch();
#endif
            benchmarkEntry.isZBounded = true;
            Stopwatch continuousSoundnessWatch = Stopwatch.StartNew();
            var (isSound, soundnessCounterexample) = Z3Heuristics.IsContinuousSound_ViaContinuousReach(net, initialMarking);
            continuousSoundnessWatch.Stop();
            benchmarkEntry.timeForContinuousSoundness = continuousSoundnessWatch.ElapsedMilliseconds;
            queryWatch.Stop();

            benchmarkEntry.isContinuousSound = isSound;
            if (!isSound)
            {
                benchmarkEntry.continuousSoundnessCounterexample = String.Join(", ", soundnessCounterexample.Where(pair => pair.Value > 0));
            }

            benchmarkEntry.timeInQuery = queryWatch.ElapsedMilliseconds;
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