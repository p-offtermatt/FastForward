using System.Collections.Generic;
using System;
using System.Linq;
using SearchAlgorithms;
using Benchmark;
using BoolForms;
using System.Diagnostics;
using System.Text.RegularExpressions;
using HeuristicFrontier;

namespace Petri
{
    public static class PetriNetUtils
    {

        /// <summary>
        ///  Searches for Transitions that were artificially
        /// added to represent an upward closed initial marking.
        /// These transitions look like this:
        /// TRANSITION t_l0
        /// CONSUME ;
        /// PRODUCE l0: 1;
        /// </summary>
        /// <param name="net">The net on which to identify artifical transitions.</param>
        /// <returns>The set of artifical transitions.</returns>
        public static HashSet<UpdateTransition> IdentifyArtificalLoopTransitions(
            PetriNet net
        )
        {
            HashSet<UpdateTransition> result = new HashSet<UpdateTransition>();
            foreach (UpdateTransition transition in net.Transitions.Where(transition => transition is UpdateTransition))
            {
                var match = Regex.Match(transition.Name, @"t_([^\s]+)");
                if (match.Success)
                {
                    string potentialPlaceName = match.Groups[1].Value;
                    if (transition.Pre.Count != 0)
                    {
                        continue;
                    }

                    if (transition.Post.Count != 1)
                    {
                        continue;
                    }

                    Place place = transition.Post.First().Key;
                    int value = transition.Post.First().Value;

                    if (place.Name != potentialPlaceName || value != 1)
                    {
                        continue;
                    }

                    result.Add(transition);
                }
            }
            return result;
        }

        public static Tuple<List<Transition>, List<Transition>> FindLoopsWithAStar(
            PetriNet handleNet,
            PetriNet cycleNet,
            Marking initialMarking,
            Func<Marking, float?> handleHeuristic,
            Func<IEnumerable<MarkingWithConstraints>, Func<Marking, float?>> loopHeuristicFactory,
            IEnumerable<Place> indicatorPlaces = null,
            LoopFindingBenchmarkEntry diagnostics = null,
            bool verbose = false)
        {
            Func<Marking, bool> handleTargetEvaluation = marking => handleHeuristic(marking) == 0;

            Stopwatch watch = null;
            if (diagnostics != null)
            {
                watch = Stopwatch.StartNew();
            }

            if (verbose) Console.WriteLine("Searching for Handle...");
            (_, List<Transition> handle) = AStarAlgorithm.FindShortestPath(
                initialNode: initialMarking,
                targetEvaluationFunction: handleTargetEvaluation,
                actions: handleNet.Transitions,
                successorFunction: PetriNetUtils.DistanceSuccessorFunction,
                heuristicFunction: handleHeuristic,
                reachabilityTestFunction: null,
                diagnostics: diagnostics,
                verbose);

            if (verbose && handle != null) Console.WriteLine("Found handle: " + String.Join(", ", handle.Select(t => t.Name)));
            if (verbose && handle == null) Console.WriteLine("Found no handle!");


            if (diagnostics != null)
            {
                watch.Stop();
                diagnostics.timeFindingHandle = watch.ElapsedMilliseconds;
            }

            if (handle == null)
            {
                return new Tuple<List<Transition>, List<Transition>>(null, null);
            }

            Marking cycleInitialMarking = new Marking(initialMarking);
            cycleInitialMarking.FireFiringSequence(handle);

            MarkingWithConstraints enrichedCycleInitialMarking = MarkingWithConstraints.MakeCoveringMarking(cycleInitialMarking);

            Func<Marking, bool> cycleTargetEvaluation = marking =>
            {
                // cycle must reach some marking not smaller than the initial cycle marking and which marks all indicator places
                if (!(cycleInitialMarking <= marking))
                {
                    return false;
                }
                if (indicatorPlaces != null)
                {
                    if (!indicatorPlaces.All(place => marking.GetValueOrDefault(place, 0) > 0))
                    {
                        return false;
                    }
                }
                return true;
            };

            Func<Marking, float?> loopHeuristic = loopHeuristicFactory(new[] { enrichedCycleInitialMarking });

            if (verbose) Console.WriteLine("Heuristic of Initial Marking on Loop: " + loopHeuristic(cycleInitialMarking));


            // A Star has to start at the successors of the first marking on the cycle, otherwise it is immediately done since potentially initial=target
            Dictionary<Marking, Transition> cycleSuccessors = new Dictionary<Marking, Transition>();
            foreach (Transition transition in cycleNet.Transitions)
            {
                HashSet<Marking> successorMarkings = PetriNetUtils.DistancelessSuccessorFunction(cycleInitialMarking, transition);
                foreach (Marking successor in successorMarkings)
                {
                    cycleSuccessors[successor] = transition;
                }
            }

            if (cycleSuccessors.Count == 0)
            {
                return new Tuple<List<Transition>, List<Transition>>(null, null);
            }

            if (diagnostics != null)
            {
                watch = Stopwatch.StartNew();
            }

            (List<Marking> cycleMarkings, List<Transition> cycleTransitions) = AStarAlgorithm.FindShortestPathMultipleInitialNodes(
                initialNodes: cycleSuccessors.Keys,
                targetEvaluationFunction: cycleTargetEvaluation,
                actions: cycleNet.Transitions,
                successorFunction: PetriNetUtils.DistanceSuccessorFunction,
                heuristicFunction: loopHeuristic,
                reachabilityTestFunction: null,
                diagnostics: diagnostics,
                verbose);

            if (diagnostics != null)
            {
                watch.Stop();
                diagnostics.timeFindingLoop = watch.ElapsedMilliseconds;
            }

            // since a star only went from successors of first marking on cycle to last marking on cycle, we are missing the first
            // transition on the cycle => reattach it at the front
            if (cycleMarkings != null && cycleMarkings.Count != 0)
            {
                Marking firstMarkingOnCycle = cycleMarkings.First();
                Transition firstTransition = cycleSuccessors[firstMarkingOnCycle];
                cycleTransitions = cycleTransitions.Prepend(firstTransition).ToList();
            }

            Marking cycleStepMarking = new Marking(cycleInitialMarking);
            if (cycleTransitions != null)
            {
                cycleStepMarking.FireFiringSequence(cycleTransitions);

            }
            if (diagnostics != null)
            {
                diagnostics.cycleInitialMarking = cycleInitialMarking != null ? cycleInitialMarking.ToString() : "";
                diagnostics.cycleStepMarking = cycleStepMarking != null ? cycleStepMarking.ToString() : "";
            }

            return new Tuple<List<Transition>, List<Transition>>(handle, cycleTransitions);
        }

        public static List<Transition> PetriNetAStar(PetriNet net, Marking initialMarking,
            MarkingWithConstraints targetMarking, Func<Marking, float?> heuristic,
            Func<Marking, bool> reachabilityCheck = null, SearchBenchmarkEntry diagnostics = null)
        {
            return PetriNetAStar(net, initialMarking, new List<MarkingWithConstraints>() { targetMarking }, heuristic, reachabilityCheck, diagnostics);
        }

        public static List<Transition> PetriNetAStar(PetriNet net, Marking initialMarking,
            List<MarkingWithConstraints> targetMarkings, Func<Marking, float?> heuristic,
            Func<Marking, bool> reachabilityCheck = null, SearchBenchmarkEntry diagnostics = null)
        {
            // if there are no target markings, we can return null before even trying A*
            if (targetMarkings.Count == 0)
            {
                return null;
            }

            Func<Marking, bool> targetEvaluationFunction =
                marking => targetMarkings.Any(x => x.SatisfiedByMarking(marking));

            // if there are no transitions, we just evaluate if initial marking satisfies a target
            if (net.Transitions.Count == 0)
            {
                return targetEvaluationFunction(initialMarking) ? new List<Transition>() : null;
            }

            Func<Marking, Transition, Tuple<Marking, float>[]> successorFunction;

            if (net is PetriNetWithCapacities)
            {
                var netWithCapas = net as PetriNetWithCapacities;
                successorFunction = (marking, transition) => PetriNetUtils.DistanceSuccessorFunctionWithCapacities(netWithCapas, marking, transition);
            }
            else
            {
                successorFunction = PetriNetUtils.DistanceSuccessorFunction;
            }

            IEnumerable<Transition> actions = net.GetTransitions();
            (_, List<Transition> shortestPath) = AStarAlgorithm.FindShortestPath(initialMarking, targetEvaluationFunction,
                actions, successorFunction, heuristic, reachabilityCheck, diagnostics);
            return shortestPath;
        }

        public static List<Transition> PetriNetAStarModelChecking(PetriNet net, Marking initialMarking, BooleanExpression targetExpression, Func<Marking, float?> heuristic, Func<Marking, bool> reachabilityCheck = null, SearchBenchmarkEntry diagnostics = null)
        {
            throw new NotImplementedException();
            // TODO
            // Func<Marking, bool> targetEvaluationFunction = marking => 
        }

        public static List<Transition> PetriNetBestFirstSearch(PetriNet net, Marking initialMarking,
            List<MarkingWithConstraints> targetMarkings, Func<Marking, float?> heuristic,
            Func<Marking, bool> reachabilityCheck = null, SearchBenchmarkEntry diagnostics = null)
        {
            // if there are no target markings, we can return null before even trying A*
            if (targetMarkings.Count == 0)
            {
                return null;
            }

            Func<Marking, bool> targetEvaluationFunction =
                marking => targetMarkings.Any(x => x.SatisfiedByMarking(marking));

            // if there are no transitions, we just evaluate if initial marking satisfies a target
            if (net.Transitions.Count == 0)
            {
                return targetEvaluationFunction(initialMarking) ? new List<Transition>() : null;
            }

            IEnumerable<Transition> actions = net.GetTransitions();
            (_, List<Transition> shortestPath) = AStarAlgorithm.FindShortestPath(initialMarking, targetEvaluationFunction,
                actions, PetriNetUtils.BestFirstSuccessorFunction, heuristic, reachabilityCheck, diagnostics);
            return shortestPath;
        }

        public static List<Transition> UnityFrontierSearch(PetriNet net, Marking initialMarking,
            List<MarkingWithConstraints> targetMarkings, HeuristicFrontier<Marking> frontier,
            Func<Marking, Transition, Tuple<Marking, float>[]> successorFunction,
            SearchBenchmarkEntry diagnostics = null)
        {
            // if there are no target markings, we can return null before even trying A*
            if (targetMarkings.Count == 0)
            {
                return null;
            }

            Func<Marking, bool> targetEvaluationFunction =
                marking => targetMarkings.Any(x => x.SatisfiedByMarking(marking));

            // if there are no transitions, we just evaluate if initial marking satisfies a target
            if (net.Transitions.Count == 0)
            {
                return targetEvaluationFunction(initialMarking) ? new List<Transition>() : null;
            }

            IEnumerable<Transition> actions = net.GetTransitions();
            (_, List<Transition> shortestPath) = UnityFrontierSearchWrapper.FindShortestPath(initialMarking,
            targetEvaluationFunction, actions, successorFunction, frontier, diagnostics, false);
            return shortestPath;
        }

        public static List<Transition> PetriNetBackwardsCover(
            PetriNet net, Marking initialMarking,
            List<MarkingWithConstraints> targetMarkings,
            Func<Marking, float?> heuristic,
            SearchBenchmarkEntry diagnostics = null)
        {
            // if there are no target markings, we can return null before even trying A*
            if (targetMarkings.Count == 0)
            {
                return null;
            }

            Func<Marking, bool> targetEvaluationFunction =
                marking => targetMarkings.Any(x => x.SatisfiedByMarking(marking));
            IEnumerable<Transition> actions = net.GetTransitions();
            List<Transition> shortestPath =
                BackwardsCoverability.DetermineCoverabilityWithQueue(net, initialMarking, targetMarkings, heuristic, diagnostics);
            return shortestPath;
        }

        public static Func<Marking, bool> GetTargetEvaluationFunction(List<MarkingWithConstraints> targetMarkings)
        {
            Func<Marking, bool> targetEvaluationFunction =
                marking => targetMarkings.Any(x => x.SatisfiedByMarking(marking));
            return targetEvaluationFunction;
        }

        public static HashSet<Marking> DistancelessSuccessorFunction(Marking currentMarking, Transition transition)
        {
            return DistanceSuccessorFunction(currentMarking, transition).Select(x => x.Item1).ToHashSet();
        }

        public static Tuple<Marking, float>[] DistanceSuccessorFunction(Marking currentMarking, Transition transition)
        {
            HashSet<Marking> successors = currentMarking.Fire(transition);
            return successors.Select(marking => new Tuple<Marking, float>(marking, 1)).ToArray();
        }

        public static Tuple<Marking, float>[] DistanceSuccessorFunctionWithCapacities(PetriNetWithCapacities net, Marking currentMarking, Transition transition)
        {
            var tmp_result = DistanceSuccessorFunction(currentMarking, transition);

            tmp_result = tmp_result.Where(tuple => tuple.Item1.All(kvPair => kvPair.Value < net.Capacities[kvPair.Key])).ToArray();
            return tmp_result;
        }

        public static Tuple<Marking, float>[] BestFirstSuccessorFunction(Marking currentMarking, Transition transition)
        {
            HashSet<Marking> successors = currentMarking.Fire(transition);
            return successors.Select(marking => new Tuple<Marking, float>(marking, 0)).ToArray();
        }

        public static Tuple<PetriNet, IEnumerable<Place>> CreateCycleNet(PetriNet handleNet, IEnumerable<IEnumerable<Transition>> atleastOneTimeSets, IEnumerable<Transition> zeroTimesSet)
        {
            HashSet<Place> indicatorPlaces = new HashSet<Place>();
            PetriNet cycleNet = new PetriNet(handleNet);
            cycleNet.Transitions = cycleNet.Transitions.Where(transition => !zeroTimesSet.Contains(transition)).ToList();

            int i = 0;
            foreach (IEnumerable<Transition> atleastOneTimeSet in atleastOneTimeSets)
            {
                i += 1;
                Place indicatorPlace = new Place("atleastOneTimeSetPlace_" + i);
                cycleNet.Places.Add(indicatorPlace);
                foreach (string transitionName in atleastOneTimeSet.Select(t => t.Name))
                {
                    Transition modifiedTransition = cycleNet.GetTransitionByName(transitionName);
                    switch (modifiedTransition)
                    {
                        case UpdateTransition update:
                            update.Post[indicatorPlace] = 1;
                            break;
                        case TransferTransition transfer:
                            transfer.UpdateBehaviour.Post[indicatorPlace] = 1;
                            break;

                    }
                    cycleNet.Transitions[cycleNet.Transitions.FindIndex(t => t.Name == transitionName)] = modifiedTransition;
                }
                indicatorPlaces.Add(indicatorPlace);
            }
            return new Tuple<PetriNet, IEnumerable<Place>>(cycleNet, indicatorPlaces);
        }

        /// <summary>
        /// Yields all the possible ways that 'number' can be split into 'pieces' many numbers such that n_1 + ... + n_pieces = number.
        /// </summary>
        /// <param name="number">The target number that the pieces should sum up to.</param>
        /// <param name="pieces">How many pieces should be returned.</param>
        /// <returns>An enumerable of lists, where each list in the enumerable holds one of the possible splits.</returns>
        public static IEnumerable<List<int>> SplitNumberIntoPieces(int number, int pieces)
        {
            if (pieces == 1)
            {
                yield return new List<int> { number };
            }

            if (pieces > 1)
            {
                int firstPiece = 0;

                while (firstPiece <= number)
                {
                    foreach (List<int> possibility in SplitNumberIntoPieces(number - firstPiece, pieces - 1))
                    {
                        possibility.Add(firstPiece);
                        yield return possibility;
                    }
                    firstPiece++;
                }
            }
        }

        /// <summary>
        /// Escapes all symbols that dotspec can not parse in the input string.
        /// For example, replaces '.' in place names with 'dot'.
        /// Note that if place names are not sufficiently distinct, this will lead to problems.
        /// </summary>
        /// <param name="inputString"></param>
        /// <returns></returns>
        public static String EscapeCharsDotspec(String inputString)
        {
            return inputString.Replace(".", "dot")
                              .Replace("-", "dash")
                              .Replace("[", "leftbracket")
                              .Replace("]", "rightbracket")
                              .Replace("$", "dollar");
        }

        /// <summary>
        /// Escapes all symbols that Lola can not parse in the input string.
        /// For example, replaces ':' in place names with '_colon_'.
        /// Note that if place names are not sufficiently distinct, this will lead to problems.
        /// </summary>
        public static String EscapeCharsLola(string input)
        {
            return input.Replace(":", "_colon_")
                        .Replace(",", "_comma_")
                        .Replace(" ", "_space_")
                        .Replace("-", "_dash_")
                        .Replace(".", "_dot_")
                        .Replace("*", "_star_")
                        .Replace("\"", "_quote_")
                        .Replace("(", "_openbracket_")
                        .Replace(")", "_closebracket_");
        }
    }
}