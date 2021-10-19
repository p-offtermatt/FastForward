using System.Collections.Generic;
using System;
using System.Linq;
using System.IO;
using System.Diagnostics;
using Benchmark;

namespace Petri
{
    public class Pruning
    {

        public static void Prune(SearchBenchmarkEntry benchmarkEntry,
                                  ref PetriNet net,
                                  ref Marking initialMarking,
                                  ref List<MarkingWithConstraints> targetMarkings,
                                  bool forward = true,
                                  bool backward = false)
        {
#if !COMP
            Stopwatch forwardPruningWatch = new Stopwatch();
            Stopwatch backwardPruningWatch = new Stopwatch();
#endif

            if (forward)
            {
#if !COMP
                forwardPruningWatch.Start();
#endif

                (net, initialMarking, targetMarkings) =
                    Pruning.PruneWithZeroPlaceHeuristic(net, initialMarking, targetMarkings);
#if !COMP
                forwardPruningWatch.Stop();

                if (benchmarkEntry != null)
                {
                    benchmarkEntry.numberOfPlacesAfterForwardPruning = net.Places.Count;
                    benchmarkEntry.numberOfTransitionsAfterForwardPruning = net.Transitions.Count;
                    benchmarkEntry.timeTakenForwardPruning = forwardPruningWatch.ElapsedMilliseconds;
                }
#endif
            }


            if (backward)
            {
#if !COMP
                backwardPruningWatch.Start();
#endif

                (net, initialMarking, targetMarkings) = Pruning.PruneWithBackwardsZeroPlaceHeuristic(net, initialMarking, targetMarkings);
#if !COMP
                backwardPruningWatch.Stop();

                if (benchmarkEntry != null)
                {
                    benchmarkEntry.timeTakenPruning =
                        forwardPruningWatch.ElapsedMilliseconds +
                        backwardPruningWatch.ElapsedMilliseconds;
                    benchmarkEntry.timeTakenBackwardPruning = backwardPruningWatch.ElapsedMilliseconds;

                    benchmarkEntry.numberOfPlacesAfterBackwardPruning = net.Places.Count;
                    benchmarkEntry.numberOfTransitionsAfterBackwardPruning = net.Transitions.Count;
                }
#endif
            }
        }

        public static Tuple<PetriNet, Marking, List<MarkingWithConstraints>> PruneWithContinuousSupportHeuristic(PetriNet net,
           Marking initialMarking, List<MarkingWithConstraints> targetMarkings)
        {
            HashSet<UpdateTransition> transitionsInContinuousSupport = Z3Heuristics.ComputeContinuousSupport(net, initialMarking, targetMarkings);
            return ReduceNetByTransitions(net, initialMarking, targetMarkings, transitionsInContinuousSupport);
        }

        public static Tuple<PetriNet, Marking, List<MarkingWithConstraints>> ReduceNetByTransitions(PetriNet net, Marking initialMarking, List<MarkingWithConstraints> targetMarkings, IEnumerable<UpdateTransition> reducedTransitions)
        {
            HashSet<Place> reducedPlaces = new HashSet<Place>();

            foreach (UpdateTransition transition in reducedTransitions)
            {
                HashSet<Place> supportPlaces = transition.GetSupportPlaces();
                foreach (Place place in supportPlaces)
                {
                    reducedPlaces.Add(place);
                }
            }

            Marking reducedInitialMarking = ReduceInitialMarking(initialMarking, reducedPlaces);
            List<MarkingWithConstraints> reducedTargetMarkings = ReduceTargetMarkings(targetMarkings, reducedPlaces);
            return new Tuple<PetriNet, Marking, List<MarkingWithConstraints>>(new PetriNet(reducedPlaces, reducedTransitions), reducedInitialMarking, reducedTargetMarkings);
        }

        public static Tuple<PetriNet, Marking, List<MarkingWithConstraints>> ReduceNetByPlaces(PetriNet net, Marking initialMarking, List<MarkingWithConstraints> targetMarkings, IEnumerable<Place> reducedPlaces, bool keepTransitions)
        {
            List<Transition> resultTransitions = new List<Transition>();

            foreach (Transition transition in net.Transitions)
            {
                HashSet<Place> guard = transition.GetGuard().Keys.ToHashSet();
                HashSet<Place> post = transition.GetPostPlaces();

                if (!guard.IsSubsetOf(reducedPlaces))
                {
                    continue;
                }
                // if we don't keep transitions that interact with the place, also remove transitions that have place as post.
                else if (!keepTransitions && post.Any(place => !reducedPlaces.Contains(place)))
                {
                    continue;
                }
                else
                {
                    Transition newTransition = transition.CopyThis();

                    foreach (Place place in post.Where(p => !reducedPlaces.Contains(p)))
                    {
                        newTransition.RemovePlaceFromPost(place);
                    }


                    resultTransitions.Add(newTransition);
                }
            }

            List<MarkingWithConstraints> restrictedTargetMarkings = ReduceTargetMarkings(targetMarkings, reducedPlaces);


            if (!(restrictedTargetMarkings is null) && restrictedTargetMarkings.Count == 0)
            {
                return new Tuple<PetriNet, Marking, List<MarkingWithConstraints>>(
                    new PetriNet(),
                    new Marking(),
                    restrictedTargetMarkings
                );
            }
            PetriNet resultNet = new PetriNet(new List<Place>(reducedPlaces), resultTransitions);
            Marking restrictedInitialMarking = ReduceInitialMarking(initialMarking, reducedPlaces);

            return new Tuple<PetriNet, Marking, List<MarkingWithConstraints>>(
                resultNet,
                restrictedInitialMarking,
                restrictedTargetMarkings
            );
        }

        private static Marking ReduceInitialMarking(Marking initialMarking, IEnumerable<Place> reducedPlaces)
        {
            return new Marking(initialMarking
                .Where(kvPair => reducedPlaces.Contains(kvPair.Key))
                .ToDictionary(kvPair => kvPair.Key, kvPair => kvPair.Value));
        }

        private static List<MarkingWithConstraints> ReduceTargetMarkings(List<MarkingWithConstraints> targetMarkings, IEnumerable<Place> reducedPlaces)
        {
            List<MarkingWithConstraints> restrictedTargetMarkings = new List<MarkingWithConstraints>();

            if (targetMarkings is null)
            {
                return null;
            }
            foreach (MarkingWithConstraints targetMarking in targetMarkings)
            {
                if (targetMarking.Marking.Any(kvPair =>
                {
                    Place place = kvPair.Key;
                    int markingInPlace = kvPair.Value;

                    // place is zero place, but marking says m[place] >= x or
                    // m[place] = x for x != 0 -> target can never be fulfilled
                    return !reducedPlaces.Contains(place) && markingInPlace != 0;
                })
                )
                {
                    // if target cannot be fulfilled, don't add it to the restricted targets
                    continue;
                }

                Marking restrictedMarking = new Marking(targetMarking.Marking
                    .Where(kvPair => reducedPlaces.Contains(kvPair.Key))
                    .ToDictionary(kvPair => kvPair.Key, kvPair => kvPair.Value));

                Constraints restrictedConstraints = new Constraints(
                    targetMarking.Constraints.Where(kvPair => reducedPlaces.Contains(kvPair.Key))
                        .ToDictionary(kvPair => kvPair.Key, kvPair => kvPair.Value)
                );

                restrictedTargetMarkings.Add(new MarkingWithConstraints(restrictedMarking, restrictedConstraints));
            }

            return restrictedTargetMarkings;
        }

        public static Tuple<PetriNet, Marking> PruneWithZeroPlaceHeuristic(PetriNet net, Marking initialMarking)
        {
            (PetriNet resultNet, Marking resultMarking, _) = PruneWithZeroPlaceHeuristic(net, initialMarking, new List<MarkingWithConstraints>());
            return new Tuple<PetriNet, Marking>(resultNet, resultMarking);
        }

        public static Tuple<PetriNet, Marking, List<MarkingWithConstraints>> PruneWithZeroPlaceHeuristic(PetriNet inputNet,
            Marking initialMarking, List<MarkingWithConstraints> targetMarkings)
        {
            List<Place> nonZeroPlaces = ComputeNonZeroPlaces(inputNet, initialMarking).ToList();
            return ReduceNetByPlaces(inputNet, initialMarking, targetMarkings, nonZeroPlaces.ToHashSet(), true);
        }

        public static HashSet<Place> ComputeNonZeroPlaces(PetriNet net, Marking initialMarking)
        {
            HashSet<Place> nonzeroPlaces = initialMarking.GetMarkedPlaces();

            // keep list of unused transitions 
            // order transitions 
            List<Transition> unusedTransitions = net.Transitions.OrderBy(transition => transition.GetPrePlaces().Count).ToList();

            while (unusedTransitions.Count > 0)
            {
                List<Transition> usedTransitionsInThisRound = new List<Transition>();

                foreach (Transition unusedTransition in unusedTransitions)
                {
                    if (nonzeroPlaces.IsSupersetOf(unusedTransition.GetGuard().Select(kvPair => kvPair.Key).ToHashSet()))
                    {
                        // transition is enabled, add its post to the nonzero places
                        foreach (Place place in unusedTransition.GetPostPlaces()) // TODO adjust to transfers
                        {
                            nonzeroPlaces.Add(place);
                        }
                        usedTransitionsInThisRound.Add(unusedTransition);
                    }
                }

                if (usedTransitionsInThisRound.Count == 0)
                {
                    break;
                }

                foreach (Transition transition in usedTransitionsInThisRound)
                {
                    unusedTransitions.Remove(transition);
                }
            }

            return nonzeroPlaces;
        }

        public static Tuple<PetriNet, Marking, List<MarkingWithConstraints>> PruneWithBackwardsZeroPlaceHeuristic(PetriNet net,
            Marking initialMarking, List<MarkingWithConstraints> targetMarkings)
        {
            IEnumerable<Place> nonZeroPlaces = new HashSet<Place>();
            foreach (MarkingWithConstraints targetMarking in targetMarkings)
            {
                nonZeroPlaces = nonZeroPlaces.Union(ComputeBackwardsNonZeroPlacesForTarget(net, targetMarking));
            }

            // TODO fix handling of initial marking in backwards pruning
            return ReduceNetByPlaces(net, initialMarking, targetMarkings, nonZeroPlaces.ToHashSet(), false);
        }

        public static HashSet<Place> ComputeBackwardsNonZeroPlacesForTarget(PetriNet net, MarkingWithConstraints targetMarking)
        {
            HashSet<Place> nonzeroPlaces = net.Places.Where(place => !targetMarking.GetZeroPlaces().Contains(place)).ToHashSet();

            // keep list of unused transitions 
            // order transitions 
            List<Transition> unusedTransitions = net.Transitions.OrderBy(transition => transition.GetPostPlaces().Count).ToList();

            while (unusedTransitions.Count > 0)
            {
                List<Transition> usedTransitionsInThisRound = new List<Transition>();

                foreach (Transition unusedTransition in unusedTransitions)
                {
                    if (nonzeroPlaces.IsSupersetOf(unusedTransition.GetBackwardsGuard().Select(kvPair => kvPair.Key).ToHashSet()))
                    {
                        // transition is backwards, add its pre to the nonzero places
                        foreach (Place place in unusedTransition.GetPrePlaces()) // TODO adjust for transfers
                        {
                            nonzeroPlaces.Add(place);
                        }
                        usedTransitionsInThisRound.Add(unusedTransition);
                    }
                }

                if (usedTransitionsInThisRound.Count == 0)
                {
                    break;
                }

                foreach (Transition transition in usedTransitionsInThisRound)
                {
                    unusedTransitions.Remove(transition);
                }
            }
            return nonzeroPlaces;
        }
    }
}