#define GUROBI

using System;
using System.Linq;
using System.Collections.Generic;
using Utils;

namespace Petri
{
    public static class StructuralHeuristics
    {

        public static Func<Marking, float?> InitializeSyntacticDistanceHeuristic(
            PetriNet net, List<MarkingWithConstraints> targetMarkings)
        {
            // precompute shortest paths from all places to set of finally marked places of each target
            HashSet<Dictionary<Place, int>> shortestPaths = PrecomputeShortestPathsPerPlacePerTarget(net, targetMarkings);

            float? syntacticDistanceHeuristic(Marking marking)
            {
                // distance of -1 indicates: the token can absolutely never 1) disappear or 2) go to a target place
                HashSet<Place> markedPlaces = marking.Where(kvPair => kvPair.Value > 0).Select(kvPair => kvPair.Key).ToHashSet();
                if (markedPlaces.Count == 0)
                {
                    return 0;
                }

                // per target, need to know the longest path it takes for any token to disappear
                HashSet<int> maxPathPerTarget = shortestPaths.Select(targetPathDict => markedPlaces.Max(place => targetPathDict.GetValueOrDefault(place, int.MaxValue))).ToHashSet();

                if (maxPathPerTarget.Min() == int.MaxValue)
                {
                    return null;
                }
                else
                {
                    // return the distance of the closest target
                    return maxPathPerTarget.Min();
                }

            }

            return syntacticDistanceHeuristic;
        }

# if GUROBI
        public static Func<Marking, float?> InitializeStructuralQReachabilityHeuristicGurobi(
            PetriNet net,
            Marking initialMarking,
            List<MarkingWithConstraints> targetMarkings,
            GurobiConsts.Domains gurobiDomain)
        {
            Dictionary<Place, float?> computedHeuristics = new Dictionary<Place, float?>();

            Func<Place, float?> structuralQReachabilityHeuristicForPlace =
                GurobiHeuristics.InitializePlaceToStructuralQReachabilityDistance(net.Places,
                                                                                  net.Transitions,
                                                                                  initialMarking,
                                                                                  targetMarkings,
                                                                                  gurobiDomain);

            float? structuralDistanceHeuristic(Marking marking)
            {
                float? shortestPathLength = null;
                foreach (Place place in marking.Where(pair => pair.Value > 0).Select(pair => pair.Key))
                {
                    if (!computedHeuristics.ContainsKey(place))
                    {
                        float? distance = structuralQReachabilityHeuristicForPlace(place);
                        computedHeuristics[place] = distance;
                    }

                    if (shortestPathLength == null || (computedHeuristics[place] != null && computedHeuristics[place] > shortestPathLength))
                    {
                        shortestPathLength = computedHeuristics[place];
                    }

                }

                return shortestPathLength;
            }

            return structuralDistanceHeuristic;
        }
# endif
        public static HashSet<Dictionary<Place, int>> PrecomputeShortestPathsPerPlacePerTarget(PetriNet net, List<MarkingWithConstraints> targetMarkings)
        {
            // for target marking t and place p (which is set to =0 in t), shortest path can either go from p to a place that isn't marked =0 in t,
            // or we can go from p to a sink transition (i.e. no outgoing arcs) and from a source transition (i.e. no incoming arcs) to a target


            HashSet<Transition> sinkTransitions = net.GetSinkTransitions();
            HashSet<Place> sinkPlaces = sinkTransitions.Count == 0 ? new HashSet<Place>() : sinkTransitions.Select(transition => transition.GetPrePlaces())
                                                       .Aggregate((acc, next) => acc.Union(next).ToHashSet());

            HashSet<Transition> sourceTransitions = net.GetSourceTransitions();
            HashSet<Place> srcPlaces = sourceTransitions.Count == 0 ?
                  new HashSet<Place>()
                : sourceTransitions.Select(transition => transition.GetPostPlaces())
                                   .Aggregate((acc, next) => acc.Union(next).ToHashSet());

            Dictionary<Place, int> placesToSinks = ShortestPathsToSet(sinkPlaces, net);

            HashSet<Dictionary<Place, int>> ShortestPathsForTargets = new HashSet<Dictionary<Place, int>>();

            foreach (MarkingWithConstraints targetMarking in targetMarkings)
            {
                // all places that are not required to be 0 are possible targets
                HashSet<Place> targets = net.Places.Where(place =>
                {
                    return !(targetMarking.Marking.GetValueOrDefault(place, 0) == 0
                             && targetMarking.Constraints.GetValueOrDefault(place, ConstraintOperators.GreaterEqual) == ConstraintOperators.Equal);
                }).ToHashSet();

                Dictionary<Place, int> placesToTargets = ShortestPathsToSet(targets, net);

                int shortestSrcToTargetPath = srcPlaces.Count == 0 ? int.MaxValue : srcPlaces.Min(place => placesToTargets.GetValueOrDefault(place, int.MaxValue));

                ShortestPathsForTargets.Add(
                    net.Places.ToDictionary(
                        keySelector: place => place,
                        elementSelector: place =>
                        {
                            // length of a path of form place -> sink and src -> target
                            int indirectPathLength = Math.Max(shortestSrcToTargetPath, placesToSinks.GetValueOrDefault(place, int.MaxValue));
                            // length of the direct path place -> target
                            int directPathLength = placesToTargets.GetValueOrDefault(place, int.MaxValue);
                            return Math.Min(indirectPathLength, directPathLength);
                        }));
            }

            return ShortestPathsForTargets;
        }

        // Finds the shortest path from each place to any target place
        public static Dictionary<Place, int> ShortestPathsToSet(HashSet<Place> target, PetriNet net)
        {

            Dictionary<Place, int> shortestPaths = new Dictionary<Place, int>();
            foreach (Place targetPlace in target)
            {
                shortestPaths.Add(targetPlace, 0);
            }

            HashSet<Place> newLastAdded = target;
            HashSet<Place> lastAdded = new HashSet<Place>();

            while (newLastAdded.Count > 0) // keep going while we have found a new place with a path in the last iteration, i.e. no fixpoint yet
            {
                lastAdded = newLastAdded;
                newLastAdded = new HashSet<Place>();
                foreach (Place lastAddedPlace in lastAdded)
                {
                    List<Transition> preTransitions = net.GetPreSet(lastAddedPlace);
                    foreach (Transition transition in preTransitions)
                    {
                        foreach (Place prePlace in transition.GetPrePlaces())
                        {
                            if (!shortestPaths.ContainsKey(prePlace)) // if no path from this place yet: add it
                            {
                                shortestPaths[prePlace] = shortestPaths[lastAddedPlace] + 1;
                                newLastAdded.Add(prePlace);
                            }
                        }
                    }
                }
            }

            return shortestPaths;

        }
    }
}
