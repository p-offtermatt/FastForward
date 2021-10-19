using System;
using System.Collections.Generic;
using Petri;
using Priority_Queue;
using System.Linq;
using Benchmark;

namespace SearchAlgorithms
{

    public static class BackwardsCoverability
    {
        public static bool DetermineCoverability(PetriNet net, Marking initialMarking, List<MarkingWithConstraints> targetMarkings)
        {
            var currentBase = new HashSet<Marking>();
            var successors = new Dictionary<Marking, Tuple<UpdateTransition, Marking>>();

            foreach (MarkingWithConstraints target in targetMarkings)
            {
                // maybe use some heuristic?
                currentBase.Add(target.Marking);
            }

            while (true)
            {
                // check if initial marking covers any base
                foreach (Marking baseElement in currentBase)
                {
                    if (baseElement.CoveredBy(initialMarking))
                    {
                        // success, return path
                        return true;
                    }
                }

                HashSet<Marking> newElements = new HashSet<Marking>();

                foreach (Marking baseElementToExpand in currentBase)
                {
                    foreach (UpdateTransition transition in net.Transitions)
                    {
                        Marking newBaseElement = BackwardsCoverUtils.ExpandBase(baseElementToExpand, transition);

                        if (!currentBase.Any(x => newBaseElement.Covers(x)))
                        {
                            newElements.Add(newBaseElement);
                        }
                    }
                }

                if (newElements.Count == 0)
                {
                    return false;
                }
                currentBase = BackwardsCoverability.MinimizeBase(currentBase.Union(newElements).ToHashSet());
            }
        }

        private static List<Transition> ReconstructBackwardsCoverPath(
                Marking initialMarking,
                Dictionary<Marking, Tuple<Transition, Marking>> successors)
        {
            List<Transition> path = new List<Transition>();
            Marking currentMarking = initialMarking;
            while (successors.GetValueOrDefault(currentMarking, null) != null)
            {
                Tuple<Transition, Marking> successorTuple = successors.GetValueOrDefault(currentMarking);
                currentMarking = successorTuple.Item2;
                path.Add(successorTuple.Item1);
            }

            return path;
        }

        public static List<Transition> DetermineCoverabilityWithQueue(
                PetriNet net, Marking initialMarking,
                IEnumerable<MarkingWithConstraints> targetMarkings,
                Func<Marking, float?> coverabilityOverapproximation,
                SearchBenchmarkEntry diagnostics = null)
        {
            var currentBase = new HashSet<Marking>();
            var candidatesToExpand = new SimplePriorityQueue<Marking, float>();
            var distanceFromTargetMarkings = new Dictionary<Marking, int>();

            Dictionary<Marking, Tuple<Transition, Marking>> successors = new Dictionary<Marking, Tuple<Transition, Marking>>();

            // minimize starting base before enqueueing
            var temp = MinimizeBase(targetMarkings.Select(m => m.Marking).ToHashSet());
            foreach (Marking candidate in temp)
            {
                System.Diagnostics.Stopwatch watch = null;

                if (diagnostics != null)
                {
                    watch = System.Diagnostics.Stopwatch.StartNew();
                }

                float? score = coverabilityOverapproximation(candidate);

                if (diagnostics != null)
                {
                    watch.Stop();
                    diagnostics.timesHeuristicCalculated += 1;
                    diagnostics.timeInHeuristicCalculation += watch.ElapsedMilliseconds;
                }

                if (score.HasValue)
                {
                    distanceFromTargetMarkings.Add(candidate, 0);
                    successors.Add(candidate, null);
                    // f(candidate) = h(candidate) + g(candidate)
                    candidatesToExpand.Enqueue(candidate, score.Value + 0);
                }
                else
                {
                    continue;
                }
            }

            // as long as there is a candidate we have not expanded yet,
            // keep expanding the best candidate
            while (candidatesToExpand.Count > 0)
            {

                HashSet<Marking> newElements = new HashSet<Marking>();

                Marking candidate = candidatesToExpand.Dequeue();

                if (diagnostics != null)
                {
                    diagnostics.ExpandedNodes += 1;
                }

                // check if initial marking covers this element - if yes, we are good
                if (candidate.CoveredBy(initialMarking))
                {
                    return ReconstructBackwardsCoverPath(candidate, successors);
                }

                // if some base element is already covered by the candidate
                // no need to expand the candidate - it gives no more information than
                // we can obtain by expanding that base element
                if (currentBase.Any(x => candidate.Covers(x)))
                {
                    continue;
                }

                // this element is not covered by initial marking,
                // and it doesn't cover the current base.
                // it can give us a bigger upward closure when we expand it,
                // so we can add it to the base now
                currentBase.Add(candidate);
                currentBase = MinimizeBase(currentBase);

                foreach (UpdateTransition transition in net.Transitions)
                {
                    Marking newCandidate = BackwardsCoverUtils.ExpandBase(candidate, transition);

                    // only add if new element does not cover any old base element
                    // TODO if something goes wrong, this might be it - this is bad
                    // with an inconsistent heuristic, since we won't reopen nodes that are already closed,
                    // i.e. we might not find a shortest path
                    if (!currentBase.Any(x => newCandidate.Covers(x)))
                    {
                        //also, only add if via this path, the distance to the target markings is less than
                        // the distance we recorded previously (if any)
                        // --> needed because our heuristic might not be consistent TODO THIS COULD GO WRONG
                        int currentDistance = distanceFromTargetMarkings[candidate] + 1;
                        int recordedDistance = distanceFromTargetMarkings.GetValueOrDefault(newCandidate, int.MaxValue);

                        if (currentDistance < recordedDistance)
                        {
                            newElements.Add(newCandidate);
                            successors[newCandidate] = new Tuple<Transition, Marking>(transition, candidate);
                            distanceFromTargetMarkings[newCandidate] = currentDistance;
                        }
                    }
                }

                // if we add no new elements, no need to minimize and add
                if (newElements.Count != 0)
                {
                    AddNewElementsToCandidatesMinimized(newElements,
                                                        ref candidatesToExpand,
                                                        distanceFromTargetMarkings,
                                                        coverabilityOverapproximation);
                }
            }

            // if target was not reached, but there are no more candidates, return unreachable
            return null;
        }

        public static HashSet<Marking> MinimizeBase(HashSet<Marking> inputBase)
        {
            HashSet<Marking> resultBase = new HashSet<Marking>();

            foreach (Marking marking in inputBase)
            {
                if (!inputBase.Any(x => !x.Equals(marking) && marking.Covers(x)))
                {
                    resultBase.Add(marking);
                }
            }
            return resultBase;
        }

        /// <summary>
        /// NOTE: modifies candidates, returns the new base
        /// </summary>
        /// <param name="oldBase"></param>
        /// <param name="newElements"></param>
        /// <param name="currentDistances"></param>
        /// <param name="candidates"></param>
        /// <param name="heuristicFunction"></param>
        /// <returns>The new base obtained by merging the old base and the new elements.</returns>
        public static void AddNewElementsToCandidatesMinimized(
            HashSet<Marking> newElements,
            ref SimplePriorityQueue<Marking, float> candidates,
            Dictionary<Marking, int> currentDistances,
            Func<Marking, float?> heuristicFunction)
        {
            // minimize new elements - no need to
            // look at any that are already not minimal even among only the new elements
            var minimalNewElements = MinimizeBase(newElements);

            // add minimal new elements to candidates
            foreach (Marking newElement in minimalNewElements)
            {
                float? hScore = heuristicFunction(newElement);
                // if heuristic says unreachable, no need to expand
                // so no need to add to the queue
                if (!hScore.HasValue)
                {
                    continue;
                }
                else
                {
                    float newScore = hScore.Value + (float)currentDistances[newElement];

                    // if new element is already in candidates, update priority
                    if (candidates.Contains(newElement))
                    {
                        float oldScore = candidates.GetPriority(newElement);
                        candidates.UpdatePriority(newElement, Math.Min(oldScore, newScore));
                    }
                    else
                    { // if new element is not already contained, insert it with new priority
                        candidates.Enqueue(newElement, newScore);
                    }
                }
            }

            BackwardsCoverUtils.MinimizeCandidates(ref candidates);
        }
    }
}