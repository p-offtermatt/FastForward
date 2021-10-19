using System;
using System.Collections.Generic;
using Petri;
using Priority_Queue;
using System.Linq;

namespace SearchAlgorithms
{
    public class BackwardsCoverUtils
    {
        /// <summary>
        /// Removes from the queue of candidates all candidates c such that
        /// there exists a c' with c > c'.
        /// </summary>
        /// <param name="candidates">A priority queue of candidate elements.</param>
        public static void MinimizeCandidates<Priority>(ref SimplePriorityQueue<Marking, Priority> candidates) where Priority : IComparable<Priority>
        {
            HashSet<Marking> candidatesToRemove = new HashSet<Marking>();
            // now minimize candidates
            foreach (Marking candidate in candidates)
            {
                if (candidates.Any(x => x.CoveredBy(candidate) && !x.Equals(candidate)))
                {
                    candidatesToRemove.Add(candidate);
                }
            }

            foreach (Marking elementToRemove in candidatesToRemove)
            {
                candidates.Remove(elementToRemove);
            }
        }

        public static Marking ExpandBase(Marking baseToExpand, UpdateTransition transition)
        {
            // don't modify baseToExpand, so make a copy
            Marking resultMarking = new Marking(baseToExpand);
            foreach (Place p in transition.Pre.Select(kvPair => kvPair.Key).ToHashSet().
                         Union(transition.Post.Select(kvPair => kvPair.Key).ToHashSet()))
            {
                resultMarking[p] = System.Math.Max(
                        transition.Pre.GetValueOrDefault(p, 0),
                        baseToExpand.GetValueOrDefault(p, 0) -
                            transition.Post.GetValueOrDefault(p, 0) +
                            transition.Pre.GetValueOrDefault(p, 0));
            }
            return resultMarking;
        }
    }
}