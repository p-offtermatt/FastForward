using System.Collections.Generic;
using System.Linq;

namespace Petri
{
    public static class WorkflowUtils
    {
        /// <summary>
        /// Attempts to identify the initial place of the given net.
        /// If you also have an initial marking, use <see cref="WorkflowUtils.GetInitialPlace(PetriNet, Marking)"/>
        /// </summary>
        /// <returns>The initial place of the net. If no unique initial place can be identified, throws a <see cref="WorkflowException"/>.</returns>
        public static Place GetInitialPlace(PetriNet net, Marking initial)
        {
            IEnumerable<Place> sourcePlaces = net.GetSourcePlaces().ToHashSet();

            if (sourcePlaces.Count() == 0)
            {
                // there is no potential initial place: cannot transform to WF net
                throw new WorkflowException("Net has no source places!");
            }
            else if (sourcePlaces.Count() == 1)
            {
                return sourcePlaces.First();
            }
            else // (sourcePlaces.Count() > 1)
            {
                // there are many potential initial places..
                if (initial is null || initial.Where(pair => pair.Value > 0).Count() != 1)
                {
                    // ..but the initial marking does not help us distinguish: cannot transform
                    throw new WorkflowException(@"Net has many source places, but initial marking  
                    marks multiple places!");
                }
                else
                {
                    // check which source place is initial by checking which is marked initially
                    var (potentialInitial, _) = initial.ToList().Where(pair => pair.Value > 0).First();
                    if (!sourcePlaces.Contains(potentialInitial))
                    {
                        throw new WorkflowException(@"Net has many source places, and initial markings marks something other than a source place!");
                    }
                    return potentialInitial;
                }
            }
        }

        /// <summary>
        /// Attempts to identify the initial place of the given net.
        /// If you also have an initial marking, use <see cref="WorkflowUtils.GetInitialPlace(PetriNet, Marking)"/>
        /// </summary>
        public static Place GetInitialPlace(PetriNet net)
        {
            return GetInitialPlace(net, null);
        }

        /// <summary>
        /// Attempts to identify the final place of the given net.
        /// </summary>
        public static Place GetFinalPlace(PetriNet net)
        {
            IEnumerable<Place> sinkPlaces = net.GetSinkPlaces();
            if (sinkPlaces.Count() > 1)
            {
                throw new WorkflowException("Net has too many sink places: " + string.Join(", ", sinkPlaces));
            }
            if (sinkPlaces.Count() == 0)
            {
                throw new WorkflowException("Net has no sink places!");
            }
            return sinkPlaces.First();
        }

        /// <summary>
        /// Uses the KHA algorithm to transform
        /// a free-choice workflow model with many final places to a workflow net with a single final place.
        /// See https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.11.7756&rep=rep1&type=pdf, Theorem 5.1.
        /// The KHA algorithm operates as follows:
        /// 0. Add a new place p_f and a new transition t_f, with t_f putting one token into p_f (its preset will be determined later) 
        /// 1. Obtain the set S_p of sink places.
        /// 2. For each sink place s, obtain the set P^s of places from which s is graph-reachable
        /// 3. Obtain the set T^s of transitions which take at least one token from P^s but put no token into P^s
        /// 4. Add a new place p_s
        /// 5. Make each transition t in T^s also put a token into p_s
        /// 6. Add a new transition that puts a token from s into p_s
        /// 7. Make t_f take one token from p_s.
        /// </summary>
        public static void TransformToWorkflowNet(PetriNet net)
        {
            TransformToWorkflowNet(net, null);
        }

        /// <summary>
        /// Uses the KHA algorithm to transform
        /// a free-choice workflow model with many final places to a workflow net with a single final place.
        /// See https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.11.7756&rep=rep1&type=pdf, Theorem 5.1.
        /// The KHA algorithm operates as follows:
        /// 0. Add a new place p_f and a new transition t_f, with t_f putting one token into p_f (its preset will be determined later) 
        /// 1. Obtain the set S_p of sink places.
        /// 2. For each sink place s, obtain the set P^s of places from which s is graph-reachable
        /// 3. Obtain the set T^s of transitions which take at least one token from P^s but put no token into P^s
        /// 4. Add a new place p_s
        /// 5. Make each transition t in T^s also put a token into p_s
        /// 6. Add a new transition that puts a token from s into p_s
        /// 7. Make t_f take one token from p_s.
        /// </summary>
        /// <param name="initial">initial is only used when there are multiple source
        /// places, so the initial place is not clear. 
        /// Then, if it marks a single of the source places, 
        /// that place will be used as the initial place.</param>
        public static void TransformToWorkflowNet(PetriNet net, Marking initial)
        {
            if (!net.IsFreeChoice())
            {
                throw new WorkflowException("Net is not free choice!");
            }
            if (!net.GetArcWeights().All(num => num <= 1))
            {
                throw new WorkflowException("Net has arc with weight > 1!");
            }

            IEnumerable<Place> sinkPlaces = net.GetSinkPlaces().ToHashSet();

            if (sinkPlaces.Count() == 0)
            {
                throw new WorkflowException(@"Net has no sinks!");
            }

            if (sinkPlaces.Count() > 1)
            {
                // more than one sink place and satisfy the conditions to make KHA soundness-preserving => apply KHA
                foreach (Place sink in sinkPlaces)
                {
                    IEnumerable<Node> reverseReachable = ComputeGraphReachableNodes(net, sink, false);

                    IEnumerable<Transition> postTransitions = reverseReachable.OfType<Place>().
                    Select(place => net.GetPostSet(place).ToHashSet()).
                    Aggregate((s1, s2) => s1.Union(s2).ToHashSet()).
                    ToHashSet();

                    foreach (Transition t in postTransitions.Where(transition => !reverseReachable.Contains(transition)))
                    {
                        t.AddPlaceToPost(sink, 1);
                    }
                }

                Dictionary<Place, int> finalPre = sinkPlaces.ToDictionary(place => place, place => 1);

                Place final = net.AddNewPlace("final");
                UpdateTransition finalTransition = new UpdateTransition("t_final",
                finalPre,
                new Dictionary<Place, int>() { { final, 1 } });
                net.AddTransition(finalTransition);
            }
        }

        /// <summary>
        /// Applies the KBA algorithm to obtain a workflow net with a single final place from the input net.
        /// Internally uses <see cref="PetriNet.TransformToWorkflowNet"/>. Differs in that it
        /// leaves the original net unchanged and returns a modified copy.
        /// </summary>
        /// <seealso cref="PetriNet.TransformToWorkflowNet"/>
        /// <returns>A copy of the input Petri net, transformed into a workflow net via the KBA algorithm.
        /// If the algorithm could not be applied, e.g. because the net is free-choice or has no sink places, returns null.</returns>
        public static PetriNet GetAsWorkflowNet(PetriNet net)
        {
            return GetAsWorkflowNet(net, null);
        }

        /// <summary>
        /// See <see cref="PetriNet.GetAsWorkflowNet"/>.
        /// This method should be used when there is also an initial marking to pass along,
        /// see <see cref="PetriNet.TransformToWorkflowNet(Marking)"/>
        /// </summary>
        /// <param name="initialMarking"></param>
        /// <returns></returns>
        public static PetriNet GetAsWorkflowNet(PetriNet net, Marking initialMarking)
        {
            PetriNet result = new PetriNet(net);
            try
            {
                TransformToWorkflowNet(result, initialMarking);
            }
            catch (WorkflowException)
            {
                return null;
            }
            return result;
        }

        /// <summary>
        /// Checks whether from the given place, each place+transition of the net is graph reachable.
        /// If forward is true, forward graph reachability will be checked, 
        /// and if it is false, backward graph reachability will be checked.
        /// </summary>
        /// <param name="place">The place at which to start the check.</param>
        /// <param name="forward">If true, forward reachability is checked, otherwise backward reachability is.</param>
        /// <returns>True if each other place+transition is (forward/backward)-graph-reachable from the given place.</returns>
        public static bool IsFullyGraphReachable(PetriNet net, Place place, bool forward)
        {
            HashSet<Node> reachableNodes = ComputeGraphReachableNodes(net, place, forward);

            if (reachableNodes.Count == net.Places.Count + net.Transitions.Count)
            {
                return true;
            }
            else
            {
                return false;
            }
        }

        private static HashSet<Node> ComputeGraphReachableNodes(PetriNet net, Place place, bool forward)
        {
            var explored = new HashSet<Node>();
            var frontier = new HashSet<Node> { place };

            while (frontier.Count > 0)
            {
                var cur = frontier.First();
                frontier.Remove(cur);
                explored.Add(cur);
                IEnumerable<Node> successors;
                if (forward)
                {
                    successors = net.GetPostSet(cur);
                }
                else
                {
                    successors = net.GetPreSet(cur);
                }

                foreach (Node successor in successors)
                {
                    if (!explored.Contains(successor))
                    {
                        frontier.Add(successor);
                    }
                }
            }

            return explored;
        }
    }
}
