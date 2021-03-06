using System.Collections.Generic;
using System;
using System.Linq;
using System.Text;

namespace Petri
{

    public class PetriNet
    {
        public List<Place> Places;
        public List<Transition> Transitions = new List<Transition>();

        // Caching results of expensive, recurring computations
        private Dictionary<Place, List<Transition>> PlacesToPresets = new Dictionary<Place, List<Transition>>();
        private Dictionary<Place, List<Transition>> PlacesToPostsets = new Dictionary<Place, List<Transition>>();

        public PetriNet(IEnumerable<Place> places, IEnumerable<Transition> transitions)
        {
            this.Places = places.ToList();
            this.Transitions = transitions.ToList();
        }

        public PetriNet()
        {
            Places = new List<Place>();
            Transitions = new List<Transition>();
        }


        public PetriNet(PetriNet net)
        {
            this.Places = net.Places.Select(p => new Place(p.Name)).ToList();
            this.Transitions = net.Transitions.Select(t => t.CopyThis()).ToList();
        }

        public IEnumerable<Transition> GetTransitions()
        {
            return Transitions;
        }

        public Transition GetTransitionByName(string name)
        {
            return this.Transitions.Where(transition => transition.Name == name).FirstOrDefault();
        }

        public List<Transition> GetPostSet(Place p)
        {
            if (this.PlacesToPostsets.ContainsKey(p))
            {
                return this.PlacesToPostsets[p];
            }
            else
            {
                List<Transition> postset = this.Transitions.Where(transition => transition.GetPrePlaces().Contains(p)).ToList();
                this.PlacesToPostsets[p] = postset;
                return postset;
            }
        }

        public List<Transition> GetPreSet(Place p)
        {
            if (this.PlacesToPresets.ContainsKey(p))
            {
                return this.PlacesToPresets[p];
            }
            else
            {
                List<Transition> preset = this.Transitions.Where(transition => transition.GetPostPlaces().Contains(p))
                    .ToList();
                this.PlacesToPresets[p] = preset;
                return preset;
            }
        }

        public IEnumerable<Node> GetPostSet(Node n)
        {
            Place p = n as Place;
            if (p != null)
            {
                return GetPostSet(p);
            }

            Transition t = n as Transition;
            if (t != null)
            {
                return t.GetPostPlaces();
            }

            throw new Exception("Unexpected behaviour: Node that is neither place nor transition");
        }

        public IEnumerable<Node> GetPreSet(Node n)
        {
            Place p = n as Place;
            if (p != null)
            {
                return GetPreSet(p);
            }

            Transition t = n as Transition;
            if (t != null)
            {
                return t.GetPrePlaces();
            }

            throw new Exception("Unexpected behaviour: Node that is neither place nor transition");
        }

        public HashSet<Transition> GetSinkTransitions()
        {
            return this.Transitions.Where(transition => transition.GetPostPlaces().Count() == 0).ToHashSet();
        }

        public HashSet<Transition> GetSourceTransitions()
        {
            return this.Transitions.Where(transition => transition.GetPrePlaces().Count() == 0).ToHashSet();
        }

        public override string ToString()
        {
            return "PLACES\n" + String.Join("\n", Places) + "\nTRANSITIONS\n" + String.Join("\n", Transitions);
        }

        /// <summary>
        /// Computes the in-degree of a place.
        /// This is the number of transitions that have this place as a pre.
        /// </summary>
        /// <param name="place">A place</param>
        /// <returns>The in-degree of the place</returns>
        public int GetInDegree(Place place)
        {
            return this.Transitions.Where(transition => transition.GetPrePlaces().Contains(place)).Count();
        }

        /// <summary>
        /// Computes the out-degree of a place.
        /// This is the number of transitions that have this place as a post.
        /// </summary>
        /// <param name="place">A place</param>
        /// <returns>The out-degree of the place</returns>
        public int GetOutDegree(Place place)
        {
            return this.Transitions.Where(transition => transition.GetPostPlaces().Contains(place)).Count();
        }

        public override bool Equals(object obj)
        {

            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }

            PetriNet otherNet = obj as PetriNet;

            if (!this.Places.OrderBy(t => t.ToString()).SequenceEqual(otherNet.Places.OrderBy(t => t.ToString())))
            {
                Console.WriteLine("Places unequal!");
                return false;
            }
            if (!this.Transitions.OrderBy(t => t.ToString()).SequenceEqual(otherNet.Transitions.OrderBy(t => t.ToString())))
            {
                return false;
            }
            return true;
        }

        // override object.GetHashCode
        public override int GetHashCode()
        {
            return this.Places.GetHashCode() + this.Transitions.GetHashCode();
        }

        /// <summary>
        /// Returns the row of the incidence matrix for this place in sparse representation.
        /// For example, if a transition puts 2 tokens into place, and takes 1 token out of place,
        /// it will be mapped to 2-1 = 1 in the incidence.
        /// </summary>
        /// <param name="place">A place of this Petri net.</param>
        /// <returns>A dictionary mapping transitions to their incidences for the input place.</returns>
        public Dictionary<Transition, int> GetIncidence(Place place)
        {
            List<Transition> preTransitions = GetPreSet(place);
            List<Transition> postTransitions = GetPostSet(place);

            Dictionary<Transition, int> result = new Dictionary<Transition, int>();

            foreach (Transition transition in preTransitions)
            {
                switch (transition)
                {
                    case UpdateTransition update:
                        result[update] = +update.Post[place];
                        break;
                    case TransferTransition transfer:
                        result[transfer] = +transfer.UpdateBehaviour.Post.GetValueOrDefault(place, 0);
                        break;
                }
            }

            foreach (Transition transition in postTransitions)
            {
                switch (transition)
                {
                    case UpdateTransition update:
                        result[update] = result.GetValueOrDefault(update, 0) - update.Pre[place];
                        break;
                    case TransferTransition transfer:
                        result[transfer] = result.GetValueOrDefault(transfer, 0) - transfer.UpdateBehaviour.Pre.GetValueOrDefault(place, 0);
                        break;
                }
            }

            // Remove empty entries (that may appear because post-pre = 0)
            foreach ((Transition t, int v) in result)
            {
                if (v == 0)
                {
                    result.Remove(t);
                }
            }

            return result;
        }

        /// <summary>
        /// Remvoes a place from this net.
        /// </summary>
        /// <param name="place">The place to remove.</param>
        /// <param name="keepTransitions">How to handle transitions that haev this place as a pre or post:
        /// True to keep those transitions, False to remove each transition that has an edge to the place.</param>
        public void RemovePlace(Place place, bool keepTransitions)
        {
            this.Places.Remove(place);

            if (!keepTransitions)
            {
                this.Transitions = this.Transitions.Where(
                        transition => !(transition.GetSupportPlaces().Contains(place))).ToList();
            }
            else
            {
                foreach (Transition transition in this.Transitions)
                {
                    transition.RemovePlaceFromPre(place);
                    transition.RemovePlaceFromPost(place);
                }
            }
        }

        /// <summary>
        ///  Returns a string representation of this net in the .tts format.
        /// </summary>
        /// <param name="initialMarking"></param>
        /// <returns></returns>
        public string ToTTS_PN()
        {
            StringBuilder builder = new StringBuilder((this.Transitions.Count * 2 + this.Places.Count) * 2);
            this.Places.Sort((x, y) => x.Name.CompareTo(y.Name));
            Dictionary<Place, int> placeToCounterNumber = GetPlaceToCounterNumDict();


            int baseState = 0;
            int garbageCounter = 0;
            int nextAvailableState = 1;
            foreach (Transition transition in this.Transitions)
            {
                builder.AppendLine("# Transition " + transition.ToString());
                (string transitionString, int nextState) = transition.ToTTS(placeToCounterNumber, baseState, nextAvailableState, garbageCounter);
                nextAvailableState = nextState;
                builder.AppendLine(transitionString);
            }

            // increase garbage counter from base state
            builder.AppendLine("# Increase Garbage Counter");
            builder.AppendLine("0 0 +> 0 0");

            // next available state is also how many states we have total
            // places + garbage counter = #places+1 counters
            builder.Insert(0, nextAvailableState.ToString() + " " + (this.Places.Count + 1) + "\n");
            return builder.ToString();
        }

        public Dictionary<Place, int> GetPlaceToCounterNumDict()
        {
            return this.Places.ToDictionary(place => place, place => this.Places.IndexOf(place) + 1);
        }

        public string ToDotspec(Marking initialMarking, List<MarkingWithConstraints> targetMarkings)
        {
            // rough approximation of needed capacity. two letters per place ("p1"),
            // 7 letters per transition (>= a... -> ...;)
            // 9 letters base "vars ... rules ..."
            // since it is the very minimum, take everything times two to avoid the first
            // expansion which is almost certain
            initialMarking.AddPlaces(this.Places);
            StringBuilder builder = new StringBuilder((this.Places.Count * 2 + this.Transitions.Count * 5 + 9) * 2);
            builder.Append("vars\n");
            builder.AppendJoin(' ', Places.Select(place => place.Name));
            builder.Append("\n\nrules\n");

            builder.AppendJoin("\n\n", Transitions.Select(transition =>
                transition.ToDotspec()
            ));

            builder.Append("\n\ninit\n");
            builder.Append(initialMarking.ToDotspec());
            builder.Append("\n\ntarget\n");
            builder.Append(MarkingWithConstraints.ListToDotspec(targetMarkings));
            builder.Append("\n");
            return builder.ToString();
        }

        /// <summary>
        /// Returns a string representation of this Petri Net, together with
        /// its given initial marking, in the .lola format. Since lola reserves certain symbols,
        /// notably ':', all such symbols will be replaced automatically by underscores.
        /// </summary>
        /// <param name="initialMarking">A marking.</param>
        /// <returns>A string representation, fit to be written to a .lola file.</returns>
        public string ToLola(Marking initialMarking)
        {
            initialMarking.AddPlaces(this.Places);
            StringBuilder stringBuilder = new StringBuilder();
            stringBuilder.AppendLine("PLACE");

            stringBuilder.AppendJoin(",", this.Places.Select(place => PetriNetUtils.EscapeCharsLola(place.Name)));
            stringBuilder.AppendLine(";");

            stringBuilder.AppendLine();

            string MarkingPairToString(KeyValuePair<Place, int> kvPair)
            {
                (Place place, int value) = kvPair;
                return PetriNetUtils.EscapeCharsLola(place.Name) + ": " + value;
            }

            stringBuilder.AppendLine("MARKING");
            stringBuilder.AppendJoin(
                ", ",
                initialMarking.Select(MarkingPairToString));
            stringBuilder.AppendLine(";");

            stringBuilder.AppendLine();

            foreach (Transition transition in this.Transitions)
            {
                stringBuilder.AppendLine(transition.ToLola());
                stringBuilder.AppendLine();
            }

            return stringBuilder.ToString();
        }

        public int ComputeNumberOfStateMachineTransitions()
        {
            return this.Transitions.Where(transition => transition.IsStateMachineTransition()).Count();
        }

        public int ComputeNumberOfMarkedGraphPlaces()
        {
            return this.Places.Where(place =>
            {
                return this.GetPostSet(place).Count == 1 && this.GetPreSet(place).Count == 1;
            }).Count();
        }

        /// <summary>
        /// Checks whether this net is a workflow net
        /// </summary>
        /// <returns>A tuple (isWFNet, InputPlace, OutputPlace), where isWFNet is true if the net is indeed a workflow net,
        /// and if it is, then InputPlace will be the dedicated InputPlace and OutputPlace will be the dedicated output place.
        /// Otherwise, the second and third return values will be null.</returns>
        public (bool, IEnumerable<Place>, IEnumerable<Place>) IsWorkflowNet()
        {
            IEnumerable<Place> sourcePlaces = null;
            IEnumerable<Place> sinkPlaces = null;

            sourcePlaces = GetSourcePlaces();
            if (sourcePlaces.Count() != 1)
            {
                return (false, sourcePlaces, sinkPlaces);
            }
            Place inputPlace = sourcePlaces.First();

            sinkPlaces = GetSinkPlaces();
            if (sinkPlaces.Count() != 1)
            {
                return (false, sourcePlaces, sinkPlaces);
            }
            Place outputPlace = sinkPlaces.First();

            if (!WorkflowUtils.IsFullyGraphReachable(this, inputPlace, true) || !WorkflowUtils.IsFullyGraphReachable(this, outputPlace, false))
            {
                return (false, sourcePlaces, sinkPlaces);
            }

            return (true, sourcePlaces, sinkPlaces);
        }

        // Returns a list of all arc weights in the net
        public IEnumerable<int> GetArcWeights()
        {
            List<int> result = new List<int>();
            foreach (Transition t in Transitions)
            {
                result.AddRange(t.GetGuard().Values);
                result.AddRange(t.GetBackwardsGuard().Values);
            }
            return result;
        }

        public bool IsFreeChoice()
        {
            for (int i = 0; i < Places.Count; i++)
            {
                Place place1 = Places[i];
                for (int j = i + 1; j < Places.Count; j++)
                {
                    Place place2 = Places[j];

                    HashSet<Transition> post1 = GetPostSet(place1).ToHashSet();
                    HashSet<Transition> post2 = GetPostSet(place2).ToHashSet();


                    if (!post1.All(place => !post2.Contains(place)) && !post1.SetEquals(post2))
                    {
                        return false;
                    }
                }
            }
            return true;
        }

        public void AddTransition(Transition transition)
        {
            // Invalidate caches due to new transition
            this.PlacesToPostsets.Clear();
            this.PlacesToPresets.Clear();
            this.Transitions.Add(transition);
        }

        /// <summary>
        /// Method to generate a new place, which attempts to use the given name.
        /// If a place of that name already exists, modifies the name by appending '1'
        /// successively until the name does not exist yet.
        /// If repeated names are not a concern, using new Place(name) yields better performance.
        /// </summary>
        /// <returns>A place with a name that does not yet occur in this Petri net.</returns>
        public Place AddNewPlace(string name)
        {
            if (Places.Any(place => place.Name == name))
            {
                return AddNewPlace(name + "1");
            }
            Place result = new Place(name);
            AddPlace(result);
            return result;
        }

        public void AddPlace(Place place)
        {
            this.Places.Add(place);
        }



        public IEnumerable<Place> GetSinkPlaces()
        {
            return this.Places.Where(place => this.GetPostSet(place).Count == 0);
        }

        public IEnumerable<Place> GetSourcePlaces()
        {
            return this.Places.Where(place => this.GetPreSet(place).Count == 0);
        }

        public PetriNet ShortCircuit(Place inputPlace, Place outputPlace)
        {
            PetriNet copy = new PetriNet(this);


            UpdateTransition shortCircuitEdge = new UpdateTransition("shortCircuit",
            new Dictionary<Place, int>() { { outputPlace, 1 } },
            new Dictionary<Place, int>() { { inputPlace, 1 } });

            copy.AddTransition(shortCircuitEdge);
            return copy;
        }
    }
}