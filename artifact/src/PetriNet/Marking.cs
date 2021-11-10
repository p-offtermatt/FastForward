using System.Collections.Generic;
using System;
using System.Linq;
using System.Text;

namespace Petri
{
    public class Marking : Dictionary<Place, int>
    {

        public Marking(IDictionary<Place, int> inputDict) : base(inputDict)
        {

        }

        public Marking() : base()
        {

        }

        /// <summary>
        /// Return the set of places that have a positive (nonzero!) amount of tokens in this marking. 
        /// </summary>
        /// <returns>The set of marked places.</returns>
        public HashSet<Place> GetMarkedPlaces()
        {
            return this.Where(kvPair => kvPair.Value > 0).Select(kvPair => kvPair.Key).ToHashSet();
        }

        public bool CoveredBy(Marking other)
        {
            return other.Covers(this);
        }

        public bool Covers(Marking other)
        {
            foreach (KeyValuePair<Place, int> markedPlace in other)
            {
                if (this.GetValueOrDefault(markedPlace.Key, 0) < markedPlace.Value)
                {
                    return false;
                }
            }
            return true;
        }

        public override bool Equals(object obj)
        {
            // markings p1: 1 p2: 0 and p1: 1 should be the same
            if ((obj == null) || !this.GetType().Equals(obj.GetType()))
            {
                return false;
            }
            else
            {
                Marking other = (Marking)obj;
                foreach (Place key in this.Keys.Concat(other.Keys))
                {
                    if (this.GetValueOrDefault(key, 0) != other.GetValueOrDefault(key, 0))
                    {
                        return false;
                    }
                }
                return true;
            }
        }

        public override int GetHashCode()
        {
            int hashCode = 0;
            foreach (Place key in this.Keys)
            {
                int multiplier = this[key];
                hashCode += multiplier * key.GetHashCode();
            }
            return hashCode;
        }

        /// <summary>
        /// Removes all the places from this marking that have 0 tokens assigned.
        /// </summary>
        public void RemovePlacesWithNoTokens()
        {
            foreach (Place place in this.Keys)
            {
                if (this[place] == 0)
                {
                    this.Remove(place);
                }
            }
        }

        public override string ToString()
        {
            return String.Join(", ", this.Where(x => x.Value > 0).Select(x => x.Key + ": " + x.Value));
        }

        public bool Enables(Transition t)
        {
            return t.IsEnabledIn(this);
        }

        public bool BackwardsEnables(Transition t)
        {
            return t.IsBackwardsEnabledIn(this);
        }

        /// <summary>
        /// Fires a transition on this marking.
        /// </summary>
        /// <param name="t">The transition to fire.</param>
        /// <returns>A HashSet of successor markings. </returns>
        public HashSet<Marking> Fire(Transition t)
        {
            return t.FireOn(this);
        }

        /// <summary>
        /// Fire the given transition backwards on this marking.
        /// </summary>
        /// <param name="t">The transition to fire.</param>
        /// <returns>A HashSet of predecessor markings.</returns>
        public HashSet<Marking> FireBackwards(Transition t)
        {
            return t.FireBackwardsOn(this);
        }

        /// <summary>
        /// Fires a list of transitions in order on this marking.
        /// The second parameter determines whether transitions are fired forwards or backwards. (Forward-firing by default.)
        /// </summary>
        /// <param name="firingSequence">The firing sequence.</param>
        public HashSet<Marking> FireFiringSequence(IEnumerable<Transition> firingSequence, bool forward = true)
        {
            HashSet<Marking> curMarkings = new HashSet<Marking> { this };
            foreach (Transition transition in firingSequence)
            {
                HashSet<Marking> nextMarkings = transition.FireOn(curMarkings, forward);
                curMarkings = nextMarkings;
            }
            return curMarkings;
        }

        public static bool operator <=(Marking obj1, Marking obj2)
        {
            return obj1.CoveredBy(obj2);
        }

        public static bool operator >=(Marking obj1, Marking obj2)
        {
            return obj1.Covers(obj2);
        }

        public static Marking operator -(Marking obj1, Marking obj2)
        {
            Marking result = new Marking();
            foreach (Place place in obj1.Keys)
            {
                result[place] = obj1[place] - obj2.GetValueOrDefault(place, 0);
            }

            foreach (Place place in obj2.Keys)
            {
                if (obj1.ContainsKey(place))
                {
                    //already added place in the first loop
                    continue;
                }
                // place not in obj1, so subtract from 0
                result[place] = -obj2[place];
            }
            return result;
        }

        // Returns 
        public float SquaredEuclideanDistance(MarkingWithConstraints other)
        {
            float sum = 0;
            foreach (Place place in this.GetMarkedPlaces().Union(other.Marking.GetMarkedPlaces()))
            {
                int thisTokens = this.GetValueOrDefault(place, 0);
                int otherTokens = other.Marking.GetValueOrDefault(place, 0);

                // if marking already covers place, add 0 to the sum -> just continue
                if (other.Constraints.GetValueOrDefault(place, ConstraintOperators.GreaterEqual)
                    == ConstraintOperators.GreaterEqual && thisTokens > otherTokens)
                {
                    // sum += 0
                    continue;
                }
                else
                {
                    float tokenDifference = this.GetValueOrDefault(place, 0) - other.Marking.GetValueOrDefault(place, 0);
                    sum += tokenDifference * tokenDifference;
                }
            }
            return sum;
        }

        /// <summary>
        /// Adds all places in the given enumerable to this marking.
        /// If they are already present, the present value will be used,
        /// otherwise they will be added with 0 tokens.
        /// This is used if one wants to ensure that the marking
        /// keeps track of all places in a net, for example to make
        /// e.g. ToString outputs more consistent.
        /// </summary>
        /// <param name="places">An enumerable of places.</param>
        public void AddPlaces(IEnumerable<Place> places)
        {
            foreach (Place place in places)
            {
                this[place] = this.GetValueOrDefault(place, 0);
            }
        }

        public String ToDotspec()
        {
            StringBuilder builder = new StringBuilder(this.Count * 3 * 2);
            builder.AppendJoin(", ", this.Select(kvPair => kvPair.Key.ToString() + "=" + kvPair.Value.ToString()));

            return builder.ToString();
        }

        public String ToTTS_PN(Dictionary<Place, int> placeToIndexDict, bool initialMarking)
        {
            StringBuilder builder = new StringBuilder();
            builder.Append("0|");

            // start garbage counter initialized
            if (initialMarking)
            {
                builder.Append("0,");
            }

            builder.AppendJoin(",", this.Where(kvPair => kvPair.Value > 0).Select(kvPair =>
            {
                Place place = kvPair.Key;
                int value = kvPair.Value;

                int placeCounterNum = placeToIndexDict[place];

                return String.Join(",", Enumerable.Repeat(placeCounterNum.ToString(), value));
            }));

            return builder.ToString();
        }

        /// <summary>
        /// Returns a string representing this marking as a liveness predicate for use as an input formula to LoLA.
        /// For example, if this marking is M, then the formula expresses AGEF P (for all reachable markings, M is reachable).
        /// </summary>
        /// <param name="net">The Petri net the predicate should be over; needed to know the complete list of places.</param>
        /// <returns></returns>
        public String ToLolaLivenessPredicate(PetriNet net)
        {
            return "AGEF " + String.Join(" AND ",
                net.Places.Select(place => PetriNetUtils.EscapeCharsLola(place.Name) + " = " + this.GetValueOrDefault(place, 0).ToString()));
        }
    }
}