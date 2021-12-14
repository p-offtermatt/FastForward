using System.Linq;
using System.Collections.Generic;
using System;

namespace Petri
{
    public abstract class Transition : Node
    {

        // store the length of the name separately, to avoid recomputing it in order to speed up equality checks
        protected readonly int NameLength;

        public abstract override string ToString();

        public Transition(string Name)
        {
            this.Name = Name;
            this.NameLength = Name.Count();
        }

        /// <summary>
        /// Fires this transition on the given marking.
        /// A second parameter determines whether the transition is fire forwards or backwards.
        /// </summary>
        /// <param name="m">The marking.</param>
        /// <param name="forwards">True if the transition should be fired forwards ("normally"), or backwards.</param>
        /// <returns>The HashSet of markings that result from firing this transition on the input marking.</returns>
        public virtual HashSet<Marking> FireOn(Marking m, bool forwards)
        {
            if (forwards)
            {
                return FireOn(m);
            }
            else
            {
                return FireBackwardsOn(m);
            }
        }

        /// <summary>
        /// Fires the transition on the given marking.
        /// Returns the set of successors.
        /// </summary>
        /// <param name="m">The marking.</param>
        /// <returns>
        /// A set of successor markings.
        /// If this transition is not enabled on the input marking,
        /// this set is empty.
        /// </returns>
        public abstract HashSet<Marking> FireOn(Marking m);

        /// <summary>
        /// Fires this transition on a set of markings.
        /// </summary>
        /// <param name="markings">A set of markings.</param>
        /// <param name="forward">True if the transition should be fired forwards ("normally"), or backwards. True by default.</param>
        /// <returns>The union of the successor sets obtained by firing this transition on each marking individually.</returns>
        public virtual HashSet<Marking> FireOn(HashSet<Marking> markings, bool forward = true)
        {
            return markings.Select(marking => this.FireOn(marking, forward)).Aggregate(new HashSet<Marking>(), (acc, successorSet) => acc.Union(successorSet).ToHashSet());
        }

        /// <summary>
        /// Fires the transition backwards on the given marking.
        /// Returns the set of predecessors.
        /// </summary>
        /// <param name="m">The marking.</param>
        /// <returns>
        /// A set of predecessor markings.
        /// If this transition is not enabled on the input marking,
        /// this set is empty.
        /// </returns>
        public abstract HashSet<Marking> FireBackwardsOn(Marking m);

        /// <summary>
        /// Checks if the given marking enables this transition.
        /// </summary>
        /// <param name="m">The marking.</param>
        /// <returns>True if this transition is enabled on the marking, false if not.</returns>
        public abstract bool IsEnabledIn(Marking m);

        public abstract bool IsBackwardsEnabledIn(Marking m);

        /// <summary>
        /// Checks if this transition is a Branching Immediate Observation (BIO) transition.
        /// Bio transitions have at most one place with negative effect.
        /// </summary>
        /// <returns>True if this transition is Bio, else false.</returns>
        public abstract bool IsBio();

        /// <summary>
        /// Returns a copy of this transition.
        /// </summary>
        /// <returns>A copy of this transition.</returns>
        public abstract Transition CopyThis();

        /// <summary>
        /// Returns the set of places that have an outgoing edge to this transition.
        /// </summary>
        /// <returns>A HashSet of pre places of this transition.</returns>
        public abstract HashSet<Place> GetPrePlaces();

        /// <summary>
        /// Returns the set of places that have an incoming edge from this transition.
        /// </summary>
        /// <returns>A HashSet of post places of this transition.</returns>
        public abstract HashSet<Place> GetPostPlaces();


        /// <summary>
        /// Returns the set of places in the support (i.e. the pre or post) of this transition.
        /// </summary>
        /// <returns>A HashSet of places in the support of this transition.</returns>
        public HashSet<Place> GetSupportPlaces()
        {
            return GetPrePlaces().Union(GetPostPlaces()).ToHashSet();
        }

        public abstract override int GetHashCode();
        public abstract override bool Equals(object o);

        public abstract bool RemovePlaceFromPre(Place place);
        public abstract bool RemovePlaceFromPost(Place place);

        public abstract void AddPlaceToPost(Place place, int value);
        public abstract void AddPlaceToPre(Place place, int value);


        /// <summary>
        /// Returns a dictionary of places and associated integers.
        /// It holds that the transition is enabled in markings that cover this guard.
        /// </summary>
        /// <returns>A dictionary from places to integers that specifies the guard of this transition.</returns>
        public abstract Dictionary<Place, int> GetGuard();

        /// <summary>
        /// Returns a dictionary of places and associated integers.
        /// It holds that the transition is backwards-enabled in markings that cover this guard.
        /// </summary>
        /// <returns>A dictionary from places to integers that specifies the backwards-guard of this transition.</returns>
        public abstract Dictionary<Place, int> GetBackwardsGuard();

        /// <summary>
        /// Returns the translation of this transition into dotspec format.
        /// </summary>
        /// <returns>A string containing this transition in dotspec format.</returns>
        public abstract string ToDotspec();

        public bool HasSelfLoop()
        {
            HashSet<Place> prePlaces = this.GetPrePlaces();
            HashSet<Place> postPlaces = this.GetPostPlaces();
            return prePlaces.Any(place => postPlaces.Contains(place));
        }

        public abstract bool HasNiceSelfLoop();

        /// <summary>
        /// Returns the translation of this transition into .tts format.
        /// </summary>
        /// /// <param name="baseState">Which state is the base state that the VASS starts in/returns to before and after the transition.</param>
        /// <param name="placeToCounter">A dictionary mapping all places of the net to the counter
        /// they occupy in the VASS.</param>
        /// <param name="nextAvailableStateNumber">The next state number that is available.
        /// Since this transition may need many states, that number is important to occupy the right states. </param>
        /// /// <param name="garbageCounterNum">The number of a counter which is there to generate and absorb tokens.</param>
        /// <returns>A tuple containing this transition as a string in tts format, as well as the next available state number
        /// after this transition.</returns>
        public abstract Tuple<string, int> ToTTS(Dictionary<Place, int> placeToCounter, int baseState, int nextAvailableStateNumber, int garbageCounterNum);

        public abstract string ToLola();
        internal abstract bool IsStateMachineTransition();

        /// <summary>
        /// Returns enumerables of transitions and places which together simulate this transition, but without arc weights.
        /// Replacing transitions in this way will preserve liveness, boundedness 
        /// and reachability.
        /// </summary>
        /// <returns>A tuple of transitions and places which together represent this transition.</returns>
        public abstract (IEnumerable<Transition>, IEnumerable<Place>) GetWithoutArcWeights();

        /// <summary>
        /// Returns whether any of the arcs of this transition have a weight greater than one.
        /// </summary>
        /// <returns></returns>
        public abstract bool HasArcWeights();
    }
}