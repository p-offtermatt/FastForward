using System.Collections.Generic;
using System;
using System.Linq;
using System.Text;

namespace Petri
{
    public class UpdateTransition : Transition
    {
        public Dictionary<Place, int> Post;

        public Dictionary<Place, int> Pre;

        public UpdateTransition(string Name, Dictionary<Place, int> Pre, Dictionary<Place, int> Post) : base(Name)
        {
            this.Pre = new Dictionary<Place, int>(Pre);
            this.Post = Post;
        }

        public UpdateTransition(string Name) : base(Name)
        {
            this.Pre = new Dictionary<Place, int>();
            this.Post = new Dictionary<Place, int>();
        }

        public override HashSet<Place> GetPrePlaces()
        {
            return this.Pre.Where(kvPair => kvPair.Value > 0).Select(kvPair => kvPair.Key).ToHashSet();
        }

        public override HashSet<Place> GetPostPlaces()
        {
            return this.Post.Where(kvPair => kvPair.Value > 0).Select(kvPair => kvPair.Key).ToHashSet();
        }

        /// <summary>
        /// Returns a dictionary representing post - pre of this transition.
        /// </summary>
        /// <returns>A dictionary for post - pre.</returns>
        public Dictionary<Place, int> GetPrePostDifference()
        {
            Dictionary<Place, int> result = new Dictionary<Place, int>();

            foreach ((Place place, int value) in this.Pre)
            {
                result[place] = result.GetValueOrDefault(place, 0) - value;
            }

            foreach ((Place place, int value) in this.Post)
            {
                result[place] = result.GetValueOrDefault(place, 0) + value;
            }
            return result;
        }

        public UpdateTransition(UpdateTransition otherTransition) : base(otherTransition.Name)
        {
            this.Pre = otherTransition.Pre.ToDictionary(kvPair => new Place(kvPair.Key.Name), kvPair => kvPair.Value);
            this.Post = otherTransition.Post.ToDictionary(kvPair => new Place(kvPair.Key.Name), kvPair => kvPair.Value);
        }

        public override string ToString()
        {
            return Name;
        }

        public override bool Equals(object obj)
        {
            if ((obj == null) || !this.GetType().Equals(obj.GetType()))
            {
                return false;
            }
            else
            {
                UpdateTransition other = (UpdateTransition)obj;
                return (this.NameLength == other.NameLength &&
                    other.Name == this.Name &&
                    other.Pre.Count == this.Pre.Count && !other.Pre.Except(this.Pre).Any() &&
                    other.Post.Count == this.Post.Count && !other.Post.Except(this.Post).Any());
            }
        }

        public override int GetHashCode()
        {
            return (this.Name == null ? 0 : this.Name.GetHashCode()) ^ (this.Pre == null ? 0 : this.Pre.
                            Aggregate(0, (acc, kvPair) =>
                                acc + kvPair.Value * kvPair.Key.GetHashCode())) ^ (this.Post == null ? 0 : this.Post.
                            Aggregate(0, (acc, kvPair) =>
                                acc + kvPair.Value * kvPair.Key.GetHashCode()));
        }

        public override bool IsBio()
        {
            Dictionary<Place, int> prePostDifference = GetPrePostDifference();
            HashSet<Place> inputPlaces = prePostDifference.Where(kvPair => kvPair.Value < 1).Select(kvPair => kvPair.Key).ToHashSet();

            return inputPlaces.Count() <= 1;
        }

        public override HashSet<Marking> FireOn(Marking m)
        {

            if (!this.IsEnabledIn(m))
            {
                return new HashSet<Marking>();
            }
            Marking resultMarking = new Marking(m);

            RemoveConsumedTokens(ref resultMarking, forwards: true);

            AddProducedTokens(ref resultMarking, forwards: true);

            // remove places that just became empty
            foreach (KeyValuePair<Place, int> pair in Pre)
            {
                Place place = pair.Key;
                if (resultMarking[place] == 0)
                {
                    resultMarking.Remove(place);
                }
            }

            return new HashSet<Marking> { resultMarking };
        }

        public void AddProducedTokens(ref Marking marking, bool forwards = true)
        {
            foreach (KeyValuePair<Place, int> pair in forwards ? Post : Pre)
            {
                Place place = pair.Key;
                int amount = pair.Value;
                marking[place] = marking.GetValueOrDefault(place, 0) + amount;
            }
        }

        public void RemoveConsumedTokens(ref Marking marking, bool forwards = true)
        {
            foreach (KeyValuePair<Place, int> pair in forwards ? Pre : Post)
            {
                Place place = pair.Key;
                int amount = pair.Value;
                marking[place] = marking.GetValueOrDefault(place, 0) - amount;
            }
        }



        public override HashSet<Marking> FireBackwardsOn(Marking m)
        {
            Marking resultMarking = new Marking(m);

            if (!this.IsBackwardsEnabledIn(m))
            {
                return new HashSet<Marking>();
            }

            RemoveConsumedTokens(ref resultMarking, forwards: false);

            AddProducedTokens(ref resultMarking, forwards: false);

            // remove places that just became empty
            foreach (KeyValuePair<Place, int> pair in Post)
            {
                Place place = pair.Key;
                if (resultMarking[place] == 0)
                {
                    resultMarking.Remove(place);
                }
            }

            return new HashSet<Marking> { resultMarking };
        }

        public override bool IsEnabledIn(Marking m)
        {
            foreach (KeyValuePair<Place, int> pair in Pre)
            {
                Place place = pair.Key;
                int amount = pair.Value;

                // if there are not enough tokens in this marking to satisfy the pre of the transition, return false
                if (m.GetValueOrDefault(place, 0) < amount)
                {
                    return false;
                }
            }

            return true;
        }

        public override bool IsBackwardsEnabledIn(Marking m)
        {
            foreach (KeyValuePair<Place, int> pair in Post)
            {
                Place place = pair.Key;
                int amount = pair.Value;

                // if there are not enough tokens in this marking to satisfy the pre of the transition, return false
                if (m.GetValueOrDefault(place, 0) < amount)
                {
                    return false;
                }
            }

            return true;
        }

        public override Transition CopyThis()
        {
            return new UpdateTransition(this);
        }

        public override bool RemovePlaceFromPre(Place place)
        {
            return this.Pre.Remove(place);
        }

        public override bool RemovePlaceFromPost(Place place)
        {
            return this.Post.Remove(place);
        }

        public override Dictionary<Place, int> GetGuard()
        {
            return this.Pre;
        }

        public override string ToDotspec()
        {
            if (this.Pre.Count == 0 && this.Post.Count == 0)
            {
                return "";
            }

            IEnumerable<String> preStrings;
            if (this.Pre.Count != 0)
            {
                preStrings = this.Pre.Select(kvPair => PetriNetUtils.EscapeCharsDotspec(kvPair.Key.ToString()) + " >= " + kvPair.Value.ToString());
            }
            else
            {
                preStrings = new HashSet<String> { this.Post.First().Key.ToString() + " >= " + 0 };
            }
            var affectedPlaces = this.Pre.Keys.Union(this.Post.Keys);
            var postStrings = affectedPlaces.Select(place =>
            {
                int placeDelta = this.Post.GetValueOrDefault(place, 0) - this.Pre.GetValueOrDefault(place, 0);

                return PetriNetUtils.EscapeCharsDotspec(place.ToString()) + "' = " + PetriNetUtils.EscapeCharsDotspec(place.ToString()) + (placeDelta >= 0 ? "+" : "") + placeDelta.ToString();
            });
            return
                String.Join(",", preStrings) +
                " ->\n" +
                String.Join(",\n", postStrings) +
                ";";
        }

        public override Tuple<string, int> ToTTS(Dictionary<Place, int> placeToCounter, int baseState, int nextAvailableStateNumber, int garbageCounterNum)
        {
            StringBuilder builder = new StringBuilder();
            int curState = baseState;

            int nextFreeState = nextAvailableStateNumber;

            // first remove pre to check if it is fulfilled
            foreach ((Place place, int value) in this.Pre)
            {
                // tts only supports +1/-1 transitions -> need to do x transitions to represent +x
                for (int i = 0; i < value; i++)
                {
                    builder.AppendLine(curState + " " + placeToCounter[place] + " -> " + nextFreeState + " " + garbageCounterNum);
                    curState = nextFreeState;
                    nextFreeState++;
                }
            }

            // re-add the pre
            foreach ((Place place, int value) in this.Pre)
            {
                for (int i = 0; i < value; i++)
                {
                    builder.AppendLine(curState + " " + garbageCounterNum + " -> " + nextFreeState + " " + placeToCounter[place]);
                    curState = nextFreeState;
                    nextFreeState++;
                }
            }

            // add post
            foreach ((Place place, int value) in this.Post)
            {
                for (int i = 0; i < value; i++)
                {
                    builder.AppendLine(curState + " " + garbageCounterNum + " -> " + nextFreeState + " " + placeToCounter[place]);
                    curState = nextFreeState;
                    nextFreeState++;
                }
            }

            // go back to base state, since we need +1/-1, use the garbage counter
            builder.AppendLine(curState + " " + garbageCounterNum + " -> " + baseState + " " + garbageCounterNum);

            return new Tuple<string, int>(builder.ToString(), nextFreeState);
        }

        public override Dictionary<Place, int> GetBackwardsGuard()
        {
            return this.Post;
        }

        public override string ToLola()
        {
            if (this.Pre.Count == 0 && this.Post.Count == 0)
            {
                return "";
            }

            StringBuilder builder = new StringBuilder();
            builder.AppendLine("TRANSITION " + PetriNetUtils.EscapeCharsLola(Name));

            // add pre places
            builder.Append("CONSUME ");
            if (this.Pre.Count > 0)
            {
                builder.AppendJoin(
                ", ",
                Pre.Select(kvPair => PetriNetUtils.EscapeCharsLola(kvPair.Key.ToString()) + ": " + kvPair.Value.ToString()));
            }
            else
            {
                // Lola specific: it does not like empty consumes/produces as CONSUME; but it needs them as CONSUME p: 0; where p is a place that appears in the transition
                builder.Append(PetriNetUtils.EscapeCharsLola(Post.First().Key.ToString()) + ": " + 0);
            }
            builder.AppendLine(";");

            // add post places
            builder.Append("PRODUCE ");

            if (this.Post.Count > 0)
            {
                builder.AppendJoin(
                    ", ",
                    this.Post.Select(kvPair => PetriNetUtils.EscapeCharsLola(kvPair.Key.ToString()) + ": " + kvPair.Value.ToString()));
            }
            else
            {
                builder.Append(PetriNetUtils.EscapeCharsLola(Pre.First().Key.ToString()) + ": " + 0);
            }
            builder.AppendLine(";");
            builder.AppendLine();
            return builder.ToString();
        }

        public override bool HasNiceSelfLoop()
        {
            if (!HasSelfLoop())
            {
                return false;
            }
            else
            {
                HashSet<Place> prePlaces = this.GetPrePlaces();
                HashSet<Place> postPlaces = this.GetPostPlaces();
                List<Place> selfLoopPlaces = prePlaces.Where(place => postPlaces.Contains(place)).ToList();

                if (selfLoopPlaces.Count != 1)
                {
                    return false;
                }
                Place selfLoopPlace = selfLoopPlaces[0];
                int pre = this.Pre[selfLoopPlace];
                int post = this.Post[selfLoopPlace];

                return pre <= post;
            }
        }

        internal override bool IsStateMachineTransition()
        {
            return this.Pre.Count == 1 && this.Pre.First().Value == 1 &&
                this.Post.Count == 1 && this.Post.First().Value == 1;
        }

        public override void AddPlaceToPost(Place place, int value)
        {
            this.Post.Add(place, value);
        }

        public override void AddPlaceToPre(Place place, int value)
        {
            this.Pre.Add(place, value);
        }

        public override (IEnumerable<Transition>, IEnumerable<Place>) GetWithoutArcWeights()
        {
            HashSet<Transition> transitions = new HashSet<Transition>();
            HashSet<Place> places = new HashSet<Place>();

            UpdateTransition newTransition = new UpdateTransition(this.Name + "_noweights");

            foreach ((Place place, int weight) in this.Pre)
            {
                if (weight == 1)
                {
                    newTransition.AddPlaceToPre(place, 1);
                }
                else
                {
                    // Stores which powers of two are factors of the arc weight
                    IEnumerable<Tuple<int, bool>> twopowers =
                        Convert.ToString(weight, 2).Reverse().
                            Select((letter, index) => new Tuple<int, bool>(index, letter == '1'));

                    int height = twopowers.Count();
                    (List<Place> leftPlaces, List<Place> rightPlaces, IEnumerable<Transition> towerTransitions) =
                        GetTwoPowerTower(place, height, place.Name + "_to_" + this.Name + "_weight_" + weight.ToString(), up: true);


                    transitions.UnionWith(towerTransitions);
                    places.UnionWith(leftPlaces);
                    places.UnionWith(rightPlaces);

                    foreach (int index in twopowers.Where((pair) => pair.Item2).Select(pair => pair.Item1))
                    {
                        newTransition.AddPlaceToPre(leftPlaces[index], 1);
                    }
                }
            }

            foreach ((Place place, int weight) in this.Post)
            {
                if (weight == 1)
                {
                    newTransition.AddPlaceToPost(place, 1);
                }
                else
                {
                    // Stores which powers of two are factors of the arc weight
                    IEnumerable<int> twopowers =
                        Convert.ToString(weight, 2).Reverse().
                            Select((letter, index) => new Tuple<int, bool>(index, letter == '1'))
                            .Where((pair) => pair.Item2).Select(pair => pair.Item1);

                    int height = twopowers.Count();
                    (List<Place> leftPlaces, List<Place> rightPlaces, IEnumerable<Transition> towerTransitions) =
                        GetTwoPowerTower(place, height, this.Name + "_to_" + place.Name + "_weight_" + weight.ToString(), up: false);

                    transitions.UnionWith(towerTransitions);
                    places.UnionWith(leftPlaces);
                    places.UnionWith(rightPlaces);

                    foreach (int index in twopowers)
                    {
                        newTransition.AddPlaceToPost(leftPlaces[index], 1);
                    }
                }
            }

            transitions.Add(newTransition);

            return (transitions, places);
        }

        /// <summary>
        /// Creates transitions for a two-power tower of the given height.
        /// The tower has two places (a left and right place) on each level except the last.
        /// The last level only has a left place.
        /// The height determines how many levels the tower will have. The lowest level is level 0,
        /// the highest level therefore height-1.
        /// A place on level l can be marked when starting with 2^l tokens in the incoming place.
        /// To simulate an arc that consumes e.g. 5 tokens, one should make a tower of height ceil(log(5) = 3.
        /// </summary>
        /// <param name="place">The place that the tower should consume tokens from</param>
        /// <param name="height">The height of the generated tower</param>
        /// <param name="name">The name that should be prefixed to all transition and place names of the tower.</param>
        /// <param name="up">If true, the tower will consume tokens from the place, and
        /// have back transitions to reverse partial commits. If false, the tower will assume to produce tokens to a place,
        /// so have only transitions taking tokens from high to low levels.</param>
        private (List<Place> leftPlaces, List<Place> rightPlaces, IEnumerable<Transition> transitions) GetTwoPowerTower(Place place,
                                                                                                                        int height,
                                                                                                                        string name,
                                                                                                                        bool up)
        {
            List<Place> leftPlaces = new List<Place>(height);
            List<Place> rightPlaces = new List<Place>(height - 1);

            HashSet<UpdateTransition> transitions = new HashSet<UpdateTransition>();

            for (int i = 0; i < height; i++)
            {
                // create places for level
                Place leftPlace = new Place(name + "_left_level_" + i.ToString());
                IEnumerable<UpdateTransition> leftTransitions = CreateTwopowerTransitionsForPlace(place, name + "_left_", leftPlaces, rightPlaces, i, leftPlace, up);
                transitions.UnionWith(leftTransitions);
                leftPlaces.Add(leftPlace);

                // only create a right place if this is not the last level
                if (i != height - 1)
                {
                    Place rightPlace = new Place(name + "_right_level_" + i.ToString());
                    IEnumerable<UpdateTransition> rightTransitions = CreateTwopowerTransitionsForPlace(place, name + "_right_", leftPlaces, rightPlaces, i, rightPlace, up);
                    transitions.UnionWith(rightTransitions);
                    rightPlaces.Add(rightPlace);
                }
            }

            return (leftPlaces, rightPlaces, transitions);
        }

        private static IEnumerable<UpdateTransition> CreateTwopowerTransitionsForPlace(Place towerIncomingPlace, string name, List<Place> leftPlaces, List<Place> rightPlaces, int level, Place currentPlace, bool up)
        {
            HashSet<UpdateTransition> transitions = new HashSet<UpdateTransition>();
            // create transitions incoming to level
            if (level == 0)
            {
                Dictionary<Place, int> previous = new Dictionary<Place, int>() { [towerIncomingPlace] = 1 };
                Dictionary<Place, int> current = new Dictionary<Place, int>() { [currentPlace] = 1 };

                // is the first level, so should take from the incoming place; if tower does not go up, don't need this transition at all
                if (up)
                {
                    UpdateTransition leftTransition = new UpdateTransition(
                        name + "_Up_level" + level.ToString(),
                        previous,
                        current
                    );
                    transitions.Add(leftTransition);
                }

                // also need the backtransition to preserve liveness in case a run guesses the wrong place to proceed to
                UpdateTransition leftBackTransition = new UpdateTransition(
                    name + "_Down_level" + level.ToString(),
                     current,
                     previous
                );

                transitions.Add(leftBackTransition);
            }
            else
            {
                // is not the first level, so take tokens from places of previous level

                Place prevLeft = leftPlaces[level - 1];
                Place prevRight = rightPlaces[level - 1];

                if (up)
                {
                    UpdateTransition leftTransition = new UpdateTransition(
                        name + "_Up_level" + level.ToString(),
                        new Dictionary<Place, int>() { [prevLeft] = 1, [prevRight] = 1 },
                         new Dictionary<Place, int>() { [currentPlace] = 1 }
                    );
                    transitions.Add(leftTransition);
                }

                UpdateTransition leftBackTransition = new UpdateTransition(
                    name + "_Down_level" + level.ToString(),
                    new Dictionary<Place, int>() { [currentPlace] = 1 },
                    new Dictionary<Place, int>() { [prevLeft] = 1, [prevRight] = 1 }
                );

                transitions.Add(leftBackTransition);
            }
            return transitions;
        }

        public override bool HasArcWeights()
        {
            return this.Pre.Any(pair => pair.Value > 1) || this.Post.Any(pair => pair.Value > 1);
        }
    }
}