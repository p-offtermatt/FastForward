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
            this.Pre = Pre;
            this.Post = Post;
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
            return "{" + String.Join(", ", Pre) + " --(" + Name + ")--> " + String.Join(", ", Post) + "}";
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
    }
}