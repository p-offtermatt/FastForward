using System;
using System.Collections.Generic;
using System.Linq;

namespace Petri
{
    public class MultitransferTransition : TransferTransition
    {

        public Dictionary<Place, HashSet<Place>> Transfers;

        public MultitransferTransition(string name, Dictionary<Place, HashSet<Place>> transfers, Dictionary<Place, int> updatePre, Dictionary<Place, int> updatePost) : this(name, transfers)
        {
            this.UpdateBehaviour = new UpdateTransition(name + "_update", updatePre, updatePost);
        }

        public MultitransferTransition(string name, Dictionary<Place, HashSet<Place>> transfers) : base(name, new Dictionary<Place, int>(), new Dictionary<Place, int>())
        {
            this.Transfers = transfers;
        }

        public override Transition CopyThis()
        {
            return new MultitransferTransition(
                this.Name,
                this.Transfers.ToDictionary(
                    kvPair => new Place(kvPair.Key.Name),
                    kvPair => new HashSet<Place>(kvPair.Value)
                ),
                new Dictionary<Place, int>(this.UpdateBehaviour.Pre),
                new Dictionary<Place, int>(this.UpdateBehaviour.Post)
            );
        }

        public override bool Equals(object obj)
        {
            if ((obj == null) || !this.GetType().Equals(obj.GetType()))
            {
                return false;
            }
            else
            {
                MultitransferTransition other = (MultitransferTransition)obj;
                return (this.NameLength == other.NameLength &&
                    other.Name == this.Name &&
                    other.Transfers.Count == this.Transfers.Count &&
                    TransferContentsEqual(other) &&
                    other.UpdateBehaviour.Equals(this.UpdateBehaviour));
            }
        }

        private bool TransferContentsEqual(MultitransferTransition other)
        {
            foreach (var kvPair in this.Transfers)
            {
                bool hasExtraEntries = other.Transfers.GetValueOrDefault(kvPair.Key, new HashSet<Place>()).Except(kvPair.Value).Any();
                if (hasExtraEntries)
                {
                    return false;
                }
            }
            return true;
        }

        public override string ToString()
        {
            return "{" + UpdateBehaviour.ToString() + "; " + String.Join(", ", Transfers.Select(kvPair => kvPair.Key.ToString() + " ~> " + String.Join(" | ", kvPair.Value))) + "}";
        }

        public override HashSet<Marking> FireOn(Marking m)
        {
            return FireOn(m, forwards: true);
        }

        public override HashSet<Marking> FireBackwardsOn(Marking m)
        {
            return FireOn(m, forwards: false);
        }

        public override HashSet<Marking> FireOn(Marking m, bool forwards)
        {
            if (forwards ? !this.IsEnabledIn(m) : !this.IsBackwardsEnabledIn(m))
            {
                return new HashSet<Marking>();
            }

            Marking baseMarking = new Marking(m);
            UpdateBehaviour.RemoveConsumedTokens(ref baseMarking, forwards);

            // count tokens that pre-sets currently hold
            List<Tuple<HashSet<Place>, int>> postSetsAndTokens = new List<Tuple<HashSet<Place>, int>>();
            if (forwards)
            {
                foreach (var kvPair in Transfers)
                {
                    Place place = kvPair.Key;
                    HashSet<Place> postset = kvPair.Value;

                    int tokenNumber = baseMarking.GetValueOrDefault(place);
                    postSetsAndTokens.Add(new Tuple<HashSet<Place>, int>(postset, tokenNumber));
                }
            }
            else
            {
                Dictionary<Place, HashSet<Place>> placesToPreSets = GetPresetsForPlaces();
                throw new NotImplementedException();
            }

            // set transfer pre-places to 0
            IEnumerable<Place> prePlaces = forwards ? this.Transfers.Keys : null;
            foreach (Place place in prePlaces)
            {
                baseMarking[place] = 0;
            }



            UpdateBehaviour.AddProducedTokens(ref baseMarking, forwards);

            // distribute tokens among transfer post places
            HashSet<Marking> result = new HashSet<Marking> { baseMarking };
            foreach (Tuple<HashSet<Place>, int> tuple in postSetsAndTokens)
            {
                HashSet<Place> postSet = tuple.Item1;
                int tokenNumber = tuple.Item2;

                // make postSet into list to be able to order places of the postset
                result = SplitOnPlaces(postSet.ToList(), tokenNumber, result);
            }
            return result;
        }

        private Dictionary<Place, HashSet<Place>> GetPresetsForPlaces()
        {
            throw new NotImplementedException();
        }

        private HashSet<Marking> SplitOnPlaces(List<Place> places, int tokenNumber, HashSet<Marking> markings)
        {
            return markings.SelectMany(marking => SplitOnPlaces(places, tokenNumber, marking)).ToHashSet();
        }

        private HashSet<Marking> SplitOnPlaces(List<Place> places, int tokenNumber, Marking marking)
        {
            HashSet<Marking> result = new HashSet<Marking>();
            foreach (List<int> split in PetriNetUtils.SplitNumberIntoPieces(tokenNumber, places.Count))
            {
                Marking curMarking = new Marking(marking);
                for (int i = 0; i < places.Count; i++)
                {
                    Place place = places[i];
                    int valueToAdd = split[i];
                    curMarking[place] = curMarking.GetValueOrDefault(place, 0) + valueToAdd;
                }
                result.Add(curMarking);
            }
            return result;
        }

        public override HashSet<Place> GetPrePlaces()
        {
            HashSet<Place> updatePre = UpdateBehaviour == null ? new HashSet<Place>() : UpdateBehaviour.GetPrePlaces();
            IEnumerable<Place> transferPre = Transfers.Keys;
            return updatePre.Union(transferPre).ToHashSet();
        }

        public override HashSet<Place> GetPostPlaces()
        {
            HashSet<Place> updatePost = UpdateBehaviour == null ? new HashSet<Place>() : UpdateBehaviour.GetPostPlaces();
            IEnumerable<Place> transferPost = Transfers.SelectMany(kvPair => kvPair.Value);
            return updatePost.Union(transferPost).ToHashSet();
        }

        public override int GetHashCode()
        {
            return
                (this.Name == null ? 0 : this.Name.GetHashCode())
                ^ (this.Transfers == null ? 0 : this.Transfers.
                            Aggregate(0, (acc, kvPair) =>
                                acc ^ kvPair.Key.GetHashCode() + kvPair.Value.Aggregate(0, (acc, place) => acc ^ place.GetHashCode())))
                ^ (this.UpdateBehaviour.GetHashCode());
        }

        public override bool RemovePlaceFromPre(Place place)
        {
            bool removedFromUpdate = UpdateBehaviour.RemovePlaceFromPre(place);
            bool removedFromTransfer = Transfers.Remove(place);
            return removedFromUpdate || removedFromTransfer;
        }

        public override bool RemovePlaceFromPost(Place place)
        {
            bool removedFromUpdate = UpdateBehaviour.RemovePlaceFromPost(place);
            bool removedFromTransfer = false;

            foreach (KeyValuePair<Place, HashSet<Place>> kvPair in Transfers)
            {
                bool removed = kvPair.Value.Remove(place);
                removedFromTransfer = removedFromTransfer || removed;
            }
            return removedFromUpdate || removedFromTransfer;
        }

        public override string ToDotspec()
        {
            throw new NotImplementedException();
        }

        public override Tuple<string, int> ToTTS(Dictionary<Place, int> placeToCounter, int baseState, int nextAvailableStateNumber, int garbageCounterNum)
        {
            throw new NotImplementedException();
        }

        public override bool HasNiceSelfLoop()
        {
            throw new NotImplementedException();
        }

        internal override bool IsStateMachineTransition()
        {
            throw new NotImplementedException();
        }

        public override void AddPlaceToPost(Place place, int value)
        {
            throw new NotImplementedException();
        }

        public override void AddPlaceToPre(Place place, int value)
        {
            throw new NotImplementedException();
        }
    }
}