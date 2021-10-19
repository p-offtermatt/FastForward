using System.Collections.Generic;
using System;
using System.Linq;

namespace Petri
{
    /// <summary>
    /// This class represents (Multi)Transfer-transitions.
    /// These transitions have one or more transfer input places and one or more transfer output places.
    /// Additionally, they have standard input and output places.
    /// Firing the transition
    /// 1) removes the specified numbers of tokens from the standard input places
    /// 2) removes all tokens from transfer input places
    /// 3) nondeterministically splits them among the transfer output places
    /// 4) adds the specified numbers of tokens to the standard output places
    /// </summary>
    public class SetTransferTransition : TransferTransition
    {

        public HashSet<Place> TransferInputs;
        public HashSet<Place> TransferOutputs;

        public SetTransferTransition(string name, HashSet<Place> input, HashSet<Place> output) : base(name, new Dictionary<Place, int>(), new Dictionary<Place, int>())
        {
            this.TransferInputs = input;
            this.TransferOutputs = output;
        }

        public SetTransferTransition(string name, HashSet<Place> transferInput, HashSet<Place> transferOutput, Dictionary<Place, int> updatePre, Dictionary<Place, int> updatePost) : this(name, transferInput, transferOutput)
        {
            this.UpdateBehaviour = new UpdateTransition(name + "_update", updatePre, updatePost);
        }

        public override Transition CopyThis()
        {
            return new SetTransferTransition(this.Name, new HashSet<Place>(this.TransferInputs), new HashSet<Place>(this.TransferOutputs), new Dictionary<Place, int>(UpdateBehaviour.Pre), new Dictionary<Place, int>(UpdateBehaviour.Post));
        }

        public override HashSet<Marking> FireBackwardsOn(Marking m)
        {
            return FireOn(m, forwards: false);
        }

        public override HashSet<Marking> FireOn(Marking m, bool forwards)
        {
            if (!(forwards ? this.IsEnabledIn(m) : this.IsBackwardsEnabledIn(m)))
            { // update behaviour is not enabled
                return new HashSet<Marking>();
            }

            Marking baseMarking = new Marking(m);
            UpdateBehaviour.RemoveConsumedTokens(ref baseMarking, forwards);

            HashSet<Marking> result = new HashSet<Marking>();

            int transferSourceSum = m.Where(kvPair => (forwards ? TransferInputs : TransferOutputs).Contains(kvPair.Key)).Sum(kvPair => kvPair.Value);

            List<Place> transferTargetPlaces = (forwards ? TransferOutputs : TransferInputs).ToList();


            foreach (Place place in (forwards ? TransferInputs : TransferOutputs))
            {
                baseMarking[place] = 0;
            }

            foreach (List<int> split in PetriNetUtils.SplitNumberIntoPieces(transferSourceSum, transferTargetPlaces.Count))
            {
                Marking curMarking = new Marking(baseMarking);
                for (int i = 0; i < transferTargetPlaces.Count; i++)
                {
                    Place place = transferTargetPlaces[i];
                    int valueToAdd = split[i];
                    curMarking[place] = curMarking.GetValueOrDefault(place, 0) + valueToAdd;
                }
                UpdateBehaviour.AddProducedTokens(ref curMarking, forwards);
                result.Add(curMarking);
            }

            return result;
        }

        public override HashSet<Marking> FireOn(Marking m)
        {
            return FireOn(m, forwards: true);
        }

        public override int GetHashCode()
        {
            return 5 * (this.Name == null ? 0 : this.Name.GetHashCode()) +
                    13 * (this.TransferInputs == null ? 0 : this.TransferInputs.
                            Aggregate(0, (acc, place) =>
                                acc + place.GetHashCode())) +
                    17 * (this.TransferOutputs == null ? 0 : this.TransferOutputs.
                            Aggregate(0, (acc, place) =>
                                acc + place.GetHashCode())) +
                    19 * (this.UpdateBehaviour.GetHashCode());
        }

        public override bool Equals(object obj)
        {
            if ((obj == null) || !this.GetType().Equals(obj.GetType()))
            {
                return false;
            }
            else
            {
                SetTransferTransition other = (SetTransferTransition)obj;
                return (this.NameLength == other.NameLength &&
                    other.Name == this.Name &&
                    other.TransferInputs.Count == this.TransferInputs.Count && !other.TransferInputs.Except(this.TransferInputs).Any() &&
                    other.TransferOutputs.Count == this.TransferOutputs.Count && !other.TransferOutputs.Except(this.TransferOutputs).Any()) &&
                    other.UpdateBehaviour.Equals(this.UpdateBehaviour);
            }
        }

        public override HashSet<Place> GetPostPlaces()
        {
            return this.TransferOutputs.Union(this.UpdateBehaviour.GetPostPlaces()).ToHashSet();
        }

        public override HashSet<Place> GetPrePlaces()
        {
            return this.TransferInputs.Union(this.UpdateBehaviour.GetPrePlaces()).ToHashSet();
        }

        public override bool RemovePlaceFromPre(Place place)
        {
            bool removedFromTransfer = TransferInputs.Remove(place);
            bool removedFromUpdate = UpdateBehaviour.RemovePlaceFromPre(place);
            return removedFromTransfer || removedFromUpdate;
        }

        public override bool RemovePlaceFromPost(Place place)
        {
            bool removedFromTransfer = TransferOutputs.Remove(place);
            bool removedFromUpdate = UpdateBehaviour.RemovePlaceFromPost(place);
            return removedFromTransfer || removedFromUpdate;
        }

        public override string ToString()
        {
            return "{" + UpdateBehaviour.ToString() + "; " + String.Join(",", TransferInputs) + "~(" + this.Name + ")~>" + String.Join(",", TransferOutputs) + "}";
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