using System.Collections.Generic;
using System;
using System.Linq;
using System.Collections;

namespace Petri
{
    public class BinaryTTSParser : TTSParser
    {
        protected override HashSet<Place> GenerateStatePlaces(int numStates)
        {
            int numBits = (int)Math.Log(numStates, 2);
            int capacity = numBits * 2;
            HashSet<Place> result = new HashSet<Place>(capacity);

            for (int i = 0; i < numBits; i++)
            {
                Place falsePlace = new Place("statebit_" + i + "_false");
                result.Add(falsePlace);
                Place truePlace = new Place("statebit_" + i + "_true");
                result.Add(truePlace);
            }
            return result;
        }

        public override Dictionary<Place, int> GetPostForState(int state) => GetStateRepresentation(state);

        public override Dictionary<Place, int> GetPreForState(int state) => GetStateRepresentation(state);

        private Dictionary<Place, int> GetStateRepresentation(int state)
        {
            if (this.NumStates == 0)
            {
                throw new Exception("NumState was not initialized - ReadNet should be called on a parser object first!");
            }
            Dictionary<Place, int> result = new Dictionary<Place, int>();

            BitArray b = new BitArray(new int[] { state });

            for (int i = 0; i < (int)Math.Log(this.NumStates, 2) * 2; i++)
            {
                if (i < b.Count && b[i])
                {
                    result[new Place("statebit_" + i + "_true")] = 1;
                }
                else
                {
                    result[new Place("statebit_" + i + "_false")] = 1;
                }
            }
            return result;
        }

        public override int GetStateNumberFromMarking(Marking marking)
        {
            // TODO if this is a performance bottleneck, get the state in a smarter way
            for (int curState = 0; curState < this.NumStates; curState++)
            {
                Marking stateMarking = this.GetMarkingForState(curState);
                if (marking.Covers(stateMarking))
                {
                    return curState;
                }
            }

            throw new Exception("Marking does not contain any state! Marking was: " + marking.ToString());
        }
    }
}