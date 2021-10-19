using System;

namespace Petri
{
    public abstract class NetParser
    {
        public abstract Tuple<PetriNet, Marking> ReadNet(string filepath);
    }
}