using System;
using System.Collections.Generic;
using System.Linq;

namespace Petri
{
    public class PetriNetWithCapacities : PetriNet
    {

        public Dictionary<Place, int> Capacities;

        public PetriNetWithCapacities(PetriNetWithCapacities net)
        {
            this.Places = net.Places.Select(p => new Place(p.Name)).ToList();
            this.Transitions = net.Transitions.Select(t => t.CopyThis()).ToList();
            this.Capacities = this.Places.ToDictionary(place => place, place => net.Capacities[place]);
        }

        public PetriNetWithCapacities(PetriNet net) :
            this(net, net.Places.ToDictionary(place => place, place => int.MaxValue))
        {
        }

        public PetriNetWithCapacities(IEnumerable<Place> places, IEnumerable<UpdateTransition> transitions) :
            this(places, transitions, places.ToDictionary(place => place, place => int.MaxValue))
        {
        }

        public PetriNetWithCapacities(PetriNet net, Dictionary<Place, int> capacities) : base(net)
        {
            this.Capacities = capacities;
        }

        public PetriNetWithCapacities(IEnumerable<Place> places, IEnumerable<UpdateTransition> transitions, Dictionary<Place, int> capacities) :
            base(places, transitions)
        {
            this.Capacities = capacities;
        }

        public void SetCapacity(Place place, int capacity)
        {
            this.Capacities[place] = capacity;
        }

        // override object.Equals
        public override bool Equals(object obj)
        {
            //
            // See the full list of guidelines at
            //   http://go.microsoft.com/fwlink/?LinkID=85237
            // and also the guidance for operator== at
            //   http://go.microsoft.com/fwlink/?LinkId=85238
            //


            if (!base.Equals(obj))
            {
                return false;
            }
            PetriNetWithCapacities otherNet = obj as PetriNetWithCapacities;
            if (!this.Capacities.OrderBy(t => t.ToString()).SequenceEqual(otherNet.Capacities.OrderBy(t => t.ToString())))
            {
                return false;
            }

            return true;
        }

        // override object.GetHashCode
        public override int GetHashCode()
        {
            return this.Capacities.GetHashCode() + base.GetHashCode();
        }

        public override string ToString()
        {
            return "PLACES\n" + String.Join("\n", Places.Select(place => place.Name.ToString() + ", Capacity: " + Capacities.GetValueOrDefault(place, int.MaxValue))) + "\nTRANSITIONS\n" + String.Join("\n", Transitions);
        }
    }
}