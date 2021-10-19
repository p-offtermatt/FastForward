using System.Collections.Generic;
using System.Linq;

namespace Petri
{
    public class Constraints : Dictionary<Place, ConstraintOperators>
    {
        public Constraints() : base()
        {
        }

        public Constraints(IDictionary<Place, ConstraintOperators> inputDict) : base(inputDict)
        {
        }

        public override int GetHashCode()
        {
            int hashCode = 0;
            foreach (Place key in this.Keys)
            {
                int multiplier = this[key] == ConstraintOperators.Equal ? 3 : 5;
                hashCode += multiplier * key.GetHashCode();
            }
            return hashCode;
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
                Constraints other = (Constraints)obj;
                if (this.Count != other.Count)
                {
                    return false;
                }

                foreach (Place key in this.Keys)
                {
                    if (!other.ContainsKey(key))
                    {
                        return false;
                    }
                    if (this[key] != other[key])
                    {
                        return false;
                    }
                }
                return true;
            }
        }
    }
}