using System;

namespace Petri
{
    public class Place : Node
    {

        private int HashCode;

        public Place(string Name)
        {
            this.Name = Name;
            this.HashCode = Name.GetHashCode();
        }

        public override string ToString()
        {
            return this.Name;
        }

        public override bool Equals(object obj)
        {
            if ((obj == null) || !this.GetType().Equals(obj.GetType()))
            {
                return false;
            }
            else
            {
                Place other = (Place)obj;
                return (other.Name == this.Name);
            }
        }

        public override int GetHashCode()
        {
            return this.HashCode;
        }
    }
}