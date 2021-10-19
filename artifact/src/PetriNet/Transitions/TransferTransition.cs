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
    public abstract class TransferTransition : TransitionWithUpdateBehaviour
    {
        protected TransferTransition(string name, Dictionary<Place, int> updatePre, Dictionary<Place, int> updatePost) : base(name, updatePre, updatePost)
        {
        }

        public override bool IsBio()
        {
            return false;
        }

        public override string ToLola()
        {
            throw new NotSupportedException("The LoLA format does not support transfer transitions!");
        }
    }
}