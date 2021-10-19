using System.Collections.Generic;
using System;
using System.Linq;

namespace Petri
{
    /// <summary>
    /// An abstract base class for transitions that have part update behaviour, part other behaviour.
    /// For example, transfers or multitransfers with guards.
    /// </summary>
    public abstract class TransitionWithUpdateBehaviour : Transition
    {
        public UpdateTransition UpdateBehaviour;


        public TransitionWithUpdateBehaviour(string name, Dictionary<Place, int> updatePre, Dictionary<Place, int> updatePost) : base(name)
        {
            this.UpdateBehaviour = new UpdateTransition(name + "_update", updatePre, updatePost);
        }

        public override bool IsBackwardsEnabledIn(Marking m)
        {
            return UpdateBehaviour.IsBackwardsEnabledIn(m);
        }
        
        public override bool IsEnabledIn(Marking m)
        {
            return UpdateBehaviour.IsEnabledIn(m);
        }

        public override Dictionary<Place, int> GetGuard()
        {
            return this.UpdateBehaviour.GetGuard();
        }

        public override Dictionary<Place, int> GetBackwardsGuard()
        {
            return this.UpdateBehaviour.GetBackwardsGuard();
        }
    }
}