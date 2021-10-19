/****************************************************************************
  This file is part of LoLA.

  LoLA is free software: you can redistribute it and/or modify it under the
  terms of the GNU Affero General Public License as published by the Free
  Software Foundation, either version 3 of the License, or (at your option)
  any later version.

  LoLA is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
  more details.

  You should have received a copy of the GNU Affero General Public License
  along with LoLA. If not, see <http://www.gnu.org/licenses/>.
****************************************************************************/

/*!
\file FireablePredicate.cc
\author Karsten
\status new

\brief class implementation for fireability state predicates
*/

#include <config.h>
#include <Core/Dimensions.h>
#include <Formula/StatePredicate/FireablePredicate.h>
#include <Net/Net.h>
#include <Net/Place.h>
#include <Net/Transition.h>
#include <Net/NetState.h>
#include <Net/Marking.h>
#include <CoverGraph/CoverGraph.h>
#include <Formula/FormulaInfo.h>

FireablePredicate::FireablePredicate(arrayindex_t tt, bool ssign) :
    t(tt),sign(ssign)
{
parent = NULL;
}

/*!
\brief participate in finding an upset: for deadlock: set need_enabled to true; for no_deadlock: return empty set

\param need_enabled reference parameter that signals that final up-set needs to contain an enabled transition
\return number of elements on stack
*/
arrayindex_t FireablePredicate::getUpSet(arrayindex_t * stack, bool * onstack, bool *need_enabled) const
{
    if (sign) // fireable
    {
	if(onstack[t])
	{
		return 0; // no need to add element
	}
	else
	{
		onstack[t] = true;
		stack[0] = t;
		return 1;  // one element added
	}
    }
    else // not fireable
    {
	arrayindex_t stackpointer = 0;
	for(arrayindex_t ttt = 0; ttt < Transition::CardConflicting[t];++ttt)
	{
		arrayindex_t element = Transition::Conflicting[t][ttt];
		if(!onstack[element])
		{
			onstack[element] = true;
			stack[stackpointer++] = element;
		}
	}
	return stackpointer;
    }
}

/*!
If value of this changes, it needs to be propagated to its parent. The
parameter is the change in the formal sum k_1 p_1 + ... + k_n p_n between the
previously considered marking and the current marking. Having a precomputed
value for this change, evaluation of the formula is accelerated.
*/
void FireablePredicate::update(NetState &ns)
{
    bool newvalue = (sign ? ns.Enabled[t] : !ns.Enabled[t] );
    if (parent)
    {
        if (newvalue && ! value)
        {
            value = newvalue;
            parent->updateFT(position);
            return;
        }
        if (!newvalue && value)
        {
	    value = newvalue;
            parent->updateTF(position);
            return;
        }
    }
    value = newvalue;
}

/*!
Evaluation starts top/down, so the whole formula is examined. Evaluation is
done w.r.t. ns.Current.

\param ns  net state to evaluate the formula
*/
void FireablePredicate::evaluate(NetState &ns)
{
    value = sign ? ns.Enabled[t]  : ! ns.Enabled[t];
}

/*!
Evaluation with Omega starts top/down, so the whole formula is examined. Evaluation is
done w.r.t. ns.Current.

\param ns  net state to evaluate the formula
*/
void FireablePredicate::evaluateOmega(NetState &ns)
{
    unknown = false;
    value = sign ? ns.Enabled[t]  : ! ns.Enabled[t];
//    if (ns.CardEnabled != 0)
//    {
//        for (arrayindex_t i = 0; i < Net::CardPre[TR][t]; i++)
//        {
//            if (ns.Current[Net::Pre[TR][t][i]] == OMEGA)
//            {
//                unknown = true;
//            }
//        }
//    }
}

arrayindex_t FireablePredicate::countAtomic() const
{
    return 0;
}

arrayindex_t FireablePredicate::collectAtomic(AtomicStatePredicate **)
{
    return 0;
}

arrayindex_t FireablePredicate::countDeadlock() const
{
    return 0;
}

arrayindex_t FireablePredicate::collectDeadlock(DeadlockPredicate **c)
{
    return 0;
}

arrayindex_t FireablePredicate::countFireable() const
{
    return 1;
}

arrayindex_t FireablePredicate::collectFireable(FireablePredicate **c)
{
    c[0] = this;
    return 1;
}

// LCOV_EXCL_START
bool FireablePredicate::DEBUG__consistency(NetState &)
{
    return true;
}
// LCOV_EXCL_STOP

/*!
\param parent  the parent predicate for the new, copied, object
*/
StatePredicate *FireablePredicate::copy(StatePredicate *parent)
{
    FireablePredicate *af = new FireablePredicate(t,sign);
    af->value = value;
    af->position = position;
    af->parent = parent;
    return af;
}

arrayindex_t FireablePredicate::getSubs(const StatePredicate *const **) const
{
    return 0;
}

StatePredicate *FireablePredicate::negate()
{
    FireablePredicate *af = new FireablePredicate(t,!sign);
    return af;
}

FormulaInfo *FireablePredicate::getInfo() const
{
    FormulaInfo *Info = new FormulaInfo();
    if (sign)
    {
        Info->tag = formula_deadlock;
    }
    else
    {
        Info->tag = formula_nodeadlock;
    }
    Info->cardChildren = 0;
    Info->f = NULL;
    return Info;
}

int FireablePredicate::countSubFormulas() const
{
    return 1;
}

char * FireablePredicate::toString()
{
	char * result;
	if(sign)
	{
		result = (char *) malloc((11+strlen(Net::Name[TR][t])) * sizeof(char));
		sprintf(result,"FIREABLE(%s)",Net::Name[TR][t]);
		return result;
	}
	else
	{
		result = (char *) malloc((15+strlen(Net::Name[TR][t])) * sizeof(char));
		sprintf(result,"NOT FIREABLE(%s)",Net::Name[TR][t]);
		return result;
	}
}
