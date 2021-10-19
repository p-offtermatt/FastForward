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
\file AtomicStatePredicate.cc
\author Karsten
\status new

\brief class implementation for deadlock state predicates
*/

#include <config.h>
#include <Core/Dimensions.h>
#include <Formula/StatePredicate/DeadlockPredicate.h>
#include <Net/Net.h>
#include <Net/Place.h>
#include <Net/NetState.h>
#include <Net/Marking.h>
#include <CoverGraph/CoverGraph.h>
#include <Formula/FormulaInfo.h>

DeadlockPredicate::DeadlockPredicate(bool ssign) :
    sign(ssign)
{
	parent = NULL;
}

/*!
\brief participate in finding an upset: for deadlock: set need_enabled to true; for no_deadlock: return empty set

\param need_enabled reference parameter that signals that final up-set needs to contain an enabled transition
\return number of elements on stack
*/
arrayindex_t DeadlockPredicate::getUpSet(arrayindex_t *, bool *, bool *need_enabled) const
{
    if (sign)
    {
        // property is deadlock
        *need_enabled = true;
    }

    return 0;
}

/*!
If value of this changes, it needs to be propagated to its parent. The
parameter is the change in the formal sum k_1 p_1 + ... + k_n p_n between the
previously considered marking and the current marking. Having a precomputed
value for this change, evaluation of the formula is accelerated.
*/
void DeadlockPredicate::update(NetState &ns)
{
    bool newvalue = (sign ? ns.CardEnabled == 0 : ns.CardEnabled != 0);
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
            parent->updateTF(position);
        }
    }
    value = newvalue;
}

/*!
Evaluation starts top/down, so the whole formula is examined. Evaluation is
done w.r.t. ns.Current.

\param ns  net state to evaluate the formula
*/
void DeadlockPredicate::evaluate(NetState &ns)
{
    value = sign ? ns.CardEnabled == 0 : ns.CardEnabled != 0;
}

/*!
Evaluation with Omega starts top/down, so the whole formula is examined. Evaluation is
done w.r.t. ns.Current.

\param ns  net state to evaluate the formula
*/
void DeadlockPredicate::evaluateOmega(NetState &ns)
{
    unknown = false;
    value = sign ? ns.CardEnabled == 0 : ns.CardEnabled != 0;
    if (ns.CardEnabled != 0)
    {
        for (arrayindex_t i = 0; i < Place::CardSignificant; i++)
        {
            if (ns.Current[i] == OMEGA)
            {
                unknown = true;
            }
        }
    }
}

arrayindex_t DeadlockPredicate::countAtomic() const
{
    return 0;
}

arrayindex_t DeadlockPredicate::collectAtomic(AtomicStatePredicate **)
{
    return 0;
}

arrayindex_t DeadlockPredicate::countDeadlock() const
{
    return 1;
}

arrayindex_t DeadlockPredicate::collectDeadlock(DeadlockPredicate **c)
{
    c[0] = this;
    return 1;
}

arrayindex_t DeadlockPredicate::countFireable() const
{
    return 0;
}

arrayindex_t DeadlockPredicate::collectFireable(FireablePredicate **c)
{
    return 0;
}

// LCOV_EXCL_START
bool DeadlockPredicate::DEBUG__consistency(NetState &)
{
    return true;
}
// LCOV_EXCL_STOP

/*!
\param parent  the parent predicate for the new, copied, object
*/
StatePredicate *DeadlockPredicate::copy(StatePredicate *parent)
{
    DeadlockPredicate *af = new DeadlockPredicate(sign);
    af->value = value;
    af->position = position;
    af->parent = parent;
    return af;
}

arrayindex_t DeadlockPredicate::getSubs(const StatePredicate *const **) const
{
    return 0;
}

StatePredicate *DeadlockPredicate::negate()
{
    DeadlockPredicate *af = new DeadlockPredicate(!sign);
    return af;
}

FormulaInfo *DeadlockPredicate::getInfo() const
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

int DeadlockPredicate::countSubFormulas() const
{
    return 1;
}

char * DeadlockPredicate::toString()
{
	char * result;
	if(sign)
	{
		result = (char *) malloc(9 * sizeof(char));
		sprintf(result,"DEADLOCK");
		return result;
	}
	else
	{
		result = (char *) malloc(11 * sizeof(char));
		sprintf(result,"NODEADLOCK");
		return result;
	}
}
