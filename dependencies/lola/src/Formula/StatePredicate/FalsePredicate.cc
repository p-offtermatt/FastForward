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
\file
\author Karsten
\status new

\brief derives constant predicate FALSE
*/

#include <Core/Dimensions.h>
#include <Formula/StatePredicate/FalsePredicate.h>
#include <Formula/StatePredicate/TruePredicate.h>
#include <Formula/FormulaInfo.h>


FalsePredicate::FalsePredicate()
{
    value = false;
    unknown = false;
}

/*!
Fills stack with an up set of the predicate. Added elements are marked true in
onstack.

\return the size of the up set

// LCOV_EXCL_START

\note getUpSet can be called only if the predicate is not satisfied. The
returned set of transitions has the property that it is impossible to turn the
predicate true without firing one of the transitions.
*/
arrayindex_t FalsePredicate::getUpSet(arrayindex_t *, bool *, bool *) const
{
    return 0;
}

// LCOV_EXCL_STOP


/*!
\return number of atomic subformulas
*/
arrayindex_t FalsePredicate::countAtomic() const
{
    return 0;
}

/*!
\note array must be malloced beforehandresult is number of inserted elements
*/
arrayindex_t FalsePredicate::collectAtomic(AtomicStatePredicate **)
{
    return 0;
}

/*!
\return number of deadlock subformulas
*/
arrayindex_t FalsePredicate::countDeadlock() const
{
    return 0;
}

/*!
\note array must be malloced beforehandresult is number of inserted elements
*/
arrayindex_t FalsePredicate::collectDeadlock(DeadlockPredicate **)
{
    return 0;
}

// LCOV_EXCL_START

/*!
\param parent  the parent predicate for the new, copied, object
*/
StatePredicate *FalsePredicate::copy(StatePredicate *parent)
{
    StatePredicate *p = new FalsePredicate();
    p->parent = parent;
    p->position = position;
    return p;
}

arrayindex_t FalsePredicate::getSubs(const StatePredicate *const **) const
{
    return 0;
}

StatePredicate *FalsePredicate::negate()
{
    return new TruePredicate();
}

bool FalsePredicate::DEBUG__consistency(NetState &)
{
    return true;
}

FormulaInfo *FalsePredicate::getInfo() const
{
    FormulaInfo *Info = new FormulaInfo();

    Info->tag = formula_false;
    Info->cardChildren = 0;
    return Info;
}

int FalsePredicate::countSubFormulas() const
{
    return 1;
}

// LCOV_EXCL_STOP

char * FalsePredicate::toString()
{
	char * result = (char *) malloc(6 * sizeof(char));
	sprintf(result,"FALSE");
	return result;
}
