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

\brief derives constant predicate TRUE
*/

#pragma once

#include <Core/Dimensions.h>
#include <Formula/StatePredicate/AtomicStatePredicate.h>

class TruePredicate: public AtomicStatePredicate
{
public:
    TruePredicate();

    /// evaluates a formula, e.g. upon initialization
    virtual void evaluate(NetState &) {}

    /// evaluates a formula using omega values
    virtual void evaluateOmega(NetState &) {} // LCOV_EXCL_LINE

    /// for direct read access for the deletion algorithm
    virtual arrayindex_t getSubs(const StatePredicate *const **subs) const;

    /// negates the formula
    virtual StatePredicate *negate();

    /// returns info on a particular node in the formula tree
    FormulaInfo *getInfo() const;
    /// returns the number of subformulas
    int countSubFormulas() const;

    /// fills stack with an up set of the predicate and return size of upset
    virtual arrayindex_t getUpSet(arrayindex_t *stack, bool *onstack, bool *needEnabled) const;

    /// updates the value of the predicate from true to false
    virtual void updateTF(arrayindex_t) {} // LCOV_EXCL_LINE

    /// updates the value of the predicate from false to true
    virtual void updateFT(arrayindex_t) {} // LCOV_EXCL_LINE

    /// debug function to check consistency
    virtual bool DEBUG__consistency(NetState &ns);

    /// counts atomic subformulas
    virtual arrayindex_t countAtomic() const;

    /// collects atomic subformulas
    virtual arrayindex_t collectAtomic(AtomicStatePredicate **);

    /// counts deadlock subformulas
    virtual arrayindex_t countDeadlock() const;

    /// collects deadlock subformulas
    virtual arrayindex_t collectDeadlock(DeadlockPredicate **);

    /// counts fireable subformulas
    virtual arrayindex_t countFireable() {return 0;}

    /// collects fireable subformulas
    virtual arrayindex_t collectFireable(FireablePredicate **){return 0;}

    /// create a new state predicate by copy this object.
    virtual StatePredicate *copy(StatePredicate *parent);
    virtual char * toString();
};
