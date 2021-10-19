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

\brief derives deadlock checking from SimpleProperty
*/

#pragma once

#include <Core/Dimensions.h>

class DeadlockPredicate;
class FormulaInfo;
class NetState;
class AtomicStatePredicate;
class FireablePredicate;

/*!
A state predicate is a formula that assigns a Boolean value to Marking::Current.
*/
class StatePredicate
{
public:
    static StatePredicate *top;

    /// the value of the predicate in Marking::Current;
    bool value;

    /// flag for unknown value in case of coverability graph checks
    bool unknown;

    /// the parent formula in the syntax tree of the formula
    StatePredicate *parent;

    /// the position of this in parent's subformula list
    arrayindex_t position;

    virtual ~StatePredicate() {}

    /// fills stack with an up set of the predicate and return size of upset
    virtual arrayindex_t getUpSet(arrayindex_t *, bool *, bool *) const = 0;

    /// updates the value of the predicate from true to false
    virtual void updateTF(arrayindex_t) = 0;

    /// updates the value of the predicate from false to true
    virtual void updateFT(arrayindex_t) = 0;

    /// evaluates a formula, e.g. upon initialization
    virtual void evaluate(NetState &) = 0;

    /// evaluates a formula including omega values
    virtual void evaluateOmega(NetState &) = 0;

    /// counts atomic subformulas
    virtual arrayindex_t countAtomic() const = 0;

    /// collects atomic subformulas
    virtual arrayindex_t collectAtomic(AtomicStatePredicate **) = 0;

    /// counts deadlock subformulas
    virtual arrayindex_t countDeadlock() const = 0;

    /// collects deadlock subformulas
    virtual arrayindex_t collectDeadlock(DeadlockPredicate **) = 0;

    /// counts FIREABLE subformulas
    virtual arrayindex_t countFireable() const = 0;

    /// collects FIREABLE subformulas
    virtual arrayindex_t collectFireable(FireablePredicate **) = 0;

    /// debug function to check consistency
    virtual bool DEBUG__consistency(NetState &) = 0;

    /// create a new state predicate by copy this object.
    virtual StatePredicate *copy(StatePredicate *) = 0;

    /// create a new state predicate by copy this object.
    StatePredicate *copy(void);

    /// for direct read access for the deletion algorithm
    virtual arrayindex_t getSubs(const StatePredicate *const **) const = 0;

    /// the following functions are added for the deletion algorithm
    /// the deletion algorithm will only read the state predicates - no changes
    virtual bool isOrNode() const;
    virtual arrayindex_t countUnsatisfied() const; // for and-nodes only!

    /// returns info on a particular node in the formula tree
    virtual FormulaInfo *getInfo() const = 0;
    /// returns the number of subformulas
    virtual int countSubFormulas() const = 0;

    virtual char * toString() = 0;
};
