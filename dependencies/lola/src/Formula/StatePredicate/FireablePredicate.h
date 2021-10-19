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

\brief class definition for fireability as atomic state predicates
*/

#pragma once

#include <Formula/StatePredicate/StatePredicate.h>

/*!
A state predicate is a formula that assigns a Boolean value to Marking::Current
*/
class FireablePredicate : public StatePredicate
{
public:
    explicit FireablePredicate(arrayindex_t,bool); // arg: true = fireable, false = !fireable

    /// updates the value of this sub formula
    void update(NetState &ns);

    /// evaluates a formula, e.g. upon initialization
    virtual void evaluate(NetState &ns);

    /// evaluates a formula including omega values
    virtual void evaluateOmega(NetState &ns);

    /// participate in finding an up-set:
    virtual arrayindex_t getUpSet(arrayindex_t *, bool *, bool *) const;

    /// returns the negated version of this property, but leaves the current one untouched
    virtual StatePredicate *negate();

    /// returns info on a particular node in the formula tree
    FormulaInfo *getInfo() const;
    /// returns the number of subformulas
    int countSubFormulas() const;

private:
    /// updates the value of the predicate from true to false
    void updateTF(arrayindex_t) {}   // LCOV_EXCL_LINE

    /// updates the value of the predicate from false to true
    void updateFT(arrayindex_t) {}   // LCOV_EXCL_LINE

    bool DEBUG__consistency(NetState &ns);

    /// direct read access for the deletion algorithm
    arrayindex_t getSubs(const StatePredicate *const **subs) const;

public:
    bool sign; // true = property is "fireable", false = property is "not fireable"
    arrayindex_t t;

    /// counts atomic subformulas
    arrayindex_t countAtomic() const;

    /// collects atomic subformulas; array must be malloced beforehand
    /// result is number of inserted elements
    arrayindex_t collectAtomic(AtomicStatePredicate **);

    /// counts deadlock subformulas
    arrayindex_t countDeadlock() const;

    /// collects deadlock subformulas; array must be malloced beforehand
    /// result is number of inserted elements
    arrayindex_t collectDeadlock(DeadlockPredicate **);

    /// counts fireable subformulas
    arrayindex_t countFireable() const;

    /// collects fireable subformulas; array must be malloced beforehand
    /// result is number of inserted elements
    arrayindex_t collectFireable(FireablePredicate **);

    // copy function
    StatePredicate *copy(StatePredicate *parent);
    virtual char * toString();
};
