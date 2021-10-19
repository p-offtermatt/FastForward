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

\brief class definition for disjunction state predicates
*/

#pragma once

class StatePredicate;
class AtomicStatePredicate;

/// A disjunction state predicate is an OR formula with multiple children that
/// assigns a Boolean value to Marking::Current
class DisjunctionStatePredicate : public StatePredicate
{
public:
    /// arg is number of subformulas
    explicit DisjunctionStatePredicate(arrayindex_t);

    virtual ~DisjunctionStatePredicate();

    /// adds i-th subformula
    void addSub(arrayindex_t i, StatePredicate *f);

    /// evaluates a formula, e.g. upon initialization
    virtual void evaluate(NetState &ns);

    /// evaluates a formula including omega values
    virtual void evaluateOmega(NetState &ns);

    // direct read access for the deletion algorithm
    virtual arrayindex_t getSubs(const StatePredicate *const **subs) const;

    bool isOrNode() const;

    /// returns info on a particular node in the formula tree
    FormulaInfo *getInfo() const;
    /// returns the number of subformulas
    int countSubFormulas() const;

private:
    /// fills stack with an up set of the predicate and return size of upset
    virtual arrayindex_t getUpSet(arrayindex_t *stack, bool *onstack, bool *needEnabled) const;

    /// updates the value of the predicate from true to false
    virtual void updateTF(arrayindex_t);

    /// updates the value of the predicate from false to true
    virtual void updateFT(arrayindex_t);

    /// debug function to check consistency
    virtual bool DEBUG__consistency(NetState &ns);

    /// the list of subformulas
    StatePredicate **sub;

    /// The number of subformulas;
    arrayindex_t cardSub;

    /// The number of satisfied subformulas
    arrayindex_t cardSat;

    /// counts atomic subformulas
    virtual arrayindex_t countAtomic() const;

    /// collects atomic subformulas
    virtual arrayindex_t collectAtomic(AtomicStatePredicate **);

    /// counts fireable subformulas
    virtual arrayindex_t countFireable() const;

    /// collects fireable subformulas
    virtual arrayindex_t collectFireable(FireablePredicate **);

    /// counts deadlock subformulas
    virtual arrayindex_t countDeadlock() const;

    /// collects deadlock subformulas
    virtual arrayindex_t collectDeadlock(DeadlockPredicate **);

    /// create a new state predicate by copy this object.
    virtual StatePredicate *copy(StatePredicate *parent);
    virtual char * toString();
};
