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

\brief class definition for atomic state predicates
*/

#pragma once

#include <Formula/StatePredicate/StatePredicate.h>

/*!
A state predicate is a formula that assigns a Boolean value to Marking::Current
An atomic predicate compares a formal sum of places with a constant The general
shape is: \f$ k_1 \cdot p_1 + \cdots + k_n \cdot p_n \leq k \f$
*/
class AtomicStatePredicate : public StatePredicate
{
protected:
    AtomicStatePredicate();

public:
    AtomicStatePredicate(arrayindex_t, arrayindex_t, int);

    virtual ~AtomicStatePredicate();

    /// adds a place (arg2) with positive factor (arg3) at position arg1
    void addPos(arrayindex_t, arrayindex_t, capacity_t);

    /// adds a place (arg2) with negative factor (arg3) at position arg1
    void addNeg(arrayindex_t, arrayindex_t, capacity_t);

    /// fills stack with an up set of the predicate and return size of upset
    virtual arrayindex_t getUpSet(arrayindex_t *stack, bool *onstack, bool *needEnabled) const;
    virtual arrayindex_t getDownSet(arrayindex_t *stack, bool *onstack, bool *needEnabled) const;

    /// updates the value of this sub formula
    void update(NetState &ns, int);

    /// evaluates a formula, e.g. upon initialization
    virtual void evaluate(NetState &ns);

    /// evaluates a formula including omega values
    virtual void evaluateOmega(NetState &ns);

    void initUpSet(); // initializies static generation of up set
    void finitUpSet(); // finalizes static generation of up set
    void addToUpSet(arrayindex_t); // adds a transition to static up set

    // down set is needed for bound computations
    void initDownSet(); // initializies static generation of up set
    void finitDownSet(); // finalizes static generation of up set
    void addToDownSet(arrayindex_t); // adds a transition to static up set

    /// returns the negated version of this property, but leaves the current one untouched
    virtual StatePredicate *negate();

    /// returns info on a particular node in the formula tree
    FormulaInfo *getInfo() const;
    /// returns the number of subformulas
    int countSubFormulas() const;

    /// apply the gcd to the factors and the sum
    void reduceFactors();

private:
    /// updates the value of the predicate from true to false
    virtual void updateTF(arrayindex_t) {}   // LCOV_EXCL_LINE

    /// updates the value of the predicate from false to true
    virtual void updateFT(arrayindex_t) {}   // LCOV_EXCL_LINE

    virtual bool DEBUG__consistency(NetState &ns);

    /// direct read access for the deletion algorithm
    virtual arrayindex_t getSubs(const StatePredicate *const **subs) const;

public:
    /// lists place p_i indices that occur with positive multiplicity k_i
    arrayindex_t *posPlaces;

    /// lists place p_i indices that occur with negative multiplicity k_i
    arrayindex_t *negPlaces;

    /// lists multiplicities  k_i of places in posPlaces
    capacity_t *posMult;

    /// lists multiplicities  k_i of places in negPlaces
    capacity_t *negMult;

    /// the number of pos entries
    arrayindex_t cardPos;

    /// the number of negative entries
    arrayindex_t cardNeg;

    /// the up set of this formula
    arrayindex_t *up;

    /// the size of the up set
    arrayindex_t cardUp;

    /// the down set of this formula (needed for bound calculation)
    arrayindex_t *down;

    /// the size of the down set
    arrayindex_t cardDown;

    /// the threshold k
    int threshold;

    /// The current value of the formal sum k_1 p_1 + ... + k_n p_n
    int sum;

    /// if true, this is an original property an not a copy, thus it has to free the arrays on the free command
    bool original;

    /// counts atomic subformulas
    virtual arrayindex_t countAtomic() const;
    /// counts deadlock subformulas
    virtual arrayindex_t countDeadlock() const;
    virtual arrayindex_t countFireable() const;

    /// collects atomic subformulas; array must be malloced beforehand
    /// result is number of inserted elements
    virtual arrayindex_t collectAtomic(AtomicStatePredicate **);

    /// collects deadlock subformulas; array must be malloced beforehand
    /// result is number of inserted elements
    virtual arrayindex_t collectDeadlock(DeadlockPredicate **);
    virtual arrayindex_t collectFireable(FireablePredicate **);

    // copy function
    virtual StatePredicate *copy(StatePredicate *parent);
    virtual char * toString();
};
