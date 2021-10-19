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

\brief represents a property to be represented by a predicate
*/

#pragma once

#include <Exploration/SimpleProperty.h>

class DeadlockPredicate;
class FireablePredicate;
class AtomicStatePredicate;
class Firelist;
class StatePredicate;


/*!
\brief represents a property to be represented by a predicate

This is a property based on a state-predicate, which makes statements about a marking.
Incremental update of the property is possible.
*/
class StatePredicateProperty: public SimpleProperty
{
public:
    explicit StatePredicateProperty(StatePredicate *);
    virtual ~StatePredicateProperty();
    StatePredicateProperty() {}
    /// prepare for search
    virtual bool initProperty(NetState &ns);

    /// check property in Marking::Current, use after fire. Argument is transition just fired.
    virtual bool checkProperty(NetState &ns, arrayindex_t);

    /// check property in Marking::Current, use after backfire. Argument is transition just backfired.
    virtual bool updateProperty(NetState &ns, arrayindex_t);

    /// return the predicate used to evaluate the property
    StatePredicate *getPredicate()
    {
        return predicate;
    }

    /// check property in Marking::Current with Omegas.
    virtual bool checkOmegaProperty(NetState &ns);

    /// determine if the result is unknown (after checkOmegaProperty)
    virtual bool isUnknown();

    /// the actual formula to be verified;
    StatePredicate *predicate;

    /// for each transition t, number of state predicates that need to be checked when t is fired
    arrayindex_t *cardChanged;
    /// the number of deadlock propositions
    arrayindex_t cardDeadlock;
    arrayindex_t *cardChangedFireable;

    /// for each transition t, an array with all state predicates that need to be checked when t is fired
    AtomicStatePredicate ** *changedPredicate;
    FireablePredicate ** *changedFireable;

    /// an array with all deadlock type state predicates; need to be checked when any t is fired
    DeadlockPredicate **changedDeadlock;

    /// changedSum[t][i] is the difference that t causes in the formal sum of state predicate changedPredicate[t][i]
    int **changedSum;

    /// create a copy of the property, needed in parallel exploration
    virtual SimpleProperty *copy();

     void createDownSets(StatePredicate *);
};
