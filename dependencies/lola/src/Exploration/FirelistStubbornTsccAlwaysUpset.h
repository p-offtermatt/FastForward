/*!
\file
\author Markus
\status new

\brief class for firelist generation. Default is firelist consisting of all
enabled transitions.
*/

#pragma once

#include <Core/Dimensions.h>
#include <Exploration/FirelistStubbornDeadlock.h>

class Firelist;
class StatePredicateProperty;

/// a stubborn firelist for the search for deadlocks
class FirelistStubbornTsccAlwaysUpset : public Firelist

{
public:

    FirelistStubbornTsccAlwaysUpset(SimpleProperty *property);
    ~FirelistStubbornTsccAlwaysUpset();

    StatePredicateProperty *sp;
    /// return value contains number of elements in fire list, argument is reference
    arrayindex_t *dfsStack;
    bool *onStack;
    /// parameter for actual list
    FirelistStubbornDeadlock *dl;

    arrayindex_t getFirelist(NetState &ns, arrayindex_t **);

    Firelist *createNewFireList(SimpleProperty *property);

};
