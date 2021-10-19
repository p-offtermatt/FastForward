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
\status approved 23.05.2012, changed

\brief Evaluates simple property (only SET of states needs to be computed).
Actual property is a parameter of the constructor
*/

#include <Core/Dimensions.h>
#include <CoverGraph/CoverGraph.h>
#include <Exploration/ComputeBoundExploration.h>
#include <Exploration/StatePredicateProperty.h>
#include <Exploration/ChooseTransition.h>
#include <Net/Marking.h>
#include <Net/Net.h>
#include <Net/Place.h>
#include <Net/Transition.h>
#include <SweepLine/Sweep.h>
#include <Formula/StatePredicate/AtomicStatePredicate.h>

/*!
The result will be the maximum (over all reachable markings) of the given formal sum of places.

\param property  contains the expression to be checked as atomic proposition
\param ns  The initial state of the net has to be given as a net state object.
\param myStore  the store to be used. The selection of the store may greatly influence the
performance of the program
\param myFirelist  the firelists to use in this search. The firelist _must_ be
applicable to the given property, else the result of this function may be
wrong. It is not guaranteed that the given firelist will actually be used. In
the parallel work-mode the given list will just be used as a base list form
which all other lists will be generated
\param threadNumber  will be ignored by the standard seach. In the parallel
execution mode this number indicates the number of threads to be used for the
search
*/
int64_t ComputeBoundExploration::depth_first_num(SimpleProperty &property, NetState &ns, Store<void> &myStore,
                                 Firelist &myFirelist, int)
{
    StatePredicateProperty * sp = reinterpret_cast<StatePredicateProperty*>(&property);
    sp -> createDownSets(sp -> predicate);
    AtomicStatePredicate * a = reinterpret_cast<AtomicStatePredicate *>(sp -> predicate);
    //// copy initial marking into current marking
    //Marking::init();

    // prepare property
    property.value = property.initProperty(ns);
    result = a -> sum;

    // add initial marking to store
    // we do not care about return value since we know that store is empty

    myStore.searchAndInsert(ns, 0, 0);

    // get first firelist
    arrayindex_t *currentFirelist;
    arrayindex_t currentEntry = myFirelist.getFirelist(ns, &currentFirelist);

    while (true) // exit when trying to pop from empty stack
    {
        if (currentEntry-- > 0)
        {
            // there is a next transition that needs to be explored in current marking

            // fire this transition to produce new Marking::Current
            Transition::fire(ns, currentFirelist[currentEntry]);

            if (myStore.searchAndInsert(ns, 0, 0))
            {
                // State exists! -->backtracking to previous state
                Transition::backfire(ns, currentFirelist[currentEntry]);
            }
            else
            {
                // State does not exist!
                Transition::updateEnabled(ns, currentFirelist[currentEntry]);
                // check current marking for property
                property.value = property.checkProperty(ns, currentFirelist[currentEntry]);
                if (a->sum > result)
                {
			result = a -> sum;
                }

                SimpleStackEntry *stack = property.stack.push();
                stack = new(stack) SimpleStackEntry(currentFirelist, currentEntry);
                currentEntry = myFirelist.getFirelist(ns, &currentFirelist);
            } // end else branch for "if state exists"
        }
        else
        {
            // firing list completed -->close state and return to previous state
            delete[] currentFirelist;
            if (property.stack.StackPointer == 0)
            {
                // have completely processed initial marking --> state not found
                return result - a -> threshold;
            }
            SimpleStackEntry &stack = property.stack.top();
            currentEntry = stack.current;
            currentFirelist = stack.fl;
            stack.fl = NULL;
            property.stack.pop();
            assert(currentEntry < Net::Card[TR]);
            Transition::backfire(ns, currentFirelist[currentEntry]);
            Transition::revertEnabled(ns, currentFirelist[currentEntry]);
            property.value = property.updateProperty(ns, currentFirelist[currentEntry]);
        }
    }
}


// LCOV_EXCL_START
Path ComputeBoundExploration::path() const
{
    return * (new Path());
}
// LCOV_EXCL_STOP

