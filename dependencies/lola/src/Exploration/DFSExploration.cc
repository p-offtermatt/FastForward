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
#include <Exploration/DFSExploration.h>
#include <Exploration/ChooseTransition.h>
#include <Net/Marking.h>
#include <Net/Net.h>
#include <Net/Place.h>
#include <Net/Transition.h>
#include <SweepLine/Sweep.h>

/*!
The result will be
- true, if a marking fulfilling the property was found
- false, if all markings have been explored and no such state was found
- no termination (in time), if the state space is too big and no 'good' marking
  was found

\param property  the property to check
\param ns  The initial state of the net has to be given as a net state object.
If the search has found a state, fulfilling the property this state will be
returned in this parameter.
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
bool DFSExploration::depth_first(SimpleProperty &property, NetState &ns, Store<void> &myStore,
                                 Firelist &myFirelist, int)
{
    //// copy initial marking into current marking
    //Marking::init();

    // prepare property
    property.value = property.initProperty(ns);

    if (property.value)
    {
        // initial marking satisfies property
        return true;
    }

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
                if (property.value)
                {
                    // current  marking satisfies property
                    // push put current transition on stack
                    // this way, the stack contains ALL transitions
                    // of witness path
                    SimpleStackEntry *stack = property.stack.push();
                    stack = new(reinterpret_cast<void *>(stack)) SimpleStackEntry(currentFirelist, currentEntry);
                    return true;
                }

                // Here: current marking does not satisfy property --> continue search
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
                return false;
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

/*!
The result will be
- true, if the state was found
- false, if max attempt(with maxdepth) exhausted without having found state

\param property the property for which a path should be found
\param ns the net state of the initial marking
\param attempts number of attempts. An argument value of 0 will lead to an
unlimited number of attempts: the function will not return if the property not
satisfiable
\param maxdepth maximal depth of each attempt
\param myFirelist the object used to generate the actual firelists, this must
correspond with given property
\param s the user have to given an instance of the empty store
\param c ChooseTransition object to determine which enabled transition to fire.
*/
bool DFSExploration::find_path(SimpleProperty &property, NetState &ns,
                               unsigned int attempts, unsigned int maxdepth,
                               Firelist &myFirelist, EmptyStore<void> &s,
                               ChooseTransition &c)
{
    // this table counts hits for various hash buckets. This is used for
    // steering search into less frequently entered areas of the state space.

    unsigned int currentattempt = 0;

    // get memory for witness path info
    arrayindex_t *witness = new arrayindex_t[maxdepth];

    // loop #attempts times
    while (attempts == 0 || currentattempt++ < attempts)
    {
        // register this try
        s.tries++;

        // copy initial marking into current marking
        memcpy(ns.Current, Marking::Initial, Net::Card[PL] * SIZEOF_ARRAYINDEX_T);
        ns.HashCurrent = Marking::HashInitial;

        // reset enabledness information
        for (arrayindex_t i = 0; i < Net::Card[PL]; i++)
        {
            ns.Current[i] = Marking::Initial[i];
            //ns.CardDisabled[i] = 0;
        }
        ns.CardEnabled = Net::Card[TR];
        for (arrayindex_t t = 0; t < Net::Card[TR]; ++t)
        {
            ns.Enabled[t] = true;
        }

        for (arrayindex_t t = 0; t < Net::Card[TR]; ++t)
        {
            Transition::checkEnabled(ns, t);
        }

        // prepare property
        property.value = property.initProperty(ns);

        if (property.value)
        {
            // initial marking satisfies property: witness path is empty
            _p.initialized = true;
            delete[] witness;
            return true;
        }

        // generate a firing sequence until given depth or deadlock is reached
        for (arrayindex_t depth = 0; depth < maxdepth; ++depth)
        {
            // register this transition's firing
            s.searchAndInsert(ns, 0, 0);

            // get firelist
            arrayindex_t *currentFirelist;
            arrayindex_t cardFirelist = myFirelist.getFirelist(ns, &currentFirelist);
            if (cardFirelist == 0)
            {
                // deadlock or empty up set (i.e. property not reachable)
                break; // go to next attempt
            }

            arrayindex_t chosen = c.choose(ns, cardFirelist, currentFirelist);
            free(currentFirelist);

            witness[depth] = chosen;
            Transition::fire(ns, chosen);
            Transition::updateEnabled(ns, chosen);

            property.value = property.checkProperty(ns, chosen);
            if (property.value)
            {
                // witness path is witness[0],...,witness[depth-1],
                // witness[depth]; current marking satisfies property.
                // copy witness to Path object _p
                _p.initialized = true;
                for (arrayindex_t i = 0; i <= depth; ++i)
                {
                    _p.addTransition(witness[i], false);
                }
                delete[] witness;
                return true;
            }
        }
    }

    // we did not find a state after #attempts
    delete[] witness;
    return false;
}

Path DFSExploration::path() const
{
    return _p;
}

/*!
\param property the property for which a witness state should be found
\param ns the net state of the initial marking
\param myStore A dummy store for counting markings
\param myFirelist the object used to generate the actual firelists, this must correspond with given property
\param frontNumber  indicates the number of sweepline-fronts to be used
\param threadNumber will be ignored by the standard search.
       In the parallel execution mode this number indicates the number of threads to be used for the search
\return if the state was found
*/
bool DFSExploration::sweepline(SimpleProperty &property, NetState &ns, SweepEmptyStore &myStore,
                               Firelist &myFirelist, int frontNumber, int threadNumber)
{
    s = new Sweep<void> (property, ns, myStore, myFirelist, frontNumber, threadNumber);
    return (*s).run();
}

/*!
\param property the property for which a witness state should be found
\param ns the net state of the initial marking
\param myStore The store where marking are saved
\param myFirelist the object used to generate the actual firelists, this must correspond with given property
\param threadNumber will be ignored by the standard search.
       In the parallel execution mode this number indicates the number of threads to be used for the search
\param type The type of formula to solve. Currently AGEF and EF (liveness/reachability) are recognised.
\return if the state was found
*/
ternary_t DFSExploration::cover_breadth_first(SimpleProperty &property, NetState &ns,
        Store<CoverPayload> &myStore,
        Firelist &myFirelist, int threadNumber, formula_t type)
{
    CoverGraph c(property, ns, myStore, myFirelist, _p, threadNumber);
    // the following condition must be replaced by one distinguishing AGEF from EF
    if (type == FORMULA_LIVENESS)
    {
        c.setAGEF();
    }
    return c.run();
}

