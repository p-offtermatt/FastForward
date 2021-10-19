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

#include <Exploration/AutomataTree.h>
#include <Exploration/Firelist.h>
#include <Exploration/LTLExploration.h>
#include <Exploration/LTLStack.h>
#include <Formula/LTL/BuechiAutomata.h>
#include <Net/Net.h>
#include <Net/NetState.h>
#include <Net/Transition.h>
#include <Witness/Path.h>

#define TRIVIAL_SCC_MARKER_DFS -1
#define FLAT_ENTRY_SIZE sizeof(AutomataTree)
//#define FLAT_ENTRY_SIZE (12*SIZEOF_INDEX_T + sizeof(dfsnum_t) + SIZEOF_VOIDP)

LTLExploration::LTLExploration(bool mode) : stateStorageMode(mode)
{}

/// check whether an automata state is contained in an automata tree
inline void searchAndInsertAutomataState(uint32_t automataState,
        AutomataTree **tree, AutomataTree **result, bool mode)
{
    if (mode)
        while (true)
        {
            if (*tree == 0)
            {
                *tree = new AutomataTree(automataState, mode);
                *result = *tree;
                return;
            }
            if ((*tree)->state == automataState)
            {
                *result = *tree;
                return;
            }
            if ((*tree)->state > automataState)
            {
                tree = &(*tree)->smaller;
            }
            else
            {
                tree = &(*tree)->bigger;
            }
        }
    else
    {
        // flat mode
        *result = reinterpret_cast<AutomataTree *>((reinterpret_cast<uint8_t *>
                  (*tree)) + FLAT_ENTRY_SIZE * automataState);
    }
}

inline bool LTLExploration::initialize_transition(NetState &ns, Firelist &firelist,
        BuechiAutomata &automata, arrayindex_t currentAutomataState,
        arrayindex_t *currentFirelistEntry, arrayindex_t **currentFirelist,
        arrayindex_t *currentStateListEntry,
        arrayindex_t *currentStateListLength, arrayindex_t **currentStateList,
        AutomataTree *currentStateEntry)
{
    // produce new firelist
    // the size of the list is not a valid index (thus -1)
    bool deadlock;
    if (currentStateEntry->firelist != 0)
    {
        *currentFirelist = currentStateEntry->firelist;
        deadlock = (*currentFirelistEntry = (currentStateEntry->cardFirelist - 1) ) == (uint32_t) - 1;
    }
    else
    {
        // TODO replace all currentAutomataState with ns.currentAutomataState 
        ns.currentAutomatonState = currentAutomataState;
        deadlock = (*currentFirelistEntry = (firelist.getFirelist(ns,
                                             currentFirelist) - 1)) ==  (uint32_t) - 1;
        currentStateEntry->firelist = *currentFirelist;
        currentStateEntry->cardFirelist = *currentFirelistEntry + 1;
    }
    if (deadlock)
    {
        return true;
    }

    // create automata states
    *currentStateListEntry = automata.getSuccessors(currentStateList, currentAutomataState);
    *currentStateListLength = *currentStateListEntry;

    if (*currentStateListLength == 0)
    {
        // mark this is a "real" deadlock in the product system
        *currentFirelistEntry = -1;
        return true;
    }
    while (true)
    {
        // fire this transition to produce new Marking::Current
        Transition::fire(ns, (*currentFirelist)[*currentFirelistEntry]);
        Transition::updateEnabled(ns, (*currentFirelist)[*currentFirelistEntry]);
        // check whether this transition goes into an non forbidden state
        bool forbidden = false;
        for (arrayindex_t i = 0; i < card_forbidden_transitions; i++)
            if (ns.Enabled[forbidden_transitions[i]])
            {
                forbidden = true;
                break;
            }
        if (!forbidden)
        {
            break;
        }
        Transition::backfire(ns, (*currentFirelist)[*currentFirelistEntry]);
        Transition::revertEnabled(ns, (*currentFirelist)[*currentFirelistEntry]);
        if (--(*currentFirelistEntry) ==  (uint32_t) - 1)
        {
            break;
        }
    }
    if (*currentFirelistEntry !=  (uint32_t) - 1)
    {
        automata.updateProperties(ns, (*currentFirelist)[*currentFirelistEntry]);
    }

    return false;
}


inline void LTLExploration::get_next_transition(BuechiAutomata &automata, NetState &ns,
        arrayindex_t *currentStateListEntry, arrayindex_t *currentFirelistEntry,
        arrayindex_t *currentFirelist,
        arrayindex_t stateListLength, arrayindex_t)
{
    // calculate the next transition
    if (*currentStateListEntry == 0)
    {
        // revert currently fired transitions
        while (true)
        {
            // revert the petrinet
            Transition::backfire(ns, currentFirelist[*currentFirelistEntry]);
            Transition::revertEnabled(ns, currentFirelist[*currentFirelistEntry]);
            automata.revertProperties(ns, currentFirelist[*currentFirelistEntry]);
            // switch to next petri-net transition
            (*currentFirelistEntry)--;
            // if there is no next transition ext
            if (*currentFirelistEntry == (uint32_t) - 1)
            {
                break;
            }
            // pre-fire transition
            Transition::fire(ns, currentFirelist[*currentFirelistEntry]);
            Transition::updateEnabled(ns, currentFirelist[*currentFirelistEntry]);
            // check current marking for property
            automata.updateProperties(ns, currentFirelist[*currentFirelistEntry]);
            // check for forbidden transition
            bool forbidden = false;
            for (arrayindex_t i = 0; i < card_forbidden_transitions; i++)
                if (ns.Enabled[forbidden_transitions[i]])
                {
                    forbidden = true;
                    break;
                }
            if (!forbidden)
            {
                break;
            }
            // set available transitions in Buechi-Automata
        }
        *currentStateListEntry = stateListLength - 1;
    }
    else
        // just switch to the next transition in the buechi-automaton
    {
        (*currentStateListEntry)--;
    }
}



////////////////////////////////////////////////////////////////////////////////
//
//
//           LTL - checks the fairness assumptions and returns
//              -1 if all are fulfilled,   -2 if a weak is not fulfilled (or büchi-property is not true)
//              n (>= 0) - number of strong(on array), which is not fulfilled
///////////////////////////////////////////////////////////////////////////////

arrayindex_t LTLExploration::checkFairness(BuechiAutomata &automata,
        Store<AutomataTree *> &store, Firelist &firelist, NetState &ns,
        arrayindex_t currentAutomataState, dfsnum_t depth, bool **enabledStrongTransitions,
        AutomataTree *currentStateEntry)
{
    // the stack for the search
    SearchStack<LTLFairnessStackEntry> stack;
    // prepare the search
    arrayindex_t *currentFirelist;
    arrayindex_t currentFirelistEntry;
    arrayindex_t *currentStateList;
    arrayindex_t currentStateListEntry, currentStateListLength;
    initialize_transition(ns, firelist, automata, currentAutomataState, &currentFirelistEntry,
                          &currentFirelist, &currentStateListEntry, &currentStateListLength, &currentStateList,
                          currentStateEntry);

    bool buechi_accepting_state = automata.isAcceptingState(currentAutomataState);

    // fairness datastructures
    // weak
    arrayindex_t card_fulfilled_weak = 0;
    bool *fulfilled_weak = new bool[assumptions.card_weak]();
    bool *fulfilled_strong = new bool[assumptions.card_strong]();
    bool *enabled_strong = new bool[assumptions.card_strong]();

    // fairness check for initial
    bool *__enabled_weak = new bool[assumptions.card_weak]();
    if (currentFirelistEntry != (uint32_t) - 1)
    {
        for (arrayindex_t i = 0; i <= currentFirelistEntry; i++)
        {
            if (assumptions.weak_backlist[currentFirelist[i]] != (uint32_t) - 1)
            {
                __enabled_weak[assumptions.weak_backlist[currentFirelist[i]]] = true;
            }
            else if (assumptions.strong_backlist[currentFirelist[i]] != (uint32_t) - 1)
            {
                enabled_strong[i] = true;
            }
        }
    }
    // write not enabled weak back
    for (arrayindex_t i = 0; i < assumptions.card_weak; i++)
        if (!__enabled_weak[i])
        {
            card_fulfilled_weak++;
            fulfilled_weak[i] = true;
        }


    while (true) // exit when trying to pop from empty stack
    {
        if (currentFirelistEntry != (uint32_t) - 1)
        {
            get_next_transition(automata, ns, &currentStateListEntry, &currentFirelistEntry, 
                    currentFirelist, currentStateListLength, currentAutomataState);
        }
        // there is a next transition that needs to be explored in current marking
        // Note: We used to check for (currentFirelistEntry != -1 && currentStateListEntry >= 0) in the if below, which did not make sense as arrayindex_t currentFirelistEntry is always >= 0.
        if (currentFirelistEntry != (uint32_t) - 1)
        {
            // search in the store
            AutomataTree **searchResult;
            store.searchAndInsert(ns, &searchResult, 0);
            AutomataTree *nextStateEntry = 0;
            // search state in state-tree
            searchAndInsertAutomataState(currentStateList[currentStateListEntry], searchResult, &nextStateEntry,
                                         stateStorageMode);

            if (nextStateEntry->dfs == -depth || nextStateEntry->dfs == -depth - 1)
            {
                // maybe the transition itself does fulfill something
                if (assumptions.weak_backlist[currentFirelist[currentFirelistEntry]] != (uint32_t) - 1)
                {
                    // weak, found a successor in set
                    if (!fulfilled_weak[assumptions.weak_backlist[currentFirelist[currentFirelistEntry]]])
                    {
                        card_fulfilled_weak++;
                        fulfilled_weak[assumptions.weak_backlist[currentFirelist[currentFirelistEntry]]] = true;
                    }
                }
                else if (assumptions.strong_backlist[currentFirelist[currentFirelistEntry]] != (uint32_t) - 1)
                {
                    // strong, found a successor in set
                    fulfilled_strong[assumptions.strong_backlist[currentFirelist[currentFirelistEntry]]] = true;
                }

                if (nextStateEntry->dfs == -depth)
                {
                    // switch to next state
                    currentAutomataState = currentStateList[currentStateListEntry];

                    // mark new state as visited
                    nextStateEntry->dfs = -depth - 1;
                    buechi_accepting_state |= automata.isAcceptingState(currentAutomataState);

                    new(stack.push()) LTLFairnessStackEntry(
                        currentFirelist, currentFirelistEntry, currentStateList,
                        currentStateListEntry, currentStateListLength);

                    // prepare for the next state
                    initialize_transition(ns, firelist, automata, currentAutomataState, &currentFirelistEntry,
                                          &currentFirelist, &currentStateListEntry, &currentStateListLength, &currentStateList,
                                          nextStateEntry);

                    // fairness check for new state
                    memset(__enabled_weak, 0, assumptions.card_weak * SIZEOF_BOOL);
                    for (arrayindex_t i = 0; i <= currentFirelistEntry; i++)
                        if (assumptions.weak_backlist[currentFirelist[i]] != (uint32_t) - 1)
                        {
                            __enabled_weak[assumptions.weak_backlist[currentFirelist[i]]] = true;
                        }
                        else if (assumptions.strong_backlist[currentFirelist[i]] != (uint32_t) - 1
                                 && !enabled_strong[assumptions.strong_backlist[currentFirelist[i]]])
                        {
                            enabled_strong[assumptions.strong_backlist[currentFirelist[i]]] = true;
                        }
                    for (arrayindex_t i = 0; i < assumptions.card_weak; i++)
                        if (!__enabled_weak[i] && !fulfilled_weak[i])
                        {
                            card_fulfilled_weak++;
                            fulfilled_weak[i] = true;
                        }
                    // end of fairness adjustments for this transition
                }
            }
        }
        else
        {
            // firing list completed -->close state and return to previous state
            //delete[] currentFirelist;
            if (stack.StackPointer == 0)
            {
                // searched all states, calculate return value
                if (card_fulfilled_weak != assumptions.card_weak
                        || !buechi_accepting_state)
                {
                    delete[] fulfilled_strong;
                    delete[] fulfilled_weak;
                    delete[] enabled_strong;
                    delete[] __enabled_weak;
                    return -2;
                }
                for (arrayindex_t i = 0; i < assumptions.card_strong; i++)
                    if (enabled_strong[i] && !fulfilled_strong[i])
                    {
                        delete[] fulfilled_strong;
                        delete[] fulfilled_weak;
                        delete[] enabled_strong;
                        delete[] __enabled_weak;
                        return i;
                    }
                delete[] fulfilled_strong;
                delete[] fulfilled_weak;
                delete[] __enabled_weak;
                // return the set of enabled strong transitions to main routine, so that we can search for a path on which all these lie
                *enabledStrongTransitions = enabled_strong;
                // all fairness assumptions are fulfilled
                return -1;
            }

            // load the own predecessor
            LTLFairnessStackEntry &stackEntry = stack.top();
            currentFirelistEntry = stackEntry.current_on_firelist;
            currentFirelist = stackEntry.fl;
            currentStateListEntry = stackEntry.current_on_statelist;
            currentStateList = stackEntry.states;
            currentStateListLength = stackEntry.length_of_statelists;
            stack.pop();

            assert(currentFirelistEntry < Net::Card[TR]);
            // revert the buechi automata
            currentAutomataState = currentStateList[currentStateListEntry];
        }
    }
}



////////////////////////////////////////////////////////////////////////////////
//
//
//           LTL - interates over all elements of an SCC, so that we can start all the searches
//              assumption: all markings of current component have DFS = -depth-1
//
///////////////////////////////////////////////////////////////////////////////

bool LTLExploration::start_restricted_searches(BuechiAutomata &automata,
        Store<AutomataTree *> &store, Firelist &firelist, NetState &ns,
        arrayindex_t currentAutomataState, AutomataTree *currentStateEntry, dfsnum_t depth,
        arrayindex_t currentNextDF)
{
    // all elements in current SCC have DFS = -depth - 1 (those, which have not been visited by a SCC-search)
    // will set on all visited markings DFS = -depth - 3

    // current markings belongs to a new SCC
    bool forbidden = false;
    for (arrayindex_t i = 0; i < card_forbidden_transitions; i++)
        if (ns.Enabled[forbidden_transitions[i]])
        {
            forbidden = true;
            break;
        }
    if (!forbidden)
    {
        if (searchFair(automata, store, firelist, ns, currentAutomataState, currentStateEntry, depth,
                       currentNextDF))
        {
            return true;
        }
    }
    // if this SCC does not fulfill the property and the fairness assumption, search for non-fain-connected SCC's


    // the stack for the search
    SearchStack<LTLStackEntry> stack;
    // get first firelist
    arrayindex_t *currentFirelist;
    // the size of the list is not a valid index (thus -1)
    arrayindex_t currentFirelistEntry;
    arrayindex_t *currentStateList;
    arrayindex_t currentStateListEntry, currentStateListLength;
    // ignore last forbidden ones
    card_forbidden_transitions--;
    initialize_transition(ns, firelist, automata, currentAutomataState, &currentFirelistEntry,
                          &currentFirelist, &currentStateListEntry, &currentStateListLength, &currentStateList,
                          currentStateEntry);
    card_forbidden_transitions++;


    while (true) // exit when trying to pop from empty stack
    {
        // calculate the next transition
        card_forbidden_transitions--;
        if (currentFirelistEntry != (uint32_t) - 1)
        {
            get_next_transition(automata, ns, &currentStateListEntry, &currentFirelistEntry, 
                    currentFirelist, currentStateListLength, currentAutomataState);
        }
        card_forbidden_transitions++;

        // Note: We used to check for (currentFirelistEntry != -1 && currentStateListEntry >= 0) in the if below, which did not make sense as arrayindex_t currentFirelistEntry is always >= 0.
        if (currentFirelistEntry != (uint32_t) - 1)
        {
            // there is a next transition that needs to be explored in current marking

            // search in the store
            AutomataTree **searchResult;
            store.searchAndInsert(ns, &searchResult, 0);
            AutomataTree *nextStateEntry = 0;
            // search state in state-tree
            searchAndInsertAutomataState(currentStateList[currentStateListEntry], searchResult, &nextStateEntry,
                                         stateStorageMode);

            // greater means, that the component is marking is 'outside' of this component
            if (nextStateEntry->dfs == -depth - 1)
            {
                // switch to next state
                currentAutomataState = currentStateList[currentStateListEntry];
                // check forbidden transitions
                forbidden = false;
                for (arrayindex_t i = 0; i < card_forbidden_transitions; i++)
                    if (ns.Enabled[forbidden_transitions[i]])
                    {
                        forbidden = true;
                        break;
                    }
                if (!forbidden)
                {
                    // found a fair example
                    if (searchFair(automata, store, firelist, ns, currentAutomataState, nextStateEntry, depth,
                                   currentNextDF))
                    {
                        // produce witness path
                        *(witness.push()) = currentFirelist[currentFirelistEntry];
                        while (stack.StackPointer)
                        {
                            *(witness.push()) = stack.top().fl[stack.top().current_on_firelist];
                            stack.top().~LTLStackEntry();
                            stack.pop();
                        }
                        //delete[] currentFirelist;
                        delete[] currentStateList;
                        return true;
                    }
                }
                // mark this state as visited
                nextStateEntry->dfs = -depth - 3;

                LTLStackEntry *stackEntry = stack.push();
                stackEntry = new(stackEntry) LTLStackEntry(currentFirelist,
                        currentFirelistEntry, currentStateList,
                        currentStateListEntry, currentStateListLength,
                        -1, currentStateEntry);

                currentStateEntry = nextStateEntry;

                card_forbidden_transitions--;
                initialize_transition(ns, firelist, automata, currentAutomataState, &currentFirelistEntry,
                                      &currentFirelist, &currentStateListEntry, &currentStateListLength, &currentStateList,
                                      nextStateEntry);
                card_forbidden_transitions++;

                if (currentFirelistEntry == (uint32_t) - 1)
                {
                    // firelist is empty, this node forms its own SCC, and is thus not important
                    //delete[] currentFirelist;
                    LTLStackEntry &stackEntry = stack.top();
                    currentFirelistEntry = stackEntry.current_on_firelist;
                    currentFirelist = stackEntry.fl;
                    currentStateListEntry = stackEntry.current_on_statelist;
                    currentStateList = stackEntry.states;
                    currentStateEntry = stackEntry.dfs;
                    currentStateListLength = stackEntry.length_of_statelists;
                    stack.pop();
                }
            }
        }
        else
        {
            // firing list completed -->close state and return to previous state
            forbidden = false;
            for (arrayindex_t i = 0; i < card_forbidden_transitions; i++)
                if (ns.Enabled[forbidden_transitions[i]])
                {
                    forbidden = true;
                    break;
                }
            // if the state is forbidden, we need to delete the current firelist
            if (forbidden)
            {
                delete[] currentFirelist;
            }
            // no example has been found
            if (stack.StackPointer == 0)
            {
                return false;
            }

            // load the own predecessor
            LTLStackEntry &stackEntry = stack.top();
            currentFirelistEntry = stackEntry.current_on_firelist;
            currentFirelist = stackEntry.fl;
            currentStateListEntry = stackEntry.current_on_statelist;
            currentStateList = stackEntry.states;
            currentStateEntry = stackEntry.dfs;
            currentStateListLength = stackEntry.length_of_statelists;
            stack.pop();

            assert(currentFirelistEntry < Net::Card[TR]);
            currentAutomataState = currentStateList[currentStateListEntry];
        }
    }
}


////////////////////////////////////////////////////////////////////////////////
//
//
//           LTL-Fairness-Checker
//
//
///////////////////////////////////////////////////////////////////////////////
bool LTLExploration::searchFair(BuechiAutomata &automata,
                                Store<AutomataTree *> &store, Firelist &firelist, NetState &ns,
                                arrayindex_t currentAutomataState, AutomataTree *currentStateEntry,
                                dfsnum_t depth, arrayindex_t initialDFS)
{
    // at this point we assume, that all states being outside the current SCC
    // depth and DFS numbers
    // dfs numbers < initialDFS    -    unprocessed outside
    // dfs numbers > -depth        -    processed outside
    // dfs number == -depth        -    SCC to check fairness
    // dfs number == -depth-1      -    visited by initial DFS (checkFair)
    // oo>dfs number>initialDFS    -    on local tarjan stack
    // dfs number == -depth-2      -    fully processed local
    // dfs number == -depth-3      -    visited by iteration over SCC (search all SCS)


    // start the new Tarjan DFS from the point of the current marking (the checkFairness will 
    // have put us there)
    // the initial next DFS number is the current one of the outer search
    // the stack for the search
    SearchStack<LTLStackEntry> stack;
    // pseudo tarjan stack, for being able to mark states as "not on tarjan-stack"
    SearchStack<AutomataTree *> tarjanStack;

    /// current global dfs number
    arrayindex_t currentNextDFSNumber = initialDFS;

    // get first firelist
    arrayindex_t *currentFirelist = NULL;
    arrayindex_t currentFirelistEntry;
    // arrayindex_t *currentStateList;
    arrayindex_t *currentStateList = NULL;
    arrayindex_t currentStateListEntry, currentStateListLength;
    arrayindex_t currentLowlink = initialDFS;

    // if there is a deadlock
    if (initialize_transition(ns, firelist, automata, currentAutomataState, &currentFirelistEntry,
                              &currentFirelist, &currentStateListEntry, &currentStateListLength, &currentStateList,
                              currentStateEntry))
    {
        // TODO deadlock semantic
        //rep->message("ACC %d",is_next_state_accepting(automata,currentAutomataState));
        if (is_next_state_accepting(automata, currentAutomataState))
        {
	  //if (currentStateList != NULL)
	      delete[] currentStateList;
            //delete[] currentFirelist;
            return true;
        }
        else
        {
            return false;
        }
    }

    // set dfs number
    currentStateEntry->dfs = currentNextDFSNumber++;

    while (true) // exit when trying to pop from empty stack
    {
        if (currentFirelistEntry != (uint32_t) - 1)
        {
            get_next_transition(automata, ns, &currentStateListEntry, &currentFirelistEntry, 
                    currentFirelist, currentStateListLength, currentAutomataState);
        }

        if (currentFirelistEntry != (uint32_t) - 1 && currentStateListEntry != (uint32_t) - 1)
        {
            // there is a next transition that needs to be explored in current marking

            // search in the store
            AutomataTree **searchResult;
            AutomataTree *nextStateEntry;
            //bool newStateFound;
            if (!store.searchAndInsert(ns, &searchResult, 0))
            {
                if (stateStorageMode)
                {
                    *searchResult = new AutomataTree(currentStateList[currentStateListEntry], stateStorageMode);
                    // set next pointer of a state entry
                    nextStateEntry = *searchResult;
                }
                else
                {
                    *searchResult = new AutomataTree[automata.getNumberOfStates()];
                    for (arrayindex_t i = 0; i < automata.getNumberOfStates(); i++)
                    {
                        reinterpret_cast<AutomataTree *>(*searchResult)[i].state = i;
                        reinterpret_cast<AutomataTree *>(*searchResult)[i].dfs = DFS_INITIAL_INVALID_NUMBER;
                        reinterpret_cast<AutomataTree *>(*searchResult)[i].firelist = NULL;
                    }
                    reinterpret_cast<AutomataTree *>(*searchResult)[currentStateList[currentStateListEntry]].state =
                        currentStateList[currentStateListEntry];
                    // set next pointer of a state entry
                    searchAndInsertAutomataState(currentStateList[currentStateListEntry], searchResult, &nextStateEntry,
                                                 stateStorageMode);
                }
            }
            else
            {
                searchAndInsertAutomataState(currentStateList[currentStateListEntry], searchResult, &nextStateEntry,
                                             stateStorageMode);
            }

            // unseen state
            if (nextStateEntry->dfs == -depth - 1)
            {
                // switch to next state
                currentAutomataState = currentStateList[currentStateListEntry];

                // push to dfs stack
                new(stack.push()) LTLStackEntry(currentFirelist,
                                                currentFirelistEntry, currentStateList,
                                                currentStateListEntry, currentStateListLength,
                                                currentLowlink, currentStateEntry);

                // if we are deadlocked, we may have found a finite example
                bool deadlock = initialize_transition(ns, firelist, automata, currentAutomataState,
                                                      &currentFirelistEntry, &currentFirelist, &currentStateListEntry, &currentStateListLength,
                                                      &currentStateList, nextStateEntry);
                if (deadlock && is_next_state_accepting(automata, currentAutomataState))
                {
                    // current marking has no successor and is in an accepting state
                    // ==> finite counter example
                    while (stack.StackPointer)
                    {
                        *(witness.push()) = stack.top().fl[stack.top().current_on_firelist];
                        // delete fire and state lists
                        stack.top().~LTLStackEntry();
                        stack.pop();
                    }
                    return true;
                }

                // set initial low-link number and local dfs
                currentLowlink = currentNextDFSNumber;
                // create a new automata state
                currentStateEntry = nextStateEntry;
                currentStateEntry->dfs = currentNextDFSNumber++;


                // this marking is not worth pursuing it
                // firelist is empty, this node forms its own SCC, and is thus not important
                if (currentFirelistEntry == (uint32_t) - 1)
                {
                    if (!deadlock)
                    {
                        delete[] currentStateList;
                        //delete[] currentFirelist;
                    }
                    currentStateEntry->dfs = TRIVIAL_SCC_MARKER_DFS;
                    LTLStackEntry &stackEntry = stack.top();
                    currentFirelistEntry = stackEntry.current_on_firelist;
                    currentFirelist = stackEntry.fl;
                    currentStateListEntry = stackEntry.current_on_statelist;
                    currentStateList = stackEntry.states;
                    currentLowlink = stackEntry.lowlink;
                    currentStateEntry = stackEntry.dfs;
                    currentStateListLength = stackEntry.length_of_statelists;
                    stack.pop();
                }
            }
            else
            {
                // State exists! -->backtracking to previous state
                // if on tarjan-stack (indicated by dfs < -depth for all depth)
                if (nextStateEntry->dfs >= initialDFS
                        && currentLowlink > nextStateEntry->dfs)
                {
                    currentLowlink = nextStateEntry->dfs;
                }
            }
        }
        else
        {
            // current marking has been completely processed

            // check for the end of the SCC
            if (currentLowlink == currentStateEntry->dfs)
            {
                // found the begin of an SCC, so check, whether it is an counter example
                bool foundcounterexample = automata.isAcceptingState(currentAutomataState);
                bool nonTrivial = tarjanStack.StackPointer && tarjanStack.top()->dfs > currentStateEntry->dfs;

                if (!nonTrivial)
                {
                    // check whether a "trivial" SCC is indeed a cycle
                    arrayindex_t *tempFireList = currentStateEntry->firelist;
                    arrayindex_t cardTempFireList = currentStateEntry->cardFirelist;
                    for (arrayindex_t i = 0; i < cardTempFireList; i++)
                        if (Transition::isCycle(tempFireList[i])
                                && is_next_state_accepting(automata, currentAutomataState))
                        {
                            nonTrivial = true;
                            break;
                        }
                    //delete[] tempFireList;
                }

                SearchStack<arrayindex_t *> firelist_stack;

                // make preparations if this is not the counter example
                // not found the counter example, so discard the component
                while (tarjanStack.StackPointer
                        && tarjanStack.top()->dfs > currentStateEntry->dfs)
                {
                    // mark all states as visited
                    tarjanStack.top()->dfs = -currentNextDepth;
                    *(firelist_stack.push()) = tarjanStack.top()->firelist;
                    // check whether is state is an accepting one
                    foundcounterexample |= automata.isAcceptingState(tarjanStack.top()->state);
                    tarjanStack.pop();
                }

                // remove current node from tarjan stack and mark it as already visited (once) for the fairness check
                // all markings in current SCC have DFS = -currentNextDepth
                // the initial marking shall have DFS = -currentNextDepth -1 (already visited)
                currentStateEntry->dfs = -currentNextDepth - 1;
                // if a counter example if found it must be inside a non trivial SCC, else it is none
                currentNextDepth += DFS_NUMBERS_PER_PNUELI; // switch to next DFS-number zone
                if (foundcounterexample && nonTrivial)
                {
                    // first we need to check all fairness assumptions via DFS
                    bool *enabled_strong_fair;
                    arrayindex_t checkResult = checkFairness(automata, store, firelist, ns, currentAutomataState,
                                               currentNextDepth - DFS_NUMBERS_PER_PNUELI, &enabled_strong_fair, currentStateEntry);
                    // now all markings current SCC have DFS = -(currentNextDepth-3) - 1;
                    if (checkResult == (uint32_t) - 1)
                    {
                        // prepare data-structures for finding a witness path
                        bool *fulfilledWeak = new bool[assumptions.card_weak]();
                        bool *fulfilledStrong = new bool[assumptions.card_strong]();
                        arrayindex_t fulfilled_conditions = 0;
                        for (arrayindex_t i = 0; i < assumptions.card_strong; i++)
                            if (!enabled_strong_fair[i])
                            {
                                fulfilled_conditions++, fulfilledStrong[i] = true;
                            }
                        if (automata.isAcceptingState(currentAutomataState))
                        {
                            fulfilled_conditions++;
                        }
                        // update weak conditions
                        bool *__enabled_weak = new bool[assumptions.card_weak]();
                        for (arrayindex_t i = 0; i < ns.CardEnabled; i++)
                            if (assumptions.weak_backlist[ns.Enabled[i]] != (uint32_t) - 1)
                            {
                                __enabled_weak[assumptions.weak_backlist[ns.Enabled[i]]] = true;
                            }
                        // write not enabled weak back
                        for (arrayindex_t i = 0; i < assumptions.card_weak; i++)
                            if (!__enabled_weak[i])
                            {
                                fulfilled_conditions++, fulfilledWeak[i] = true;
                            }
                        // produce the witness path inside the SCC
                        if (fulfilled_conditions == assumptions.card_strong + assumptions.card_weak + 1)
                        {
                            completeWitness(automata, store, firelist, ns, currentAutomataState, currentStateEntry,
                                            currentNextDepth - DFS_NUMBERS_PER_PNUELI, currentNextDepth, currentStateEntry);
                        }
                        else
                            produceWitness(automata, store, firelist, ns, currentAutomataState, currentStateEntry,
                                           currentNextDepth - DFS_NUMBERS_PER_PNUELI, currentNextDepth,
                                           fulfilledWeak, fulfilledStrong, fulfilled_conditions,
                                           automata.isAcceptingState(currentAutomataState), currentStateEntry);

                        // free needed datastructures
                        delete[] fulfilledWeak;
                        delete[] fulfilledStrong;
                        //delete[] currentFirelist;
                        delete[] currentStateList;
                        // add path from the initial marking to the SCC
                        *(witness.push()) = -1;
                        while (stack.StackPointer)
                        {
                            *(witness.push()) = stack.top().fl[stack.top().current_on_firelist];
                            stack.top().~LTLStackEntry();
                            stack.pop();
                        }
                        // free all firelists on the current stack
                        while (stack.StackPointer)
                        {
                            delete[] stack.top().dfs->firelist;
                            stack.pop();
                        }
                        delete[] __enabled_weak;
                        return true;
                    }
                    if (checkResult != (uint32_t) - 2)
                    {
                        // if a strong fairness assumption is not fulfilled -> search smaller components
                        forbidden_transitions[card_forbidden_transitions++] = assumptions.strong_fairness[checkResult];

                        // check for fairness via lichtenstein-pnueli
                        if (start_restricted_searches(automata, store, firelist, ns,
                                                      currentAutomataState, currentStateEntry, currentNextDepth - DFS_NUMBERS_PER_PNUELI,
                                                      currentNextDFSNumber))
                        {
                            //delete[] currentFirelist;
                            delete[] currentStateList;
                            while (stack.StackPointer)
                            {
                                *(witness.push()) = stack.top().fl[stack.top().current_on_firelist];
                                stack.top().~LTLStackEntry();
                                stack.pop();
                            }
                            return true;
                        }
                        // the transition is not any more forbidden
                        --card_forbidden_transitions;
                    }
                    else
                        // weak fairness assumption is not fulfilled, so delete all firelists
                        while (firelist_stack.StackPointer)
                        {
                            delete[] firelist_stack.top();
                            firelist_stack.pop();
                        }
                }
                else
                {
                    // these firelists will be used never again
                    while (firelist_stack.StackPointer)
                    {
                        delete[] firelist_stack.top();
                        firelist_stack.pop();
                    }
                    // delete firelists, they will not be needed any more
                    if (!nonTrivial)
                    {
                        currentStateEntry->dfs = TRIVIAL_SCC_MARKER_DFS;
                        // if a counter example if found it must be inside a non trivial SCC, else it is none
                        currentNextDepth -= DFS_NUMBERS_PER_PNUELI;
                    }
                }
            }
            else
            {
                // push state onto tarjan stack
                // this stack contains all elements, which are on the "real" tarjan but not on the dfs stack
                AutomataTree **tarjanEntry = tarjanStack.push();
                *tarjanEntry = currentStateEntry;
            }

            // switch back to the previous state
            // save current lowlink number
            arrayindex_t oldstateLowLink = currentLowlink;

            // firing list completed -->close state and return to previous state
            //delete[] currentFirelist;
            delete[] currentStateList;
            if (stack.StackPointer == 0)
            {
                // have completely processed initial marking --> SCC not found
                return false;
            }

            // load the own predecessor
            LTLStackEntry &stackEntry = stack.top();

            currentFirelistEntry = stackEntry.current_on_firelist;
            currentFirelist = stackEntry.fl;
            currentStateListEntry = stackEntry.current_on_statelist;
            currentStateList = stackEntry.states;
            currentLowlink = stackEntry.lowlink;
            currentStateEntry = stackEntry.dfs;
            currentStateListLength = stackEntry.length_of_statelists;
            stack.pop();

            // adjust own lowlink
            if (oldstateLowLink < currentLowlink)
            {
                currentLowlink = oldstateLowLink;
            }

            assert(currentFirelistEntry < Net::Card[TR]);
            // revert the buechi automata
            currentAutomataState = currentStateList[currentStateListEntry];
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//            LTL-Modelchecker
//
//
///////////////////////////////////////////////////////////////////////////////
bool LTLExploration::checkProperty(BuechiAutomata &automata,
                                   Store<AutomataTree *> &store, Firelist &firelist, NetState &ns)
{
    // prepare strong fairness assumptions
    assumptions.card_strong = 0;
    for (arrayindex_t i = 0; i < Net::Card[TR]; i++)
        if (Transition::Fairness[i] == STRONG_FAIRNESS)
        {
            assumptions.card_strong++;
        }
    assumptions.strong_fairness = new arrayindex_t[assumptions.card_strong]();
    assumptions.strong_backlist = new arrayindex_t[Net::Card[TR]]();
    // put all strong fair transitions into an array
    arrayindex_t __card_on_sf = 0;
    for (arrayindex_t i = 0; i < Net::Card[TR]; i++)
    {
        if (Transition::Fairness[i] == STRONG_FAIRNESS)
        {
            assumptions.strong_fairness[__card_on_sf] = i;
            assumptions.strong_backlist[i] = __card_on_sf++;
        }
        else
        {
            assumptions.strong_backlist[i] = -1;
        }
    }

    // put all weak fair transitions into an array
    assumptions.card_weak = 0;
    for (arrayindex_t i = 0; i < Net::Card[TR]; i++)
        if (Transition::Fairness[i] == WEAK_FAIRNESS)
        {
            assumptions.card_weak++;
        }
    assumptions.weak_fairness = new arrayindex_t[assumptions.card_weak]();
    assumptions.weak_backlist = new arrayindex_t[Net::Card[TR]]();
    arrayindex_t __card_on_wf = 0;
    for (arrayindex_t i = 0; i < Net::Card[TR]; i++)
    {
        if (Transition::Fairness[i] == WEAK_FAIRNESS)
        {
            assumptions.weak_fairness[__card_on_wf] = i;
            assumptions.weak_backlist[i] = __card_on_wf++;
        }
        else
        {
            assumptions.weak_backlist[i] = -1;
        }
    }

    // prepare forbidden transtitions
    forbidden_transitions = new arrayindex_t[assumptions.card_strong]();
    ////rep->message("FORB %x",forbidden_transtitions);
    card_forbidden_transitions = 0;

    // initialize the properties
    automata.initProperties(ns);

    /// current state of the buechi-automata
    arrayindex_t currentAutomataState = 0;

    /// current global dfs number
    arrayindex_t currentNextDFSNumber = 1;

    // we need one number for the trivial SCC's
    currentNextDepth = 5;

    // iterate over all possible initial states
    //arrayindex_t* currentStateList;
    //arrayindex_t stateListLenght = automata.getSuccessors(&currentStateList, currentAutomataState);

    AutomataTree **currentStateEntry;
    store.searchAndInsert(ns, &currentStateEntry, 0);
    if (stateStorageMode)
    {
        *currentStateEntry = new AutomataTree(currentAutomataState, stateStorageMode);
    }
    else
    {
        *currentStateEntry = new AutomataTree[automata.getNumberOfStates()];
        for (arrayindex_t i = 0; i < automata.getNumberOfStates(); i++)
        {
            reinterpret_cast<AutomataTree *>(*currentStateEntry)[i].dfs = DFS_INITIAL_INVALID_NUMBER;
            reinterpret_cast<AutomataTree *>(*currentStateEntry)[i].firelist = NULL;
        }
        reinterpret_cast<AutomataTree *>(*currentStateEntry)[currentAutomataState].state =
            currentAutomataState;
    }
    // set dfs number
    (*currentStateEntry)->dfs = currentNextDFSNumber;

    const bool result = searchFair(automata, store, firelist, ns, currentAutomataState, *currentStateEntry,
                             currentNextDepth - DFS_NUMBERS_PER_PNUELI, currentNextDFSNumber);

    // cleanup
    delete[] assumptions.strong_backlist;
    delete[] assumptions.strong_fairness;
    delete[] assumptions.weak_backlist;
    delete[] assumptions.weak_fairness;
    delete[] forbidden_transitions;

    return result;
}

////////////////////////////////////////////////////////////////////////////////
//
//  Construct a witness path
//      -> all states in current SCC have       DFS = -depth - 1
//
//  need to find a path which fulfills all weak fairness assumptions and all strong ones (not visiting forbidden ones will give them)
//
//  ==>     witnessdepth = depth + 4
//
//  all visited states if a DFS have            DFS = -witnessdepth
//  the final one uses                          DFS = -depth - 2
//
//
//
////////////////////////////////////////////////////////////////////////////////




////////////////////////////////////////////////////////////////////////////////
//
//
//           LTL - produces the witness path
//              assumption: all markings of current component have DFS = -depth-1
//
///////////////////////////////////////////////////////////////////////////////

void LTLExploration::produceWitness(BuechiAutomata &automata,
                                    Store<AutomataTree *> &store, Firelist &firelist, NetState &ns,
                                    arrayindex_t currentAutomataState, AutomataTree *currentStateEntry,
                                    dfsnum_t depth, dfsnum_t witness_depth, bool *fulfilled_weak, bool *fulfilled_strong,
                                    arrayindex_t fulfilled_conditions, bool acceptingStateFound, AutomataTree *targetPointer)
{
    // the stack for the search
    SearchStack<LTLStackEntry> stack;
    // get first firelist
    arrayindex_t *currentFirelist;
    // the size of the list is not a valid index (thus -1)
    arrayindex_t currentFirelistEntry;
    arrayindex_t *currentStateList;
    arrayindex_t currentStateListEntry, currentStateListLength;
    initialize_transition(ns, firelist, automata, currentAutomataState, &currentFirelistEntry,
                          &currentFirelist, &currentStateListEntry, &currentStateListLength, &currentStateList,
                          currentStateEntry);

    while (true) // exit when trying to pop from empty stack
    {
        // calculate the next transition
        if (currentFirelistEntry != (uint32_t) - 1)
        {
            get_next_transition(automata, ns, &currentStateListEntry, &currentFirelistEntry, 
                    currentFirelist, currentStateListLength, currentAutomataState);
        }
        // there is a next transition that needs to be explored in current marking
        // Note: We used to check for (currentFirelistEntry != -1 && currentStateListEntry >= 0) in the if below, which did not make sense as arrayindex_t currentFirelistEntry is always >= 0.
        if (currentFirelistEntry != (uint32_t) - 1)
        {
            // search in the store
            AutomataTree **searchResult;
            store.searchAndInsert(ns, &searchResult, 0);
            AutomataTree *nextStateEntry = 0;
            // search state in state-tree
            searchAndInsertAutomataState(currentStateList[currentStateListEntry], searchResult, &nextStateEntry,
                                         stateStorageMode);

            // if state belongs to the current SCC
            if (nextStateEntry->dfs <= - depth - 1 && nextStateEntry->dfs >= -witness_depth)
            {
                bool newly_fulfilled = false;
                // maybe the transition itself has made us fair
                if (assumptions.weak_backlist[currentFirelist[currentFirelistEntry]] != (uint32_t) - 1)
                {
                    // found a successor
                    if (!fulfilled_weak[assumptions.weak_backlist[currentFirelist[currentFirelistEntry]]])
                    {
                        fulfilled_conditions++;
                        newly_fulfilled = true;
                        fulfilled_weak[assumptions.weak_backlist[currentFirelist[currentFirelistEntry]]] = true;
                    }
                }
                else if (assumptions.strong_backlist[currentFirelist[currentFirelistEntry]] != (uint32_t) - 1)
                {
                    // strong, found a successor in set
                    fulfilled_strong[assumptions.strong_backlist[currentFirelist[currentFirelistEntry]]] = true;
                    fulfilled_conditions++;
                    newly_fulfilled = true;
                }

                // check for possible new assumptions to be fulfilled
                bool *__enabled_weak = new bool[assumptions.card_weak]();
                for (arrayindex_t i = 0; i <= currentFirelistEntry; i++)
                    if (assumptions.weak_backlist[currentFirelist[i]] != (uint32_t) - 1)
                    {
                        __enabled_weak[assumptions.weak_backlist[currentFirelist[i]]] = true;
                    }
                // check for newly fulfilled conditions
                for (arrayindex_t i = 0; i < assumptions.card_weak; i++)
                    if (!__enabled_weak[i] && !fulfilled_weak[i])
                    {
                        fulfilled_weak[i] = true;
                        fulfilled_conditions++;
                        newly_fulfilled = true;
                    }
                delete[] __enabled_weak;
                if (automata.isAcceptingState(currentStateList[currentStateListEntry]) && !acceptingStateFound)
                {
                    acceptingStateFound = true;
                    fulfilled_conditions++;
                    newly_fulfilled = true;
                }
                // more fairness assumptions are fulfilled
                if (newly_fulfilled)
                {
                    // if everything is fulfilled search for the begin of the SCC and finish
                    if (fulfilled_conditions == assumptions.card_strong + assumptions.card_weak + 1)
                    {
                        if (nextStateEntry != targetPointer)
                        {
                            completeWitness(automata, store, firelist, ns, currentStateList[currentStateListEntry],
                                            targetPointer, depth, witness_depth + 1, nextStateEntry);
                        }
                    }
                    else
                    {
                        // not all are fulfilled, but new ones
                        produceWitness(automata, store, firelist, ns, currentStateList[currentStateListEntry],
                                       nextStateEntry, depth, witness_depth + 1,
                                       fulfilled_weak, fulfilled_strong, fulfilled_conditions, acceptingStateFound, targetPointer);
                    }
                    *(witness.push()) = currentFirelist[currentFirelistEntry];
                    while (stack.StackPointer)
                    {
                        *(witness.push()) = stack.top().fl[stack.top().current_on_firelist];
                        stack.top().~LTLStackEntry();
                        stack.pop();
                    }
                    //delete[] currentFirelist;
                    delete[] currentStateList;
                    return;
                }
                // nothing newly fulfilled
                if (nextStateEntry->dfs != - witness_depth)
                {
                    // switch to next state
                    currentAutomataState = currentStateList[currentStateListEntry];
                    // mark new state as visited
                    nextStateEntry->dfs = -witness_depth;

                    LTLStackEntry *stackEntry = stack.push();
                    stackEntry = new(stackEntry) LTLStackEntry(currentFirelist,
                            currentFirelistEntry, currentStateList,
                            currentStateListEntry, currentStateListLength,
                            -1, currentStateEntry);

                    initialize_transition(ns, firelist, automata, currentAutomataState, &currentFirelistEntry,
                                          &currentFirelist, &currentStateListEntry, &currentStateListLength, &currentStateList,
                                          nextStateEntry);
                    currentStateEntry = nextStateEntry;
                }
            }
        }
        else
        {
            // firing list completed -->close state and return to previous state
            delete[] currentStateList;
            //delete[] currentFirelist;
            // this will not happen
            assert(stack.StackPointer);

            // load the own predecessor
            LTLStackEntry &stackEntry = stack.top();
            currentFirelistEntry = stackEntry.current_on_firelist;
            currentFirelist = stackEntry.fl;
            currentStateListEntry = stackEntry.current_on_statelist;
            currentStateList = stackEntry.states;
            currentStateEntry = stackEntry.dfs;
            currentStateListLength = stackEntry.length_of_statelists;
            stack.pop();

            assert(currentFirelistEntry < Net::Card[TR]);
            // revert the buechi automata
            currentAutomataState = currentStateList[currentStateListEntry];
        }
    }
}



////////////////////////////////////////////////////////////////////////////////
//
//
//           LTL - produces the witness path
//              assumption: all markings of current component have DFS = -depth-1
//                  except: on stack=> DFS = -depth-2, removed from stack DFS = -depth-3
///////////////////////////////////////////////////////////////////////////////

void LTLExploration::completeWitness(BuechiAutomata &automata, Store<AutomataTree *> &store,
                                     Firelist &firelist, NetState &ns,
                                     arrayindex_t currentAutomataState,  AutomataTree *targetPointer, dfsnum_t depth,
                                     dfsnum_t witness_depth,
                                     AutomataTree *currentStateEntry)
{
    // the stack for the search
    SearchStack<LTLFairnessStackEntry> stack;
    // get first firelist
    arrayindex_t *currentFirelist;
    // the size of the list is not a valid index (thus -1)
    arrayindex_t currentFirelistEntry;
    arrayindex_t *currentStateList;
    arrayindex_t currentStateListEntry, currentStateListLength;
    initialize_transition(ns, firelist, automata, currentAutomataState, &currentFirelistEntry,
                          &currentFirelist, &currentStateListEntry, &currentStateListLength, &currentStateList,
                          currentStateEntry);

    while (true) // exit when trying to pop from empty stack
    {
        if (currentFirelistEntry != (uint32_t) - 1)
        {
            get_next_transition(automata, ns, &currentStateListEntry, &currentFirelistEntry, 
                    currentFirelist, currentStateListLength, currentAutomataState);
        }

        // Note: We used to check for (currentFirelistEntry != -1 && currentStateListEntry >= 0) in the if below, which did not make sense as arrayindex_t currentFirelistEntry is always >= 0.
        if (currentFirelistEntry != (uint32_t) - 1)
        {
            // there is a next transition that needs to be explored in current marking
            // search in the store
            AutomataTree **searchResult;
            store.searchAndInsert(ns, &searchResult, 0);
            AutomataTree *nextStateEntry = 0;
            // search state in state-tree
            searchAndInsertAutomataState(currentStateList[currentStateListEntry], searchResult, &nextStateEntry,
                                         stateStorageMode);

            if (nextStateEntry == targetPointer)
            {
                // found the state we are looking for
                *(witness.push()) = currentFirelist[currentFirelistEntry];
                while (stack.StackPointer)
                {
                    *(witness.push()) = stack.top().fl[stack.top().current_on_firelist];
                    stack.top().~LTLFairnessStackEntry();
                    stack.pop();
                }
                //delete[] currentFirelist;
                delete[] currentStateList;
                return;
            }

            if (nextStateEntry->dfs <= - depth - 1 && nextStateEntry->dfs > -witness_depth)
            {
                // switch to next state
                currentAutomataState = currentStateList[currentStateListEntry];
                // mark new state as visited
                nextStateEntry->dfs = -witness_depth;

                LTLFairnessStackEntry *stackEntry = stack.push();
                stackEntry = new(stackEntry) LTLFairnessStackEntry(currentFirelist,
                        currentFirelistEntry, currentStateList,
                        currentStateListEntry, currentStateListLength);
                initialize_transition(ns, firelist, automata, currentAutomataState,
                                      &currentFirelistEntry, &currentFirelist, &currentStateListEntry, &currentStateListLength,
                                      &currentStateList, nextStateEntry);
            }
        }
        else
        {
            // firing list completed -->close state and return to previous state
            //delete[] currentFirelist;
            delete[] currentStateList;
            // this can happen
            if (!stack.StackPointer) 
                return;
            assert(stack.StackPointer);

            // load the own predecessor
            LTLFairnessStackEntry &stackEntry = stack.top();
            currentFirelistEntry = stackEntry.current_on_firelist;
            currentFirelist = stackEntry.fl;
            currentStateListEntry = stackEntry.current_on_statelist;
            currentStateList = stackEntry.states;
            currentStateListLength = stackEntry.length_of_statelists;
            stack.pop();

            assert(currentFirelistEntry < Net::Card[TR]);
            // revert the buechi automata
            currentAutomataState = currentStateList[currentStateListEntry];
        }
    }
}



bool LTLExploration::is_next_state_accepting(BuechiAutomata &automata,
        arrayindex_t currentAutomataState)
{
    arrayindex_t *nextStates;
    arrayindex_t cardNext = automata.getSuccessors(&nextStates, currentAutomataState);
    //rep->message("CN %d",cardNext);
    for (arrayindex_t i = 0; i < cardNext; i++)
        if (automata.isAcceptingState(nextStates[i]))
        {
            return true;
        }
    return false;
}


Path LTLExploration::path()
{
    static Path p;

    if (not p.initialized)
    {
        bool containsCycle = false;
        p.initialized = true;
        while (witness.StackPointer > 0)
        {
            arrayindex_t &s = witness.top();
            if (s == (uint32_t) - 1)
            {
                p.beginCycle(false);
                containsCycle = true;
            }
            else
            {
                p.addTransition(s, false);
            }
            witness.pop();
        }

        // make sure we close opened cycles
        if (containsCycle)
        {
            p.endCycle(false);
        }
    }
    return p;
}
