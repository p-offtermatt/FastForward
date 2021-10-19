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
\author Gregor
\status new

\brief Evaluates an LTL Property
*/

#pragma once

#include <config.h>
#include <Exploration/SearchStack.h>
#include <Stores/Store.h>

class AutomataTree;
class BuechiAutomata;
class Path;
class Firelist;
class NetState;

struct fairness_assumptions
{
    arrayindex_t card_strong;
    arrayindex_t *strong_fairness;
    arrayindex_t *strong_backlist;
    arrayindex_t card_weak;
    arrayindex_t *weak_fairness;
    arrayindex_t *weak_backlist;
};

class LTLExploration
{
public:
    explicit LTLExploration(bool mode);

    bool checkProperty(BuechiAutomata &automata, Store<AutomataTree *> &store,
                       Firelist &firelist, NetState &ns);

    /// a data structure to manage a counterexample/witness
    SearchStack<arrayindex_t> witness;

    /// return a witness path
    Path path();

private:
    fairness_assumptions assumptions;
    arrayindex_t *forbidden_transitions;
    arrayindex_t card_forbidden_transitions;

    /// storage mode for automata trees, true means tree storage, false means flat storage
    bool stateStorageMode;

    dfsnum_t currentNextDepth;

    bool searchFair(BuechiAutomata &automata, Store<AutomataTree *> &store,
                    Firelist &firelist, NetState &ns, arrayindex_t currentAutomataState, AutomataTree *currentEntry,
                    dfsnum_t depth, arrayindex_t currentNextDFS);
    arrayindex_t checkFairness(BuechiAutomata &automata, Store<AutomataTree *> &store,
                               Firelist &firelist, NetState &ns, arrayindex_t currentAutomataState,
                               dfsnum_t depth, bool **enabledStrongTransitions, AutomataTree *currentStateEntry);
    void produceWitness(BuechiAutomata &automata,
                        Store<AutomataTree *> &store, Firelist &firelist, NetState &ns,
                        arrayindex_t currentAutomataState, AutomataTree *currentStateEntry,
                        dfsnum_t depth, dfsnum_t witness_depth, bool *fulfilledWeak, bool *fulfilledStrong,
                        arrayindex_t fulfilled_conditions, bool acceptingStateFound, AutomataTree *targetPointer);

    bool start_restricted_searches(BuechiAutomata &automata,
                                   Store<AutomataTree *> &store, Firelist &firelist, NetState &ns,
                                   arrayindex_t currentAutomataState, AutomataTree *currentStateEntry, dfsnum_t depth,
                                   arrayindex_t currentNextDF);

    void completeWitness(BuechiAutomata &automata,
                         Store<AutomataTree *> &store, Firelist &firelist, NetState &ns,
                         arrayindex_t currentAutomataState,  AutomataTree *targetPointer, dfsnum_t depth,
                         dfsnum_t witness_depth,
                         AutomataTree *currentStateEntry);

    // helper functions
    bool is_next_state_accepting(BuechiAutomata &automata, arrayindex_t currentAutomataState);
    inline void get_next_transition(BuechiAutomata &automata, NetState &ns,
                                    arrayindex_t *currentStateListEntry, arrayindex_t *currentFirelistEntry,
                                    arrayindex_t *currentFirelist,
                                    arrayindex_t stateListLength, arrayindex_t currentAutomataState);
    inline bool initialize_transition(NetState &ns, Firelist &firelist, BuechiAutomata &automata,
                                      arrayindex_t currentAutomataState,
                                      arrayindex_t *currentFirelistEntry, arrayindex_t **currentFirelist,
                                      arrayindex_t *currentStateListEntry,
                                      arrayindex_t *currentStateListLength, arrayindex_t **currentStateList,
                                      AutomataTree *currentStateEntry);
};
