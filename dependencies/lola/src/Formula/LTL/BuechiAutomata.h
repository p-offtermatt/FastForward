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

\brief a buechi class for the description of an buechi automata
*/

#pragma once

#include <Core/Dimensions.h>

class AtomicStatePredicate;
class StatePredicateProperty;
class NetState;

// datastructure for parsing buechi automatons from LTL formulae
extern std::map<int, AtomicStatePredicate *> predicateMap;

char *produce_next_string(int *);

/// A Büchi-Automata
class BuechiAutomata
{
public:
    /// returns an array with all successor-states of the current netstate in the parameter
    /// this function will also call automatically updateProperty
    /// @return number of successors
    int getSuccessors(arrayindex_t **list, arrayindex_t currentState);

    /// updates all properties
    void updateProperties(NetState &ns, arrayindex_t transition);

    /// reverts all Properties
    void revertProperties(NetState &ns, arrayindex_t transition);

    /// initialize all atomic properties
    void initProperties(NetState &ns);

    /// returns true if the given state is accepting
    bool isAcceptingState(arrayindex_t state);

    /// returns the number of states
    arrayindex_t getNumberOfStates();

    /// default destructor
    ~BuechiAutomata();

public:
    /// number of atomic propositions
    uint32_t cardAtomicPropositions;
    /// an array containing the atomic propositions used by this buechi automata
    StatePredicateProperty **atomicPropositions;
    /// contains for each atomic proposition the
    arrayindex_t *atomicPropotions_backlist;

    /// number of states
    uint32_t cardStates;
    /// number of possible transitions per automata state
    uint32_t *cardTransitions;
    /// the transition "matrix" -- first index=state, second=number of transition, third= 0->proposition 1->next state
    uint32_t ** *transitions;
    /// current number of successor states
    arrayindex_t *cardEnabled;
    /// an array, indicating at each position i, whether the state i is an accepting one
    bool *isStateAccepting;
    void writeBuechi();
};
