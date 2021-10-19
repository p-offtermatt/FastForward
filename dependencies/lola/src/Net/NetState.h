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
\author Christian K and Gregor B
\status unknown
*/

#pragma once

#include <Core/Dimensions.h>

/*!
\brief container object which includes all informations about the marking of a
petri net

Basically this is a struct containing all information available and necessary
about a marking. This includes the marking itself, its hash, the enabled
transitions and several further information, to be able to efficiently
calculate the NetState of all successor markings.

\author Christian K and Gregor B
 */
class NetState
{
public:
    NetState(): membersInitialized(false) {}
    ~NetState();

    //--------- COPIED FOR MARKING.h
    /// current  marking
    capacity_t *Current;

    /// hash value of initial marking
    hash_t HashCurrent;


    //--------- COPIED FROM TRANSITION.h
    /// Activation status
    bool *Enabled;

    /// number of enabled transitions
    arrayindex_t CardEnabled;




    /// create a NetState object from the global variables set by the parsing process
    static NetState *createNetStateFromInitial();
    /// copy constructor
    NetState(const NetState &ns);
    // copy operator, be aware that this changes the given ns (this is necessary to ensure proper deallocation of memory)
    NetState &operator=(const NetState &ns);

    // swaps all internal members with the given NetState
    void swap(NetState &ns);
    
    
    //--------- For LTL
    
    // Represents the current Buechi automaton state for LTL model checking.
    arrayindex_t currentAutomatonState;

private:
    /// marker variable, whether we have to delete some of the arrays ourself
    bool membersInitialized;
};
