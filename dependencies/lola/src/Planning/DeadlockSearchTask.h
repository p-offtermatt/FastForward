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

#pragma once

#include <Core/Dimensions.h>
#include <Witness/Path.h>
#include <Planning/Task.h>
#include <Exploration/DFSExploration.h>
#include <Exploration/Firelist.h>
#include <Stores/Store.h>
#include <Net/NetState.h>
#include <Exploration/SimpleProperty.h>
#include <CoverGraph/CoverGraph.h>


/*!
\brief the verification task

This class collects all information for executing a state space search for 
deadlocks.

*/

class DeadlockSearchTask: public Task
{
public:
    SimpleProperty *p;
    Firelist * fl;
    DFSExploration *exploration;
    NetState *ns;
    Store<void> *store;
    Store<CoverPayload> *covStore;
    DeadlockSearchTask();
    ~DeadlockSearchTask();

    /// run the actual verification algorithm
    ternary_t getResult();
    /// interprete and display the result
    void interpreteResult(ternary_t result);

    /// return the witness path
    Path getWitnessPath();
    /// return the target marking
    capacity_t *getMarking();

    /// return the number of stored markings
    void getStatistics();
    uint64_t previousNrOfMarkings;
    static Task * buildTask(){ return new DeadlockSearchTask();}
    char * getStatus(uint64_t);
    char * early_abortion(){return store->check_abortion();}
    bool goStatus; // report status only if this is true
};
