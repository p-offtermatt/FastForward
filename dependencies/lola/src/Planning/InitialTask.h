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
#include <Exploration/Firelist.h>
#include <Exploration/StatePredicateProperty.h>
#include <Net/NetState.h>
#include <Exploration/SimpleProperty.h>


/*!
\brief the verification task

This class handles initial satisfiability. 
*/

class InitialTask : public Task
{
public:
    InitialTask();
    ~InitialTask(){}

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
    static Task * buildTask();
    StatePredicate * spFormula;
    Firelist * fl;
    NetState * ns;
    SimpleProperty * p;
    char * getStatus(uint64_t);
    // for initial task, it is unlikely that we run for more than 5 secs
    // LCOV_EXCL_START
    char * early_abortion(){return NULL;}
    // LCOV_EXCL_STOP
};
