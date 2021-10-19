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
#include <Net/NetState.h>
#include <Exploration/SimpleProperty.h>
#include <Exploration/Firelist.h>
#include <Exploration/SimpleProperty.h>


class DFSExploration;
class StatePredicate;
struct CoverPayload;
/*!
\brief the verification task

This class schedules subtasks of a top level conjunction

*/

class ConjunctionTask : public Task
{
public:
	ternary_t result;
        ternary_t subresult1;
        ternary_t subresult2;
    ConjunctionTask(Task*,Task*);
    ~ConjunctionTask();

    /// run the actual verification algorithm
    virtual ternary_t getResult();
	
    /// interprete and display the result
    virtual void interpreteResult(ternary_t result);

    // witnesses are left to subproblems
    // LCOV_EXCL_START
    /// return the witness path
    virtual Path getWitnessPath(){return *(new Path);}
    /// return the target marking

    virtual capacity_t *getMarking(){return NULL;} 
    // left to child process
    virtual char * getStatus(uint64_t){return NULL;} // do not yet have a good idea... 
    virtual char * early_abortion(){return NULL;} // does not apply  
    // LCOV_EXCL_STOP

    /// return the number of stored markings
    virtual void getStatistics(){} // do not yet have a good idea... 
    static Task * buildTask();

    Task * subTask1; /// the left subtask
    Task * subTask2; /// the right subtask
};
