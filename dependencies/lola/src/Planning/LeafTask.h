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

This class handles a single leaf in a top level Boolean formula.
It classifies the assigned subproblem and executes the related check.
As it is not the only subproblem that requires execution, and we do not
want to reset huge data structures, we fork a child process to do all this.

*/

class LeafTask : public Task
{
public:
	std::string resultstring;
	ternary_t result;
    LeafTask(kc::tStatePredicate);
    LeafTask(kc::tFormula);
    ~LeafTask();

    /// run the actual verification algorithm
    virtual ternary_t getResult();
	
    // never called for leaf tasks:
    // LCOV_EXCL_START
    /// interprete and display the result
    virtual void interpreteResult(ternary_t result){} // is never called as a leaf task cannot be the top level
                                                      // task in a Boolean hierarchy

    /// return the witness path
    virtual Path getWitnessPath(){return *(new Path);}
    /// return the target marking

    virtual capacity_t *getMarking(){return NULL;} 

    /// return the number of stored markings
    virtual void getStatistics(){} // do not yet have a good idea... 
    virtual char * getStatus(uint64_t){return NULL;}
    static Task * buildTask();

    char * early_abortion(){return NULL;} // does not apply  
    // LCOV_EXCL_STOP
    Task * subTask;
    kc::tFormula formula;
};
