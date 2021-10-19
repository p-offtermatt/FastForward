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


class ComputeBoundExploration;
class StatePredicate;
struct CoverPayload;
/*!
\brief the verification task

This class wraps the calculation of place bounds

*/

class ComputeBoundTask : public Task
{
public:
    ternary_t result;
    int64_t resultvalue;
    ComputeBoundTask();
    ~ComputeBoundTask();

    /// run the actual verification algorithm
    virtual ternary_t getResult();
	
    /// interprete and display the result
    virtual void interpreteResult(ternary_t); 

   // bound computation is not able to produce meaningful witnesses
   // LCOV_EXCL_START
    /// return the witness path
    virtual Path getWitnessPath(){Path * p = new Path(); return *p;}
    /// return the target marking
    virtual capacity_t *getMarking(){return NULL;}
    // LCOV_EXCL_STOP

    /// return the number of stored markings
    virtual void getStatistics(); 
    static Task * buildTask();

    NetState * ns;
    Firelist * fl;
    ComputeBoundExploration * exploration;
    SimpleProperty * p;
    Store<void> * store;
    Store<CoverPayload> * covStore;
    StatePredicate * spFormula;
    char * getStatus(uint64_t);
    char * early_abortion(){return store->check_abortion();}
    uint64_t previousNrOfMarkings;
	bool goStatus;
};
