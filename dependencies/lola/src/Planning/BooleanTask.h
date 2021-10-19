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
#include <Planning/ReachabilityFindpathTask.h>
#include <Planning/ReachabilitySearchTask.h>

extern kc::tFormula TheFormula;

/*
\brief the verification task

This is a dummy class, i.e. no objects will be generated. The only purpose
of this class is to have a buildTask method. This will dive down all
Boolean operators on top of the first temporal operators and will translate
this into a hierachy of ConjunctionTasks, DisjunctionTasks, and LeafTasks.
*/

class BooleanTask : public Task
{
public:
    /// run the actual verification algorithm
    ternary_t getResult() {return TERNARY_UNKNOWN;}

    /// interprete and display the result
    void interpreteResult(ternary_t result) {}

    /// return the witness path
    Path getWitnessPath() { Path * p = new Path(); return *p;}
    /// return the target marking
    capacity_t *getMarking() { return NULL;}

    /// return the number of stored markings
    void getStatistics() {}
    char * getStatus(uint64_t){return NULL;}
    char * early_abortion(){return NULL;}
    static Task * buildTask() 
    {
	unparsed.clear();
	TheFormula->unparse(myprinter,toplevelboolean);
	return TheFormula->task;
    }
};
