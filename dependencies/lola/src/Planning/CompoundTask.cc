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


#include <Frontend/Parser/ast-system-k.h>
#include <Frontend/Parser/ast-system-rk.h>
#include <Frontend/Parser/ast-system-unpk.h>
#include <CoverGraph/CoverPayload.h>
#include <Symmetry/Constraints.h>
#include <SweepLine/Sweep.h>
#include <Core/Dimensions.h>
#include <Core/Runtime.h>
#include <Core/Handlers.h>
#include <Witness/Path.h>
#include <Planning/Task.h>
#include <Planning/StoreCreator.h>
#include <Planning/CompoundTask.h>
#include <Exploration/StatePredicateProperty.h>
#include <Exploration/FirelistStubbornDeletion.h>
#include <Exploration/FirelistStubbornStatePredicate.h>
#include <Exploration/DFSExploration.h>
#include <Exploration/ParallelExploration.h>



/*!
\brief the verification task

This class handles a compound task

*/

extern kc::tFormula TheFormula;

CompoundTask::CompoundTask(Task * s1, Task * s2)
{
    // count the number of compound tasks
    RT::numberOfCompoundTasks++;
    
    RT::needLocalTimeLimit = false;
    subTask1 = s1;
    subTask2 = s2;
}

CompoundTask::~CompoundTask()
{
#ifndef USE_PERFORMANCE
    delete subTask1;
    delete subTask2;
#endif
}

ternary_t CompoundTask::getResult()
{
	subresult1 = subTask1->getResult();
	subresult2 = subTask2->getResult();
	return TERNARY_TRUE; // has no particular meaning
}

void CompoundTask::interpreteResult(ternary_t result)
{
        RT::rep->status("summary: %s", RT::rep->markup(MARKUP_GOOD, RT::interim_result.c_str()).str());
}
 
Task * CompoundTask::buildTask()
{
	unparsed.clear();
        TheFormula->unparse(myprinter,compound);
        return TheFormula->task;
}

