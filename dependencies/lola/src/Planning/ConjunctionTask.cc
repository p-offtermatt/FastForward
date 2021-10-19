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
#include <Planning/ConjunctionTask.h>
#include <Exploration/StatePredicateProperty.h>
#include <Exploration/FirelistStubbornDeletion.h>
#include <Exploration/FirelistStubbornStatePredicate.h>
#include <Exploration/DFSExploration.h>
#include <Exploration/ParallelExploration.h>



/*!
\brief the verification task

This class handles a task with top level conjunction

*/

extern kc::tFormula TheFormula;

ConjunctionTask::ConjunctionTask(Task * s1, Task * s2)
{
    RT::needLocalTimeLimit = false;
    subTask1 = s1;
    subTask2 = s2;
}

ConjunctionTask::~ConjunctionTask()
{
#ifndef USE_PERFORMANCE
    delete subTask1;
    delete subTask2;
#endif
}

ternary_t ConjunctionTask::getResult()
{
	subresult1 = subTask1->getResult();
	if(subresult1 == TERNARY_FALSE)
	{
		result = TERNARY_FALSE;
		return result;
	}
	result = subresult2 = subTask2->getResult();
	if((subresult1 == TERNARY_UNKNOWN) && (subresult2 != TERNARY_FALSE))
	{
		result = TERNARY_UNKNOWN;
	}
	return result;
}

void ConjunctionTask::interpreteResult(ternary_t result)
{
    switch(result)
    {
    case TERNARY_TRUE:
        RT::rep->status("result: %s", RT::rep->markup(MARKUP_GOOD, "yes").str());
        RT::data["analysis"]["result"] = true;
        RT::rep->status("%s", RT::rep->markup(MARKUP_GOOD, "The Boolean predicate is true.").str());
        break;

    case TERNARY_FALSE:
        RT::rep->status("result: %s", RT::rep->markup(MARKUP_BAD, "no").str());
        RT::data["analysis"]["result"] = false;
        RT::rep->status("%s", RT::rep->markup(MARKUP_BAD, "The Boolean predicate is false.").str());
        break;

    case TERNARY_UNKNOWN:
        RT::rep->status("result: %s", RT::rep->markup(MARKUP_WARNING, "unknown").str());
        RT::data["analysis"]["result"] = JSON::null;
        RT::rep->status("%s", RT::rep->markup(MARKUP_WARNING, "The Boolean predicate may be true or false.").str());
        break;
    }
}
 
Task * ConjunctionTask::buildTask()
{
	return NULL; // we do not want to create such task using the buildTask procedure (use BooleanTask instead)
}

