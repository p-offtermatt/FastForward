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

#include <Exploration/ChooseTransition.h>
#include <Exploration/ChooseTransitionRandomly.h>
#include <Exploration/ChooseTransitionHashDriven.h>
#include <Exploration/DeadlockExploration.h>
#include <Exploration/DFSExploration.h>
#include <Exploration/ParallelExploration.h>
#include <Exploration/Firelist.h>
#include <Exploration/FirelistStubbornDeadlock.h>
#include <Exploration/FirelistStubbornDeletion.h>
#include <Core/Dimensions.h>
#include <Core/Handlers.h>
#include <Stores/Store.h>
#include <Core/Runtime.h>
#include <Witness/Path.h>
#include <Planning/Task.h>
#include <Planning/DeadlockFindpathTask.h>
#include <Planning/StoreCreator.h>
#include <Net/NetState.h>

class SimpleProperty;

/*!
\brief the verification task

This class wraps the stateless search for a deadlock

*/

DeadlockFindpathTask::DeadlockFindpathTask()
{
	ns = NetState::createNetStateFromInitial();
	RT::data["store"]["type"] = "empty";
        RT::data["store"]["search"] = "findpath";
	store = new EmptyStore<void>(number_of_threads);
 	p = new DeadlockExploration();
	RT::rep->indent(-2);
	RT::rep->status("SEARCH");
	RT::rep->indent(2);

	switch(RT::args.stubborn_arg)
	{
	case stubborn_arg_deletion:
	    RT::rep->status("using deadlock preserving stubborn set method with deletion algorithm (%s)",RT::rep->markup(MARKUP_PARAMETER,"--stubborn=deletion").str());
            fl = new FirelistStubbornDeletion();
	    break;
	case stubborn_arg_tarjan:
	    RT::rep->status("using deadlock preserving stubborn set method with insertion algorithm (%s)",RT::rep->markup(MARKUP_PARAMETER,"--stubborn=tarjan").str());
            fl = new FirelistStubbornDeadlock();
	    break;
	case stubborn_arg_off:
	    RT::rep->status("not using stubborn set method(%s)",RT::rep->markup(MARKUP_PARAMETER,"--stubborn=tarjan").str());
	    fl = new Firelist();
	    break;
	default:
	    assert(false); // case consideration exhaustive
	}
	if (number_of_threads == 1)
            {
                exploration = new DFSExploration();
            }
            else
            {
                exploration = new ParallelExploration();
            }
}

DeadlockFindpathTask::~DeadlockFindpathTask()
{
	delete ns;
	delete p;
	delete fl;
	delete store;
	delete exploration;
}

ternary_t DeadlockFindpathTask::getResult()
{
    RT::rep->status("starting randomized, memory-less exploration (%s)",
	    RT::rep->markup(MARKUP_PARAMETER, "--search=findpath").str());

    RT::rep->status("searching for paths with maximal depth %d (%s)",
		    RT::args.depthlimit_arg,
		    RT::rep->markup(MARKUP_PARAMETER, "--depthlimit").str());

    if (RT::args.retrylimit_arg == 0)
    {
	RT::rep->status("no retry limit given (%s)",
	RT::rep->markup(MARKUP_PARAMETER, "--retrylimit").str());
    }
    else
    {
	RT::rep->status("restarting search at most %d times (%s)",
			RT::args.retrylimit_arg,
			RT::rep->markup(MARKUP_PARAMETER, "--retrylimit").str());
    }

    bool bool_result;
    // added a scope to allow a local definition of choose
    {
	ChooseTransition *choose = NULL;
    	RT::rep->status("transitions are chosen randomly");
        choose = new ChooseTransitionRandomly();
	bool_result = exploration->find_path(*p, *ns, RT::args.retrylimit_arg,
	     RT::args.depthlimit_arg, *fl, *((EmptyStore<void> *)store), *choose);
	delete choose;
    }
// temporary result transfer, as long as the variable bool_result is needed
    if (bool_result)
    {
        result = TERNARY_TRUE;
    }
    else
    {
	result = TERNARY_UNKNOWN;
    }

    return result;
}
void DeadlockFindpathTask::interpreteResult(ternary_t result)
{
    switch (result)
    {
    case TERNARY_TRUE:
        RT::rep->status("result: %s", RT::rep->markup(MARKUP_GOOD, "yes").str());
        RT::data["analysis"]["result"] = true;

            RT::rep->status("%s", RT::rep->markup(MARKUP_GOOD, "The net has deadlock(s).").str());

        break;

    case TERNARY_FALSE:
	    assert(false);
            break;

    case TERNARY_UNKNOWN:
        RT::rep->status("result: %s", RT::rep->markup(MARKUP_WARNING, "unknown").str());
        RT::data["analysis"]["result"] = JSON::null;

            RT::rep->status("%s", RT::rep->markup(MARKUP_WARNING,
                                                  "The net may or may not have deadlocks.").str());
        break;
    }
} 

Path DeadlockFindpathTask::getWitnessPath()
{
	return exploration->path();
}

capacity_t *DeadlockFindpathTask::getMarking()
{
   // we only provide witness states for simple properties where we found
    // a result
    if (p and p->value)
    {
        return ns->Current;
    }
    else
    {
        return NULL;
    }
}

void DeadlockFindpathTask::getStatistics()
{
uint64_t markingcount = store->get_number_of_markings();
        RT::data["analysis"]["stats"]["states"] = static_cast<int>(result);
        uint64_t edgecount = store->get_number_of_calls();
        RT::data["analysis"]["stats"]["edges"] = static_cast<int>(result);
}


Task * DeadlockFindpathTask::buildTask()
{
	return new DeadlockFindpathTask();
}

char * DeadlockFindpathTask::getStatus(uint64_t elapsed)
{
	char * result = new char[STATUSLENGTH];
	sprintf(result,"%10d tries, %10llu fired transitions, %5llu secs", store->tries, store->get_number_of_calls(), elapsed);
        return result;
}
