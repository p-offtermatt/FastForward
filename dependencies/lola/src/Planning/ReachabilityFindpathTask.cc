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
#include <Core/Dimensions.h>
#include <Core/Handlers.h>
#include <Witness/Path.h>
#include <Planning/Task.h>
#include <Planning/ReachabilityFindpathTask.h>
#include <Exploration/StatePredicateProperty.h>
#include <Exploration/SimpleProperty.h>
#include <Exploration/DFSExploration.h>
#include <Exploration/ParallelExploration.h>
#include <Exploration/FirelistStubbornDeletion.h>
#include <Exploration/FirelistStubbornStatePredicate.h>
#include <Exploration/ChooseTransition.h>
#include <Exploration/ChooseTransitionHashDriven.h>
#include <Exploration/ChooseTransitionRandomly.h>
#include <Frontend/Parser/ast-system-k.h>
#include <Frontend/Parser/ast-system-rk.h>
#include <Frontend/Parser/ast-system-unpk.h>


/*!
\brief the verification task

This class wraps the stateless reachability check

*/
extern kc::tFormula TheFormula;

ReachabilityFindpathTask::ReachabilityFindpathTask()
{
        // extract state predicate from formula
        TheFormula = TheFormula->rewrite(kc::singletemporal);
        TheFormula = TheFormula->rewrite(kc::simpleneg);
        TheFormula = TheFormula->rewrite(kc::booleanlists);
    // prepare counting of place in the formula
    extern bool *place_in_formula;
    extern unsigned int places_mentioned;
    extern unsigned int unique_places_mentioned;
    place_in_formula = new bool[Net::Card[PL]]();
    places_mentioned = 0;
    unique_places_mentioned = 0;

        TheFormula->unparse(myprinter, kc::internal);
        spFormula = TheFormula->formula;
 //Task::outputFormulaAsProcessed();

    // set the net
    ns = NetState::createNetStateFromInitial();
    store = new EmptyStore<void>(number_of_threads);
      p = new StatePredicateProperty(spFormula);
	RT::rep->indent(-2);
	RT::rep->status("SEARCH");
	RT::rep->indent(2);
            switch(RT::args.stubborn_arg)
            {
	    case stubborn_arg_deletion:
RT::rep->status("using reachability preserving stubborn set method with deletion algorithm (%s)", RT::rep->markup(MARKUP_PARAMETER, "--stubborn=deletion").str());
                fl = new FirelistStubbornDeletion(spFormula);
		break;
	    case stubborn_arg_tarjan:
RT::rep->status("using reachability preserving stubborn set method with insertion algorithm (%s)", RT::rep->markup(MARKUP_PARAMETER, "--stubborn=tarjan").str());
                fl = new FirelistStubbornStatePredicate(spFormula);
		break;
	    case stubborn_arg_off:
RT::rep->status("not using stubborn set method (%s)", RT::rep->markup(MARKUP_PARAMETER, "--stubborn=off").str());
		fl = new Firelist();
		break;
	    default:
		assert(false); // exhaustive enumeration
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

ReachabilityFindpathTask::~ReachabilityFindpathTask()
{
	 delete ns;
    delete store;
    delete p;
    //delete spFormula;
    delete fl;
    delete exploration;
}

ternary_t ReachabilityFindpathTask::getResult()
{
	
    bool bool_result;
    RT::rep->status("starting randomized, memory-less exploration (%s)",
    RT::rep->markup(MARKUP_PARAMETER, "--findpath").str());

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

    // added a scope to allow a local definition of choose
    {
	ChooseTransition *choose = NULL;
	RT::rep->status("transitions are chosen hash-driven");
	choose = new ChooseTransitionHashDriven();
	bool_result = exploration->find_path(*p, *ns, RT::args.retrylimit_arg,
		     RT::args.depthlimit_arg, *fl, *((EmptyStore<void> *)store), *choose);
	delete choose;
    }
    if(bool_result)
    {
	return TERNARY_TRUE;
    }
    else
    {
	return TERNARY_UNKNOWN;
    }
}

void ReachabilityFindpathTask::interpreteResult(ternary_t result)
{
	if (result == TERNARY_FALSE)
        {
            result = TERNARY_UNKNOWN;
        }
	switch (result)
    {
    case TERNARY_TRUE:
        RT::rep->status("result: %s", RT::rep->markup(MARKUP_GOOD, "yes").str());
        RT::data["analysis"]["result"] = true;
        RT::rep->status("%s", RT::rep->markup(MARKUP_GOOD, "The predicate is reachable.").str());

        break;
    case TERNARY_FALSE:
	    assert(false);
            break;
case TERNARY_UNKNOWN:
        RT::rep->status("result: %s", RT::rep->markup(MARKUP_WARNING, "unknown").str());
        RT::data["analysis"]["result"] = JSON::null;
        RT::rep->status("%s", RT::rep->markup(MARKUP_WARNING,
                                                  "The predicate may or may not be reachable.").str());
        break;
    default: assert(false);
    }
}
 
Path ReachabilityFindpathTask::getWitnessPath()
{
	return exploration->path();
}


capacity_t *ReachabilityFindpathTask::getMarking()
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

void ReachabilityFindpathTask::getStatistics()
{
	uint64_t markingcount = store->get_number_of_markings();
	RT::data["analysis"]["stats"]["states"] = static_cast<int>(result);
	uint64_t edgecount = store->get_number_of_calls();

        RT::data["analysis"]["stats"]["edges"] = static_cast<int>(result);
}
 
Task * ReachabilityFindpathTask::buildTask()
{
	return new ReachabilityFindpathTask();
}


char * ReachabilityFindpathTask::getStatus(uint64_t elapsed)
{
        char * result = new char[STATUSLENGTH];
        sprintf(result,"%10d tries, %10llu fired transitions, %5llu secs", store->tries, store->get_number_of_calls(), elapsed);
        return result;
}

