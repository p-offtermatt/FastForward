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
#include <Core/Dimensions.h>
#include <Core/Runtime.h>
#include <Core/Handlers.h>
#include <Witness/Path.h>
#include <Planning/Task.h>
#include <Planning/StoreCreator.h>
#include <Planning/AGEFTask.h>
#include <Exploration/StatePredicateProperty.h>
#include <Exploration/FirelistStubbornDeletion.h>
#include <Exploration/FirelistStubbornStatePredicate.h>
#include <Exploration/FirelistStubbornTsccAlwaysUpset.h>
#include <Exploration/FirelistStubbornTsccUpset.h>
#include <Exploration/DFSExploration.h>
#include <Exploration/ParallelExploration.h>
#include <Exploration/TSCCExploration.h>



/*!
\brief the verification task

This class wraps the liveness check by statespace exploration

*/

class TSCCExplorationAGEF;
extern kc::tFormula TheFormula;

AGEFTask::AGEFTask()
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


	previousNrOfMarkings = 0;

	// set the net
    ns = NetState::createNetStateFromInitial();

    switch (RT::args.search_arg)
    {
    case search_arg_covergraph:
    {
        RT::data["store"]["search"] = "covergraph";
        if (RT::args.encoder_arg != encoder_arg_fullcopy)
        {
            RT::rep->status("warning: encoder does not fully support coverability graphs");
        }
        covStore = StoreCreator<CoverPayload>::createStore(number_of_threads);
        break;
    }
    default:
    {
        RT::data["store"]["search"] = "depth_first_search";

        // choose a store
	store = StoreCreator<statenumber_t>::createStore(number_of_threads);

        break;
    }
    }
    // \todo switch for coverability graph
    // choose a simple property
        RT::data["analysis"]["type"] = "modelchecking";
            p = new StatePredicateProperty(spFormula);
	RT::rep->indent(-2);
	RT::rep->status("SEARCH");
	RT::rep->indent(2);
        if(RT::args.stubborn_arg==stubborn_arg_off)
        {
        RT::rep->status("not using stubborn set method (%s)", RT::rep->markup(MARKUP_PARAMETER, "--stubborn=off").str());
		fl = new Firelist();
	}
	else
	{
        RT::rep->status("using tscc preserving stubborn set method (%s)", RT::rep->markup(MARKUP_PARAMETER, "--stubborn").str());
                fl = new FirelistStubbornTsccAlwaysUpset(p);
	}
        exploration = new TSCCExplorationAGEF();
}

AGEFTask::~AGEFTask()
{
#ifndef USE_PERFORMANCE
    delete ns;
    delete store;
    delete covStore;
    delete p;
    delete spFormula;
    delete fl;
    delete exploration;
#endif
}

ternary_t AGEFTask::getResult()
{
	// compute symmetries
    if (RT::args.symmetry_given)
    {
        SymmetryCalculator *SC = NULL;
	SC = new SymmetryCalculator(spFormula);
        assert(SC);
        SC->ComputeSymmetries();
        delete SC;
    }

    bool bool_result(false);
    ternary_t result(TERNARY_FALSE);
    bool_result = exploration->depth_first(*p, *ns, *store, *fl, number_of_threads);

    // temporary result transfer, as long as the variable bool_result is needed
    if (bool_result)
    {
        result = TERNARY_TRUE;
    }
    return result;
}

void AGEFTask::interpreteResult(ternary_t result)
{
    switch (result)
    {
    case TERNARY_TRUE:
        RT::rep->status("result: %s", RT::rep->markup(MARKUP_GOOD, "yes").str());
        RT::data["analysis"]["result"] = true;
        RT::rep->status("%s", RT::rep->markup(MARKUP_GOOD, "The predicate is live.").str());

        break;

    case TERNARY_FALSE:
        RT::rep->status("result: %s", RT::rep->markup(MARKUP_BAD, "no").str());
        RT::data["analysis"]["result"] = false;

        RT::rep->status("%s", RT::rep->markup(MARKUP_BAD,
                                                  "The predicate is not live.").str());
        break;

    case TERNARY_UNKNOWN:
        RT::rep->status("result: %s", RT::rep->markup(MARKUP_WARNING, "unknown").str());
        RT::data["analysis"]["result"] = JSON::null;

        RT::rep->status("%s", RT::rep->markup(MARKUP_WARNING,
                                                  "The predicate may or may not be live.").str());

        break;
    }

}
 
Path AGEFTask::getWitnessPath()
{
        // simple property
        return p->path();
}

capacity_t *AGEFTask::getMarking()
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

void AGEFTask::getStatistics()
{
	uint64_t markingcount;
	if(store)
	{
		markingcount = store->get_number_of_markings();
	}
	else if(covStore)
	{
		markingcount = covStore->get_number_of_markings();
	}
	else
	{
		assert(false);
	}
	RT::data["analysis"]["stats"]["states"] = static_cast<int>(markingcount);
	uint64_t edgecount;
    if (store)
    {
        edgecount = store->get_number_of_calls();
    }
	else if (covStore)
    {
        edgecount = covStore->get_number_of_calls();
    }
    else
    {
	assert(false);
    }
    RT::data["analysis"]["stats"]["edges"] = static_cast<int>(edgecount);
    RT::rep->status("%llu markings, %llu edges", markingcount, edgecount);
} 

Task * AGEFTask::buildTask()
{
	return new AGEFTask();
}


char * AGEFTask::getStatus(uint64_t elapsed)
{
        char * result = new char[STATUSLENGTH];
        uint64_t m = store ? store->get_number_of_markings() : covStore->get_number_of_markings();
        sprintf(result,"%10llu markings, %10llu edges, %8.0f markings/sec, %5llu secs", m, store->get_number_of_calls(), ((m - previousNrOfMarkings) / (float)REPORT_FREQUENCY), elapsed); 
	previousNrOfMarkings = m;
        return result;
}


