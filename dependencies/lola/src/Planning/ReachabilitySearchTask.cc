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
#include <Planning/ReachabilitySearchTask.h>
#include <Exploration/StatePredicateProperty.h>
#include <Exploration/FirelistStubbornDeletion.h>
#include <Exploration/FirelistStubbornStatePredicate.h>
#include <Exploration/DFSExploration.h>
#include <Exploration/ParallelExploration.h>



/*!
\brief the verification task

This class wraps the reachability check by statespace exploration

*/

extern kc::tFormula TheFormula;

ReachabilitySearchTask::ReachabilitySearchTask()
{
    goStatus = false;
    // extract state predicate from formula
    assert(TheFormula);
    // copy formula for additional rewrite to avoid conflict if stateequation 
    // check is also used
    kc::tFormula TheFormulaRS;
    TheFormulaRS = reinterpret_cast<kc::tFormula> (TheFormula->copy(true));
    assert(TheFormulaDNF);
    TheFormulaRS = TheFormulaRS->rewrite(kc::singletemporal);
    TheFormulaRS = TheFormulaRS->rewrite(kc::simpleneg);
    TheFormulaRS = TheFormulaRS->rewrite(kc::booleanlists);
    // prepare counting of place in the formula
    extern bool *place_in_formula;
    extern unsigned int places_mentioned;
    extern unsigned int unique_places_mentioned;
    place_in_formula = new bool[Net::Card[PL]]();
    places_mentioned = 0;
    unique_places_mentioned = 0;

	unparsed.clear();
        TheFormulaRS->unparse(myprinter, kc::internal);      
        spFormula = TheFormulaRS->formula;
// Task::outputFormulaAsProcessed();

	previousNrOfMarkings = 0;
	// set the net
    ns = NetState::createNetStateFromInitial();

    switch (RT::args.search_arg)
    {
    case search_arg_sweepline:
    {


        // dummy store for the sweepline method, only counts markings and calls
        RT::data["store"]["type"] = "empty";
        RT::data["store"]["search"] = "sweepline";
        store = new SweepEmptyStore();
	covStore = NULL;
        break;
    }
    case search_arg_covergraph:
    {
        RT::data["store"]["search"] = "covergraph";
        if (RT::args.encoder_arg != encoder_arg_fullcopy)
        {
            RT::rep->status("warning: encoder does not fully support coverability graphs");
        }
        covStore = StoreCreator<CoverPayload>::createStore(number_of_threads);
	store = NULL;
        break;
    }
    case search_arg_depth:
    {
        RT::data["store"]["search"] = "depth_first_search";

        // choose a store
	store = StoreCreator<void>::createStore(number_of_threads);
	covStore = NULL;
        break;
    }
    case search__NULL:
        assert(false);
    }
	    RT::rep->indent(-2);
	    RT::rep->status("SEARCH");
	    RT::rep->indent(2);

    switch (RT::args.search_arg)
    {
    case search_arg_sweepline:
    {

	    RT::rep->status("using sweepline method (%s)", RT::rep->markup(MARKUP_PARAMETER, "--search=sweep").str());

        break;
    }
    case search_arg_covergraph:
    {
            RT::rep->status("warning: encoder does not fully support coverability graphs");
        break;
    }
    case search_arg_depth:
    {
	    RT::rep->status("using reachability  graph (%s)", RT::rep->markup(MARKUP_PARAMETER, "--search=depth").str());
        break;
    }
    case search__NULL:
        assert(false);
    }

    // choose a simple property
        RT::data["analysis"]["type"] = "modelchecking";
            p = new StatePredicateProperty(spFormula);
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
	    default: assert(false); // exhaustive enumeration
            }

    // set the correct exploration algorithm
            if (number_of_threads == 1)
            {
                exploration = new DFSExploration();
            }
            else
            {
                exploration = new ParallelExploration();
            }
}

ReachabilitySearchTask::~ReachabilitySearchTask()
{
#ifndef USE_PERFORMANCE
    delete ns;
    delete store;
    delete covStore;
    delete p;
    //delete spFormula;
	RT::rep->status("h6");
	RT::rep->status("h7");
#endif
}

ternary_t ReachabilitySearchTask::getResult()
{
    bool bool_result(false);
    ternary_t result(TERNARY_FALSE);

    // compute symmetries
    if (RT::args.symmetry_given && RT::args.search_arg != search_arg_sweepline)
    {
        SymmetryCalculator *SC = NULL;
	SC = new SymmetryCalculator(spFormula);
        assert(SC);
        SC->ComputeSymmetries();
        delete SC;
	if(store) store = reinterpret_cast<SymmetryStore<void>*>(store)->setGeneratingSet(SymmetryCalculator::G);
	if(covStore) covStore = reinterpret_cast<SymmetryStore<CoverPayload>*>(covStore)->setGeneratingSet(SymmetryCalculator::G);
    }

    goStatus = true; // start reporting progress

    switch (RT::args.search_arg)
    {
	case search_arg_depth:
	    bool_result = exploration->depth_first(*p, *ns, *store, *fl, number_of_threads);
	    break;

	case search_arg_sweepline:
	    // no choice of stores for sweepline method here
	    bool_result = exploration->sweepline(*p, *ns, *reinterpret_cast<SweepEmptyStore *>(store), *fl,
						 RT::args.sweepfronts_arg, number_of_threads);
	    break;

	case search_arg_covergraph:
	    result = exploration->cover_breadth_first(*p, *ns, *covStore, *fl, number_of_threads, FORMULA_REACHABLE);
	    break;

	default:
	    assert(false);
    }

    // temporary result transfer, as long as the variable bool_result is needed
    if (bool_result)
    {
        result = TERNARY_TRUE;
    }
    return result;
}

void ReachabilitySearchTask::interpreteResult(ternary_t result)
{
	if (RT::args.store_arg == store_arg_bloom)
    {
        double n;
        if(store)
        {
        	n = static_cast<double>(store->get_number_of_markings());
	}
	else
	{
		assert(covStore);
        	n = static_cast<double>(covStore->get_number_of_markings());
		
	}
        const double k = RT::args.hashfunctions_arg;
        const double m = static_cast<double>(BLOOM_FILTER_SIZE);
        RT::rep->status("Bloom filter: probability of false positive is %.10lf",
                        pow((1.0 - exp((-k * n) / m)), k));
        RT::rep->status("Bloom filter: optimal number of hash functions is %.1f",
                        log(m / n) / log(2.0));
    }
    // if the Bloom store did not find anything, the result is unknown
    if (RT::args.store_arg == store_arg_bloom)
    {
            if (result == TERNARY_FALSE)
            {
                result = TERNARY_UNKNOWN;
            }
    }
    switch (result)
    {
    case TERNARY_TRUE:
        RT::rep->status("result: %s", RT::rep->markup(MARKUP_GOOD, "yes").str());
        RT::data["analysis"]["result"] = true;

            RT::rep->status("%s", RT::rep->markup(MARKUP_GOOD, "The predicate is reachable.").str());

        break;

    case TERNARY_FALSE:
        RT::rep->status("result: %s", RT::rep->markup(MARKUP_BAD, "no").str());
        RT::data["analysis"]["result"] = false;

            RT::rep->status("%s", RT::rep->markup(MARKUP_BAD,
                                                  "The predicate is unreachable.").str());
        break;

    case TERNARY_UNKNOWN:
        RT::rep->status("result: %s", RT::rep->markup(MARKUP_WARNING, "unknown").str());
        RT::data["analysis"]["result"] = JSON::null;

            RT::rep->status("%s", RT::rep->markup(MARKUP_WARNING,
                                                  "The predicate may or may not be reachable.").str());
        break;
    }

}
 
Path ReachabilitySearchTask::getWitnessPath()
{
    if (RT::args.search_arg == search_arg_covergraph)
    {
        // cover graph
        return exploration->path();
    }
    else
    {
        // simple property
        return p->path();
    }
}

capacity_t *ReachabilitySearchTask::getMarking()
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

void ReachabilitySearchTask::getStatistics()
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

Task * ReachabilitySearchTask::buildTask()
{
	return new ReachabilitySearchTask();
}


char * ReachabilitySearchTask::getStatus(uint64_t elapsed)
{
        char * result;
	uint64_t m;

	if(!goStatus)
	{
		return NULL; // do not report anything since symmetry calculation is still under way
	}
	switch(RT::args.search_arg)
	{
	case search_arg_covergraph:
	{
                result = new char[STATUSLENGTH];
		m = covStore->get_number_of_markings();
		sprintf(result,"%10llu markings, %10llu edges, %8.0f markings/sec, %5llu secs", m, covStore->get_number_of_calls(), ((m - previousNrOfMarkings) / (float)REPORT_FREQUENCY), elapsed); 
		break;
	}
	case search_arg_sweepline:
		result = exploration -> s -> getStatus();
		sprintf(result + strlen(result)," %5llu secs", elapsed);
		break;
	default:
	{
                result = new char[STATUSLENGTH];
		m = store->get_number_of_markings();
		sprintf(result,"%10llu markings, %10llu edges, %8.0f markings/sec, %5llu secs", m, store->get_number_of_calls(), ((m - previousNrOfMarkings) / (float)REPORT_FREQUENCY), elapsed); 
	previousNrOfMarkings = m;
            // report probability of a false positive in the Bloom filter
            if (RT::args.store_arg == store_arg_bloom)
            {
                const double n = static_cast<double>(m);
                static const double k = static_cast<double>(RT::args.hashfunctions_arg);
                static const double mm = static_cast<double>(BLOOM_FILTER_SIZE);
                sprintf(result+strlen(result),"%10lu hash table size      false positive probability: %.10lf", BLOOM_FILTER_SIZE, pow((1.0 - exp((-k * n) / mm)), k));
            }
	}
	}
        return result;
	

}


