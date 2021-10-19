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
#include <Planning/ComputeBoundTask.h>
#include <Exploration/StatePredicateProperty.h>
#include <Exploration/FirelistStubbornComputeBound.h>
#include <Exploration/ComputeBoundExploration.h>



/*!
\brief the verification task

This class wraps the reachability check by statespace exploration

*/

extern kc::tFormula TheFormula;

ComputeBoundTask::ComputeBoundTask()
{
	goStatus = false;
        // extract state predicate from formula
	assert(TheFormula);
    // prepare counting of place in the formula
    extern bool *place_in_formula;
    extern unsigned int places_mentioned;
    extern unsigned int unique_places_mentioned;
    place_in_formula = new bool[Net::Card[PL]]();
    places_mentioned = 0;
    unique_places_mentioned = 0;

        TheFormula->unparse(myprinter, kc::internal);
 //Task::outputFormulaAsProcessed();
        spFormula = TheFormula->formula;
	previousNrOfMarkings = 0;
	// set the net
    ns = NetState::createNetStateFromInitial();

        RT::data["store"]["search"] = "depth_first_search";

        // choose a store
	store = StoreCreator<void>::createStore(1); // 1 = nr_of_threads
	covStore =NULL;
    // choose a simple property
        RT::data["analysis"]["type"] = "modelchecking";
            p = new StatePredicateProperty(spFormula);
	RT::rep->indent(-2);
	RT::rep->status("SEARCH");
            switch(RT::args.stubborn_arg)
            {
	    case stubborn_arg_off:
RT::rep->status("not using stubborn set method (%s)", RT::rep->markup(MARKUP_PARAMETER, "--stubborn=off").str());
		fl = new Firelist();
		break;
	    default: 
RT::rep->status("using bound preserving stubborn set method (%s)", RT::rep->markup(MARKUP_PARAMETER, "--stubborn").str());
                fl = new FirelistStubbornComputeBound(spFormula);
            ; 
            }

                exploration = new ComputeBoundExploration();
}

ComputeBoundTask::~ComputeBoundTask()
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

ternary_t ComputeBoundTask::getResult()
{
	// compute symmetries
    if (RT::args.symmetry_given)
    {
        SymmetryCalculator *SC = NULL;
	SC = new SymmetryCalculator(spFormula);
        assert(SC);
        SC->ComputeSymmetries();
	if(store) store = reinterpret_cast<SymmetryStore<void> *>(store)->setGeneratingSet(SymmetryCalculator::G);
	if(covStore) covStore = reinterpret_cast<SymmetryStore<CoverPayload> *>(covStore)->setGeneratingSet(SymmetryCalculator::G);
        delete SC;
    }
    goStatus = true;

	    resultvalue = reinterpret_cast<ComputeBoundExploration*>(exploration)->depth_first_num(*p, *ns, *store, *fl, 1); // 1 = nr_of_threads
    result = TERNARY_FALSE;
    return result;
}

void ComputeBoundTask::interpreteResult(ternary_t result)
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
                result = TERNARY_UNKNOWN;
    }
	char * value = new char[1000];
    switch(result)
    {
    case TERNARY_FALSE:
        sprintf(value,"%lld",resultvalue);
        RT::rep->status("result: %s", RT::rep->markup(MARKUP_GOOD, value).str());
        RT::data["analysis"]["result"] = (int)resultvalue;
        sprintf(value,"The maximum value of the given expression is %lld",resultvalue);
            RT::rep->status("%s", RT::rep->markup(MARKUP_GOOD, value).str());
        break;

    case TERNARY_UNKNOWN:
        sprintf(value,"%lld",resultvalue);
        RT::rep->status("result: %s", RT::rep->markup(MARKUP_WARNING, value).str());
        RT::data["analysis"]["result"] = (int)(resultvalue);
        sprintf(value,"The maximum value of the given expression is at least %lld",resultvalue);
            RT::rep->status("%s", RT::rep->markup(MARKUP_WARNING, value).str());
        break;

    default: assert(false);
    }

}
 
void ComputeBoundTask::getStatistics()
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

Task * ComputeBoundTask::buildTask()
{
	return new ComputeBoundTask();
}


char * ComputeBoundTask::getStatus(uint64_t elapsed)
{
	if(!goStatus)
	{
		return NULL;
	}
        char * result = new char[STATUSLENGTH];
        uint64_t m = store->get_number_of_markings();
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
        return result;

}


