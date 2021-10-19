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

#include <config.h>
#include <Core/Dimensions.h>
#include <Exploration/Firelist.h>
#include <Exploration/DFSExploration.h>
#include <Exploration/FirelistStubbornDeadlock.h>
#include <Exploration/FirelistStubbornDeletion.h>
#include <Exploration/ParallelExploration.h>
#include <Exploration/DeadlockExploration.h>
#include <Exploration/StatePredicateProperty.h>
#include <Frontend/Parser/ast-system-k.h>
#include <Frontend/Parser/ast-system-rk.h>
#include <Frontend/Parser/ast-system-unpk.h>
#include <Frontend/SymbolTable/SymbolTable.h>
#include <InputOutput/InputOutput.h>
#include <Stores/Store.h>
#include <Witness/Path.h>
#include <Planning/DeadlockSearchTask.h>
#include <Planning/StoreCreator.h>
#include <Symmetry/Symmetry.h>
#include <Symmetry/Constraints.h>
#include <SweepLine/Sweep.h>
#include <CoverGraph/CoverGraph.h>
#include <Core/Handlers.h>

DeadlockSearchTask::DeadlockSearchTask()
{
     goStatus = false;
     previousNrOfMarkings = 0;
    // set the net
    ns = NetState::createNetStateFromInitial();
    // prepare task


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
            RT::rep->status("warning: encoder does not support coverability graphs -- replacing");
        }
        covStore = StoreCreator<CoverPayload>::createStore(number_of_threads);
	store = NULL;
        break;
    }
	default:
        // choose a store
	store = StoreCreator<void>::createStore(number_of_threads);
	covStore = NULL;
    }

    
    RT::rep->indent(-2);
    RT::rep->status("SEARCH");
    RT::rep->indent(2);
    switch(RT::args.search_arg)
    {
    case search_arg_covergraph:
                    RT::rep->status("using coverability graph (%s)", RT::rep->markup(MARKUP_PARAMETER,
                                    "--search=cover").str());
	p = new StatePredicateProperty(new DeadlockPredicate(true));
	break;
    case search_arg_sweepline:
                    RT::rep->status("using sweep-line method (%s)", RT::rep->markup(MARKUP_PARAMETER,
                                    "--search=sweep").str());
	    p = new DeadlockExploration();
            break;
    default:
                    RT::rep->status("using reachability graph (%s)", RT::rep->markup(MARKUP_PARAMETER,
                                    "--search=depth").str());
	    p = new DeadlockExploration();
    }
    switch (RT::args.stubborn_arg)
    {
    case stubborn_arg_deletion:
		RT::rep->status("using deadlock preserving stubborn set method with deletion algorithm(%s)", RT::rep->markup(MARKUP_PARAMETER, "--stubborn=deletion").str());
                fl = new FirelistStubbornDeletion();
		break;
    case stubborn_arg_tarjan:
		RT::rep->status("using deadlock preserving stubborn set method with insertion algorithm(%s)", RT::rep->markup(MARKUP_PARAMETER, "--stubborn=tarjan").str());
                fl = new FirelistStubbornDeadlock();
	 	break;
    case stubborn_arg_off:
		RT::rep->status("not using stubborn set method (%s)", RT::rep->markup(MARKUP_PARAMETER, "--stubborn=off").str());
		fl = new Firelist();
		break;
    default:
	assert(false); // enumeration exhaustive
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


/*!
\post memory for all members is deallocated
*/
DeadlockSearchTask::~DeadlockSearchTask()
{
#ifndef USE_PERFORMANCE
    delete ns;
    delete covStore;
    delete store;
    delete p;
    delete fl;
    delete exploration;
#endif
}


ternary_t DeadlockSearchTask::getResult()
{
    //TODO can we make these assumptions clearer that the asserts are creating
    assert(ns);
    assert(store);
    assert(exploration);
    assert(p);
    assert(fl);


    // compute symmetries
    if (RT::args.symmetry_given)
    {
        SymmetryCalculator *SC = new SymmetryCalculator();
        assert(SC);
        SC->ComputeSymmetries();
        delete SC;
	if(store) store = reinterpret_cast<SymmetryStore<void>*>(store)->setGeneratingSet(SymmetryCalculator::G);
	if(covStore) covStore = reinterpret_cast<SymmetryStore<CoverPayload>*>(covStore)->setGeneratingSet(SymmetryCalculator::G);
    }
 	goStatus = true;

    bool bool_result(false);
    ternary_t result(TERNARY_FALSE);
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

                    result = exploration->cover_breadth_first(*p, *ns, *covStore, *fl, number_of_threads, FORMULA_DEADLOCK);
                    break;

                default:
                    assert(false);
                }

    // temporary result transfer, as long as the variable bool_result is needed
    if (bool_result)
    {
        result = TERNARY_TRUE;
    }

    //TODO can we make these assumptions clearer that the asserts are creating
    return result;
}


void DeadlockSearchTask::interpreteResult(ternary_t result)
{
    if (RT::args.store_arg == store_arg_bloom)
    {
	    // if the Bloom store did not find anything, the result is unknown
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

            RT::rep->status("%s", RT::rep->markup(MARKUP_GOOD, "The net has deadlock(s).").str());

        break;

    case TERNARY_FALSE:
        RT::rep->status("result: %s", RT::rep->markup(MARKUP_BAD, "no").str());
        RT::data["analysis"]["result"] = false;

            RT::rep->status("%s", RT::rep->markup(MARKUP_BAD,
                                                  "The net does not have deadlocks.").str());
            break;

        break;

    case TERNARY_UNKNOWN:
        RT::rep->status("result: %s", RT::rep->markup(MARKUP_WARNING, "unknown").str());
        RT::data["analysis"]["result"] = JSON::null;

            RT::rep->status("%s", RT::rep->markup(MARKUP_WARNING,
                                                  "The net may or may not have deadlocks.").str());
        break;
    }
}


Path DeadlockSearchTask::getWitnessPath() 
{
        return p->path();
}


capacity_t *DeadlockSearchTask::getMarking() 
{
    if (p and p->value)
    {
        return ns->Current;
    }
    else
    {
        return NULL;
    }

}

void DeadlockSearchTask::getStatistics()
{
	if (RT::args.store_arg == store_arg_bloom)
    {
        const double n = static_cast<double>(store->get_number_of_markings());
        const double k = RT::args.hashfunctions_arg;
        const double m = static_cast<double>(BLOOM_FILTER_SIZE);
        RT::rep->status("Bloom filter: probability of false positive is %.10lf",
                        pow((1.0 - exp((-k * n) / m)), k));
        RT::rep->status("Bloom filter: optimal number of hash functions is %.1f",
                        log(m / n) / log(2.0));
    }

	uint64_t markingcount = store ? store -> get_number_of_markings() : 
                                    covStore -> get_number_of_markings();
        uint64_t edgecount = store ? store -> get_number_of_calls() :
                                  covStore -> get_number_of_calls();
        RT::rep->status("%llu markings, %llu edges", markingcount, edgecount);
RT::data["analysis"]["stats"]["states"] = static_cast<int>(markingcount);
RT::data["analysis"]["stats"]["edges"] = static_cast<int>(edgecount);

}

char * DeadlockSearchTask::getStatus(uint64_t elapsed)
{
	char * result;
	if(!goStatus)
	{
		return NULL;
	}
	uint64_t m = store->get_number_of_markings();
	switch(RT::args.search_arg)
	{
	case search_arg_sweepline:
		result = exploration -> s -> getStatus();
		sprintf(result + strlen(result)," %5llu secs", elapsed);
		break;
	case search_arg_covergraph:

	result = new char[STATUSLENGTH];
	sprintf(result,"%10llu markings, %10llu edges, %8.0f markings/sec, %5llu secs", m, store->get_number_of_calls(), ((m - previousNrOfMarkings) / (float)REPORT_FREQUENCY), elapsed);

       previousNrOfMarkings = m;
	    break;
	default:
	result = new char[STATUSLENGTH];
	sprintf(result,"%10llu markings, %10llu edges, %8.0f markings/sec, %5llu secs", m, store->get_number_of_calls(), ((m - previousNrOfMarkings) / (float)REPORT_FREQUENCY), elapsed);

       previousNrOfMarkings = m;
            // report probability of a false positive in the Bloom filter
            if (RT::args.store_arg == store_arg_bloom)
            {
                const double n = static_cast<double>(m);
                static const double k = static_cast<double>(RT::args.hashfunctions_arg);
                static const double mm = static_cast<double>(BLOOM_FILTER_SIZE);
                sprintf(result+strlen(result),"%10lu hash table size      false positive probability: %.10lf",
                                BLOOM_FILTER_SIZE, pow((1.0 - exp((-k * n) / mm)), k));
            }
	}
	return result;

}
