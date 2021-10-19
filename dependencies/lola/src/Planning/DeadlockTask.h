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
#include <Planning/DeadlockSearchTask.h>
#include <Planning/SiphonTrapTask.h>
#include <Planning/SequentialTask.h>
#include <Planning/ParallelTask.h>
#include <Planning/DeadlockFindpathTask.h>



/*!
\brief the verification task

This is a dummy class, i.e. no objects will be generated. The only purpose
of this class is to have a buildTask method that evaluates cmdline options
and assembles a--possibly complex--task for deadlock checking.

*/

class DeadlockTask : public Task
{
public:
     // there will be no members of this class
     // LCOV_EXCL_START
    ~DeadlockTask(){}

    /// run the actual verification algorithm
    ternary_t getResult() {return TERNARY_UNKNOWN;} // dummy

    /// interprete and display the result
    void interpreteResult(ternary_t result) {} //dummy

    /// return the witness path
    Path getWitnessPath() { assert(false); return *(new Path());;} // dumy
    /// return the target marking
    capacity_t *getMarking() { return NULL;} // dummy

    /// return the number of stored markings
    void getStatistics() {} // dummy
    char * getStatus(uint64_t){ return NULL;}
    char * early_abortion(){return NULL;}
    // LCOV_EXCL_STOP
    static Task * buildTask() // NOT dummy
    {  
	/* currently, there are three methods for deadlock checking:
           - a state space search (which has many configuration options)
           - a siphon/trap check (an incomplete structural method)
    	   - findpath (memoryless path generation, incomplete)
 	   The latter two can be run in parallel, before the main state
 	   space search, or in isolation.       
	*/
	
	// variant 1: search
	if(RT::args.findpath_arg==findpath_arg_off &&
           RT::args.siphontrap_arg==siphontrap_arg_off)
	{	
		RT::rep->status("Planning: workflow for deadlock check: search (%s,%s)",RT::rep->markup(MARKUP_PARAMETER,"--findpath=off").str(),RT::rep->markup(MARKUP_PARAMETER,"--siphontrap=off").str());
		return DeadlockSearchTask::buildTask();
	}
	// variant 2: siphon
	if(RT::args.findpath_arg==findpath_arg_off &&
           RT::args.siphontrap_arg==siphontrap_arg_alone)
	{	
		RT::rep->status("Planning: workflow for deadlock check: siphon (%s,%s)",RT::rep->markup(MARKUP_PARAMETER,"--findpath=off").str(),RT::rep->markup(MARKUP_PARAMETER,"--siphontrap=alone").str());
		return SiphonTrapTask::buildTask();
	}
	// variant 3: siphon;search
	if(RT::args.findpath_arg==findpath_arg_off &&
           RT::args.siphontrap_arg==siphontrap_arg_seq)
	{	
		RT::rep->status("Planning: workflow for deadlock check: siphon;search (%s,%s)",RT::rep->markup(MARKUP_PARAMETER,"--findpath=off").str(),RT::rep->markup(MARKUP_PARAMETER,"--siphontrap=seq").str());
		return SequentialTask::buildTask(SiphonTrapTask::buildTask(),DeadlockSearchTask::buildTask());
	}
	// variant 4: siphon||search
	if(RT::args.findpath_arg==findpath_arg_off &&
           RT::args.siphontrap_arg==siphontrap_arg_par)
	{	
		RT::rep->status("Planning: workflow for deadlock check: siphon||search (%s,%s)",RT::rep->markup(MARKUP_PARAMETER,"--findpath=off").str(),RT::rep->markup(MARKUP_PARAMETER,"--siphontrap=par").str());
		return ParallelTask::buildTask(SiphonTrapTask::buildTask(),DeadlockSearchTask::buildTask());
	}
	// variant 5: findpath
	if(RT::args.findpath_arg==findpath_arg_alone &&
           RT::args.siphontrap_arg==siphontrap_arg_off)
	{	
		RT::rep->status("Planning: workflow for deadlock check: findpath (%s,%s)",RT::rep->markup(MARKUP_PARAMETER,"--findpath=alone").str(),RT::rep->markup(MARKUP_PARAMETER,"--siphontrap=off").str());
		return DeadlockFindpathTask::buildTask();
	}
	// variant 6: findpath;search
	if(RT::args.findpath_arg==findpath_arg_seq &&
           RT::args.siphontrap_arg==siphontrap_arg_off)
	{	
		RT::rep->status("Planning: workflow for deadlock check: findpath;search (%s,%s)",RT::rep->markup(MARKUP_PARAMETER,"--findpath=seq").str(),RT::rep->markup(MARKUP_PARAMETER,"--siphontrap=off").str());
		return SequentialTask::buildTask(DeadlockFindpathTask::buildTask(),DeadlockSearchTask::buildTask());
	}
	// variant 7: findpath||search
	if(RT::args.findpath_arg==findpath_arg_par &&
           RT::args.siphontrap_arg==siphontrap_arg_off)
	{	
		RT::rep->status("Planning: workflow for deadlock check: findpath||search (%s,%s)",RT::rep->markup(MARKUP_PARAMETER,"--findpath=par").str(),RT::rep->markup(MARKUP_PARAMETER,"--siphontrap=off").str());
		return ParallelTask::buildTask(DeadlockFindpathTask::buildTask(),DeadlockSearchTask::buildTask());
	}
	// variant 8: findpath||siphon
	if(
               (RT::args.findpath_arg==findpath_arg_alone &&
               RT::args.siphontrap_arg==siphontrap_arg_alone)
		||
               (RT::args.findpath_arg==findpath_arg_par &&
               RT::args.siphontrap_arg==siphontrap_arg_alone)
		||
               (RT::args.findpath_arg==findpath_arg_alone &&
               RT::args.siphontrap_arg==siphontrap_arg_par))
	{	
		RT::rep->status("Planning: workflow for deadlock check: findpath||search (%s,%s)",RT::rep->markup(MARKUP_PARAMETER,"--findpath=alone|par").str(),RT::rep->markup(MARKUP_PARAMETER,"--siphontrap=alone|par").str());
		return ParallelTask::buildTask(DeadlockFindpathTask::buildTask(),SiphonTrapTask::buildTask());
	}
	// variant 9: findpath;siphon
	if(RT::args.findpath_arg==findpath_arg_seq &&
           RT::args.siphontrap_arg==siphontrap_arg_alone)
	{	
		RT::rep->status("Planning: workflow for deadlock check: findpath;siphon (%s,%s)",RT::rep->markup(MARKUP_PARAMETER,"--findpath=seq").str(),RT::rep->markup(MARKUP_PARAMETER,"--siphontrap=alone").str());
		return SequentialTask::buildTask(DeadlockFindpathTask::buildTask(),SiphonTrapTask::buildTask());
	}
	// variant 10: siphon;findpath
	if(RT::args.findpath_arg==findpath_arg_alone &&
           RT::args.siphontrap_arg==siphontrap_arg_seq)
	{	
		RT::rep->status("Planning: workflow for deadlock check: siphon;findpath (%s,%s)",RT::rep->markup(MARKUP_PARAMETER,"--findpath=alone").str(),RT::rep->markup(MARKUP_PARAMETER,"--siphontrap=seq").str());
		return SequentialTask::buildTask(SiphonTrapTask::buildTask(),DeadlockFindpathTask::buildTask());
	}
	// variant 11: (findpath||siphon);search
	if(RT::args.findpath_arg==findpath_arg_seq &&
           RT::args.siphontrap_arg==siphontrap_arg_seq)
	{	
		RT::rep->status("Planning: workflow for deadlock check: (siphon||findpath);search (%s,%s)",RT::rep->markup(MARKUP_PARAMETER,"--findpath=seq").str(),RT::rep->markup(MARKUP_PARAMETER,"--siphontrap=seq").str());
		return SequentialTask::buildTask(ParallelTask::buildTask(SiphonTrapTask::buildTask(),DeadlockFindpathTask::buildTask()),DeadlockSearchTask::buildTask());
	}
	// variant 12: findpath;(siphon||search)
	if(RT::args.findpath_arg==findpath_arg_seq &&
           RT::args.siphontrap_arg==siphontrap_arg_par)
	{	
		RT::rep->status("Planning: workflow for deadlock check: findpath;(siphon||search) (%s,%s)",RT::rep->markup(MARKUP_PARAMETER,"--findpath=seq").str(),RT::rep->markup(MARKUP_PARAMETER,"--siphontrap=par").str());
		return SequentialTask::buildTask(DeadlockFindpathTask::buildTask(),ParallelTask::buildTask(SiphonTrapTask::buildTask(),DeadlockSearchTask::buildTask()));
	}
	// variant 13: siphon;(findpath||search)
	if(RT::args.findpath_arg==findpath_arg_par &&
           RT::args.siphontrap_arg==siphontrap_arg_seq)
	{	
		RT::rep->status("Planning: workflow for deadlock check: siphon;(findpath||search) (%s,%s)",RT::rep->markup(MARKUP_PARAMETER,"--findpath=par").str(),RT::rep->markup(MARKUP_PARAMETER,"--siphontrap=seq").str());
		return SequentialTask::buildTask(SiphonTrapTask::buildTask(),ParallelTask::buildTask(DeadlockFindpathTask::buildTask(),DeadlockSearchTask::buildTask()));
	}
  	// variant 14: siphon||findpath||search
	if(RT::args.findpath_arg==findpath_arg_par &&
           RT::args.siphontrap_arg==siphontrap_arg_par)
	{	
		RT::rep->status("Planning: workflow for deadlock check: siphon||findpath||search) (%s,%s)",RT::rep->markup(MARKUP_PARAMETER,"--findpath=par").str(),RT::rep->markup(MARKUP_PARAMETER,"--siphontrap=par").str());
		return ParallelTask::buildTask(SiphonTrapTask::buildTask(),ParallelTask::buildTask(DeadlockFindpathTask::buildTask(),DeadlockSearchTask::buildTask()));
	}
	// the above case consideration should be exhaustive
	assert(false);
 	return NULL;
    }
    
};
