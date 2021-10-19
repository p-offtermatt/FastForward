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
#include <Planning/SequentialTask.h>
#include <Planning/ParallelTask.h>
#include <Planning/StateEquationTask.h>


/*!
\brief the verification task

This is a dummy class, i.e. no objects will be generated. The only purpose
of this class is to have a buildTask method that evaluates cmdline options
and creates a a task for reachability checking.

*/

class ReachabilityTask : public Task
{
public:
    ~ReachabilityTask(){}

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
    static Task * buildTask() {

	// 16 different variants:
	// variant 1: (findpath||stateequation);search
	if ((RT::args.findpath_arg == findpath_arg_seq) && (RT::args.stateequation_arg == stateequation_arg_seq))
	{
		RT::rep->status("Planning: workflow for reachability check: (findpath||statequation);search (%s)", RT::rep->markup(MARKUP_PARAMETER, "--findpath=seq").str(),
		RT::rep->markup(MARKUP_PARAMETER, "--stateequation=seq").str());
                return SequentialTask::buildTask(ParallelTask::buildTask(ReachabilityFindpathTask::buildTask(), StateEquationTask::buildTask()), ReachabilitySearchTask::buildTask());
	}

	// variant 2: findpath||stateequation||search
	else if ((RT::args.findpath_arg == findpath_arg_par) && (RT::args.stateequation_arg == stateequation_arg_par))
	{
		RT::rep->status("Planning: workflow for reachability check: findpath||stateequation||search (%s)", RT::rep->markup(MARKUP_PARAMETER, "--findpath=par").str(),
		RT::rep->markup(MARKUP_PARAMETER, "--stateequation=par").str());
				return ParallelTask::buildTask(StateEquationTask::buildTask(), ParallelTask::buildTask(ReachabilityFindpathTask::buildTask(), ReachabilitySearchTask::buildTask()));
	}

	// variant 3: findpath||stateequation
	else if (((RT::args.findpath_arg == findpath_arg_alone) && (RT::args.stateequation_arg == stateequation_arg_alone)) ||
		((RT::args.findpath_arg == findpath_arg_par) && (RT::args.stateequation_arg == stateequation_arg_alone)) ||
		((RT::args.findpath_arg == findpath_arg_alone) && (RT::args.stateequation_arg == stateequation_arg_par)))
	{
		RT::rep->status("Planning: workflow for reachability check: findpath||stateequation (%s)", RT::rep->markup(MARKUP_PARAMETER, "--findpath=alone|par").str(),
		RT::rep->markup(MARKUP_PARAMETER, "--stateequation=alone|par").str());
                return ParallelTask::buildTask(StateEquationTask::buildTask(), ReachabilityFindpathTask::buildTask());
	}

	// variant 4: search
	else if ((RT::args.findpath_arg == findpath_arg_off) && (RT::args.stateequation_arg == stateequation_arg_off))
	{
		RT::rep->status("Planning: workflow for reachability check: search (%s)", RT::rep->markup(MARKUP_PARAMETER, "--findpath=off").str(),
		RT::rep->markup(MARKUP_PARAMETER, "--stateequation=off").str());
                return ReachabilitySearchTask::buildTask();
	}

	// variant 5: findpath;(stateequation||search)
	else if ((RT::args.findpath_arg == findpath_arg_seq) && (RT::args.stateequation_arg == stateequation_arg_par))
	{
		RT::rep->status("Planning: workflow for reachability check: findpath;(stateequation||search) (%s)", RT::rep->markup(MARKUP_PARAMETER, "--findpath=seq").str(),
		RT::rep->markup(MARKUP_PARAMETER, "--stateequation=par").str());
				return SequentialTask::buildTask(ReachabilityFindpathTask::buildTask(), ParallelTask::buildTask(StateEquationTask::buildTask(), ReachabilitySearchTask::buildTask()));
	}

	// variant 6: findpath
	else if ((RT::args.findpath_arg == findpath_arg_alone) && (RT::args.stateequation_arg == stateequation_arg_off))
	{
		RT::rep->status("Planning: workflow for reachability check: findpath (%s)", RT::rep->markup(MARKUP_PARAMETER, "--findpath=alone").str(),
		RT::rep->markup(MARKUP_PARAMETER, "--stateequation=off").str());
				return ReachabilityFindpathTask::buildTask();
	}

	// variant 7: stateequation;search
	else if ((RT::args.findpath_arg == findpath_arg_off) && (RT::args.stateequation_arg == stateequation_arg_seq))
	{
		RT::rep->status("Planning: workflow for reachability check: stateequation;search (%s)", RT::rep->markup(MARKUP_PARAMETER, "--findpath=off").str(),
		RT::rep->markup(MARKUP_PARAMETER, "--stateequation=seq").str());
				return SequentialTask::buildTask(StateEquationTask::buildTask(), ReachabilitySearchTask::buildTask());
	}

	// variant 8: findpath;stateequation
	else if ((RT::args.findpath_arg == findpath_arg_seq) && (RT::args.stateequation_arg == stateequation_arg_alone))
	{
		RT::rep->status("Planning: workflow for reachability check: findpath;stateequation (%s)", RT::rep->markup(MARKUP_PARAMETER, "--findpath=seq").str(),
		RT::rep->markup(MARKUP_PARAMETER, "--stateequation=alone").str());
				return SequentialTask::buildTask(ReachabilityFindpathTask::buildTask(), StateEquationTask::buildTask());
	}

	// variant 9: findpath||search
	else if ((RT::args.findpath_arg == findpath_arg_par) && (RT::args.stateequation_arg == stateequation_arg_off))
	{
		RT::rep->status("Planning: workflow for reachability check: findpath||search (%s)", RT::rep->markup(MARKUP_PARAMETER, "--findpath=par").str(),
		RT::rep->markup(MARKUP_PARAMETER, "--stateequation=off").str());
				return ParallelTask::buildTask(ReachabilityFindpathTask::buildTask(), ReachabilitySearchTask::buildTask());
	}

	// variant 10: stateequation;findpath
	else if ((RT::args.findpath_arg == findpath_arg_alone) && (RT::args.stateequation_arg == stateequation_arg_seq))
	{
		RT::rep->status("Planning: workflow for reachability check: stateequation;findpath (%s)", RT::rep->markup(MARKUP_PARAMETER, "--findpath=alone").str(),
		RT::rep->markup(MARKUP_PARAMETER, "--stateequation=seq").str());
				return SequentialTask::buildTask(StateEquationTask::buildTask(), ReachabilityFindpathTask::buildTask());
	}

	// variant 11: stateequation||search
	else if ((RT::args.findpath_arg == findpath_arg_off) && (RT::args.stateequation_arg == stateequation_arg_par))
	{
		RT::rep->status("Planning: workflow for reachability check: stateequation||search (%s)", RT::rep->markup(MARKUP_PARAMETER, "--findpath=off").str(),
		RT::rep->markup(MARKUP_PARAMETER, "--stateequation=par").str());
				return ParallelTask::buildTask(StateEquationTask::buildTask(), ReachabilitySearchTask::buildTask());
	}

	// variant 12: findpath;search
	else if ((RT::args.findpath_arg == findpath_arg_seq) && (RT::args.stateequation_arg == stateequation_arg_off))
	{
		RT::rep->status("Planning: workflow for reachability check: findpath;search (%s)", RT::rep->markup(MARKUP_PARAMETER, "--findpath=seq").str(),
		RT::rep->markup(MARKUP_PARAMETER, "--stateequation=off").str());
				return SequentialTask::buildTask(ReachabilityFindpathTask::buildTask(), ReachabilitySearchTask::buildTask());
	}

	// variant 13: stateequation;(findpath||search)
	else if ((RT::args.findpath_arg == findpath_arg_par) && (RT::args.stateequation_arg == stateequation_arg_seq))
	{
		RT::rep->status("Planning: workflow for reachability check: stateequation;(findpath||search) (%s)", RT::rep->markup(MARKUP_PARAMETER, "--findpath=par").str(),
		RT::rep->markup(MARKUP_PARAMETER, "--stateequation=seq").str());
		return SequentialTask::buildTask(StateEquationTask::buildTask(), ParallelTask::buildTask(ReachabilityFindpathTask::buildTask(), ReachabilitySearchTask::buildTask()));
	}

	// variant 14: stateequation
	else if ((RT::args.findpath_arg == findpath_arg_off) && (RT::args.stateequation_arg == stateequation_arg_alone))
	{
		RT::rep->status("Planning: workflow for reachability check: stateequation (%s)", RT::rep->markup(MARKUP_PARAMETER, "--findpath=off").str(),
		RT::rep->markup(MARKUP_PARAMETER, "--stateequation=alone").str());
				return StateEquationTask::buildTask();
	}
	else { return NULL; }

	}
    char * getStatus(uint64_t){return NULL;}
    char * early_abortion(){return NULL;}
};
