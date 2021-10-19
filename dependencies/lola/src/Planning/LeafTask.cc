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
#include <Planning/CTLTask.h>
#include <Planning/LTLTask.h>
#include <Planning/InvariantTask.h>
#include <Planning/Task.h>
#include <Planning/DeadlockTask.h>
#include <Planning/ReachabilityTask.h>
#include <Planning/ComputeBoundTask.h>
#include <Planning/ParallelTask.h>
#include <Planning/BooleanTask.h>
#include <Planning/SequentialTask.h>
#include <Planning/NoDeadlockTask.h>
#include <Planning/AGEFTask.h>
#include <Planning/EFAGTask.h>
#include <Planning/EFAGEFTask.h>
#include <Planning/AGEFAGTask.h>
#include <Planning/InitialTask.h>
#include <Planning/StoreCreator.h>
#include <Planning/LeafTask.h>
#include <Exploration/StatePredicateProperty.h>
#include <Exploration/FirelistStubbornDeletion.h>
#include <Exploration/FirelistStubbornStatePredicate.h>
#include <Exploration/DFSExploration.h>
#include <Exploration/ParallelExploration.h>

// copied from lola2/src/InputOutput/JSON.cc: ... //
#include <sstream>
/////////////////////
// HELPER FUNCTION //
/////////////////////

#ifndef __cplusplus11

inline std::string int_to_string(int i)
{
    std::stringstream s;
    s << i;
    return s.str();
}

inline std::string float_to_string(double f)
{
    std::stringstream s;
    s << f;
    return s.str();
}
#endif
// ... copied from lola2/src/InputOutput/JSON.cc //

/*!
\brief the verification task

This class handles a task that is a leaf in a formula tree of top level Booleans
or compund formulas. It main duty is to fork a child process that is doing
the actual work.

 */

void * report_status(void*);
extern kc::tFormula TheFormula;

LeafTask::LeafTask(kc::tStatePredicate f)
{
    formula = StatePredicateFormula(f);
    formula -> type = f -> type;
}

LeafTask::LeafTask(kc::tFormula f)
{
    formula = f;
}

LeafTask::~LeafTask()
{
}

ternary_t LeafTask::getResult()
{
    // 1. launch child process

    // 1a. prepare a channel between child and parent

    int mypipe[2];
    if (pipe(mypipe))
    {
        // pipe creation failed
        RT::rep->status("Creation of communication channel failed: cannot handle subproblem");
        return TERNARY_UNKNOWN;
    }

    // 1b. fork new process

    pid_t process_id = fork();
    if (process_id < (pid_t) 0)
    {
        // fork failed
        RT::rep->status("Cannot launch process for handling subproblem");
        return TERNARY_UNKNOWN;
    }
    if (process_id > (pid_t) 0)
    {
        RT::compoundNumber++;
        // code for parent process
        // 2p. parent process waits for result and returns

        resultstring = "unknown";
        RT::childpid = process_id;
        close(mypipe[1]); // we do not want to send information to child
        char childmessage[128];
        childmessage[0] ='\0';
        read(mypipe[0], childmessage, 128);
        int64_t rrr;
        switch (childmessage[0])
        {
            case 't': resultstring = "yes";
                RT::interim_result += " " + resultstring;
                RT::childpid = 0;
                return TERNARY_TRUE;
            case 'f': resultstring = "no";
                RT::childpid = 0;
                RT::interim_result += " " + resultstring;
                return TERNARY_FALSE;
            case 'u': RT::interim_result += " " + resultstring;
                RT::childpid = 0;
                return TERNARY_UNKNOWN;
            case 'n': sscanf(childmessage + 1, "%lld", &rrr);
                resultstring =
#ifdef __cplusplus11
                        std::to_string(rrr);
#else
                        int_to_string(rrr);
#endif

                RT::interim_result += " " + resultstring;
                RT::childpid = 0;
                return TERNARY_TRUE;
            default: RT::rep->status("Child process aborted or communication problem between parent and child process");
                resultstring = "unknown";
                RT::interim_result += " " + resultstring;
                RT::childpid = 0;
                return TERNARY_UNKNOWN;
        }
    }
    // code for child process


    close(mypipe[0]); // we do not expect info from parent;
    TheFormula = formula; // switch to subproblem

    // child not responsible for announcing prelimiary results
    RT::interim_result = "";
    
    // replace time measuring thread
    // pthread_cancel(RT::reporter_thread);
    pthread_t local_reporter_thread;
    if (RT::args.localtimelimit_given)
    {
        // check if dynamic localtimelimit should be used
        if (RT::args.localtimelimit_arg <= 0 && RT::args.timelimit_arg > 0)
        {
            // use dynamic localtimelimit
            time_t end;
            // get local time limit for the current compound formula
            // (timelimit - elasped time) / remaining compound tasks
            int localtimelimitauto = 
                (int)((RT::args.timelimit_arg - (int)(difftime(time(&end),RT::startTime))) /
                (RT::numberOfCompoundTasks - RT::compoundNumber));
            // set localtimelimit at least to 1, to get low hanging fruits
            RT::localTimeLimitDynamic = localtimelimitauto > 0 ? localtimelimitauto : 1;
            
            RT::rep->status("subprocess %i will run for %d seconds at most (%s)",
                    RT::compoundNumber, RT::localTimeLimitDynamic, RT::rep->markup(MARKUP_PARAMETER, 
                    "--localtimelimit=0").str());
        }
        else
        {
            // use for localtimelimit the passed argument
            RT::rep->status("subprocess %i will run for %d seconds at most (%s)",
                    RT::compoundNumber, RT::args.localtimelimit_arg, RT::rep->markup(MARKUP_PARAMETER, 
                    "--localtimelimit").str());
        }
        // create localtimelimit thread
        const int ret = pthread_create(&local_reporter_thread, NULL, 
                RT::local_reporter_internal, NULL);
        // LCOV_EXCL_START
        if (UNLIKELY(ret != 0))
        {
            RT::rep->status("thread could not be created");
            RT::rep->abort(ERROR_THREADING);
        }
        // LCOV_EXCL_STOP
    }

    // 2c. announce subproblem
    RT::rep->status("========================================");
    unparsed.clear();
    TheFormula->unparse(stringprinter, kc::out);
    if (unparsed.size() < FORMULA_PRINT_SIZE)
    {
        RT::rep->status("...considering subproblem: %s", unparsed.c_str());
    }
    else
    {
        RT::rep->status("...considering subproblem: %s... (shortened)", unparsed.substr(0, FORMULA_PRINT_SIZE).c_str());
    }
    RT::rep->status("========================================");

    // 3c. child process turns subformula into task
    RT::rep->status("SUBTASK");
    RT::rep->indent(2);
    switch (TheFormula->type)
    {
        case FORMULA_BOOLEAN:
            RT::rep->status("checking a Boolean combination of formulas");
            RT::data["analysis"]["formula"]["type"] = "deadlock";
            subTask = BooleanTask::buildTask();
            break;
        case FORMULA_BOUND:
            RT::rep->status("computing bound of an expression");
            RT::data["analysis"]["formula"]["type"] = "bound";
            subTask = ComputeBoundTask::buildTask();
            break;
        case FORMULA_DEADLOCK:
            RT::rep->status("checking reachability of deadlocks");
            RT::data["analysis"]["formula"]["type"] = "deadlock";
            subTask = DeadlockTask::buildTask();
            break;
        case FORMULA_NODEADLOCK:
            RT::rep->status("checking absence of deadlocks");
            RT::data["analysis"]["formula"]["type"] = "nodeadlock";
            subTask = NoDeadlockTask::buildTask();
            break;
        case FORMULA_REACHABLE:
            RT::rep->status("checking reachability");
            RT::data["analysis"]["formula"]["type"] = "reachability";
            subTask = ReachabilityTask::buildTask();
            break;
        case FORMULA_INVARIANT:
            RT::rep->status("checking invariance");
            RT::data["analysis"]["formula"]["type"] = "invariance";
            subTask = InvariantTask::buildTask();
            break;
        case FORMULA_LIVENESS:
            RT::rep->status("checking liveness");
            RT::data["analysis"]["formula"]["type"] = "liveness";
            //result = AGEFTask::buildTask(); 
            RT::rep->status("liveness not yet implemented, converting to CTL...");
            subTask = CTLTask::buildTask();
            break;
        case FORMULA_EGAGEF:
            RT::rep->status("checking possible liveness");
            RT::data["analysis"]["formula"]["type"] = "possible_liveness";
            //result = EFAGEFTask::buildTask(); 
            RT::rep->status("possible liveness not yet implemented, converting to CTL...");
            subTask = CTLTask::buildTask();
            break;
        case FORMULA_EFAG:
            RT::rep->status("checking possible invariance");
            RT::data["analysis"]["formula"]["type"] = "possible_invariance";
            //result = EFAGTask::buildTask(); 
            RT::rep->status("possible invariance not yet implemented, converting to CTL...");
            subTask = CTLTask::buildTask();
            break;
        case FORMULA_AGEFAG:
            RT::rep->status("checking globally possible invariance");
            RT::data["analysis"]["formula"]["type"] = "globally_possible_invariance";
            //result = AGEFAGTask::buildTask(); 
            RT::rep->status("lobally possible invariance not yet implemented, converting to CTL...");
            subTask = CTLTask::buildTask();
            break;
        case FORMULA_FAIRNESS:
            RT::rep->status("checking fairness");
            RT::data["analysis"]["formula"]["type"] = "fairness";
            RT::rep->status("fairness not yet implemented, converting to LTL...");
            subTask = LTLTask::buildTask();
            break;
        case FORMULA_STABILIZATION:
            RT::rep->status("checking stabilization");
            RT::data["analysis"]["formula"]["type"] = "stabilization";
            RT::rep->status("stabilization not yet implemented, converting to LTL...");
            subTask = LTLTask::buildTask();
            break;
        case FORMULA_EVENTUALLY:
            RT::rep->status("checking eventual occurrence");
            RT::data["analysis"]["formula"]["type"] = "eventual occurrence";
            RT::rep->status("eventual occurrence not yet implemented, converting to LTL...");
            subTask = LTLTask::buildTask();
            break;
        case FORMULA_INITIAL:
            RT::rep->status("checking initial satisfaction");
            RT::data["analysis"]["formula"]["type"] = "initial satisfaction";
            subTask = InitialTask::buildTask();
            break;
        case FORMULA_LTL:
            RT::rep->status("checking LTL");
            RT::data["analysis"]["formula"]["type"] = "LTL";
            subTask = LTLTask::buildTask();
            break;
        case FORMULA_CTL:
            RT::rep->status("checking CTL");
            RT::data["analysis"]["formula"]["type"] = "CTL";
            subTask = CTLTask::buildTask();
            break;
        default:
            RT::rep->status("checking CTL*");
            RT::data["analysis"]["formula"]["type"] = "CTL*";
            RT::rep->message("check not yet implemented");
            subTask = NULL;
    }

    // 4c. child process executes task

    ternary_t bresult;
    pthread_t reporter; // status messages from child process
    if (subTask)
    {
        const int ret = pthread_create(&reporter, NULL, report_status, reinterpret_cast<void*> (subTask));
        if (UNLIKELY(ret != 0))
        {
            RT::rep->status("thread could not be created");
            RT::rep->abort(ERROR_THREADING);
        }


        RT::rep->indent(-2);
        RT::rep->status("RUNNING");
        RT::rep->indent(2);
        bresult = subTask->getResult(); // run the check
    }
    else
    {
        bresult = TERNARY_UNKNOWN;
    }

    // 5c. child process transmits result and exits

    RT::rep->indent(-2);
    RT::rep->status("SUBRESULT");
    RT::rep->indent(2);
    subTask->interpreteResult(bresult); //make sense of returned result
    if (RT::args.state_given)
    {
        RT::rep->status("print witness state (%s)",
                RT::rep->markup(MARKUP_PARAMETER, "--state").str());
        Output o("witness state", RT::args.state_arg);

        const capacity_t *current = subTask->getMarking();
        if (current)
        {
            for (arrayindex_t p = 0; p < Net::Card[PL]; ++p)
            {
                if (current[p] == OMEGA)
                {
                    fprintf(o, "%s : oo\n", Net::Name[PL][p]);
                }
                else if (current[p] > 0)
                {
                    fprintf(o, "%s : %d\n", Net::Name[PL][p], current[p]);
                }
            }
        }
        else
        {
            RT::rep->status("no witness state generated");
            fprintf(o, "NOSTATE\n");
        }
    }
    if (RT::args.path_given)
    {
        const Path p = subTask->getWitnessPath();
        if (p.initialized)
        {
            if (RT::args.pathshape_arg == pathshape_arg_linear)
            {
                RT::rep->status("print witness path (%s)",
                        RT::rep->markup(MARKUP_PARAMETER, "--path").str());
                Output o("witness path", RT::args.path_arg);
                p.print(o);
            }
            else
            {
                RT::rep->status("print distributed run (%s)",
                        RT::rep->markup(MARKUP_PARAMETER, "--pathshape").str());
                Output o("distributed run", RT::args.path_arg);
                p.printRun(o);
            }
        }
    }

    subTask -> getStatistics();

    RT::rep->indent(-2);
    // create message to parent
    char message[128];
    if (formula->type != FORMULA_BOUND)
    {
        switch (bresult)
        {
            case TERNARY_TRUE:
                message[0] = 't';
                message[1] = '\0';
                break;
            case TERNARY_FALSE:
                message[0] = 'f';
                message[1] = '\0';
                break;
            case TERNARY_UNKNOWN:
                message[0] = 'u';
                message[1] = '\0';
                break;
        }
    }
    else
    {
        message[0] = 'n';
        sprintf(message + 1, "%lld", reinterpret_cast<ComputeBoundTask*> (subTask)->resultvalue);
    }
    write(mypipe[1], message, strlen(message) + 1);
    pthread_cancel(reporter); // stop reporter thread
    RT::rep->status("========================================");
    _exit(0);
}

Task * LeafTask::buildTask()
{
    return NULL; // we do not want to create such task using the buildTask procedure (use BooleanTask instead)
}

