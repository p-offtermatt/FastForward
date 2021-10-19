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

/*!
\file
\author Niels
\status new
\ingroup g_runtime

\brief Implementation of exit and termination handlers.
*/

#include <Core/Dimensions.h>
#include <Core/Handlers.h>
#include <Core/Runtime.h>
#include <InputOutput/InputOutput.h>
#include <InputOutput/SimpleString.h>
#include <InputOutput/Socket.h>
#include <Net/Marking.h>
#include <Net/Net.h>
#include <Net/Place.h>
#include <Net/Transition.h>

pthread_t Handlers::terminationHandler_thread;
time_t Handlers::start_time;

/*!
\param signum The signal to react to.
\note This function is only called for those signals that have been registered
      by calling signal() first - see Handlers::installTerminationHandlers().

\post LoLA is exited with exit code EXIT_TERMINATION.
*/
void Handlers::signalTerminationHandler(int signum)
{
    RT::rep->message("caught signal %s - aborting LoLA",
                     RT::rep->markup(MARKUP_WARNING, strsignal(signum)).str());

    // add signal name to JSON output
    RT::data["call"]["signal"] = strsignal(signum);

    exit(EXIT_TERMINATION);
}


/*!
The new handler is called in case memory allocation using "new" or "malloc"
fails. This function has little possibility to fix the problem, but can at
least end LoLA systematically.

\post LoLA is exited with exit code EXIT_TERMINATION.
*/
// LCOV_EXCL_START
void Handlers::newHandler()
{
    RT::rep->message("memory allocation failed");

    RT::data["call"]["error"] = "memory allocation failed";

    exit(EXIT_TERMINATION);
}
// LCOV_EXCL_STOP


/*!
The termination handler allows to terminate LoLA by sending a predefined secret
via socket. Once the message is received, LoLA's execution is terminated by
calling sending the SIGUSR1 signals which are processed in
Handlers::signalTerminationHandler().

\return Always returns NULL - This function is only of type void* to be
        callable by pthread_create.

\post LoLA will eventually exit as the SIGUSR1 signal is sent.
*/
void *Handlers::remoteTerminationHandler(void *)
{
    Socket listener_socket(RT::args.inputport_arg);
    char *sender = listener_socket.waitFor(RT::args.remoteTermination_arg);
    assert(sender);
    RT::rep->message("received %s packet (%s) from %s - shutting down",
                     RT::rep->markup(MARKUP_BAD, "KILL").str(),
                     RT::rep->markup(MARKUP_IMPORTANT, RT::args.remoteTermination_arg).str(),
                     RT::rep->markup(MARKUP_FILE, sender).str());
    delete[] sender;

    // abort LoLA by sending SIGUSR1 signal
    kill(getpid(), SIGUSR1);
    return NULL;
}


/*!
Determine memory and time consumption. For the former, `ps` is called.

\note We assume `ps` to be callable by LoLA.

\todo If concurrent processes of the same binary run, the process id should be
used to find the process rather than the name.
\todo The configure script should check if the `ps` tool is present.
*/
void Handlers::statistics()
{
    std::string call = std::string("ps -o rss -o comm | ") + TOOL_GREP + " " +
                       PACKAGE + " | " + TOOL_AWK +
                       " '{ if ($1 > max) max = $1 } END { print max \" KB\" }'";
    FILE *ps = popen(call.c_str(), "r");
    unsigned int memory = 0;
    int res = fscanf(ps, "%20u", &memory);
    assert(res != EOF);
    pclose(ps);
    RT::rep->message("memory consumption: %u KB", memory);
    RT::data["stats"]["memory"] = static_cast<int>(memory);

    time_t now;
    time(&now);
    RT::rep->message("time consumption: %.0lf seconds", difftime(now, start_time));
    RT::data["stats"]["time"] = difftime(now, start_time);
}


/*!
The exit handler allows to organize the termination of LoLA. This includes
closing files, reporting exit, and releasing memory.

\post Upcon completion, no other function is called.
*/
void Handlers::exitHandler()
{
    // shut down killer thread
    if (RT::args.remoteTermination_given)
    {
        const int ret = pthread_cancel(terminationHandler_thread);
        if (ret == 0)
        {
            RT::rep->status("killed listener thread");
        }
    }

    // report preliminary result
    if(RT::interim_result.length() > 0)
    {
	RT::rep->status("preliminary result: %s",RT::interim_result.c_str());
    }
    if (RT::args.json_given)
    {
        RT::rep->status("print data as JSON (%s)",
                        RT::rep->markup(MARKUP_PARAMETER, "--json").str());

        const std::string jsonstring = RT::data.toString();

        // manually add the size of the JSON file to the output
        RT::data["files"]["JSON"]["size"] = static_cast<int>(jsonstring.size() / 1024);

        Output o("JSON", RT::args.json_arg);
        fprintf(o, "%s\n", jsonstring.c_str());
    }

    // process call data
    RT::callHome();

    // kill child if any
    if(RT::childpid > 0)
    {
	kill(RT::childpid,SIGUSR1);
    }
    
    // kill sara if running
    if (RT::saraPID > 0)
    {
        kill(RT::saraPID,SIGUSR1);
    }

    // quick exit to avoid lengthy destructor calls
#ifndef USE_PERFORMANCE
    // release memory from command line parser
    // note: RT::args cannot be used from now on
    cmdline_parser_free(&RT::args);

    // tidy up
    Net::deleteNodes();
    Place::deletePlaces();
    Transition::deleteTransitions();
    Marking::deleteMarkings();

    // should be the very last call
    delete RT::rep;
#endif
}


/*!
This function is a pure wrapper of the atexit function. It also tracks the
start time of LoLA for later time statistics.

\post Installs Handlers::exitHandler as exit handler.
*/
void Handlers::installExitHandler()
{
    // set the function to call on normal termination
    atexit(exitHandler);

    time(&start_time);
}


/*!
Installs handlers for the signals SIGTERM, SIGINT, SIGUSR1, and SIGUSR2. It
further sets up a remote termination thread in case the `--remoteTermination`
flag is used.

\post Installs signal handlers for SIGTERM, SIGINT, SIGUSR1, and SIGUSR2.
\post Starts a remote termination thread if `--remoteTermination` is given.
*/
void Handlers::installTerminationHandlers()
{
    // listen to software termination signal (kill)
    signal(SIGTERM, signalTerminationHandler);
    // listen to interrupt program (ctrl+c)
    signal(SIGINT, signalTerminationHandler);
    // listen to user-defined signal 1 (used for remote termination)
    signal(SIGUSR1, signalTerminationHandler);
    // listen to user-defined signal 2 (used for self termination)
    signal(SIGUSR2, signalTerminationHandler);

    // start up listener thread
    if (RT::args.remoteTermination_given)
    {
        RT::rep->status("enabling remote termination (%s)", RT::rep->markup(MARKUP_PARAMETER,
                        "--remoteTermination").str());
        RT::rep->status("setting up listener socket at port %s - secret is %s",
                        RT::rep->markup(MARKUP_FILE, "%d", RT::args.inputport_arg).str(),
                        RT::rep->markup(MARKUP_IMPORTANT, RT::args.remoteTermination_arg).str());

        const int ret = pthread_create(&terminationHandler_thread, NULL,
                                       remoteTerminationHandler, NULL);
        // LCOV_EXCL_START
        if (UNLIKELY(ret != 0))
        {
            RT::rep->status("thread could not be created");
            RT::rep->abort(ERROR_THREADING);
        }
        // LCOV_EXCL_STOP
    }
}
