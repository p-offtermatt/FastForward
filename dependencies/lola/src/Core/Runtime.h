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

\brief Definition of runtime specific functionality.
*/

#pragma once

#include <cmdline.h>
#include <config.h>
#include <Core/Dimensions.h>
#include <InputOutput/InputOutput.h>
#include <InputOutput/JSON.h>
#include <InputOutput/Reporter.h>

/*!
This structure collects all kinds of variables and functiosn that assist LoLA
during runtime. As it makes no sense to have more than one copy of this
structure, all functions are static.
*/
struct RT
{
public:
    /// key/value store for structured information
    static JSON data;

    /// a global reporter
    static Reporter *rep;

    /// the command line parameters
    static gengetopt_args_info args;

    /// the current file to read a Petri net from
    static Input *currentInputFile;

    /// the current file name to read a formula from
    static std::string inputFormulaFileName;

    /// a thread that runs the reporter_thread function
    static pthread_t reporter_thread;

    /// initialize the runtime structure
    static void initialize(int argc, char **argv);

    /// send the used configuration to a logging server
    static void callHome();

    /// PID of a potential sara process; 0 if no sara process is there
    static int saraPID;

    /// PID of a potential child process; 0 if no child is there
    static int childpid;

    /// string containing the result to be put if LoLA is aborted early
    static std::string interim_result;
    
    /// indicating if a localtimelimit for a single task is needed
    static bool needLocalTimeLimit;
    
    /// int containing the compound number for unique sara task files
    static int compoundNumber;

    /// int containing the number of compound tasks
    static int numberOfCompoundTasks;
    
    /// containing the start time of LoLA used for '--localtimelimit=0'
    static time_t startTime;

    /// containing the dynamic localtimelimit value
    static int localTimeLimitDynamic;

    /// a function that implements the localtimeout for `--localtimelimit'
    static void *local_reporter_internal(void *);

private:
    // forbid the creation of objects
    explicit RT();

    /// a function that implements the timeout for `--timelimit'
    static void *reporter_internal(void *);

    /// evaluate the command line parameters
    static void evaluateParameters(int argc, char **argv);
};
