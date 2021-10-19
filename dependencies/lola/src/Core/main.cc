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
\status approved 25.01.2012

\brief Main entry point for LoLA.
 */

#include <config.h>
#include <Core/Dimensions.h>
#include <Core/Handlers.h>
#include <Core/Runtime.h>
#include <CoverGraph/CoverGraph.h>
#include <Frontend/Parser/ParserPTNet.h>
#include <Frontend/SymbolTable/SymbolTable.h>
#include <InputOutput/CompressedIO.h>
#include <InputOutput/InputOutput.h>
#include <InputOutput/JSON.h>
#include <Planning/Task.h>
#include <Witness/Path.h>

/*!
\brief symbol tables for the parsed net
\ingroup g_globals
 */
ParserPTNet *symbolTables = NULL;

/*!
\brief symbol table for a given Büchi automaton
\ingroup g_globals
 */
SymbolTable *buechiStateTable = NULL;

/*!
\brief The body of the thread that issues status messages during task execution
 */

void * report_status(void* arg)
{
    int last_state,last_type;
    pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS,&last_type);
    pthread_setcancelstate(PTHREAD_CANCEL_ENABLE,&last_state);
    Task * t = reinterpret_cast<Task *> (arg);
    unsigned int intervals = 0;
    while (true) // do until cancelled by main thread
    {
        sleep(REPORT_FREQUENCY);
        unsigned int time_elapsed = intervals++ * REPORT_FREQUENCY;
        char * stat = t -> getStatus(time_elapsed);
        if (stat)
        {
            RT::rep->status(stat); // task knows what to report
        }
        char * c = t -> early_abortion(); // task knows whether to abort early
        if (c) // NULL = no abortion, !=NULL = the message describing
            // the reason for abortion
        {
            RT::rep->status(RT::rep->markup(MARKUP_IMPORTANT, c).str());
            kill(getpid(), SIGUSR2);
        }
    }
    return NULL;
}

/*!
\brief the main workflow of LoLA
 */
int main(int argc, char **argv)
{
    //=================
    // (1) set up LoLA
    //=================

    // initialize the runtime
    RT::initialize(argc, argv);


    //===================
    // (2) process input
    //===================

    // file input

    RT::rep->status("NET");
    RT::rep->indent(2);

    if (RT::args.compressed_given)
    {
        // If net is given in compressed form, sara cannot be used to check reachability
        if (RT::args.stateequation_arg == stateequation_arg_par || 
                RT::args.stateequation_arg == stateequation_arg_seq)
        {
            RT::rep->status("net input must be given as file  in uncompressed format"
                "to use the state equation option");
            RT::args.stateequation_arg = stateequation_arg_off;
        }
            
        // read compressed net input files
        RT::rep->status("input: compressed net (%s)",
                RT::rep->markup(MARKUP_PARAMETER, "--compressed").str());

        // read from stdin
        if (RT::args.inputs_num == 0)
        {
            Input in("compressed net");
            ReadNetFile(in);
            RT::rep->status("no name file given");
        }
        if (RT::args.inputs_num == 1)
        {
            Input in("compressed net", RT::args.inputs[0]);
            ReadNetFile(in);
            RT::rep->status("no name file given");
        }
        if (RT::args.inputs_num == 2)
        {
            Input currentInputFile("compressed net", RT::args.inputs[0]);
            ReadNetFile(currentInputFile);
            Input namefile("names", RT::args.inputs[1]);
            symbolTables = new ParserPTNet();
            // initializes a symbol table that is rudimentary filled such that mapping name->id is possible
            ReadNameFile(namefile, symbolTables);
        }
        if (RT::args.inputs_num > 2)
        {

            RT::rep->message("too many files given - expecting at most two");
            RT::rep->abort(ERROR_COMMANDLINE);
        }
    }
    else
    {
        // handle input
        if (RT::args.inputs_num == 0)
        {
            // read from stdin
            RT::currentInputFile = new Input("net");
            // If no net file is given, sara cannot be used to check reachability
            if (RT::args.stateequation_arg == stateequation_arg_par || 
                    RT::args.stateequation_arg == stateequation_arg_seq)
            {
                RT::rep->status("net input must be given as file to use "
                    "the state equation option");
                RT::args.stateequation_arg = stateequation_arg_off;
            }
        }
        else if (RT::args.inputs_num == 1)
        {
            RT::currentInputFile = new Input("net", RT::args.inputs[0]);
        }
        else
        {
            RT::rep->message("too many files given - expecting at most one");
            RT::rep->abort(ERROR_COMMANDLINE);
        }
        RT::data["net"]["filename"] = RT::currentInputFile->getFilename();

        // pass the opened file pointer to flex via FILE *yyin
        extern FILE *ptnetlola_in;
        ptnetlola_in = *RT::currentInputFile;

        // read the input file(s)
        extern ParserPTNet * ParserPTNetLoLA();
        symbolTables = ParserPTNetLoLA();

        RT::rep->status("finished parsing");

        // close net file
        delete RT::currentInputFile;
        RT::currentInputFile = NULL;
        
        // tidy parser
        extern int ptnetlola_lex_destroy();
        ptnetlola_lex_destroy();

        // translate into general net structures
        symbolTables->symboltable2net();
    }

    // report hash table usage
    if (symbolTables)
    {
        RT::rep->status("%d/%d symbol table entries, %d collisions",
                symbolTables->PlaceTable->getCard() + symbolTables->TransitionTable->getCard(),
                SIZEOF_SYMBOLTABLE, SymbolTable::collisions);
    }

    // net is read completely now, time to preprocess
    RT::rep->status("preprocessing... ");
    Net::preprocess();

    // create a task object to process (we need to do this before we delete the
    // symbolTables, because the Büchi parser uses them)

    RT::rep->indent(-2);
    RT::rep->status("TASK");
    RT::rep->indent(2);
    Task * task = Task::buildTask();

    RT::rep->indent(-2);
    delete symbolTables;


    //===========================
    // (3) miscellaneous options
    //===========================

    if (RT::args.printNet_given)
    {
        RT::rep->status("debug function: print net (%s)",
                RT::rep->markup(MARKUP_PARAMETER, "--printNet").str());
        Net::print();
    }

    if (RT::args.writeCompressed_given)
    {
        RT::rep->status("output: compressed net (%s)",
                RT::rep->markup(MARKUP_PARAMETER, "--writeCompressed").str());

        Output netfile("compressed net",
                std::string(RT::args.writeCompressed_arg) + ".net");
        WriteNetFile(netfile);

        Output namefile("compressed net names",
                std::string(RT::args.writeCompressed_arg) + ".names");
        WriteNameFile(namefile);
    }


    //=====================
    // (4) excecute checks
    //=====================

    //======================
    // (5a) start reporter for status messages
    //======================

    pthread_t reporter; // status messages
    const int ret = pthread_create(&reporter, NULL, report_status, reinterpret_cast<void*> (task));
    if (UNLIKELY(ret != 0))
    {
        // LCOV_EXCL_START	
        RT::rep->status("thread could not be created");
        RT::rep->abort(ERROR_THREADING);
        // LCOV_EXCL_STOP
    }

    //======================
    // (5b) the actual check
    //======================

    RT::rep->status("RUNNING");
    RT::rep->indent(2);    
    if (RT::needLocalTimeLimit == true)
    {
        // replace time measuring thread
        // pthread_cancel(RT::reporter_thread);
        pthread_t local_reporter_thread;
        if (RT::args.localtimelimit_given)
        {
            RT::rep->status("subprocess will run for %d seconds at most (%s)",
                    RT::args.localtimelimit_arg, RT::rep->markup(MARKUP_PARAMETER, "--localtimelimit").str());
            const int ret = pthread_create(&local_reporter_thread, NULL, RT::local_reporter_internal, NULL);
            // LCOV_EXCL_START
            if (UNLIKELY(ret != 0))
            {
                RT::rep->status("thread could not be created");
                RT::rep->abort(ERROR_THREADING);
            }
            // LCOV_EXCL_STOP
        }
    }    
    const ternary_t result = task->getResult(); // run the check

    pthread_cancel(reporter); // stop reporter thread
    RT::rep->indent(-2);
    RT::rep->status("RESULT");
    RT::rep->indent(2);

    task->interpreteResult(result); //make sense of returned result
    RT::interim_result = "";

    task -> getStatistics();

    //============
    // (6) output
    //============

    for (unsigned int i = 0; i < RT::args.jsoninclude_given; ++i)
    {
        if (RT::args.jsoninclude_arg[i] == jsoninclude_arg_state)
        {
            RT::data["state"] = JSON();
            const capacity_t *current = task->getMarking();
            if (current)
            {
                for (arrayindex_t p = 0; p < Net::Card[PL]; ++p)
                {
                    if (current[p] == OMEGA)
                    {
                        RT::data["state"][Net::Name[PL][p]] = "oo";
                    }
                    else if (current[p] > 0)
                    {
                        RT::data["state"][Net::Name[PL][p]] = static_cast<int> (current[p]);
                    }
                }
            }
            break;
        }
    }
    // print witness state
    if (RT::args.state_given)
    {
        RT::rep->status("print witness state (%s)",
                RT::rep->markup(MARKUP_PARAMETER, "--state").str());
        Output o("witness state", RT::args.state_arg);

        const capacity_t *current = task->getMarking();
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
    // add data to JSON if required
    for (unsigned int i = 0; i < RT::args.jsoninclude_given; ++i)
    {
        if (RT::args.jsoninclude_arg[i] == jsoninclude_arg_path)
        {
            const Path p = task->getWitnessPath();
            if (p.initialized)
            {
                RT::data["path"] = p.json();
            }
            else
            {
                RT::data["path"] = JSON();
            }
            break;

        }
    }

    // print witness path
    if (RT::args.path_given)
    {
        const Path p = task->getWitnessPath();
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

    //================
    // (7) statistics
    //================


    // print statistics
    if (RT::args.stats_flag)
    {
        RT::rep->status("print statistics (%s)",
                RT::rep->markup(MARKUP_PARAMETER, "--stats").str());
        Handlers::statistics();
    }

    delete task;

     Handlers::exitHandler();
    _exit(EXIT_NORMAL);
}
