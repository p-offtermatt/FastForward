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

#include <Core/Runtime.h>
#include <Planning/Task.h>
#include <Planning/StateEquationTask.h>
#include <Frontend/Parser/ast-system-k.h>
#include <Frontend/Parser/ast-system-rk.h>
#include <Frontend/Parser/ast-system-unpk.h>
#include <Exploration/DFSExploration.h>

#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>

#include <unistd.h>
#include <sys/wait.h>

/////////////////////
// HELPER FUNCTION //
/////////////////////

#ifndef __cplusplus11
inline std::string int_to_string(int i) {
    std::stringstream s;
    s << i;
    return s.str();
}
#endif

extern kc::tFormula TheFormula;

bool saraIsRunning = false;

StateEquationTask::StateEquationTask()
{
}

StateEquationTask::~StateEquationTask()
{
}

ternary_t StateEquationTask::getResult()
{
    // reset 
    saraIsRunning = false;
    // extract state predicate from formula
    assert(TheFormula);
    // copy formula for additional dnf rewrite
    kc::tFormula TheFormulaDNF;
    TheFormulaDNF = reinterpret_cast<kc::tFormula> (TheFormula->copy(true));
    assert(TheFormulaDNF);

    TheFormulaDNF = TheFormulaDNF->rewrite(kc::singletemporal);
    TheFormulaDNF = TheFormulaDNF->rewrite(kc::goodbye_fireable);
    TheFormulaDNF = TheFormulaDNF->rewrite(kc::goodbye_unfireable);
    TheFormulaDNF = TheFormulaDNF->rewrite(kc::simpletautology);
    TheFormulaDNF = TheFormulaDNF->rewrite(kc::simpleneg);

    // get the assumed length of DNF formula and the assumed number of ORs
    unparsed.clear();
    TheFormulaDNF->unparse(stringprinter, kc::orsAndAndsAndLength);
    //    RT::rep->status("DNF length: %.0f" , TheFormulaDNF -> length);
    //    RT::rep->status("DNF ors: %.0f", TheFormulaDNF -> number_of_or_dnf);
    //    RT::rep->status("current ors: %.0f", TheFormulaDNF -> number_of_or);
    //    RT::rep->status("current ands: %.0f", TheFormulaDNF -> number_of_and);
    //    RT::rep->status("only_fireable: %d", TheFormulaDNF -> only_fireable);

    // check if DNF is to big \todo evalute the reference value
    RT::rep->status("Transformation into DNF: expect %.0f literals",TheFormulaDNF->length);
    if (TheFormulaDNF->length > 100000)
    {
        RT::rep->status("DNF formula is too big, sara cannot be used");
        return TERNARY_UNKNOWN;
    }
    TheFormulaDNF = TheFormulaDNF->rewrite(kc::dnf);
    TheFormulaDNF = TheFormulaDNF->rewrite(kc::booleanlists);

    // unparse the content of the problem file for sara
    unparsed.clear();
    TheFormulaDNF->unparse(stringprinter, kc::problemwriter);

    static std::string baseFileName = "";
    static std::string saraProblemFileName = "";
    static std::string saraResultFileName = "";

    std::string formulaNumber = "";
    // check if formula is a compound formula
    if (RT::compoundNumber > 1)
    {
        // compound formula, add formula counter to the file name
#ifdef __cplusplus11
        formulaNumber = "-" + std::to_string(RT::compoundNumber);
#else
        formulaNumber = "-" + int_to_string(RT::compoundNumber);
#endif
    }
    
    // check if the task was given by a file
    if (RT::inputFormulaFileName != "")
    {
        // formula file is given
        baseFileName = RT::inputFormulaFileName;
        // remove file extension if present
        const size_t period_idx = baseFileName.rfind('.');
        if (std::string::npos != period_idx)
        {
            baseFileName.erase(period_idx);
        }
        saraProblemFileName = baseFileName + formulaNumber + ".sara";
        saraResultFileName = baseFileName + formulaNumber + ".sararesult";
    }
    else
    {
        // No formula file is given
        saraProblemFileName = "stateEquationProblem" + formulaNumber + ".sara";
        saraResultFileName = "stateEquationProblem" + formulaNumber + ".sararesult";
    }

    // delete old problem and result file
    std::remove(saraProblemFileName.c_str());
    std::remove(saraResultFileName.c_str());
    // Create new problem file
    std::ofstream lolafile(saraProblemFileName.c_str(), std::ios::out);

    // check if file could be created
    if (!lolafile)
    {
        // Problem file could not be constructed
        RT::rep->status("input file construction for sara not succesful");
    }
    else
    {
        // Problem file was constructed
        RT::rep->status("write sara problem file to %s", 
                RT::rep->markup(MARKUP_FILE, saraProblemFileName.c_str()).str());
        // Write problem in file
        lolafile << unparsed.c_str() << std::endl;
        lolafile.close();
    }
    
    // result is as default unknown
    ternary_t result(TERNARY_UNKNOWN);
    
    // fork to get a PID for sara to kill her, if necessary
    pid_t pid_sara = fork();
    if (pid_sara < 0)
    {
        // fork failed
        RT::rep->status("cannot launch process for handling problem with sara");
        return TERNARY_UNKNOWN;
    }
    else if (pid_sara == 0)
    {
        // fork worked - child process
        // call sara
        RT::rep->status("calling and running sara");
        // set flag, that sara is running for the status message
        saraIsRunning = true;
        execlp("sara", "sara", "-i", saraProblemFileName.c_str(), "-v", "-o",
                saraResultFileName.c_str(), (char*) 0);
        // call sara failed
        saraIsRunning = false;
        RT::rep->status("call sara failed (perhaps sara is not installed)");
        // exit this process, otherwise we would have two processes
        exit(1);
    }
    else
    {
        // parent process after fork succeeded
        // set saraPID to kill sara if necessary
        RT::saraPID = pid_sara;
        // save sara's process status
        int status = 0;
        // wait until sara is finished
        waitpid(pid_sara, &status, 0);
        if (WIFEXITED(status) == true)
        {
            // sara finished - check sara's result
            if (system(("grep -q 'SOLUTION' " + saraResultFileName).c_str()) == 0)
            {
                // solution found - result is true
                result = TERNARY_TRUE;
                RT::rep->status("sara:solution produced");
            }
            else if (system(("grep -q 'INFEASIBLE' " + saraResultFileName).c_str()) == 0)
            {
                // solution is infeasible - result is false
                result = TERNARY_FALSE;
                RT::rep->status("sara:solution impossible");
            }
            else
            {
                // sara can't decide the problem - result is unknonw
                result = TERNARY_UNKNOWN;
                RT::rep->status("sara:solution unknown");
            }
        }
    }
    return result;
}

void StateEquationTask::interpreteResult(ternary_t result)
{
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
            RT::rep->status("%s", RT::rep->markup(MARKUP_BAD, "The predicate is unreachable.").str());
            break;

        case TERNARY_UNKNOWN:
            RT::rep->status("result: %s", RT::rep->markup(MARKUP_WARNING, "unknown").str());
            RT::data["analysis"]["result"] = JSON::null;
            RT::rep->status("%s", RT::rep->markup(MARKUP_WARNING,
                    "The predicate may or may not be reachable.").str());
            break;
    }
}

Path StateEquationTask::getWitnessPath()
{
    // \todo add witness path
    Path * p = new Path();

    return *p;
}

capacity_t * StateEquationTask::getMarking()
{
    return NULL;
}

void StateEquationTask::getStatistics()
{
}

char * StateEquationTask::getStatus(uint64_t elapsed)
{
    char * result;
    result = new char[STATUSLENGTH];
    if (saraIsRunning)
    {
        sprintf(result + strlen(result), " sara is running %5llu secs", elapsed);
    }
    else
    {
        sprintf(result + strlen(result), "sara is not running");
    }
    return result;
}

char * StateEquationTask::early_abortion()
{
    return NULL;
}

Task * StateEquationTask::buildTask()
{
    return new StateEquationTask();
}

