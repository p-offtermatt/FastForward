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
#include <Siphon/lola2minisat.h>


/*!
\brief the verification task

This class wraps the investigation of the siphon/trap property
*/

class SiphonTrapTask : public Task
{
public:
     siphon_result_t result;
     bool finished;
     bool printFinalResult;
    ~SiphonTrapTask(){ 
    }

    /// run the actual verification algorithm
    ternary_t getResult()
    {
        finished = false;
        result = lola2minisat();
        finished = true;
        // display information about the siphon/trap property and set the returned result value
        switch (result)
        {
            case SIPHON_PROPERTY_TRUE: RT::rep->status("The siphon/trap property holds");
                return TERNARY_FALSE;
            case SIPHON_PROPERTY_FALSE: RT::rep->status("The siphon/trap property does not hold");
                return TERNARY_UNKNOWN;
            case SIPHON_INHOMOGENIOUS: RT::rep->status("The siphon/trap property is not applicable "
                        "since the net is inhomogeneous");
                return TERNARY_UNKNOWN;
            case SIPHON_INCONCLUSIVE: RT::rep->status("The siphon/trap property is inconclusive "
                        "since the generated formula is too short");
                return TERNARY_UNKNOWN;
            case SIPHON_INDETERMINATE: RT::rep->status("The siphon/trap property cannot be "
                        "evaluated since the SAT solver cannot handle the generated formula");
                return TERNARY_UNKNOWN;
        }
    }

    /// interprete and display the result
    void interpreteResult(ternary_t r)
    {
        if (result == SIPHON_PROPERTY_TRUE)
        {
            RT::rep->status("result: %s", RT::rep->markup(MARKUP_BAD, "no").str());
            RT::data["analysis"]["result"] = false;
            RT::rep->status("%s", RT::rep->markup(MARKUP_BAD,
                    "The net does not have deadlocks.").str());
        }
        else
        {
            RT::rep->status("result: %s", RT::rep->markup(MARKUP_WARNING, "unknown").str());
            RT::data["analysis"]["result"] = JSON::null;
            RT::rep->status("%s", RT::rep->markup(MARKUP_WARNING,
                    "The net may or may not have deadlocks.").str());
        }
    }

    /// return the witness path
    Path getWitnessPath() {Path * p = new Path(); return *p;}
    /// return the target marking
    capacity_t *getMarking() {return NULL;}

    /// return the number of stored markings
    void getStatistics() {} // \TODO: add stat on minisat
    static Task * buildTask() {return new SiphonTrapTask();}
    char *  getStatus(uint64_t){ char * result = new char[STATUSLENGTH]; 
    if(finished)
    {
	sprintf(result,"STP completed");
    }
    else
    {
	sprintf(result,"STP running");
    }
    return result;}
    char * early_abortion(){return NULL;}
};
