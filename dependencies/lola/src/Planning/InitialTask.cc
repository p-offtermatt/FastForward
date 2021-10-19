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
#include <Frontend/SymbolTable/SymbolTable.h>

#include <Core/Dimensions.h>
#include <Core/Handlers.h>
#include <Frontend/Parser/formula_abstract.h>
#include <Core/Runtime.h>
#include <Witness/Path.h>
#include <Planning/Task.h>
#include <Planning/InitialTask.h>
#include <Exploration/DeadlockExploration.h>

extern kc::tFormula TheFormula;


/*!
\brief the verification task

This class handles initial satisfiability. 
*/

InitialTask::InitialTask()
{
extern bool *place_in_formula;
extern int ptformula_lex_destroy();
extern unsigned int places_mentioned;
extern unsigned int unique_places_mentioned;
place_in_formula = new bool[Net::Card[PL]]();
    places_mentioned = 0;
    unique_places_mentioned = 0;
    if (RT::args.formula_given)
    {
	// copy restructured formula into internal data structures
	// extract state predicate from formula
        TheFormula = TheFormula->rewrite(kc::singletemporal);
        TheFormula = TheFormula->rewrite(kc::simpleneg);
        TheFormula = TheFormula->rewrite(kc::booleanlists);
    // prepare counting of place in the formula
    extern bool *place_in_formula;
    extern unsigned int places_mentioned;
    extern unsigned int unique_places_mentioned;
    place_in_formula = new bool[Net::Card[PL]]();
    places_mentioned = 0;
    unique_places_mentioned = 0;

        TheFormula->unparse(myprinter, kc::internal);
        StatePredicate *result = TheFormula->formula;
 //Task::outputFormulaAsProcessed();

        assert(result);
        RT::rep->status("processed formula with %d atomic propositions",
                       result->countAtomic());
        RT::data["analysis"]["formula"]["atomic_propositions"] = static_cast<int>(result->countAtomic());

        spFormula = result;
    }
    // report places mentioned in the formula and clean up
    //RT::rep->status("formula mentions %d of %d places; total mentions: %d",
    //                unique_places_mentioned, Net::Card[PL], places_mentioned);
    //delete[] place_in_formula;

    RT::data["analysis"]["formula"]["places_mentioned"] = static_cast<int>(places_mentioned);
    RT::data["analysis"]["formula"]["places_mentioned_unique"] = static_cast<int>
            (unique_places_mentioned);

    if (RT::args.formula_given)
    {
        delete RT::currentInputFile;
        RT::currentInputFile = NULL;
    }
    ns = NetState::createNetStateFromInitial();
    fl = new Firelist();
    if(spFormula)
    {
	p = new StatePredicateProperty(spFormula);
    }
    else
    {
	p = new DeadlockExploration();
    }

}

ternary_t InitialTask::getResult()
{
    bool bool_result(false);
    ternary_t result(TERNARY_FALSE);
    bool_result = p->initProperty(*ns);
    // temporary result transfer, as long as the variable bool_result is needed
    if (bool_result)
    {
        result = TERNARY_TRUE;
    }

    return result;
}
 
void InitialTask::interpreteResult(ternary_t result)
{
    switch (result)
    {
    case TERNARY_TRUE:
        RT::rep->status("result: %s", RT::rep->markup(MARKUP_GOOD, "yes").str());
        RT::data["analysis"]["result"] = true;

            RT::rep->status("%s", RT::rep->markup(MARKUP_GOOD, "The net satisfies the property already in its initial state.").str());

        break;

    case TERNARY_FALSE:
        RT::rep->status("result: %s", RT::rep->markup(MARKUP_BAD, "no").str());
        RT::data["analysis"]["result"] = false;

            RT::rep->status("%s", RT::rep->markup(MARKUP_BAD,
                                                  "The net violates the given property already in its initial state.").str());

        break;

    case TERNARY_UNKNOWN:
	assert(false);
        break;
    default:
            break; // cannot happen - only to silence compiler  LCOV_EXCL_LINE
        }

}
 
Path InitialTask::getWitnessPath()
{
	return p -> path();
}

capacity_t *InitialTask::getMarking()
{
    // we only provide witness states for simple properties where we found
    // a result
    if (p and p->value)
    {
        return ns->Current;
    }
    else
    {
        return NULL;
    }
}

void InitialTask::getStatistics()
{
	 RT::data["analysis"]["stats"]["states"] = 0;
         RT::data["analysis"]["stats"]["edges"] = 0;
         RT::rep->status("%u markings, %u edges", 0, 0);
}

Task * InitialTask::buildTask()
{
	return new InitialTask();
}

// it is unlikely that we run for +5secs
// LCOV_EXCL_START
char * InitialTask::getStatus(uint64_t elapsed)
{
	char * result = new char[STATUSLENGTH];
	sprintf(result,"processing ... %5llu secs",elapsed);
	return result;
}

// LCOV_EXCL_STOP
