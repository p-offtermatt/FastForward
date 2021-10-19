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
#include <Frontend/Parser/ast-system-k.h>
#include <Frontend/Parser/ast-system-rk.h>
#include <Frontend/Parser/ast-system-unpk.h>
#include <InputOutput/InputOutput.h>
#include <Net/NetState.h>
#include <Planning/Task.h>
#include <Planning/FullTask.h>
#include <Planning/BooleanTask.h>
#include <Planning/ComputeBoundTask.h>
#include <Planning/CompoundTask.h>
#include <Planning/LTLTask.h>
#include <Planning/CTLTask.h>
#include <Planning/DeadlockTask.h>
#include <Planning/NoDeadlockTask.h>
#include <Planning/AGEFAGTask.h>
#include <Planning/EFAGTask.h>
#include <Planning/ReachabilityTask.h>
#include <Planning/InvariantTask.h>
#include <Planning/InitialTask.h>
#include <Planning/EmptyTask.h>
#include <Witness/Path.h>
#include <Stores/NetStateEncoder/BitEncoder.h>
#include <Stores/NetStateEncoder/CopyEncoder.h>
#include <Stores/NetStateEncoder/FullCopyEncoder.h>
#include <Stores/NetStateEncoder/SimpleCompressedEncoder.h>
#include <Frontend/SymbolTable/SymbolTable.h>

// the parsers
extern int ptformula_parse();
extern int ptformula_lex_destroy();
extern int ptbuechi_parse();
extern int ptbuechi_lex_destroy();

// input files
extern FILE *ptformula_in;
extern FILE *ptbuechi_in;

// code to parse from a string
struct yy_buffer_state;
typedef yy_buffer_state *YY_BUFFER_STATE;
extern YY_BUFFER_STATE ptformula__scan_string(const char *yy_str);
extern YY_BUFFER_STATE ptbuechi__scan_string(const char *yy_str);

extern SymbolTable *buechiStateTable;
extern FILE *tl_out;

threadid_t Task::number_of_threads;

// Kimwitu++ objects
extern kc::tFormula TheFormula;
extern kc::tBuechiAutomata TheBuechi;

/// process formula and check cmdline parameters
/// to dispatch to the right task object.

Task * Task::buildTask() 
{
    Task * result = NULL;
    // prepare counting of place in the formula
    extern bool *place_in_formula;
    extern unsigned int places_mentioned;
    extern unsigned int unique_places_mentioned;
    place_in_formula = new bool[Net::Card[PL]]();
    places_mentioned = 0;
    unique_places_mentioned = 0;

    // feed json "limits" part

    RT::data["store"]["bucketing"] = static_cast<int>(RT::args.bucketing_arg);
    RT::data["store"]["threads"] = static_cast<int>(RT::args.threads_arg);

    if (RT::args.timelimit_arg != 0)
    {
        RT::data["limits"]["time"] = static_cast<int>(RT::args.timelimit_arg);
    }
    else
    {
        RT::data["limits"]["time"] = JSON::null;
    }

    if (RT::args.markinglimit_arg != 0)
    {
        RT::data["limits"]["markings"] = static_cast<int>(RT::args.markinglimit_arg);
    }
    else
    {
        RT::data["limits"]["markings"] = JSON::null;
    }


    
    number_of_threads = static_cast<threadid_t>(RT::args.threads_arg);

    if (RT::args.check_arg == check_arg_none || RT::args.check_arg == check__NULL)
    {
	RT::rep->status("checking nothing (%s)",
	RT::rep->markup(MARKUP_PARAMETER, "--check=none").str());
        RT::data["analysis"]["type"] = "none";
	return EmptyTask::buildTask();
    }
    if (RT::args.check_arg == check_arg_full)
    {
	RT::rep->status("creating the whole state space (%s)",
	RT::rep->markup(MARKUP_PARAMETER, "--check=full").str());
        RT::data["analysis"]["type"] = "full";
	return FullTask::buildTask();
    }

    if (RT::args.check_arg == check_arg_modelchecking)
    {
	// model checking requires a property whch must be given either
	// as formula or as buchi automaton
        if ((not RT::args.formula_given) and (not RT::args.buechi_given))
        {
            RT::rep->message("%s given without %s or %s",
	     RT::rep->markup(MARKUP_PARAMETER, "--check=modelchecking").str(),
	     RT::rep->markup(MARKUP_PARAMETER, "--formula").str(),
	     RT::rep->markup(MARKUP_PARAMETER, "--buechi").str());
            RT::rep->abort(ERROR_COMMANDLINE);
        }
        if (RT::args.formula_given and RT::args.buechi_given)
        {
            RT::rep->message("both %s and %s given",
	     RT::rep->markup(MARKUP_PARAMETER, "--formula").str(),
	     RT::rep->markup(MARKUP_PARAMETER, "--buechi").str());
            RT::rep->abort(ERROR_COMMANDLINE);
        }
    }

    // process the formula according to the type
    if (RT::args.buechi_given)
    {
	RT::rep->status("checking LTL");
        RT::data["analysis"]["formula"]["type"] = "LTL";
	return LTLTask::buildTask();
    }

    // process formula

    if (RT::args.formula_given)
    {
        setFormula();
    }
    switch (TheFormula->type)
    {
    case FORMULA_COMPOUND:
	RT::rep->status("computing a collection of formulas");
        RT::data["analysis"]["formula"]["type"] = "bound";
        result = CompoundTask::buildTask(); 
	break;
    case FORMULA_BOUND:
	RT::rep->status("computing the bound for an expression");
        RT::data["analysis"]["formula"]["type"] = "bound";
        result = ComputeBoundTask::buildTask(); 
	break;
    case FORMULA_DEADLOCK:
	RT::rep->status("checking reachability of deadlocks");
        RT::data["analysis"]["formula"]["type"] = "deadlock";
        result = DeadlockTask::buildTask(); 
	break;
    case FORMULA_NODEADLOCK:
	RT::rep->status("checking absence of deadlocks");
        RT::data["analysis"]["formula"]["type"] = "nodeadlock";
        result = NoDeadlockTask::buildTask(); 
	break;
    case FORMULA_REACHABLE:
	RT::rep->status("checking reachability");
        RT::data["analysis"]["formula"]["type"] = "reachability";
	result = ReachabilityTask::buildTask(); 
	break;
    case FORMULA_INVARIANT:
	RT::rep->status("checking invariance");
        RT::data["analysis"]["formula"]["type"] = "invariance";
	result = InvariantTask::buildTask(); 
	break;
    case FORMULA_LIVENESS:
	RT::rep->status("checking liveness");
        RT::data["analysis"]["formula"]["type"] = "liveness";
	//result = AGEFTask::buildTask(); 
        RT::rep->status("liveness not yet implemented, converting to CTL...");
	result = CTLTask::buildTask(); 
	break;
    case FORMULA_EGAGEF:
	RT::rep->status("checking possible liveness");
        RT::data["analysis"]["formula"]["type"] = "possible_liveness";
	//result = EFAGEFTask::buildTask(); 
        RT::rep->status("possible liveness not yet implemented, converting to CTL...");
	result = CTLTask::buildTask(); 
	break;
    case FORMULA_EFAG:
	RT::rep->status("checking possible invariance");
        RT::data["analysis"]["formula"]["type"] = "possible_invariance";
	//result = EFAGTask::buildTask(); 
        RT::rep->status("possible invariance not yet implemented, converting to CTL...");
	result = CTLTask::buildTask(); 
	break;
    case FORMULA_AGEFAG:
	RT::rep->status("checking globally possible invariance");
        RT::data["analysis"]["formula"]["type"] = "globally_possible_invariance";
	//result = AGEFAGTask::buildTask(); 
        RT::rep->status("globally possible invariance not yet implemented, converting to CTL...");
	result = CTLTask::buildTask(); 
	break;
    case FORMULA_FAIRNESS:
        RT::rep->status("checking fairness");
        RT::data["analysis"]["formula"]["type"] = "fairness";
        RT::rep->status("fairness not yet implemented, converting to LTL...");
	result = LTLTask::buildTask(); 
	break;
    case FORMULA_STABILIZATION:
        RT::rep->status("checking stabilization");
        RT::data["analysis"]["formula"]["type"] = "stabilization";
        RT::rep->status("stabilization not yet implemented, converting to LTL...");
	result = LTLTask::buildTask(); 
	break;
    case FORMULA_EVENTUALLY:
        RT::rep->status("checking eventual occurrence");
        RT::data["analysis"]["formula"]["type"] = "eventual occurrence";
        RT::rep->status("eventual occurrence not yet implemented, converting to LTL...");
	result = LTLTask::buildTask(); 
	break;
    case FORMULA_INITIAL:
	RT::rep->status("checking initial satisfaction");
        RT::data["analysis"]["formula"]["type"] = "initial satisfaction";
	result = InitialTask::buildTask(); 
	break;
    case FORMULA_LTL:
	RT::rep->status("checking LTL");
        RT::data["analysis"]["formula"]["type"] = "LTL";
	result = LTLTask::buildTask();
	break;
    case FORMULA_CTL:
	RT::rep->status("checking CTL");
        RT::data["analysis"]["formula"]["type"] = "CTL";
        result = CTLTask::buildTask(); 
	break;
    case FORMULA_BOOLEAN:
	RT::rep->status("checking a Boolean combination of subproblems");
        RT::data["analysis"]["formula"]["type"] = "Boolean";
        result = BooleanTask::buildTask(); 
	break;
    case FORMULA_MODELCHECKING:
        RT::rep->status("checking CTL*");
        RT::data["analysis"]["formula"]["type"] = "CTL*";
        RT::rep->message("check not yet implemented");
        RT::rep->abort(ERROR_COMMANDLINE);
    }
    // tidy parser
    ptformula_lex_destroy();

    return result;
}

void Task::setFormula()
{

    RT::currentInputFile = NULL;

    // Check if the paramter of --formula is a file that we can open: if that
    // works, parse the file. If not, parse the string.
    FILE *file = fopen(RT::args.formula_arg, "r");
    if (file == NULL and (errno == ENOENT or errno == ENAMETOOLONG))
    {
        // reset error
        errno = 0;
        ptformula__scan_string(RT::args.formula_arg);
    }
    else
    {
        fclose(file);
        RT::currentInputFile = new Input("formula", RT::args.formula_arg);
        ptformula_in = *RT::currentInputFile;
        // Save formula input file name for sara problem file name
        RT::inputFormulaFileName = RT::currentInputFile->getFilename();
    }

    // parse the formula
    ptformula_parse();

    // output formula as parsed
    unparsed.clear();
    TheFormula->unparse(stringprinter, kc::out);
    if (unparsed.size() < FORMULA_PRINT_SIZE)
    {
        RT::rep->status("read: %s", unparsed.c_str());
    }
    else
    {
        RT::rep->status("read: %s... (shortened)", unparsed.substr(0, FORMULA_PRINT_SIZE).c_str());
    }
    RT::rep->status("formula length: %d", unparsed.size());
    RT::data["analysis"]["formula"]["parsed"] = unparsed;
    RT::data["analysis"]["formula"]["parsed_size"] = static_cast<int>(unparsed.size());

    // restructure the formula:

    // Phase 1: remove syntactic sugar
    TheFormula = TheFormula->rewrite(kc::goodbye_doublearrows);
    TheFormula = TheFormula->rewrite(kc::goodbye_singlearrows);
    TheFormula = TheFormula->rewrite(kc::goodbye_xor);
    TheFormula = TheFormula->rewrite(kc::goodbye_initial);

    // Phase 2: Normalize atomic propositions
    // - places to left, constants to right
    TheFormula = TheFormula->rewrite(kc::sides);
    // - left side to lists
    TheFormula = TheFormula->rewrite(kc::productlists);
    // - remove == != >= < (<= and > remain)
    TheFormula = TheFormula->rewrite(kc::leq);

    // Phase 3: Apply logical tautologies.
    TheFormula = TheFormula->rewrite(kc::tautology);

    // Phase 3a: Remove empty path quantifiers (containsTemporal is set in kc::temporal)
    TheFormula->unparse(myprinter, kc::temporal);
    TheFormula = TheFormula->rewrite(kc::emptyquantifiers);

    // 4a: detect formula type
    TheFormula->unparse(myprinter, kc::temporal);
}


void Task::outputFormulaAsProcessed()
{
    extern bool *place_in_formula;
    extern unsigned int places_mentioned;
    extern unsigned int unique_places_mentioned;
    unparsed.clear();
    TheFormula->unparse(stringprinter, kc::out);
    if (unparsed.size() < FORMULA_PRINT_SIZE)
    {
        RT::rep->status("processed formula: %s", unparsed.c_str());
    }
    else
    {
        RT::rep->status("processed formula: %s... (shortened)", unparsed.substr(0,
                        FORMULA_PRINT_SIZE).c_str());
    }
    RT::rep->status("processed formula length: %d", unparsed.size());
    RT::data["analysis"]["formula"]["processed"] = unparsed;
    RT::data["analysis"]["formula"]["processed_size"] = static_cast<int>(unparsed.size());

    RT::rep->status("%d rewrites", rule_applications);
    RT::data["analysis"]["formula"]["rewrites"] = static_cast<int>(rule_applications);


	
//       RT::rep->status("processed formula with %d atomic propositions",
//                      (TheFormula->formula)->countAtomic());
//     RT::data["analysis"]["formula"]["atomic_propositions"] = static_cast<int>((TheFormula->formula)->countAtomic());

    // report places mentioned in the formula and clean up
    RT::rep->status("formula mentions %d of %d places; total mentions: %d",
                    unique_places_mentioned, Net::Card[PL], places_mentioned);
    delete[] place_in_formula;

    RT::data["analysis"]["formula"]["places_mentioned"] = static_cast<int>(places_mentioned);
    RT::data["analysis"]["formula"]["places_mentioned_unique"] = static_cast<int>
            (unique_places_mentioned);

    if (RT::args.formula_given)
    {
        delete RT::currentInputFile;
        RT::currentInputFile = NULL;
    }
}

