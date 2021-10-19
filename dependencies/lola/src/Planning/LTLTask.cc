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
#include <Planning/StoreCreator.h>
#include <Exploration/StatePredicateProperty.h>
#include <Exploration/Firelist.h>
#include <Exploration/LTLExploration.h>
#include <Exploration/SimpleProperty.h>
#include <Formula/LTL/BuechiFromLTL.h>
#include <Formula/StatePredicate/TruePredicate.h>
#include <Formula/StatePredicate/AtomicStatePredicate.h>
#include <Formula/StatePredicate/ConjunctionStatePredicate.h>
#include <Formula/StatePredicate/DisjunctionStatePredicate.h>
#include <Frontend/Parser/ast-system-k.h>
#include <Frontend/Parser/ast-system-rk.h>
#include <Frontend/Parser/ast-system-unpk.h>
#include <Planning/LTLTask.h>
#include <Stores/Store.h>
#include <Witness/Path.h>
#include <Frontend/SymbolTable/SymbolTable.h>
#include <Exploration/FirelistStubbornLTL.h>

extern int ptbuechi_parse();

extern int ptbuechi_lex_destroy();

// input files
extern FILE *ptformula_in;
extern FILE *ptbuechi_in;

// code to parse from a string
struct yy_buffer_state;
typedef yy_buffer_state *YY_BUFFER_STATE;
extern YY_BUFFER_STATE ptbuechi__scan_string(const char *yy_str);

extern SymbolTable *buechiStateTable;
extern FILE *tl_out;

// Kimwitu++ objects
extern kc::tFormula TheFormula;
extern kc::tBuechiAutomata TheBuechi;


/*!
\ingroup g_globals
\todo Is this mapping actually needed or was it this just added for debugging
purposes.
 */
std::map<int, AtomicStatePredicate *> predicateMap;

/* prints the content of a set for spin */
StatePredicate *buildPropertyFromList(int *pos, int *neg)
{
    std::vector<StatePredicate *> subForms;

    // bad hack from library
    int mod = 8 * SIZEOF_INT;

    for (int i = 0; i < sym_size; i++)
    {
        for (int j = 0; j < mod; j++)
        {
            if (pos[i] & (1 << j))
            {
                // the compiler doesn't have a clue which function i mean, so tell him
                if (atoi(sym_table[mod * i + j]) > 1)
                {
                    subForms.push_back(
                            predicateMap[atoi(sym_table[mod * i + j])]->StatePredicate::copy());
                }
            }
            if (neg[i] & (1 << j))
            {
                if (atoi(sym_table[mod * i + j]) > 1)
                {
                    subForms.push_back(predicateMap[atoi(sym_table[mod * i + j])]->negate());
                }
            }
        }
    }

    if (subForms.empty())
    {
        return new TruePredicate();
    }

    ConjunctionStatePredicate *result = new ConjunctionStatePredicate(subForms.size());
    for (arrayindex_t i = 0; i < subForms.size(); i++)
    {
        result->addSub(i, subForms[i]);
    }
    return result;
}

LTLTask::LTLTask()
{
    // process formula
    previousNrOfMarkings = 0;

    if (RT::args.formula_given)
    {
        RT::rep->message("transforming LTL-Formula into a Büchi-Automaton");

	// remove FIREABLE predicates and do subsequent rewriting
	TheFormula = TheFormula -> rewrite(kc::goodbye_fireable);
	TheFormula = TheFormula -> rewrite(kc::sides);
	TheFormula = TheFormula -> rewrite(kc::productlists);
	TheFormula = TheFormula -> rewrite(kc::leq);
	TheFormula = TheFormula -> rewrite(kc::tautology);
	unparsed.clear();
	TheFormula ->unparse(myprinter,kc::countdeadlock);
	if(TheFormula -> containsDeadlock)
	{
		RT::rep->message("DEADLOCK atomic propositions are not supported in LTL model checking");
		RT::rep->abort(ERROR_COMMANDLINE);
	}
        ns = NetState::createNetStateFromInitial();
        // prepare counting of place in the formula
        extern bool *place_in_formula;
        extern unsigned int places_mentioned;
        extern unsigned int unique_places_mentioned;
        place_in_formula = new bool[Net::Card[PL]]();
        places_mentioned = 0;
        unique_places_mentioned = 0;

        // extract the Node*
        TheFormula->unparse(myprinter, kc::ltl);
 //Task::outputFormulaAsProcessed();

        tl_Node *n = TheFormula->ltl_tree;
        //n = bin_simpler(n);
        assert(n);
        tl_out = stdout;
        trans(n);
        // build the buechi-automation structure needed for LTL model checking
        // put the state predicates
        bauto = new BuechiAutomata();

        // extract the states from the ltl2ba data structures
        if (bstates->nxt == bstates)
        {
            // TODO the search result is FALSE!
            RT::rep->message("Not yet implemented, result FALSE");
            RT::rep->abort(ERROR_COMMANDLINE);
        }

        if (bstates->nxt->nxt == bstates && bstates->nxt->id == 0)
        {
            // TODO the search result is TRUE!
            RT::rep->message("Not yet implemented, result TRUE");
            RT::rep->abort(ERROR_COMMANDLINE);
        }

        bauto->cardStates = 0;
        // map-> final,id
        std::map<int, std::map<int, int > > state_id;
        BState *s;
        BTrans *t;
        for (s = bstates->prv; s != bstates; s = s->prv)
        {
            state_id[s->final][s->id] = bauto->cardStates;
            bauto->cardStates++;
        }

        //RT::rep->message("Buechi-automaton has %d states", bauto->cardStates);
        // now i do know the number of states
        bauto->cardTransitions = new uint32_t[bauto->cardStates]();
        bauto->transitions = new uint32_t **[bauto->cardStates]();
        bauto->cardEnabled = new arrayindex_t[bauto->cardStates]();
        bauto->isStateAccepting = new bool[bauto->cardStates]();

        std::vector<StatePredicate *> neededProperties;
        std::map<StatePredicate *, int> neededProperties_backmap;

        // read out the datastructure
        int curState = -1;
        int curProperty = 0;
        for (s = bstates->prv; s != bstates; s = s->prv)
        {
            curState++;
            if (s->id == 0)
            {
                // build a TRUE-loop
                bauto->isStateAccepting[curState] = true;
                bauto->cardTransitions[curState] = 1;
                bauto->transitions[curState] = new uint32_t *[1]();
                bauto->transitions[curState][0] = new uint32_t[2]();
                bauto->transitions[curState][0][0] = neededProperties.size();
                bauto->transitions[curState][0][1] = curState;
                curProperty++;
                neededProperties.push_back(new TruePredicate());
                neededProperties_backmap[neededProperties.back()] = curState;
                continue;
            }
            if (s->final == accepting_state)
            {
                bauto->isStateAccepting[curState] = true;
            }

            // build the successor list
            bauto->cardTransitions[curState] = 0;
            for (t = s->trans->nxt; t != s->trans; t = t->nxt)
            {
                // now build the property
                std::vector<StatePredicate *> disjunctionproperty;
                disjunctionproperty.push_back(buildPropertyFromList(t->pos, t->neg));
                BTrans *t1;
                for (t1 = t; t1->nxt != s->trans;)
                {
                    if (t1->nxt->to->id == t->to->id && t1->nxt->to->final == t->to->final)
                    {
                        disjunctionproperty.push_back(buildPropertyFromList(t1->nxt->pos,
                                t1->nxt->neg));
                        t1->nxt = t1->nxt->nxt;
                    } else
                    {
                        t1 = t1->nxt;
                    }
                }

                if (disjunctionproperty.size() == 1)
                {
                    neededProperties.push_back(disjunctionproperty[0]);
                } else
                {
                    DisjunctionStatePredicate *disjucntion = new DisjunctionStatePredicate(
                            disjunctionproperty.size());
                    for (size_t i = 0; i < disjunctionproperty.size(); i++)
                    {
                        disjucntion->addSub(i, disjunctionproperty[i]);
                    }
                    neededProperties.push_back(disjucntion);
                }
                //RT::rep->message("CREATE %d -> %d", neededProperties.size(), curState);
                neededProperties_backmap[neededProperties.back()] = curState;

                // increment number of transitions
                bauto->cardTransitions[curState]++;
            }

            bauto->transitions[curState] = new uint32_t *[bauto->cardTransitions[curState]]();
            int current_on_trans = -1;
            for (t = s->trans->nxt; t != s->trans; t = t->nxt)
            {
                // bauto data structures
                current_on_trans++;
                bauto->transitions[curState][current_on_trans] = new uint32_t[2]();
                //RT::rep->message("Transition %d -> %d", curState, state_id[t->to->final][t->to->id]);
                bauto->transitions[curState][current_on_trans][0] = curProperty++;
                bauto->transitions[curState][current_on_trans][1] =
                        state_id[t->to->final][t->to->id];
                //RT::rep->message("FROM TO %d %d", curState, state_id[t->to->final][t->to->id]);
            }
        }

        //
        // build a list of all needed propositions
        //

        // if the automata contains an all-accepting state
        bauto->cardAtomicPropositions = neededProperties.size();
        bauto->atomicPropositions = new StatePredicateProperty *[bauto->cardAtomicPropositions]();
        bauto->atomicPropotions_backlist = new arrayindex_t[bauto->cardAtomicPropositions]();
        for (size_t i = 0; i < neededProperties.size(); i++)
        {
            bauto->atomicPropositions[i] = new StatePredicateProperty(neededProperties[i]);
            //RT::rep->message("BL %d %d", i, neededProperties_backmap[neededProperties[i]]);
            bauto->atomicPropotions_backlist[i] =
                    neededProperties_backmap[neededProperties[i]];
        }

        RT::rep->status("the resulting Büchi automaton has %d states", bauto->getNumberOfStates());
        if (RT::args.writeBuechi_given)
        {
            RT::rep->status("output: Buechi automaton (%s)",
                    RT::rep->markup(MARKUP_PARAMETER, "--writeBuechi").str());
            bauto->writeBuechi();
        }

    }
    if (RT::args.buechi_given)
    {
        {
            RT::currentInputFile = NULL;
            buechiStateTable = new SymbolTable();

            // Check if the paramter of --buechi is a file that we can open: if that
            // works, parse the file. If not, parse the string.
            FILE *file;
            if ((file = fopen(RT::args.buechi_arg, "r")) == NULL and errno == ENOENT)
            {
                // reset error
                errno = 0;
                ptbuechi__scan_string(RT::args.buechi_arg);
            } else
            {
                fclose(file);
                RT::currentInputFile = new Input("Buechi", RT::args.buechi_arg);
                ptbuechi_in = *RT::currentInputFile;
            }

            //RT::rep->message("Parsing Büchi-Automaton");
            // parse the formula
            ptbuechi_parse();

            //RT::rep->message("Finished Parsing");

            // restructure the formula: unfold complex constructs and handle negations and tautologies
            TheBuechi = TheBuechi->rewrite(kc::goodbye_doublearrows);
            TheBuechi = TheBuechi->rewrite(kc::goodbye_singlearrows);
            TheBuechi = TheBuechi->rewrite(kc::goodbye_xor);
            TheBuechi = TheBuechi->rewrite(kc::goodbye_fireable);
            TheBuechi = TheBuechi->rewrite(kc::goodbye_initial);

            // restructure the formula: again tautoglies and simplification
            TheBuechi = TheBuechi->rewrite(kc::sides);
            TheBuechi = TheBuechi->rewrite(kc::productlists);
            TheBuechi = TheBuechi->rewrite(kc::leq);
            TheBuechi = TheBuechi->rewrite(kc::tautology);

            // expand the transitions rules
            TheBuechi = TheBuechi->rewrite(kc::rbuechi);

            //RT::rep->message("parsed Buechi");
            //TheBuechi->unparse(myprinter, kc::out);

            //RT::rep->message("checking LTL");

            // prepare counting of place in the formula
            extern bool *place_in_formula;
            extern unsigned int places_mentioned;
            extern unsigned int unique_places_mentioned;
            place_in_formula = new bool[Net::Card[PL]]();
            places_mentioned = 0;
            unique_places_mentioned = 0;

            // copy restructured formula into internal data structures
            TheBuechi->unparse(myprinter, kc::buechi);
            bauto = TheBuechi->automata;
            // XXX: this _must_ work according to the kimwitu docu, but it does not, kimwitu produces memory leaks!
            //TODO: this makes buechi LTL checks segfaulting in some cases ( now leakes
            //memory (we have to take a closer look)
            //TheBuechi->free(true);
            //delete TheBuechi;
            delete buechiStateTable;

            //RT::rep->message("Processed Büchi-Automaton");

            // report places mentioned in the formula and clean up
            RT::rep->status("formula mentions %d of %d places; total mentions: %d",
                    unique_places_mentioned, Net::Card[PL], places_mentioned);
            delete[] place_in_formula;

            // tidy parser
            ptbuechi_lex_destroy();
            //delete TheFormula;

            if (RT::args.buechi_given)
            {
                delete RT::currentInputFile;
                RT::currentInputFile = NULL;
            }

            // reading the buechi automata
            assert(bauto);
        }
    }

    // prepare task
    RT::data["store"]["search"] = "depth_first_search";
    ltlStore = StoreCreator<AutomataTree *>::createStore(number_of_threads);

    // Check, if stubborn sets should be used
    // TODO use the following commented code when firelist for LTL is fully
    // implemented. Remove the subsequently if-check of ltlstubborn_arg and 
    // remove ltlstubborn_arg from cmdline.ggo 
    //    switch(RT::args.stubborn_arg)
    //    {
    //    case stubborn_arg_off:
    //        fl = new Firelist();
    //        break;
    //    default: 
    //        fl = new FirelistStubbornLTL();
    //    ; 
    //    }
    RT::rep->indent(-2);
    RT::rep->status("SEARCH");
    RT::rep->indent(2);
    if (RT::args.ltlstubborn_arg == ltlstubborn_arg_on)
    {
RT::rep->status("using ltl preserving stubborn set method (%s)", RT::rep->markup(MARKUP_PARAMETER, "--stubborn=ltlstubborn").str());
        RT::rep->status("%s",RT::rep->markup(MARKUP_WARNING,"not implemented yet").str());
        fl = new FirelistStubbornLTL(bauto);
    } else
    {
RT::rep->status("not using stubborn set method (%s)", RT::rep->markup(MARKUP_PARAMETER, "--stubborn=off").str());
        fl = new Firelist();
    }
    RT::data["analysis"]["type"] = "modelchecking";
    ltlExploration = new LTLExploration(RT::args.ltlmode_arg == ltlmode_arg_tree);
}

/*!
\post memory for all members is deallocated
 */
LTLTask::~LTLTask()
{
    // quick exit to avoid lengthy destructor calls
#ifndef USE_PERFORMANCE
    //delete ns;
    //delete ltlStore;
    //delete fl;
    //delete ltlExploration;
    //delete bauto;
#endif
}

/*!
This method starts the actual state space exploration and returns the raw
result.

\return the result of the state exploration
\note This result needs to be interpreted by Task::interpreteResult.
 */
ternary_t LTLTask::getResult()
{
    //TODO can we make these assumptions clearer that the asserts are creating
    assert(ns);
    assert(ltlStore);
    assert(bauto);
    assert(ltlExploration);
    assert(fl);

    bool bool_result(false);
    ternary_t result(TERNARY_FALSE);
    bool_result = ltlExploration->checkProperty(*bauto, *ltlStore, *fl, *NetState::createNetStateFromInitial());

    // temporary result transfer, as long as the variable bool_result is needed
    if (bool_result)
    {
        result = TERNARY_TRUE;
    }
    switch(result)
    {
	case TERNARY_TRUE: result = TERNARY_FALSE; break;
	case TERNARY_FALSE: result = TERNARY_TRUE; break;
	case TERNARY_UNKNOWN: result = TERNARY_UNKNOWN; break;
	default: break;
    }

    return result;
}

/*!
\post The result is interpreted and printed using Reporter::message
\warning This method must not be called more than once.

\todo This method should be internal and automatically be called by
Task::getResult after the result was calculated. Then, a member of type
trinary_t can be displayed.
 */
void LTLTask::interpreteResult(ternary_t result)
{
    switch (result)
    {
        case TERNARY_TRUE:
            RT::rep->status("result: %s", RT::rep->markup(MARKUP_GOOD, "yes").str());
            RT::data["analysis"]["result"] = true;
            RT::rep->status("%s", RT::rep->markup(MARKUP_GOOD, "The net satisfies the given formula (language of the product automaton is empty).").str());
            break;

        case TERNARY_FALSE:
            RT::rep->status("result: %s", RT::rep->markup(MARKUP_BAD, "no").str());
            RT::data["analysis"]["result"] = false;

            RT::rep->status("%s", RT::rep->markup(MARKUP_BAD,
                    "The net does not satisfy the given formula (language of the product automaton is nonempty).").str());
            break;

        case TERNARY_UNKNOWN:
            RT::rep->status("result: %s", RT::rep->markup(MARKUP_WARNING, "unknown").str());
            RT::data["analysis"]["result"] = JSON::null;

            RT::rep->status("%s", RT::rep->markup(MARKUP_WARNING,
                    "The net may or may not satisfy the given formula.").str());
            break;
    }
}

Path LTLTask::getWitnessPath()
{
    return ltlExploration->path();
}

capacity_t *LTLTask::getMarking()
{
    return NULL;
}

void LTLTask::getStatistics()
{
    uint64_t markings = 0;
    markings = ltlStore->get_number_of_markings();
    RT::data["analysis"]["stats"]["states"] = static_cast<int> (markings);
    uint64_t edges = 0;
    edges = ltlStore->get_number_of_calls();
    RT::data["analysis"]["stats"]["edges"] = static_cast<int> (edges);

    RT::rep->status("%llu markings, %llu edges", markings, edges);

}

char * LTLTask::getStatus(uint64_t elapsed)
{
    char * result = new char[STATUSLENGTH];
    uint64_t m = ltlStore -> get_number_of_markings();
    sprintf(result, "%10llu markings, %10llu edges, %8.0f markings/sec, %5llu secs", m, ltlStore->get_number_of_calls(), ((m - previousNrOfMarkings) / (float) REPORT_FREQUENCY), elapsed);
    previousNrOfMarkings = m;
    return result;

}
