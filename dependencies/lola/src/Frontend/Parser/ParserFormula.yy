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
\brief formula syntax
\author <unknown>
\status new
\ingroup g_frontend

Parses a formula in LoLA syntax.
*/

%{
#include <config.h>
#include <Core/Dimensions.h>
#include <CoverGraph/CoverGraph.h>
#include <Frontend/SymbolTable/ArcList.h>
#include <Frontend/SymbolTable/PlaceSymbol.h>
#include <Frontend/SymbolTable/TransitionSymbol.h>
#include <Frontend/SymbolTable/SymbolTable.h>
#include <Frontend/Parser/ParserPTNet.h>
#include <Frontend/Parser/error.h>
#include <Frontend/Parser/ast-system-k.h>
#include <Frontend/Parser/ast-system-yystype.h>
#include <Net/Net.h>

#include <limits.h>
#include <libgen.h>
#include <cstdarg>
#include <cstdio>
#include <string>
#include <set>


extern ParserPTNet* symbolTables;
%}

%error-verbose /* more verbose and specific error message string */
%defines       /* write an output file containing macro definitions for the token types */
%name-prefix="ptformula_"
%locations     /* we want to use token locations for better error messages */

%type <yt_tFormula> compoundformula
%type <yt_tFormula> formula
%type <yt_tFormula> computeboundformula
%type <yt_tStatePredicate> statepredicate
%type <yt_tAtomicProposition> atomic_proposition
%type <yt_tTerm> term
%type <yt_integer> NUMBER
%type <yt_casestring> IDENTIFIER _CTLOPERATOR_ identifier

%token _CTLOPERATOR_       "CTL* operator"
%token IDENTIFIER          "identifier"
%token NUMBER              "number"
%token _RELEASE_           "temporal operator RELEASE"
%token _NEXTSTATE_         "temporal operator NEXTSTATE"
%token _INITIAL_           "keyword INITIAL"
%token _DEADLOCK_          "keyword DEADLOCK"
%token _FORMULA_           "keyword FORMULA"
%token _MAX_           	   "keyword MAX"
%token _AND_               "Boolean conjuction"
%token _NOT_               "Boolean negation"
%token _OR_                "Boolean disjunction"
%token _XOR_               "Boolean exclusive disjunction"
%token _iff_               "Boolean iff"
%token _ALLPATH_           "path quantifier ALLPATH"
%token _ALWAYS_            "temporal operator ALWAYS"
%token _EVENTUALLY_        "temporal operator EVENTUALLY"
%token _EXPATH_            "path quantifier EXPATH"
%token _UNTIL_             "temporal operator UNTIL"
%token _REACHABLE_         "keyword REACHABLE"
%token _INVARIANT_         "keyword INVARIANT"
%token _IMPOSSIBLE_        "keyword IMPOSSIBLE"
%token _notequal_          "not-equals sign"
%token _implies_           "Boolean implication"
%token _equals_            "equals sign"
%token _plus_              "plus sign"
%token _minus_             "minus sign"
%token _times_             "multiplication sign"
%token _leftparenthesis_   "opening parenthesis"
%token _rightparenthesis_  "closing parenthesis"
%token _greaterthan_       "greater-than sign"
%token _lessthan_          "less-than sign"
%token _greaterorequal_    "greater-than-or-equal sign"
%token _lessorequal_       "less-than-or-equal sign"
%token _semicolon_         "semicolon"
%token _TRUE_              "Boolean TRUE"
%token _FALSE_             "Boolean FALSE"
%token _FIREABLE_          "keyword FIREABLE"
%token _omega_             "omega"
%token _colon_             "colon"
%token END 0               "end of file"

// precedences (lowest written first, e.g. PLUS/MINUS) and precedences
%nonassoc _REACHABLE_ _INVARIANT_ _IMPOSSIBLE_
%left _iff_
%left _implies_
%left _OR_ _XOR_
%left _AND_
%right _NOT_
%left _ALLPATH_ _EXPATH_ _CTLOPERATOR_ _UNTIL_ _ALWAYS_ _EVENTUALLY_ _NEXTSTATE_
%nonassoc _lessthan_ _lessorequal_ _greaterthan_ _greaterorequal_  _equals_ _notequal_
%left _plus_ _minus_
%left _times_

%{
// parser essentials
extern int ptformula_lex();
void ptformula_error(char const*);


std::set<arrayindex_t> target_place;
std::set<arrayindex_t> target_transition;
%}

%{
/* globals */
tFormula TheFormula;
%}

%%

compoundformula:
     formula { $$ = TheFormula = $1;}
| compoundformula _colon_ formula {$$ = TheFormula = Compound($1,$3);}
;
formula:
  _FORMULA_ statepredicate _semicolon_
    { $$ = StatePredicateFormula($2); }
| _FORMULA_ statepredicate
    { $$ = StatePredicateFormula($2); }
| statepredicate
    { $$ = StatePredicateFormula($1); }
| statepredicate _semicolon_
    { $$ = StatePredicateFormula($1); }
| computeboundformula 
    { $$ = $1; }
;

statepredicate:
  _leftparenthesis_ statepredicate _rightparenthesis_
    { $$ = $2; }
| atomic_proposition
    { $$ = AtomicProposition($1); }
| _NOT_ statepredicate
    { $$ = Negation($2); }
| statepredicate _AND_ statepredicate
    { $$ = Conjunction($1, $3); }
| statepredicate _OR_ statepredicate
    { $$ = Disjunction($1, $3); }
| statepredicate _XOR_ statepredicate
    { $$ = ExclusiveDisjunction($1, $3); }
| statepredicate _implies_ statepredicate
    { $$ = Implication($1, $3); }
| statepredicate _iff_ statepredicate
    { $$ = Equivalence($1, $3); }
| _ALLPATH_ statepredicate
    { $$ = AllPath($2); }
| _EXPATH_ statepredicate
    { $$ = ExPath($2); }
| _ALWAYS_ statepredicate
    { $$ = Always($2); }
| _EVENTUALLY_ statepredicate
    { $$ = Eventually($2); }
| _CTLOPERATOR_ statepredicate
    {
        kc::tStatePredicate result = $2;
        std::string op($1->name);
        for (int i = op.size()-1; i >= 0; i--)
        {
            if (op[i] == 'A') result = AllPath(result);
            if (op[i] == 'E') result = ExPath(result);
            if (op[i] == 'F') result = Eventually(result);
            if (op[i] == 'G') result = Always(result);
            if (op[i] == 'X') result = NextState(result);
            if (op[i] == 'U') yyerrors($1->name, @1, "operator 'U' is not allowed here");
            if (op[i] == 'R') yyerrors($1->name, @1, "operator 'R' is not allowed here");
        }
        $$ = result;
    }
| _leftparenthesis_ statepredicate _UNTIL_ statepredicate _rightparenthesis_
    { $$ = Until($2, $4); }
| _leftparenthesis_ statepredicate _RELEASE_ statepredicate _rightparenthesis_
    { $$ = Release($2, $4); }
| _leftparenthesis_ statepredicate _CTLOPERATOR_ statepredicate _rightparenthesis_
    {
        std::string op($3->name);
        if (op == "R") {
            $$ = Release($2, $4);
        } else if (op == "U") {
            $$ = Until($2, $4);
        } else {
            yyerrors($3->name, @3, "operator '%s' is not allowed here", $3->name);
        }
    }
| _NEXTSTATE_ statepredicate
    { $$ = NextState($2); }
| _REACHABLE_ statepredicate
    { $$ = ExPath(Eventually($2)); }
| _INVARIANT_ statepredicate
    { $$ = AllPath(Always($2)); }
| _IMPOSSIBLE_ statepredicate
    { $$ = AllPath(Always(Negation($2))); }
;

atomic_proposition:
  term _equals_ term
    { $$ = EqualsAtomicProposition($1, $3); }
| term _notequal_ term
    { $$ = NotEqualsAtomicProposition($1, $3); }
| term _greaterthan_ term
    { $$ = GreaterAtomicProposition($1, $3); }
| term _greaterorequal_ term
    { $$ = GreaterEqualAtomicProposition($1, $3); }
| term _lessthan_ term
    { $$ = LessAtomicProposition($1, $3); }
| term _lessorequal_ term
    { $$ = LessEqualAtomicProposition($1, $3); }
| _TRUE_
    { $$ = True(); }
| _FALSE_
    { $$ = False(); }
| _FIREABLE_ _leftparenthesis_ identifier _rightparenthesis_
    {
        Symbol *t = symbolTables->TransitionTable->lookup($3->name);
        if (UNLIKELY(t == NULL))
        {
            yyerrors($3->name, @3, "transition '%s' unknown", $3->name);
        }
        $$ = Fireable(mkinteger(t->getIndex()));
        target_transition.insert(t->getIndex());
    }
| _INITIAL_
    { $$ = Initial(); }
| _DEADLOCK_
    { $$ = Deadlock(); }
;

computeboundformula:
	_MAX_ _leftparenthesis_ term _rightparenthesis_ {$$ = ComputeBound(LessEqualAtomicProposition($3,Number(mkinteger(0))));}

term:
  _leftparenthesis_ term _rightparenthesis_
    { $$ = $2; }
| identifier
    {
        Symbol *p = symbolTables->PlaceTable->lookup($1->name);
        if (UNLIKELY(p == NULL))
        {
            yyerrors($1->name, @1, "place '%s' unknown", $1->name);
        }
        $$ = Node(mkinteger(p->getIndex()));
        target_place.insert(p->getIndex());
    }
| NUMBER
    { $$ = Number($1); }
| term _plus_ term
    { $$ = Sum($1, $3); }
| term _minus_ term
    { $$ = Difference($1, $3); }
| NUMBER _times_ term
    { $$ = Product($1, $3); }
| _omega_
    { $$ = Number(kc::mkinteger(OMEGA));
      if (RT::args.search_arg != search_arg_covergraph)
      {
          RT::rep->message("%s: omega markings used without %s",
              RT::rep->markup(MARKUP_WARNING, "warning").str(),
              RT::rep->markup(MARKUP_PARAMETER, "--search=cover").str());
      }
    }
;

identifier:
  IDENTIFIER     { $$ = $1; }
| _CTLOPERATOR_  { $$ = $1; }
;

%%

/// display a parser error and exit
void ptformula_error(char const* mess) __attribute__((noreturn));
void ptformula_error(char const* mess)
{
    extern char* ptformula_text;  ///< the current token text from Flex
    yyerrors(ptformula_text, ptformula_lloc, mess);
}
