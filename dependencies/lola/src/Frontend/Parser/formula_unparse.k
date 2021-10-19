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

%{ KC_UNPARSE
// LCOV_EXCL_START
/*!
\brief printer-function for Kimiwtu's output on stdout

To use an unparser in Kimwitu++, a function with this signature needs to be
passed to the unparse() function.

\param[in] s  string to print

\note This function is not "found" by the code coverage detection and is hence
hidden from it.
*/
void myprinter(const char *s, kc::uview)
{
    printf("%s", s);
}
// LCOV_EXCL_STOP

/// result string for stringprinter
std::string unparsed;

// LCOV_EXCL_START
/*!
\brief printer-function for Kimiwtu's output to a string object

To use an unparser in Kimwitu++, a function with this signature needs to be
passed to the unparse() function.

\param[in] s  string to add

\note This function is not "found" by the code coverage detection and is hence
hidden from it.
\post Output is written to string unparsed which can be accessed wherever an
unparser can be used. You need to clear the string after use to avoid keeping
old data.
*/
void stringprinter(const char *s, kc::uview)
{
    unparsed += s;
}
// LCOV_EXCL_STOP

bool *place_in_formula = NULL;
unsigned int places_mentioned = 0;
unsigned int unique_places_mentioned = 0;
%}

%{ KC_UNPARSE_HEADER
#include <CoverGraph/CoverGraph.h>
#include <Formula/StatePredicate/DeadlockPredicate.h>
#include <Formula/StatePredicate/FireablePredicate.h>
#include <Net/Net.h>
#include <string>

extern std::string unparsed;

void myprinter(const char *s, kc::uview v);
void stringprinter(const char *s, kc::uview v);
%}



//////////////////////////////////////////////////////////////////////////
%uview out;

StatePredicateFormula(x) -> [out: x];
Compound(x,y) -> [out: x " : " y];
ComputeBound(LessEqualAtomicProposition(x,y)) -> [out:  "MAX(" x " - " y ")"];

AtomicProposition(x) -> [out: x];
Negation(x) -> [out: "!(" x ")"];
Conjunction(x, y) -> [out: "(" x " AND " y ")"];
Disjunction(x, y) -> [out:  "(" x " OR " y ")"];
ExclusiveDisjunction(x, y) -> [out: "(" x " XOR " y ")"];
Implication(x, y) -> [out: "(" x " -> " y ")"];
Equivalence(x, y) -> [out: "(" x " <-> " y ")"];
AllPath(x) -> [out: "A" x];
ExPath(x)  -> [out: "E" x];
Always(x)     -> [out: "G (" x ")"];
Eventually(x) -> [out: "F (" x ")"];
NextState(x) -> [out: "X (" x ")"];
Until(phi,psi) -> [out: "(" phi " U " psi ")"];
Release(phi,psi) -> [out: "(" phi " R " psi ")"];
EU(x,y) -> [out: "E(" x " U " y ")"];
AR(x,y) -> [out: "A(" x " R " y ")"];
ER(x,y) -> [out: "E(" x " R " y ")"];
AU(x,y) -> [out: "A(" x " U " y ")"];
AF(x)   -> [out: "AF(" x ")"];
AG(x)   -> [out: "AG(" x ")"];
AX(x)   -> [out: "AX(" x ")"];
EX(x)   -> [out: "EX(" x ")"];
EF(x)   -> [out: "EF(" x ")"];
EG(x)   -> [out: "EG(" x ")"];

ConjunctionList(l) -> [out: "(" l ")"];
ConstConjunction_list(h, NiltConjunction_list()) -> [out: h];
ConstConjunction_list(h, l) -> [out: h " AND " l];

DisjunctionList(l) -> [out: "(" l ")"];
ConstDisjunction_list(h, NiltDisjunction_list()) -> [out: h];
ConstDisjunction_list(h, l) -> [out: h " OR " l];


EqualsAtomicProposition(x, y) -> [out: x " = " y];
NotEqualsAtomicProposition(x, y) -> [out: x " != " y];
GreaterAtomicProposition(x, y) -> [out: x " > " y];
GreaterEqualAtomicProposition(x, y) -> [out: x " >= " y];
LessAtomicProposition(x, y) -> [out: x " < " y];
LessEqualAtomicProposition(x, y) -> [out: x " <= " y];
True() -> [out: "TRUE"];
False() -> [out: "FALSE"];
Initial() -> [out: "INITIAL"];
Deadlock() -> [out: "DEADLOCK"];
NoDeadlock() -> [out: "! DEADLOCK"];
Fireable(x)   -> [out: "FIREABLE(" { kc_printer(kc_t(Net::Name[TR][x->value]), kc_current_view); } ")"];
Unfireable(x)   -> [out: "! FIREABLE(" { kc_printer(kc_t(Net::Name[TR][x->value]), kc_current_view); } ")"];

Node(x) -> [out: { kc_printer(kc_t(Net::Name[PL][x->value]), kc_current_view); }];
Number(x) provided (x->value == -FINITE) -> [out: "-finite"];
Number(x) provided (x->value == FINITE) -> [out: "finite"];
Number(x) provided (x->value == -OMEGA) -> [out: "-oo"];
Number(x) provided (x->value == OMEGA) -> [out: "oo"];
Number(x) -> [out: x];
Sum(x,y) -> [out: x " + " y];
Difference(x,y) -> [out: x " - " y];
Product(x,y) provided (x->value == 1) -> [out: y];
Product(x,y) provided (x->value == -1) -> [out: "-" y];
Product(x,y) -> [out: x "*" y];
ConstProduct_list(h, NiltProduct_list()) -> [out: h];
ConstProduct_list(h=Product(x,*), l=ConstProduct_list(*,*)) provided(x->value < 0) -> [out: l " + " h];
ConstProduct_list(h, l) -> [out: l " + " h];


BuechiAutomaton(trans,accept) -> [out: trans "\n" accept];
BuechiNull() -> [out:];

EmptyBuechiRules() -> [out:];
BuechiRule(i,t) -> [out: "STATE" i t];
ExpandedBuechiRule(from, formula, to) -> [out: from "(" formula ") -> " to "\n"];
BuechiRules(a,b) -> [out: a "&" b];

EmptyTransitionRules() -> [out:];
TransitionRule(formu,i) -> [out: formu "->" i];
TransitionRules(a,b) -> [out: a "&" b];

EmptyAcceptingSet() -> [out:];
AcceptingState(*) -> [out:];
AcceptingSet(*,*) -> [out:];


// The following commented block is the formula classification
// before insertion of FORMULA_BOOLEAN which requires correct
// clasification of (some) subformulas
//////////////////////////////////////////////////////////////////////////
//%uview temporal;
//
//// assumption: ExclusiveDisjunction, Implication, and Equivalence are gone.
//
//f=ComputeBound(x) -> [temporal: {f->type = FORMULA_BOUND;}];
//
//f=StatePredicateFormula(x) -> [temporal: x {
    //with(x) {
        //ExPath(Eventually(AtomicProposition(Deadlock()))) :
            //{ f->type = FORMULA_DEADLOCK; }
        //AllPath(Always(AtomicProposition(NoDeadlock()))) :
            //{ f->type = FORMULA_NODEADLOCK; }
        //ExPath(Eventually(y)) provided (not y->containsTemporal) :
            //{ f->type = FORMULA_REACHABLE; }
        //AllPath(Always(y)) provided (not y->containsTemporal) :
            //{ f->type = FORMULA_INVARIANT; }
        //AllPath(Always(ExPath(Eventually(y)))) provided (not y->containsTemporal) :
            //{ f->type = FORMULA_LIVENESS; }
        //ExPath(Eventually(AllPath(Always(y)))) provided (not y->containsTemporal) :
            //{ f->type = FORMULA_EFAG; }
        //AllPath(Always(ExPath(Eventually(AllPath(Always(y)))))) provided (not y->containsTemporal) :
            //{ f->type = FORMULA_AGEFAG; }
        //ExPath(Eventually(AllPath(Always(ExPath(Eventually(y)))))) provided (not y->containsTemporal) :
            //{ f->type = FORMULA_EGAGEF; }
        //Always(Eventually(y)) provided (not y->containsTemporal) :
            //{ f->type = FORMULA_FAIRNESS; }
        //Eventually(Always(y)) provided (not y->containsTemporal) :
            //{ f->type = FORMULA_STABILIZATION; }
        //Eventually(y) provided (not y->containsTemporal) :
            //{ f->type = FORMULA_EVENTUALLY; }
        //default provided (not x->containsTemporal) :
            //{ f->type = FORMULA_INITIAL; }
        //default provided (x->containsTemporal && x->validLTLFormula) :
            //{ f->type = FORMULA_LTL; }
        //default provided (x->containsTemporal && x->validCTLStateFormula) :
            //{ f->type = FORMULA_CTL; }
        //default :
            //{ f->type = FORMULA_MODELCHECKING; }
    //}
//}];
//
//f=AtomicProposition(*) -> [temporal: {
    //f->containsTemporal = false;
    //f->validCTLPathFormula = false;
    //f->validCTLStateFormula = true;
    //f->validLTLFormula = true;
//}];
//
//f=Negation(x) -> [temporal: x {
    //f->containsTemporal = x->containsTemporal;
    //f->validCTLStateFormula = x->validCTLStateFormula;
    //f->validCTLPathFormula = false;
    //f->validLTLFormula = x->validLTLFormula;
//}];
//
//f=Conjunction(x,y), f=Disjunction(x,y) -> [temporal: x y {
    //f->containsTemporal = (x->containsTemporal || y->containsTemporal);
    //f->validCTLStateFormula = (x->validCTLStateFormula && y->validCTLStateFormula);
    //f->validCTLPathFormula = false;
    //f->validLTLFormula = (x->validLTLFormula && y->validLTLFormula);
//}];
//
//f=AllPath(x), f=ExPath(x) -> [temporal: x {
    //f->containsTemporal = true;
    //f->validCTLStateFormula = x->validCTLPathFormula;
    //f->validCTLPathFormula = false;
    //f->validLTLFormula = false;
//}];
//
//f=Always(x), f=Eventually(x), f=NextState(x) -> [temporal: x {
    //f->containsTemporal = true;
    //f->validCTLStateFormula = false;
    //f->validCTLPathFormula = x->validCTLStateFormula;
    //f->validLTLFormula = x->validLTLFormula;
//}];
//
//f=Until(x,y), f=Release(x,y) -> [temporal: x y {
    //f->containsTemporal = true;
    //f->validCTLStateFormula = false;
    //f->validCTLPathFormula = (x->validCTLStateFormula && y->validCTLStateFormula);
    //f->validLTLFormula = (x->validLTLFormula && y->validLTLFormula);
//}];
//
//
//// \todo: next ones redundant: have no pairs when this view is applied
//f=AU(x,y), f=EU(x,y) , f=ER(x,y), f=AR(x,y) -> [temporal: x y {
    //f->containsTemporal = true;
    //f->validCTLStateFormula = x->validCTLStateFormula && y->validCTLStateFormula;
    //f->validCTLPathFormula = false;
    //f->validLTLFormula = false;
//}];
//
//f=AX(x), f=EX(x), f=EF(x), f=EG(x), f=AF(x), f=AG(x) -> [temporal: x {
    //f->containsTemporal = true;
    //f->validCTLStateFormula = x->validCTLPathFormula;
    //f->validCTLPathFormula = false;
    //f->validLTLFormula = false;
//}];


//////////////////////////////////////////////////////////////////////////
%uview temporal;

// assumption: ExclusiveDisjunction, Implication, and Equivalence are gone.

f=Compound(x,y) -> [temporal: x y
    {
	f -> type = FORMULA_COMPOUND; 
	f -> cannotcompute = x -> cannotcompute || y -> cannotcompute;
    }];

f=ComputeBound(x) -> [temporal: 
    {
	f -> type = FORMULA_BOUND; 
	f -> cannotcompute = false;
    }];


f=StatePredicateFormula(x) -> [temporal: x 
    {
	f -> type = x -> type; 
        f -> cannotcompute = x -> cannotcompute || 
                (! x -> validLTLFormula) || (! x -> validCTLStateFormula);
    }];

f=ExPath(g=Eventually(h=AtomicProposition(Deadlock()))) -> [temporal: 
    { 
	f -> type = FORMULA_DEADLOCK;
	f -> cannotcompute = false;
	f -> validCTLStateFormula = true;
	f -> validCTLPathFormula = false;
	f -> validLTLFormula = false;
        f -> containsTemporal = true;
        g -> containsTemporal = true;
        h -> containsTemporal = false;
    }];

f=AllPath(g=Always(h=AtomicProposition(NoDeadlock())))  -> [temporal: 
    { 
	f -> type = FORMULA_NODEADLOCK;
	f -> cannotcompute = false;
	f -> validCTLStateFormula = true;
	f -> validCTLPathFormula = false;
	f -> validLTLFormula = false;
        f -> containsTemporal = true;
        g -> containsTemporal = true;
        h -> containsTemporal = false;
    }];

f=AllPath(g=Always(h=ExPath(i=Eventually(j=AllPath(k=Always(y)))))) -> [temporal: y
    { 
	if(not y->containsTemporal)
	{
		f -> type = FORMULA_AGEFAG;
		f -> cannotcompute = false;
		f -> validCTLStateFormula = true;
	}
	else
	{
		f -> cannotcompute = (y->cannotcompute || !y -> validCTLStateFormula);
		f -> validCTLStateFormula = y -> validCTLStateFormula;
		if( ! f -> cannotcompute)
		{
			f -> type = FORMULA_CTL; 
		}
	}
	f -> validCTLPathFormula = false;
	f -> validLTLFormula = false;
        f -> containsTemporal = true;
        g -> containsTemporal = true;
        h -> containsTemporal = true;
        i -> containsTemporal = true;
        j -> containsTemporal = true;
        k -> containsTemporal = true;
    }];

f=ExPath(g=Eventually(h=AllPath(i=Always(j=ExPath(k=Eventually(y)))))) -> [temporal: y
    { 
	if(not y->containsTemporal)
	{
		f -> type = FORMULA_EGAGEF;
		f -> cannotcompute = false;
		f -> validCTLStateFormula = true;
	}
	else
	{
		f -> cannotcompute = (y->cannotcompute || !y -> validCTLStateFormula);
		f -> validCTLStateFormula = y -> validCTLStateFormula;
		if(!f -> cannotcompute)
		{
			f -> type = FORMULA_CTL; 
		}
	}
	f -> validCTLPathFormula = false;
	f -> validLTLFormula = false;
        f -> containsTemporal = true;
        g -> containsTemporal = true;
        h -> containsTemporal = true;
        i -> containsTemporal = true;
        j -> containsTemporal = true;
        k -> containsTemporal = true;
    }];

f=ExPath(g=Eventually(h=AllPath(i=Always(y)))) ->[temporal: y
    { 
	if(not y->containsTemporal)
	{
		f -> type = FORMULA_EFAG;
		f -> cannotcompute = false;
		f -> validCTLStateFormula = true;
	}
	else
	{
		f -> cannotcompute = (y->cannotcompute || ! y -> validCTLStateFormula);
		f -> validCTLStateFormula = y -> validCTLStateFormula;
		if(!f -> cannotcompute)
		{
			f -> type = FORMULA_CTL; 
		}
	}
	f -> validCTLPathFormula = false;
	f -> validLTLFormula = false;
        f -> containsTemporal = true;
        g -> containsTemporal = true;
        h -> containsTemporal = true;
        i -> containsTemporal = true;
    }];

f=AllPath(g=Always(h=ExPath(i=Eventually(y)))) -> [temporal: y 
    { 
	if(not y->containsTemporal)
	{
		f -> type = FORMULA_LIVENESS;
		f -> cannotcompute = false;
		f -> validCTLStateFormula = true;
	}
	else
	{
		f -> cannotcompute = (y->cannotcompute || !y -> validCTLStateFormula);
		f -> validCTLStateFormula = y -> validCTLStateFormula;
		if(! f -> cannotcompute)
		{
			f -> type = FORMULA_CTL; 
		}
	}
	f -> validCTLPathFormula = false;
	f -> validLTLFormula = false;
        f -> containsTemporal = true;
        g -> containsTemporal = true;
        h -> containsTemporal = true;
        i -> containsTemporal = true;
    }];

f=ExPath(g=Eventually(y)) -> [temporal: y 
    { 
	if(not y->containsTemporal)
	{
		f -> type = FORMULA_REACHABLE;
		f -> cannotcompute = false;
		f -> validCTLStateFormula = true;
	}
	else
	{
		f -> cannotcompute = (y->cannotcompute || !y -> validCTLStateFormula);
		f -> validCTLStateFormula = y -> validCTLStateFormula;
		if(!f -> cannotcompute)
		{
			f -> type = FORMULA_CTL; 
		}
	}
	f -> validCTLPathFormula = false;
	f -> validLTLFormula = false;
        f -> containsTemporal = true;
        g -> containsTemporal = true;
    }];

f=AllPath(g=Always(y)) -> [temporal: y 
    { 
	if(not y->containsTemporal)
	{
		f -> type = FORMULA_INVARIANT;
		f -> cannotcompute = false;
		f -> validCTLStateFormula = true;
                f -> validLTLFormula = y -> validLTLFormula;
	}
	else
	{   
            if (y -> validLTLFormula)
            {
                f -> type = FORMULA_LTL;
                f -> cannotcompute = false;
            }
            else
            {
		f -> cannotcompute = (y->cannotcompute || !y -> validCTLStateFormula);
		f -> validCTLStateFormula = y -> validCTLStateFormula;
		if(! f -> cannotcompute)
		{
			f -> type = FORMULA_CTL; 
		}
            }
	}
	f -> validCTLPathFormula = false;
        f -> validLTLFormula = false;
        f -> containsTemporal = true;
        g -> containsTemporal = true;
    }];

f=Always(g=Eventually(y)) -> [temporal: y
    { 
	if(not y->containsTemporal)
	{
		f -> type = FORMULA_FAIRNESS;
		f -> cannotcompute = false;
		f -> validLTLFormula = true;
	}
	else
	{
		f -> cannotcompute = (y->cannotcompute || ! y -> validLTLFormula);
		f -> validLTLFormula = y -> validLTLFormula;
		if(!f -> cannotcompute)
		{
			f -> type = FORMULA_LTL; 
		}
	}
	f -> validCTLPathFormula = false;
	f -> validCTLStateFormula = false;
        f -> containsTemporal = true;
        g -> containsTemporal = true;
    }];

f=Eventually(g=Always(y)) -> [temporal: y
    { 
	if(not y->containsTemporal)
	{
		f -> type = FORMULA_STABILIZATION;
		f -> cannotcompute = false;
		f -> validLTLFormula = true;
	}
	else
	{
		f -> cannotcompute = (y->cannotcompute || ! y -> validLTLFormula);
		f -> validLTLFormula = y -> validLTLFormula;
		if(!f -> cannotcompute)
		{
			f -> type = FORMULA_LTL; 
		}
	}
	f -> validCTLPathFormula = false;
	f -> validCTLStateFormula = false;
        f -> containsTemporal = true;
        g -> containsTemporal = true;
    }];

f=Eventually(y) -> [temporal: y
    { 
	if(not y->containsTemporal)
	{
		f -> type = FORMULA_EVENTUALLY;
		f -> cannotcompute = false;
		f -> validLTLFormula = true;
	}
	else
	{
		f -> cannotcompute = (y->cannotcompute || ((!y -> validLTLFormula) && !(y -> validCTLStateFormula)));
		f -> validLTLFormula = y -> validLTLFormula;
		if(!f -> cannotcompute)
		{
			f -> type = FORMULA_LTL; 
		}
	}
	f -> validCTLPathFormula = y -> validCTLStateFormula;
	f -> validCTLStateFormula = false;
        f -> containsTemporal = true;
    }];

f=AtomicProposition(*) -> [temporal: 
    {
        f->type = FORMULA_INITIAL;
	f->cannotcompute=false;
    	f->containsTemporal = false;
    	f->validCTLPathFormula = false;
    	f->validCTLStateFormula = true;
    	f->validLTLFormula = true;
    }];

f=Negation(x) -> [temporal: x 
    {
	if(not x->containsTemporal)
	{
        	f->type = FORMULA_INITIAL;
	}
	else
	{
        	f->type = FORMULA_BOOLEAN;
	}
	f->cannotcompute=x->cannotcompute;
	if(f->cannotcompute)
	{
		f->type = FORMULA_MODELCHECKING;
	}
    	f->containsTemporal = x->containsTemporal;
    	f->validCTLStateFormula = x->validCTLStateFormula;
    	f->validCTLPathFormula = false;
    	f->validLTLFormula = x->validLTLFormula;
    }];

f=Conjunction(x,y), f=Disjunction(x,y) -> [temporal: x y 
    {
	if(not (x->containsTemporal || y->containsTemporal))
	{
        	f->type = FORMULA_INITIAL;
	}
	else
	{
        	f->type = FORMULA_BOOLEAN;
	}
	f->cannotcompute=x->cannotcompute || y -> cannotcompute;
	if(f->cannotcompute)
	{
		f->type = FORMULA_MODELCHECKING;
	}
    	f->containsTemporal = (x->containsTemporal || y->containsTemporal);
   	f->validCTLStateFormula = (x->validCTLStateFormula && y->validCTLStateFormula);
    	f->validCTLPathFormula = false;
    	f->validLTLFormula = (x->validLTLFormula && y->validLTLFormula);
    }];

f=AllPath(x) -> [temporal: x 
    {
        if (x -> validLTLFormula)
        {
            f->type = FORMULA_LTL;
        }
        else
        {
            f->cannotcompute = (x -> cannotcompute || !(x->validCTLPathFormula));
            if(! f->cannotcompute)
            {
                    f->type = FORMULA_CTL;
            }
        }
    	f->containsTemporal = true;
    	f->validCTLStateFormula = x->validCTLPathFormula;
    	f->validCTLPathFormula = false;
    	f->validLTLFormula = false;
    }];

f=ExPath(x) -> [temporal: x 
    {
	f->cannotcompute = (x -> cannotcompute || !(x->validCTLPathFormula));
	if(! f->cannotcompute)
	{
		f->type = FORMULA_CTL;
	}
    	f->containsTemporal = true;
    	f->validCTLStateFormula = x->validCTLPathFormula;
    	f->validCTLPathFormula = false;
    	f->validLTLFormula = false;
    }];

f=Always(x), f=NextState(x) -> [temporal: x 
    {
	f->cannotcompute = (x -> cannotcompute || !(x->validLTLFormula || x->validCTLStateFormula));
	if((not f->cannotcompute)&& x->validLTLFormula)
	{
		f->type = FORMULA_LTL;
	}
    	f->containsTemporal = true;
    	f->validCTLStateFormula = false;
    	f->validCTLPathFormula = x->validCTLStateFormula;
    	f->validLTLFormula = x->validLTLFormula;
    }];

f=Until(x,y), f=Release(x,y) -> [temporal: x y 
    {
	f->cannotcompute = ( x -> cannotcompute || y -> cannotcompute || !((x->validLTLFormula && y -> validLTLFormula)||(x->validCTLStateFormula && y->validCTLStateFormula)));
    	f->containsTemporal = true;
    	f->validCTLStateFormula = false;
    	f->validCTLPathFormula = (x->validCTLStateFormula && y->validCTLStateFormula);
    	f->validLTLFormula = (x->validLTLFormula && y->validLTLFormula);
	if((not f->cannotcompute) && f->validLTLFormula)
	{
		f->type = FORMULA_LTL;
	}
    }];


// \todo: next ones redundant: have no pairs when this view is applied
f=EU(x,y) , f=ER(x,y) -> [temporal: x y 
    {
        f->containsTemporal = true;
        f->validCTLStateFormula = x->validCTLStateFormula && y->validCTLStateFormula;
        f->validCTLPathFormula = false;
        f->validLTLFormula = false;
        f->cannotcompute = (x->cannotcompute || y -> cannotcompute || ! x->validCTLStateFormula || ! y->validCTLStateFormula);
        if(!f->cannotcompute)
        {
            f->type = FORMULA_CTL;
        } 
    }];
    
f=AU(x,y), f=AR(x,y) -> [temporal: x y 
    {
        f->containsTemporal = true;
        f->validCTLStateFormula = x->validCTLStateFormula && y->validCTLStateFormula;
        f->validCTLPathFormula = false;
        f->validLTLFormula = false;
        if (x -> validLTLFormula && y -> validLTLFormula)
        {
            f->type = FORMULA_LTL;
            f->cannotcompute = false;
        }
        else
        {
            f->cannotcompute = (x->cannotcompute || y -> cannotcompute || ! x->validCTLStateFormula || ! y->validCTLStateFormula);
            if(!f->cannotcompute)
            {
                f->type = FORMULA_CTL;
            }  
        }
    }];

f=EX(x), f=EF(x), f=EG(x) -> [temporal: x 
    {
        f->containsTemporal = true;
        f->validCTLStateFormula = x->validCTLStateFormula;
        f->validCTLPathFormula = false;
        f->validLTLFormula = false;
        f->cannotcompute = (x->cannotcompute || ! x->validCTLStateFormula);
        if(!f->cannotcompute)
        {
            f->type = FORMULA_CTL;
        } 
    }];

f=AX(x), f=AF(x), f=AG(x) -> [temporal: x 
    {
        f->containsTemporal = true;
        f->validCTLStateFormula = x->validCTLStateFormula;
        f->validCTLPathFormula = false;
        f->validLTLFormula = false;
        if (x -> validLTLFormula)
        {
            f->type = FORMULA_LTL;
            f->cannotcompute = false;
        }
        else
        {
            f->cannotcompute = (x->cannotcompute || ! x->validCTLStateFormula);
            if(!f->cannotcompute)
            {
                f->type = FORMULA_CTL;
            }  
        }
    }];



//////////////////////////////////////////////////////////////////////////
%uview internal;

%{ KC_UNPARSE
#include <config.h>
#include <Core/Dimensions.h>
#include <Formula/StatePredicate/AtomicStatePredicate.h>
#include <Formula/StatePredicate/ConjunctionStatePredicate.h>
#include <Formula/StatePredicate/DisjunctionStatePredicate.h>
#include <Formula/StatePredicate/FalsePredicate.h>
#include <Formula/StatePredicate/StatePredicate.h>
#include <Formula/StatePredicate/TruePredicate.h>
#include <Net/Marking.h>
#include <Net/Net.h>

#include <map>
#include <vector>
#include <stack>

// mapping from node ids to factors
std::map<int, int> current;

// mapping from tree node ids to list of intermediate formulas
std::map<int, std::vector<StatePredicate *> > formulas;

// invariant: top is always my parent
std::stack<size_t> id_stack;

// next free id
size_t nextId = 0;
%}

f=StatePredicateFormula(x) -> [internal: {
	// clear all data-structures
	current.clear();
	formulas.clear();
	while (not id_stack.empty())
		id_stack.pop();
	nextId = 0;
    id_stack.push(nextId++);
}
x
{
    nextId = id_stack.top();
    id_stack.pop();
    // return the only entry for the root
    assert(nextId == 0);
    assert(formulas[0].size() == 1);
    f->formula = formulas[0][0];
}];

Conjunction(a,b) -> [internal: {
    id_stack.push(nextId++);
}
a b
{
    // after returning from my childs, I am the top of the stack
    const arrayindex_t myId = id_stack.top();
    id_stack.pop();

    StatePredicate *f = new ConjunctionStatePredicate(formulas[myId].size());
    for (size_t i = 0; i < formulas[myId].size(); ++i)
    {
        assert(formulas[myId][i]);
        ((ConjunctionStatePredicate*)f)->addSub(i, formulas[myId][i]);
    }

    formulas[id_stack.top()].push_back(f);
}];

ConjunctionList(l) -> [internal: {
    id_stack.push(nextId++);
}
l
{
    // after returning from my childs, I am the top of the stack
    const arrayindex_t myId = id_stack.top();
    id_stack.pop();

    StatePredicate *f = new ConjunctionStatePredicate(l->length());
    
    for (int i = 0; i < l->length(); ++i)
    {
        assert(formulas[myId][i]);
        ((ConjunctionStatePredicate*)f)->addSub(i, formulas[myId][i]);
    }

    formulas[id_stack.top()].push_back(f);
}];

Disjunction(a,b) -> [internal: {
    id_stack.push(nextId++);
}
a b
{
    // after returning from my childs, I am the top of the stack
    const arrayindex_t  myId = id_stack.top();
    id_stack.pop();

    StatePredicate *f = new DisjunctionStatePredicate(formulas[myId].size());
    for (size_t i = 0; i < formulas[myId].size(); ++i)
    {
        assert(formulas[myId][i]);
        ((DisjunctionStatePredicate*)f)->addSub(i, formulas[myId][i]);
    }

    formulas[id_stack.top()].push_back(f);
}];

DisjunctionList(l) -> [internal: {
    id_stack.push(nextId++);
}
l
{
    // after returning from my childs, I am the top of the stack
    const arrayindex_t myId = id_stack.top();
    id_stack.pop();

    StatePredicate *f = new DisjunctionStatePredicate(l->length());
    
    for (int i = 0; i < l->length(); ++i)
    {
        assert(formulas[myId][i]);
        ((DisjunctionStatePredicate*)f)->addSub(i, formulas[myId][i]);
    }

    formulas[id_stack.top()].push_back(f);
}];

AtomicProposition(a) -> [internal: a];

LessEqualAtomicProposition(x, Number(y)) -> [internal: x {
    arrayindex_t factors_positive = 0;
    arrayindex_t factors_negative = 0;

    // count positive and negative entries
    for (std::map<int, int>::const_iterator it = current.begin(); it != current.end(); ++it)
    {
        if (it->second > 0)
        {
            factors_positive++;
        }
        else if (it->second < 0)
        {
            factors_negative++;
        }
    }

    // create formula object
    StatePredicate *f = new AtomicStatePredicate(factors_positive, factors_negative, y->value);
    arrayindex_t position_pos = 0;
    arrayindex_t position_neg = 0;

    // add positive and negative entries
    for (std::map<int, int>::const_iterator it = current.begin(); it != current.end(); ++it)
    {
        if (it->second > 0)
        {
            ((AtomicStatePredicate*)f)->addPos(position_pos++, it->first, it->second);
        }
        else if (it->second < 0)
        {
            ((AtomicStatePredicate*)f)->addNeg(position_neg++, it->first, -it->second);
        }
    }

    // reduce with respect to the gcd
    ((AtomicStatePredicate*)f)->reduceFactors();

    // reset current
    current.clear();

    // register formula at parent
    formulas[id_stack.top()].push_back(f);
}];

GreaterAtomicProposition(x, Number(y)) -> [internal: 
     {   
     }
     x 
     {
    arrayindex_t factors_positive = 0;
    arrayindex_t factors_negative = 0;

    // count positive and negative entries
    for (std::map<int, int>::const_iterator it = current.begin(); it != current.end(); ++it)
    {
        if (it->second > 0)
        {
            factors_positive++;
        }
        else if (it->second < 0)
        {
            factors_negative++;
        }
    }

    // create formula object: but need to transform from > to <=
    StatePredicate *f = new AtomicStatePredicate(factors_negative, factors_positive, (- y->value) -1);
    arrayindex_t position_pos = 0;
    arrayindex_t position_neg = 0;

    // add positive and negative entries
    for (std::map<int, int>::const_iterator it = current.begin(); it != current.end(); ++it)
    {
        if (it->second < 0) // add negatives as positive (due to > to <=)
        {
            ((AtomicStatePredicate*)f)->addPos(position_pos++, it->first, -it->second);
        }
        else if (it->second > 0) // add negatives as positive (due to > to <=))
        {
            ((AtomicStatePredicate*)f)->addNeg(position_neg++, it->first, it->second);
        }
    }

    // reduce with respect to the gcd
    ((AtomicStatePredicate*)f)->reduceFactors();

    // reset current
    current.clear();

    // register formula at parent
    formulas[id_stack.top()].push_back(f);
}];

True() -> [internal: {
    StatePredicate *f = new TruePredicate();
    // register formula at parent
    formulas[id_stack.top()].push_back(f);    
}];

False() -> [internal: {
    StatePredicate *f = new FalsePredicate();
    // register formula at parent
    formulas[id_stack.top()].push_back(f);    
}];

Deadlock() -> [internal: {
    StatePredicate *f = new DeadlockPredicate(true);
    // register formula at parent
    formulas[id_stack.top()].push_back(f);    
}];

Fireable(t) -> [internal: {
    StatePredicate *f = new FireablePredicate(t->value,true);
    // register formula at parent
    formulas[id_stack.top()].push_back(f);    
}];

Unfireable(t) -> [internal: {
    StatePredicate *f = new FireablePredicate(t->value,false);
    // register formula at parent
    formulas[id_stack.top()].push_back(f);    
}];

NoDeadlock() -> [internal: {
    StatePredicate *f = new DeadlockPredicate(false);
    // register formula at parent
    formulas[id_stack.top()].push_back(f);    
}];

Product(x, Node(p)) -> [internal: {
    current[p->value] += x->value;

    // count this place as mentioned
    if (place_in_formula[p->value] == false)
    {
        unique_places_mentioned++;
        place_in_formula[p->value] = true;
    }
    places_mentioned++;
}];

Number(*) -> [internal: /* do not print values */];

f = ComputeBound(LessEqualAtomicProposition(x,Number(y))) -> [internal: 
    {
	// clear all data-structures
	current.clear();
	formulas.clear();
	while (not id_stack.empty())
		id_stack.pop();
	nextId = 0;
    id_stack.push(nextId++);
    }
    x
    {
    arrayindex_t factors_positive = 0;
    arrayindex_t factors_negative = 0;

    // count positive and negative entries
    for (std::map<int, int>::const_iterator it = current.begin(); it != current.end(); ++it)
    {
        if (it->second > 0)
        {
            factors_positive++;
        }
        else if (it->second < 0)
        {
            factors_negative++;
        }
    }

    // create formula object
    StatePredicate *g = new AtomicStatePredicate(factors_positive, factors_negative, y->value);
    arrayindex_t position_pos = 0;
    arrayindex_t position_neg = 0;

    // add positive and negative entries
    for (std::map<int, int>::const_iterator it = current.begin(); it != current.end(); ++it)
    {
        if (it->second > 0)
        {
            ((AtomicStatePredicate*)g)->addPos(position_pos++, it->first, it->second);
        }
        else if (it->second < 0)
        {
            ((AtomicStatePredicate*)g)->addNeg(position_neg++, it->first, -it->second);
        }
    }

    // reset current
    current.clear();

    // register formula at parent
    f -> formula = g;
}];


//////////////////////////////////////////////////////////////////////////
%uview buechi;


%{ KC_UNPARSE
std::map<uint32_t, std::vector<std::pair<uint32_t,uint32_t> > > transitions ;
std::set<uint32_t> acceptingset;
std::map<uint32_t, StatePredicate*> predicates;
std::map<uint32_t, uint32_t> states_to_interal;

uint32_t get_state_number(uint32_t state){
	if (states_to_interal.find(state) == states_to_interal.end()){
		// get a new state number
		uint32_t new_state_number = states_to_interal.size();
		states_to_interal[state] = new_state_number;
		return new_state_number;
	}
	return states_to_interal[state];
}
%}


b=BuechiNull -> [buechi: { RT::rep->message("NULL BUECHI"); b->automata = 0; }];
b=BuechiAutomaton(trans,accept) -> [buechi:
	trans
	accept
    {
    	//RT::rep->message("REAL BUECHI");
    	
    	// get number of states
    	uint32_t number_of_states = states_to_interal.size();
		
		// construction of the buechi automata
		b->automata = new BuechiAutomata();
		b->automata->cardStates = number_of_states;

		//RT::rep->message("transfer predicates");
    	// transfer all state predicates
    	b->automata->cardAtomicPropositions = predicates.size();
    	b->automata->atomicPropositions = new StatePredicateProperty*[predicates.size()]();
    	b->automata->atomicPropotions_backlist = new arrayindex_t[predicates.size()]();
    	for (uint32_t i = 0; i < predicates.size(); i++)
    		b->automata->atomicPropositions[i] = new StatePredicateProperty(predicates[i]);	
    	
    	
    	//RT::rep->message("transfer transitions");
    	// build the transition-data-structure
    	b->automata->cardTransitions = new uint32_t[number_of_states]();
    	b->automata->transitions = new uint32_t**[number_of_states]();
    	b->automata->cardEnabled = new arrayindex_t[number_of_states]();
    	for (uint32_t i = 0; i < number_of_states; i++){
    		b->automata->cardTransitions[i] = transitions[i].size();
    		b->automata->transitions[i] = new uint32_t*[transitions[i].size()]();
    		for (uint32_t j = 0; j < transitions[i].size(); j++){
    			//RT::rep->message("TRANSITION %d %d %d", i,j,transitions[i][j].second);
    			b->automata->transitions[i][j] = new uint32_t[2]();
    			b->automata->transitions[i][j][0] = transitions[i][j].first;
    			b->automata->transitions[i][j][1] = transitions[i][j].second;
    			b->automata->atomicPropotions_backlist[transitions[i][j].first] = i;
    		}
    	}
    	
    	//RT::rep->message("transfer accepting states");
        // produce a list for the accepting states
        b->automata->isStateAccepting = new bool[number_of_states]();
        for (__typeof__(acceptingset.begin()) i = acceptingset.begin(); i != acceptingset.end(); i++)
        	b->automata->isStateAccepting[*i] = true;
    }
];


ExpandedBuechiRule(from, formula, to) -> [buechi:
    {
    
    	formula->unparse(myprinter, kc::internal);
        StatePredicate* result = formula->formula;
        //formula->free(true);
        
        
        
    	//RT::rep->message("RULE %d -> %d (%d)", from, to,formula->formula);
    	// get a number for the formula
    	uint32_t formula_number = (uint32_t)(predicates.size());
    	predicates[formula_number] = result;
    	// get numbers for the from and to state
    	uint32_t from_number = get_state_number(from->value);
    	uint32_t to_number = get_state_number(to->value);
    	// insert transition
        transitions[from_number].push_back(std::make_pair(formula_number, to_number));
    } 
];

EmptyBuechiRules() -> [buechi:];

BuechiRules(lrules, rrules) -> [buechi:
    lrules
    rrules
];

AcceptingState(state) -> [buechi:
    {
        acceptingset.insert(get_state_number(state->value));
    }
];

AcceptingSet(set1, set2) -> [buechi:
  set1
  set2
];

EmptyAcceptingSet() -> [buechi:];



//////////////////////////////////////////////////////////////////////////
%uview ctl;
%{ KC_UNPARSE

#include <Core/Dimensions.h>
#include <Exploration/StatePredicateProperty.h>
#include <Formula/CTL/AUFormula.h>
#include <Formula/CTL/AXFormula.h>
#include <Formula/CTL/AtomicFormula.h>
#include <Formula/CTL/CTLFormula.h>
#include <Formula/CTL/ConjunctionFormula.h>
#include <Formula/CTL/DisjunctionFormula.h>
#include <Formula/CTL/EUFormula.h>
#include <Formula/CTL/EXFormula.h>
#include <Formula/CTL/NotFormula.h>
#include <Net/Net.h>

#include <map>
#include <vector>
#include <stack>

// mapping from node ids to factors
std::map<int, int> ctl_current;

// mapping from tree node ids to list of intermediate formulas
std::map<int, std::vector<CTLFormula_p> > ctl_formulas;

// invariant: top is always my parent
std::stack<size_t> ctl_id_stack;

// next free id
size_t ctl_nextId = 0;

// needs dfs-storage;
size_t ctl_dfs_id = 0;

// needs cached results id
size_t ctl_result_cache_id = 0;

// temporal storage to be able to include the LessEqualStatePredicate into an AtomicFormula
StatePredicateProperty* spp;
%}

//EINSTIEG

f=Conjunction provided (not f->containsTemporal) , 
f=Disjunction provided (not f->containsTemporal) , 
f=AtomicProposition provided (not f->containsTemporal) -> [ctl: {
	// We call the unparse process again to avoid unneccessary code
    // replication. Furthermore this may decrease evaluation time, since now
    // some parts of some formula can be checked without taking care of
    // temporal operators.
    with(StatePredicateFormula(f))
        // Here a little memory leak may occurs. A new object
        // "StatePredicateFormula" is created but never deconstructed. Since
        // Kimwitu++ has memory leaks nonetheless and this one is very small,
        // we considered it to be acceptable.
    {
        x=StatePredicateFormula(*) :
        {
            // unparse using code of different goal to reuse existing code
            x->unparse(myprinter, kc::internal);
            AtomicFormula* af = new AtomicFormula();
            af->inner = new StatePredicateProperty(x->formula);
	        ctl_formulas[ctl_id_stack.top()].push_back(af);
	    }
	}
}];

f=StatePredicateFormula(x) -> [ctl: {
    f->unparse(myprinter, kc::temporal);
    // making sure that correct information about contained temporal operators is available at every node. Note that this information is _inevietable_ in order to unparse CTL-Formulas correctly!
    ctl_id_stack.push(ctl_nextId++);
}
x
{
    ctl_nextId = ctl_id_stack.top();
    ctl_id_stack.pop();

    // return the only entry for the root
    assert(ctl_nextId == 0);
    f->ctl_formula = ctl_formulas[0][0];
    f->ctl_formula->payloadsize = ctl_dfs_id*SIZEOF_STATENUMBER_T + (2*ctl_result_cache_id+7)/8;
}];

//Boolsche Operatoren

// Negation can only be in as consequence of workarounds, see phase 5 in rewrite
Negation(a) -> [ctl: {
    ctl_id_stack.push(ctl_nextId++);
}
a
{
    // after returning from my childs, I am the top of the stack
    const arrayindex_t myId = ctl_id_stack.top();
    ctl_id_stack.pop();

    assert(ctl_formulas[myId].size() == 1);
    NotFormula *nf = new NotFormula(ctl_formulas[myId][0]);
    ctl_formulas[ctl_id_stack.top()].push_back(nf);
}];

f=Conjunction(a,b) provided (f->containsTemporal) -> [ctl: {
    ctl_id_stack.push(ctl_nextId++);
}
a b
{
    // after returning from my childs, I am the top of the stack
    const arrayindex_t myId = ctl_id_stack.top();
    ctl_id_stack.pop();

    ConjunctionFormula *cf = new ConjunctionFormula(ctl_formulas[myId]);
    ctl_formulas[ctl_id_stack.top()].push_back(cf);
}];

f=Disjunction(a,b) provided (f->containsTemporal) -> [ctl: {
    ctl_id_stack.push(ctl_nextId++);
}
a b
{
    // after returning from my childs, I am the top of the stack
    const arrayindex_t myId = ctl_id_stack.top();
    ctl_id_stack.pop();

    DisjunctionFormula *df = new DisjunctionFormula(ctl_formulas[myId]);
    ctl_formulas[ctl_id_stack.top()].push_back(df);
}];

//CTL-Operatoren

AX(x) -> [ctl: {
    ctl_id_stack.push(ctl_nextId++);
}
x
{
	// after returning from my childs, I am the top of the stack
	const arrayindex_t myId = ctl_id_stack.top();
	ctl_id_stack.pop();

	assert(ctl_formulas[myId].size() == 1);
	AXFormula* axf = new AXFormula(ctl_formulas[myId][0]);
	
	ctl_formulas[ctl_id_stack.top()].push_back(axf);
}];

AU(a,b) -> [ctl: {
    ctl_id_stack.push(ctl_nextId++);
	const size_t myDFSId = ctl_dfs_id++;
	const size_t myCacheId = ctl_result_cache_id++;
}
a b
{
    // after returning from my childs, I am the top of the stack
    const arrayindex_t myId = ctl_id_stack.top();
    ctl_id_stack.pop();

	assert(ctl_formulas[myId].size() == 2);
	AUFormula* auf = new AUFormula(ctl_formulas[myId][0], ctl_formulas[myId][1]);
	auf->index = 2 * myCacheId;
	auf->dfsindex = myDFSId * SIZEOF_STATENUMBER_T;

	ctl_formulas[ctl_id_stack.top()].push_back(auf);
}];

EX(x) -> [ctl: {
    ctl_id_stack.push(ctl_nextId++);
}
x
{
	// after returning from my childs, I am the top of the stack
	const arrayindex_t myId = ctl_id_stack.top();
	ctl_id_stack.pop();

	assert(ctl_formulas[myId].size() == 1);
	EXFormula* exf = new EXFormula(ctl_formulas[myId][0]);

	ctl_formulas[ctl_id_stack.top()].push_back(exf);
}];

EU(a,b) -> [ctl: {
    ctl_id_stack.push(ctl_nextId++);
	const size_t myDFSId = ctl_dfs_id++;
	const size_t myCacheId = ctl_result_cache_id++;
}
a b
{
    // after returning from my childs, I am the top of the stack
    const arrayindex_t myId = ctl_id_stack.top();
    ctl_id_stack.pop();

	assert(ctl_formulas[myId].size() == 2);
	EUFormula* euf = new EUFormula(ctl_formulas[myId][0], ctl_formulas[myId][1]);
	euf->index = 2 * myCacheId;
	euf->dfsindex = myDFSId * SIZEOF_STATENUMBER_T;

	ctl_formulas[ctl_id_stack.top()].push_back(euf);
}];



//////////////////////////////////////////////////////////////////////////
%uview ltl;

%{ KC_UNPARSE
#include <Core/Runtime.h>
#include <Frontend/Parser/ast-system-rk.h>
#include <vector>

std::vector<LTLTree_p> ltlstack;
%}


f=StatePredicateFormula(x) -> [ltl:
	x
	{
		// put top of stack into memoy
		f->ltl_tree = bin_simpler(push_negation(tl_nn(NOT, ltlstack.back(), NULL)));
		//f->ltl_tree = ltlstack.back();
	}];

AtomicProposition(x) -> [ltl:
	{
		// parse atomic propostion with internal
		kc::tFormula internalFormula = StatePredicateFormula(AtomicProposition(x));
		internalFormula->unparse(myprinter, kc::internal);
        StatePredicate* result = internalFormula->formula;
        
        // produce a new string
        int num;
        char_p sym = produce_next_string(&num);
        // I know what i am doing, the current sub-tree is an atomic propositon
        predicateMap[num] = (AtomicStatePredicate*)result;
        LTLTree_p newTree = tl_nn(PREDICATE, NULL, NULL);
        newTree->sym = tl_lookup(sym);
        newTree = bin_simpler(newTree);
        //RT::rep->message("SYM %x %x",newTree,newTree->sym);
        assert(newTree->sym);
        ltlstack.push_back(newTree);
	}
];

Eventually(x) -> [ltl: 
	x
	{
		LTLTree_p a = ltlstack.back();
		ltlstack.pop_back();
		ltlstack.push_back(bin_simpler(tl_nn(U_OPER, tl_nn(TRUE, NULL, NULL), a)));
	}
 ];
Always(x)     -> [ltl:
	x
	{
		LTLTree_p a = ltlstack.back();
		ltlstack.pop_back();
		ltlstack.push_back(bin_simpler(tl_nn(V_OPER, tl_nn(FALSE, NULL, NULL), a)));
	}
];
NextState(x)   -> [ltl:
	x
	{
		LTLTree_p a = ltlstack.back();
		ltlstack.pop_back();
		ltlstack.push_back(bin_simpler(tl_nn(NEXT, a, NULL)));
	}
];
Until(phi,psi) -> [ltl: 
	phi
	psi
	{
		LTLTree_p a = ltlstack.back();
		ltlstack.pop_back();
		LTLTree_p b = ltlstack.back();
		ltlstack.pop_back();
		ltlstack.push_back(bin_simpler(tl_nn(U_OPER, b, a)));
	}
];

Release(phi,psi) -> [ltl: 
	phi
	psi
	{
		LTLTree_p a = ltlstack.back();
		ltlstack.pop_back();
		LTLTree_p b = ltlstack.back();
		ltlstack.pop_back();
		ltlstack.push_back(bin_simpler(tl_nn(V_OPER, b, a)));
	}
];

Negation(x) -> [ltl:
	x
	{
		LTLTree_p a = ltlstack.back();
		ltlstack.pop_back();
		ltlstack.push_back(bin_simpler(tl_nn(NOT, a, NULL)));
	}
];
Conjunction(x, y) -> [ltl: 
	x
	y
	{
		LTLTree_p a = ltlstack.back();
		ltlstack.pop_back();
		LTLTree_p b = ltlstack.back();
		ltlstack.pop_back();
		ltlstack.push_back(bin_simpler(tl_nn(AND, a, b)));
	}
];
Disjunction(x, y) -> [ltl:
	x
	y
	{
		LTLTree_p a = ltlstack.back();
		ltlstack.pop_back();
		LTLTree_p b = ltlstack.back();
		ltlstack.pop_back();
		ltlstack.push_back(bin_simpler(tl_nn(OR, a, b)));
	}
];
Implication(x, y) -> [ltl:
 	x
	y
	{
		LTLTree_p a = ltlstack.back();
		ltlstack.pop_back();
		LTLTree_p b = ltlstack.back();
		ltlstack.pop_back();
		ltlstack.push_back(bin_simpler(tl_nn(IMPLIES, a, b)));
	}
];
Equivalence(x, y) -> [ltl:
	x
	y
	{
		LTLTree_p a = ltlstack.back();
		ltlstack.pop_back();
		LTLTree_p b = ltlstack.back();
		ltlstack.pop_back();
		ltlstack.push_back(bin_simpler(tl_nn(EQUIV, a, b)));
	}
];

//////////////////////////////////////////////////////////////////////////
%uview toplevelboolean;

// This view is for translating the top level Boolean operators into
//a hierarchy of tasks.

%{ KC_UNPARSE
#include <Core/Runtime.h>
#include <Frontend/Parser/ast-system-rk.h>
#include <vector>
#include <Planning/Task.h>
#include <Planning/ConjunctionTask.h>
#include <Planning/DisjunctionTask.h>
#include <Planning/LeafTask.h>
%}

f=StatePredicateFormula(x) -> [toplevelboolean: x
{
	f->task = x->task;
}];
f=Conjunction(x,y) provided(f->type != FORMULA_INITIAL) -> [toplevelboolean: x y 
{
	f->task = new ConjunctionTask(x->task,y->task);
}];

f=Disjunction(x,y) provided(f->type != FORMULA_INITIAL) -> [toplevelboolean: x y 
{
	f->task = new DisjunctionTask(x->task,y->task);
}];

f=AtomicProposition(*) ,
f=Negation(*),
f=Conjunction(*, *) provided(f->type == FORMULA_INITIAL),
f=Disjunction(*, *) provided(f->type == FORMULA_INITIAL) ,
f=ExclusiveDisjunction(*, *) ,
f=Implication(*, *) ,
f=Equivalence(*, *),
f=AllPath(*) ,
f=ExPath(*) ,
f=Always(*),
f=Eventually(*),
f=NextState(*),
f=Until(*,*) ,
f=Release(*,*),
f=EU(*,*) ,
f=AR(*,*) ,
f=ER(*,*),
f=AU(*,*),
f=AF(*) ,
f=AG(*),
f=AX(*),
f=EX(*),
f=EF(*),
f=EG(*) -> [toplevelboolean:
	{
		f -> task = new LeafTask(f);
	}];

//////////////////////////////////////////////////////////////////////////
%uview compound;

// This view is for translating a compound formula into 
//a hierarchy of tasks.

%{ KC_UNPARSE
#include <Core/Runtime.h>
#include <Frontend/Parser/ast-system-rk.h>
#include <vector>
#include <Planning/Task.h>
#include <Planning/CompoundTask.h>
#include <Planning/LeafTask.h>
%}


f=Compound(x,y) -> [compound: x y 
{
	f->task = new CompoundTask(x->task,y->task);
}];

f=StatePredicateFormula(*),
f=ComputeBound(*) -> [compound:
	{
		f -> task = new LeafTask(f);
	}];

f=AtomicProposition(*) ,
f=Negation(*),
f=Conjunction(*, *) provided(f->type == FORMULA_INITIAL),
f=Disjunction(*, *) provided(f->type == FORMULA_INITIAL) ,
f=ExclusiveDisjunction(*, *) ,
f=Implication(*, *) ,
f=Equivalence(*, *),
f=AllPath(*) ,
f=ExPath(*) ,
f=Always(*),
f=Eventually(*),
f=NextState(*),
f=Until(*,*) ,
f=Release(*,*),
f=EU(*,*) ,
f=AR(*,*) ,
f=ER(*,*),
f=AU(*,*),
f=AF(*) ,
f=AG(*),
f=AX(*),
f=EX(*),
f=EF(*),
f=EG(*) -> [compound:
	{
		f -> task = new LeafTask(f);
	}];

//////////////////////////////////////////////////////////////////////////
%uview problemwriter;

%{ KC_UNPARSE
#include <Core/Runtime.h>
#include <Net/Net.h>

#ifndef __cplusplus11
inline std::string int_to_string(int i) {
    std::stringstream s;
    s << i;
    return s.str();
}
#endif

bool is_first = false;
%}

StatePredicateFormula(AtomicProposition(x)) -> [problemwriter:
    "PROBLEM saraProblem:\n"
    "GOAL REACHABILITY;\n"
    "FILE "
    { kc_printer(RT::args.inputs[0], kc_current_view); }
    " TYPE LOLA;\n"
    "INITIAL "
    {
 	is_first = false;
        for (arrayindex_t i = 0; i < Net::Card[PL]; i++)
        {
            if (Marking::Initial[i] > 0)
            {
                if (!is_first)
                {
                    kc_printer(kc_t(Net::Name[PL][i]), kc_current_view);
#ifdef __cplusplus11                    
                    kc_printer((":" + std::to_string(Marking::Initial[i])).c_str(), 
                            kc_current_view);
#else
                    kc_printer((":" + int_to_string(Marking::Initial[i])).c_str(), 
                            kc_current_view);
#endif
                    is_first = true;
                }
                else
                {
                    kc_printer(",", kc_current_view);
                    kc_printer(kc_t(Net::Name[PL][i]), kc_current_view);
#ifdef __cplusplus11                    
                    kc_printer((":" + std::to_string(Marking::Initial[i])).c_str(), 
                            kc_current_view);
#else
                    kc_printer((":" + int_to_string(Marking::Initial[i])).c_str(), 
                            kc_current_view);
#endif
                }
            }
        }
    }
    ";\n"
    "FINAL COVER;\n"
    "CONSTRAINTS " x ";"];

StatePredicateFormula(ConjunctionList(l)) -> [problemwriter:
    "PROBLEM saraProblem:\n"
    "GOAL REACHABILITY;\n"
    "FILE "
    { kc_printer(RT::args.inputs[0], kc_current_view); }
    " TYPE LOLA;\n"
    "INITIAL "
    {
        for (arrayindex_t i = 0; i < Net::Card[PL]; i++)
        {
            if (Marking::Initial[i] > 0)
            {
                if (!is_first)
                {
                    kc_printer(kc_t(Net::Name[PL][i]), kc_current_view);
#ifdef __cplusplus11                    
                    kc_printer((":" + std::to_string(Marking::Initial[i])).c_str(), 
                            kc_current_view);
#else
                    kc_printer((":" + int_to_string(Marking::Initial[i])).c_str(), 
                            kc_current_view);
#endif
                    is_first = true;
                }
                else
                {
                    kc_printer(",", kc_current_view);
                    kc_printer(kc_t(Net::Name[PL][i]), kc_current_view);
#ifdef __cplusplus11                    
                    kc_printer((":" + std::to_string(Marking::Initial[i])).c_str(), 
                            kc_current_view);
#else
                    kc_printer((":" + int_to_string(Marking::Initial[i])).c_str(), 
                            kc_current_view);
#endif
                }
            }
        }
    }
    ";\n"
    "FINAL COVER;\n"
    "CONSTRAINTS " l ";"];
ConjunctionList(l) -> [problemwriter: l
];
ConstConjunction_list(h, NiltConjunction_list()) -> [problemwriter: h
]; 
ConstConjunction_list(h, l) -> [problemwriter: h "," l
];

StatePredicateFormula(DisjunctionList(l)) -> [problemwriter: l
];

ConstDisjunction_list(h, NiltDisjunction_list()) -> [problemwriter:
    "PROBLEM saraProblem:\n"
    "GOAL REACHABILITY;\n"
    "FILE "
    { kc_printer(RT::args.inputs[0], kc_current_view); }
    " TYPE LOLA;\n"
    "INITIAL "
    {
        
        // needs to be reseted first
        is_first = false;
        for (arrayindex_t i = 0; i < Net::Card[PL]; i++)
        {
            if (Marking::Initial[i] > 0)
            {
                if (!is_first)
                {
                    kc_printer(kc_t(Net::Name[PL][i]), kc_current_view);
#ifdef __cplusplus11                    
                    kc_printer((":" + std::to_string(Marking::Initial[i])).c_str(), 
                            kc_current_view);
#else
                    kc_printer((":" + int_to_string(Marking::Initial[i])).c_str(), 
                            kc_current_view);
#endif
                    is_first = true;
                }
                else
                {
                    kc_printer(",", kc_current_view);
                    kc_printer(kc_t(Net::Name[PL][i]), kc_current_view);
#ifdef __cplusplus11                    
                    kc_printer((":" + std::to_string(Marking::Initial[i])).c_str(), 
                            kc_current_view);
#else
                    kc_printer((":" + int_to_string(Marking::Initial[i])).c_str(), 
                            kc_current_view);
#endif
                }
            }
        }
    }
    ";\n"
    "FINAL COVER;\n"
    "CONSTRAINTS " h ";"];
  

ConstDisjunction_list(h, l) -> [problemwriter:
    "PROBLEM saraProblem:\n"
    "GOAL REACHABILITY;\n"
    "FILE "
    { kc_printer(RT::args.inputs[0], kc_current_view); }
    " TYPE LOLA;\n"
    "INITIAL "
    {
        // needs to be reseted first
        is_first = false;
        for (arrayindex_t i = 0; i < Net::Card[PL]; i++)
        {
            if (Marking::Initial[i] > 0)
            {
                
                if (!is_first)
                {
                    kc_printer(kc_t(Net::Name[PL][i]), kc_current_view);
#ifdef __cplusplus11                    
                    kc_printer((":" + std::to_string(Marking::Initial[i])).c_str(), 
                            kc_current_view);
#else
                    kc_printer((":" + int_to_string(Marking::Initial[i])).c_str(), 
                            kc_current_view);
#endif

                    is_first = true;
                }
                else
                {
                    kc_printer(",", kc_current_view);
                    kc_printer(kc_t(Net::Name[PL][i]), kc_current_view);
#ifdef __cplusplus11       
                    kc_printer((":" + std::to_string(Marking::Initial[i])).c_str(), 
                            kc_current_view);
#else
                    kc_printer((":" + int_to_string(Marking::Initial[i])).c_str(), 
                            kc_current_view);
#endif

                }
            }
        }
    }
    ";\n"
    "FINAL COVER;\n"
    "CONSTRAINTS " h ";\n\n"
    "RESULT OR formula;\n\n" l];

AtomicProposition(x) -> [problemwriter: x
];
LessEqualAtomicProposition(x,y) -> [problemwriter: x " < " y ];
GreaterAtomicProposition(x, Number(y)) -> [problemwriter: x " > " {y -> value = y -> value + 1;} y {y -> value = y -> value - 1;}];
Node(x) -> [problemwriter: { kc_printer(kc_t(Net::Name[PL][x->value]), kc_current_view); }];

Number(x) -> [problemwriter: x];
Product(x,y) provided (x->value == 1) -> [problemwriter: y];
Product(x,y) provided (x->value == -1) -> [problemwriter: "-1" y];
Product(x,y) -> [problemwriter: x y];
ConstProduct_list(h, NiltProduct_list()) -> [problemwriter: h];
ConstProduct_list(h=Product(x,*), l=ConstProduct_list(*,*)) provided(x->value < 0) -> [problemwriter: l " + " h];
ConstProduct_list(h, l) -> [problemwriter: l " + " h];

%uview orsAndAndsAndLength;

%{ KC_UNPARSE
%}

f=StatePredicateFormula(x) -> [orsAndAndsAndLength: x {

    f -> length = x -> length;
    f -> number_of_and = x -> number_of_and;
    f -> number_of_or = x -> number_of_or;
    f -> number_of_or_dnf = x -> number_of_or_dnf;
    f -> only_fireable = x -> only_fireable;

}];


f=AtomicProposition(x) -> [orsAndAndsAndLength: x {

    f -> length = 1;
    f -> number_of_and = 0;
    f -> number_of_or = 0;
    f -> number_of_or_dnf = 0;
    f -> only_fireable = x -> only_fireable;

}];

f=Negation(x) -> [orsAndAndsAndLength: x {

    f -> length = x -> length;
    f -> number_of_and = x -> number_of_and;
    f -> number_of_or = x -> number_of_or;
    f -> number_of_or_dnf = x -> number_of_or_dnf;
    f -> only_fireable = x -> only_fireable;

}];

f=Conjunction(l,r) ->[orsAndAndsAndLength: l r {

    f -> length = (l -> length) * (r -> number_of_or + 1) + (r -> length) * (l -> number_of_or + 1);
    f -> number_of_or_dnf = ((l -> number_of_or + 1)*(r -> number_of_or + 1) -1);
    f -> number_of_and = l -> number_of_and + r -> number_of_and + 1;
    f -> number_of_or = (l -> number_of_or ) + (r -> number_of_or );
    f -> only_fireable = l -> only_fireable && r -> only_fireable;

}];


f=Disjunction(l,r) -> [orsAndAndsAndLength: l r {

    f -> length = l -> length + r -> length;
    f -> number_of_or_dnf = l -> number_of_or + r -> number_of_or + 1;
    f -> number_of_and = l -> number_of_and + r -> number_of_and;
    f -> number_of_or = (l -> number_of_or ) + (r -> number_of_or ) + 1;
    f -> only_fireable = l -> only_fireable && r -> only_fireable;

}];

f=AllPath(x) -> [orsAndAndsAndLength: x {

    f -> length = x -> length;
    f -> number_of_and = x -> number_of_and;
    f -> number_of_or = x -> number_of_or;
    f -> only_fireable = x -> only_fireable;

}];

f=ExPath(x) -> [orsAndAndsAndLength: x {

    f -> length = x -> length;
    f -> number_of_and = x -> number_of_and;
    f -> number_of_or = x -> number_of_or;
    f -> only_fireable = x -> only_fireable;

}];

f=Always(x) -> [orsAndAndsAndLength: x {

    f -> length = x -> length;
    f -> number_of_and = x -> number_of_and;
    f -> number_of_or = x -> number_of_or;
    f -> only_fireable = x -> only_fireable;

}];

f=Eventually(x) -> [orsAndAndsAndLength: x {

    f -> length = x -> length;
    f -> number_of_and = x -> number_of_and;
    f -> number_of_or = x -> number_of_or;
    f -> only_fireable = x -> only_fireable;

}];

f=NextState(x) -> [orsAndAndsAndLength: x {

    f -> length = x -> length;
    f -> number_of_and = x -> number_of_and;
    f -> number_of_or = x -> number_of_or;
    f -> only_fireable = x -> only_fireable;

}];

f=Until(l,r) -> [orsAndAndsAndLength: l r {

    f -> length = l -> length + r -> length;
    f -> number_of_and = l -> number_of_and + f -> number_of_and;
    f -> number_of_or = (l -> number_of_or ) + (r -> number_of_or );
    f -> only_fireable = l -> only_fireable && r -> only_fireable;

}];

f=Release(l,r) -> [orsAndAndsAndLength: l r {

    f -> length = l -> length + r -> length;
    f -> number_of_and = l -> number_of_and + r -> number_of_and;
    f -> number_of_or = (l -> number_of_or ) + (r -> number_of_or );
    f -> only_fireable = l -> only_fireable && r -> only_fireable;

}];

f=GreaterAtomicProposition(l , r) -> [orsAndAndsAndLength: {

    f -> only_fireable = false;

}];

f=LessEqualAtomicProposition(l,r) -> [orsAndAndsAndLength: {

    f -> only_fireable = false;

}];

f=Deadlock() -> [orsAndAndsAndLength: {

    f -> only_fireable = false;

}];

f=Initial() -> [orsAndAndsAndLength: {

    f -> only_fireable = false;

}];

/*************************************************************************/
/*                   High Level Net                                      */
/*************************************************************************/

%uview hl_staticanalysis;

Net(c,d,p,m,t) -> [hl_staticanalysis: c d p m t]; 

EmptyDefinitionsList() -> [hl_staticanalysis: ];
DefinitionsList(l,d) -> [hl_staticanalysis: l d];

Constant(t,c,a,e) -> [hl_staticanalysis: t c a e 
{
	/* check whether assigned expression is constant */
	if(! (e->is_constant))
	{
		/* todo: error message */
	}
 	/* todo: determine required type for c from t and a */
	/* todo: check compatibility between required and inferred type */
	/* todo: check whether value of e is within boundaries */
	/* todo: create constant object */
	
}
];
Sort(t,c,a) -> [hl_staticanalysis: t c 
{
	a -> inherited_type = t;
}
a 
{
	/* todo: create type object from a -> type */
}
];
Function(t,c,a,f,b) -> [hl_staticanalysis: t c 
{
	a -> inherited_type = t;
}
a f b
{
	/* todo: create return type from a -> type*/
	/* todo: determine parameter list with types */
        /* todo: create function object */
}
];

TypeBool() -> [hl_staticanalysis: 
{
	size = 2;
	is_finite = true;
	is_scalar = true;
	/* todo: create type object */
}
];

TypeEnum(i) -> [hl_staticanalysis:  i
{
	size = i -> nrvalues;
	if(size == 0)
	{
		/* todo: error message */
	}
	is_finite = true;
	is_scalar = true;
	/* todo: create type object */
}
];

TypeIdent(c) -> [hl_staticanalysis:
{
	/* todo: retrieve type object from c */
	/* todo: set is_finite, is_scalar, and size */
}
];

TypeIntInterval(e,f) -> [hl_staticanalysis: e f 
{
	if(!(e->is_constant && f->is_constant))
	{
		/* todo: error message */
	}
	if(!(e -> is_scalar && f -> is_scalar))
	{
		/* todo: error message */
	}
	arrayindex_t left = *(reinterpret_cast<arrayindex_t *>(e->value));
	arrayindex_t right = *(reinterpret_cast<arrayindex_t *>(f->value));
	if(right < left)
	{
		/* todo: error message */
	}
	is_finite = true;
	is_scalar = true;
	size = right - left +1;
}
];
TypeStruct(s) -> [hl_staticanalysis: s
{
	size = s -> size;
	is_finite = s -> is_finite;
	is_scalar = false;
}
];
TypeInt() -> [hl_staticanalysis:
{
	is_finite = false;
	is_scalar = true;
	size = 0; /* irrelevant */
}
];
TypeMultiset(t) -> [hl_staticanalysis: t
{
	if(! (t->is_finite))
	{
		/* todo: error message */
	}
	is_finite = false;
	is_scalar = false;
	size = 0; /* irrelevant */
}
];
TypeBlack()  -> [hl_staticanalysis: 
{
	is_finite = true;
	is_scalar = true;
	size = 1;
}
];

EmptyOptNumber() -> [hl_staticanalysis:
{
	value = ARRAYINDEX_T_MAX;
	set = false;
}
];

OptNumber(i) -> [hl_staticanalysis:
{
	value = i-> value;
	set = true;
}
];

EmptyIdentList() -> [hl_staticanalysis: 
{
	maxvalue = 0;
	nrvalues = 0;
}
];

IdentList(i,d) -> [hl_staticanalysis: i d
{
	nrvalues = i -> nrvalues + 1;
	if(d -> value <= i -> maxvalue)
	{
		/* todo: error message */
	}
	if(!(d -> set))
	{
		d -> value = i -> maxvalue + 1;
	}
	/* todo: record d -> value in symboltable of symbol d -> c */
	maxvalue = d -> value;
	
}
];

Idents(c,o) -> [hl_staticanalysis: c o
{
	value = o -> value;
	set = o -> set;
}
];

EmptyStructTypeList() -> [hl_staticanalysis:
{
	size = 0;
	nrcomponents = 0;
	is_finite = true;
}
];
StructTypeList(l,s) -> [hl_staticanalysis: l s
{
	size = l -> size * s -> size;
	nrcomponents = l -> nrcomponents + 1;
	is_finite = (l -> is_finite) && (s -> is_finite);
}
];

StructType(t,c,a) -> [hl_staticanalysis: t c 
{
	a -> inherited_type = t;
}
a
{
	size = t -> size * a -> nrelements;
	is_finite = t -> is_finite;
	is_scalar = false;
}
];

VarOrArray(c,a) -> [hl_staticanalysis: 
{
	a -> inherited_type = inherited_type;
}
c a 
{
	type = a -> type;
}
];


EmptyArrayList() -> [hl_staticanalysis:
{
	nrdimensions = 0;
	nrelements = 1;
	type = inherited_type;
}
];
ArrayList(a,e) -> [hl_staticanalysis: 
{
	a -> inherited_type = inherited_type;
}
a e 
{
	if(!(e -> is_constant))
	{
		/* todo: error message */
	}
	if(e -> value == 0)
	{
		/* todo: error message */
	}
	nrdimensions = a -> nrdimensions + 1;
	// todo: nrelements = (a -> nrelements) * reinterpret_cast<int>(e -> value[0]);
	// todo: type = kc::TypeArray(a -> type, e -> value);
	type -> size = nrelements * a -> type -> size;
	type -> is_finite = a -> type -> is_finite;
	type -> is_scalar = false;
	/* todo: test for arrays with too many elements */
}
];
/*
functionParametersList:
  EmptyFunctionParametersList()
| FunctionParametersList(functionParameters functionParametersList)
;

functionParameters:
  FunctionParameters(type varOrArray)
;

expression:
  ExprLeftvalue(leftvalue)
| ExprInitializerList(initializerList)
| AssignEqual(leftvalue expression)
| AssignPlus(leftvalue expression)
| AssignMinus(leftvalue expression)
| AssignTimes(leftvalue expression)
| AssignDivide(leftvalue expression)
| AssignMod(leftvalue expression)
| IncrementVal(leftvalue)
| DecrementVal(leftvalue)
| ValIncrement(leftvalue)
| ValDecrement(leftvalue)
| PositiveExpr(expression)
| NegativeExpr(expression)
| ExprAddition(expression expression)
| ExprSubtraction(expression expression)
| ExprMultiplication(expression expression)
| ExprDivision(expression expression)
| ExprModulo(expression expression)
| ExprNumber(integer)
| NotExpr(expression)
| ExprAnd(expression expression)
| ExprOr(expression expression)
| ExprTrue()
| ExprFalse()
| ExprEquivalent(expression expression)
| ExprNotEqual(expression expression)
| ExprLessThan(expression expression)
| ExprGreaterThan(expression expression)
| ExprLessOrEqual(expression expression)
| ExprGreaterOrEqual(expression expression)
| FunctionCall(casestring expressionlist)
| ExprCommaSeparated(expressionlist)
| ExprOtherIf(expression expression expression)
| ExprAll(type)
{ checkName check = UNDEF; }
;


leftvalue:
  LeftValIdent(casestring)       
| LeftValBrackets(leftvalue expression)
| LeftValDot(leftvalue casestring)
;

expressionlist:
   EmptyExpressionList()
|  ExpressionList(expression expressionlist)
;

initializerList:
  ExprInBraces(expressionlist)
| ExprInBracesColon(expression expression expressionListColon)
;

expressionListColon:
  EmptyExpressionListColon()
| ExpressionListColon(expression expression expressionListColon)
;

placeblocklist:
  EmptyPlaceBlockList()
| PlaceBlockList(placeblock placeblocklist)
;

optSafe:
  EmptySafe()
| Safe(integer)
;

place:
  Place(casestring)
;

placelist:
  EmptyPlaceList()
| PlaceList(place placelist)
;

placeblock:
  PlaceBlock(optSafe type placelist)
;

marking:
  EmptyMarking()
| Marking(casestring expression marking)
;

transition:
  EmptyTransition()
| Transition(casestring fairness variable guard identExprList identExprList transition)
;

fairness:
  EmptyFairness()
| WeakFair()
| StrongFair()
;

variable:
  EmptyVariable()
| Variable(type varOrArrayList variable)
;

varOrArrayList:
  EmptyVarOrArrayList()
| VarOrArrayList(varOrArray varOrArrayList)
;

guard:
 Guard(expression)
;


identExprList:
  EmptyIdentExprList()
| IdentExprList(casestring expression identExprList)
;

block:
  Block(declarationOrStatement)
;

declarationOrStatement:
  EmptyDeclarationOrStatement()
| DeclOrStatemDeclaration(declaration declarationOrStatement)
| DeclOrStatemStatement(statement declarationOrStatement)
;

declaration: 
  Declaration(type varOrArrayList)
;

statement:
  StatementBlock(block)
| StatementExprSemicolon(expression)
| StatementIf(expression statement statement)
| StatementWhile(expression statement)
| StatementDoWhile(statement expression)
| StatementForExpr(expression expression expression statement)
| StatementForTypeExpr(type casestring expression expression expression statement)
| StatementForIdentColon(casestring expression statement)
| StatementForAll(type casestring type)
| StatementSwitch(expression switchCaseList)
| StatementBreak()
| StatementContinue()
| StatementReturn(expression)
| StatementSkip()
;

switchCase:
  SwitchCase(expression statement)
| SwitchDefault(statement)
;

switchCaseList:
  EmptySwitchCaseList()
| SwitchCaseList(switchCase switchCaseList)
;
*/

%uview countdeadlock;

StatePredicateFormula(x) -> [countdeadlock: x {containsDeadlock = x -> containsDeadlock;}];
Compound(x,y) -> [countdeadlock: x y {containsDeadlock = x -> containsDeadlock + y -> containsDeadlock;}];
ComputeBound(LessEqualAtomicProposition(x,y)) -> [countdeadlock: {containsDeadlock = 0;} ];

AtomicProposition(x) -> [countdeadlock: x {containsDeadlock = x -> containsDeadlock;}];
Negation(x) -> [countdeadlock: x {containsDeadlock = x -> containsDeadlock;}];
Conjunction(x, y) -> [countdeadlock: x y {containsDeadlock = x -> containsDeadlock + y -> containsDeadlock;}];
Disjunction(x, y) -> [countdeadlock:  x y {containsDeadlock = x -> containsDeadlock + y -> containsDeadlock;}];
ExclusiveDisjunction(x, y) -> [countdeadlock: x y {containsDeadlock = x -> containsDeadlock + y -> containsDeadlock;}];
Implication(x, y) -> [countdeadlock: x y {containsDeadlock = x -> containsDeadlock + y -> containsDeadlock;}];
Equivalence(x, y) -> [countdeadlock: x y {containsDeadlock = x -> containsDeadlock + y -> containsDeadlock;}];
AllPath(x) -> [countdeadlock: x {containsDeadlock = x -> containsDeadlock;}];
ExPath(x)  -> [countdeadlock: x {containsDeadlock = x -> containsDeadlock;}];
Always(x)     -> [countdeadlock: x {containsDeadlock = x -> containsDeadlock;}];
Eventually(x) -> [countdeadlock:  x {containsDeadlock = x -> containsDeadlock;}];
NextState(x) -> [countdeadlock: x {containsDeadlock = x -> containsDeadlock;}];
Until(phi,psi) -> [countdeadlock: phi psi {containsDeadlock = phi -> containsDeadlock + psi -> containsDeadlock;}];
Release(phi,psi) -> [countdeadlock: phi psi {containsDeadlock = phi->containsDeadlock + psi -> containsDeadlock;}];
EU(x,y) -> [countdeadlock: x y {containsDeadlock = x -> containsDeadlock + y -> containsDeadlock;}];
AR(x,y) -> [countdeadlock: x y {containsDeadlock = x -> containsDeadlock + y -> containsDeadlock;}];
ER(x,y) -> [countdeadlock: x y {containsDeadlock = x -> containsDeadlock + y -> containsDeadlock;}];
AU(x,y) -> [countdeadlock: x y {containsDeadlock = x -> containsDeadlock + y -> containsDeadlock;}];
AF(x)   -> [countdeadlock: x {containsDeadlock = x -> containsDeadlock;}];
AG(x)   -> [countdeadlock: x {containsDeadlock = x -> containsDeadlock;}];
AX(x)   -> [countdeadlock: x {containsDeadlock = x -> containsDeadlock;}];
EX(x)   -> [countdeadlock: x {containsDeadlock = x -> containsDeadlock;}];
EF(x)   -> [countdeadlock: x {containsDeadlock = x -> containsDeadlock;}];
EG(x)   -> [countdeadlock: x {containsDeadlock = x -> containsDeadlock;}];

ConjunctionList(l) -> [countdeadlock: l {containsDeadlock = l -> containsDeadlock;}];

NiltConjunction_list() -> [countdeadlock: {containsDeadlock = 0;}];
ConstConjunction_list(h, l) -> [countdeadlock: h l {containsDeadlock = h -> containsDeadlock + l -> containsDeadlock;}];

DisjunctionList(l) -> [countdeadlock: l {containsDeadlock = l -> containsDeadlock;}];
NiltDisjunction_list() -> [countdeadlock: {containsDeadlock = 0;}];
ConstDisjunction_list(h, l) -> [countdeadlock: h l {containsDeadlock = h -> containsDeadlock + l -> containsDeadlock;}];


EqualsAtomicProposition(x, y) -> [countdeadlock: {containsDeadlock = 0;}];
NotEqualsAtomicProposition(x, y) -> [countdeadlock: {containsDeadlock = 0;}];
GreaterAtomicProposition(x, y) -> [countdeadlock: {containsDeadlock = 0;}];
GreaterEqualAtomicProposition(x, y) -> [countdeadlock: {containsDeadlock = 0;}];
LessAtomicProposition(x, y) -> [countdeadlock: {containsDeadlock = 0;}];
LessEqualAtomicProposition(x, y) -> [countdeadlock: {containsDeadlock = 0;}];
True() -> [countdeadlock: {containsDeadlock = 0;}];
False() -> [countdeadlock: {containsDeadlock = 0;}];
Initial() -> [countdeadlock: {containsDeadlock = 0;}];
Deadlock() -> [countdeadlock: {containsDeadlock = 1;}];
NoDeadlock() -> [countdeadlock: {containsDeadlock = 1;}];
Fireable(x)   -> [countdeadlock: {containsDeadlock = 0;}];
Unfireable(x)   -> [countdeadlock: {containsDeadlock = 0;}];

Node(*) -> [countdeadlock: ];
Number(*) -> [countdeadlock: ];
Sum(*,*) -> [countdeadlock: ];
Difference(*,*) -> [countdeadlock: ];
Product(*,*) -> [countdeadlock: ];
NiltProduct_list() -> [countdeadlock: ];
ConstProduct_list(*, *) -> [countdeadlock: ];

