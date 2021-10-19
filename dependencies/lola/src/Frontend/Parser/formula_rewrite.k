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

%{ KC_REWRITE_HEADER
/// variable counting the rule applications in the rewrite rule sets
extern size_t rule_applications;
%}

%{ KC_REWRITE /* code redirection */
#include <Frontend/Parser/ast-system-rk.h>
#include <CoverGraph/CoverGraph.h>
#include <Net/Marking.h>
#include <Net/Net.h>

/*
Uncomment the next line to define a debug function that traces all matches in
the rewriting process.
*/
//#define KC_TRACE_REWRITE_RESULT(VIEW,FILE,LINE,NODE) { RT::rep->status("rewrite %s:%d", FILE, LINE); }

/*!
Count the rule applications
*/
size_t rule_applications = 0;
#define KC_TRACE_REWRITE_MATCH(VIEW,FILE,LINE,NODE) { ++rule_applications; }

integer plus( integer a, integer b ) {
    // +-----+-----+-----+-----+
    // |   + |   y |  oo | -oo |
    // +-----+-----+-----+-----+
    // |   x | x+y |  oo | -oo |
    // |  oo |  oo |  oo | n.d.|
    // | -oo | -oo | n.d.| -oo |
    // +-----+-----+-----+-----+

    // exclude undefinied behavior
    if(  (a->value == -OMEGA and b->value == OMEGA) ||
         (a->value == OMEGA and b->value == -OMEGA))
    {
	RT::rep->status("error in formula: invalid addition of oo or appearance of oo on both sides of a comparison");
        RT::rep->abort(ERROR_SYNTAX);
    }

    if (a->value == OMEGA or b->value == OMEGA)
    {
        return mkinteger(OMEGA);
    }

    if (a->value == -OMEGA or b->value == -OMEGA)
    {
        return mkinteger(-OMEGA);
    }
    
    return mkinteger( a->value + b->value );
}

integer minus( integer a, integer b ) {
    // +-----+-----+-----+-----+
    // |   - |   y |  oo | -oo |
    // +-----+-----+-----+-----+
    // |   x | x-y | -oo |  oo |
    // |  oo |  oo | n.d.|  oo |
    // | -oo | -oo | -oo | n.d.|
    // +-----+-----+-----+-----+

    // exclude undefinied behavior
    if(  (a->value == OMEGA and b->value == OMEGA) ||
         (a->value == -OMEGA and b->value == -OMEGA))
    {
	RT::rep->status("error in formula: invalid subtraction of oo or appearance of oo on both sides of a comparison");
        RT::rep->abort(ERROR_SYNTAX);
    }

    if (a->value == OMEGA or b->value == -OMEGA)
    {
        return mkinteger(OMEGA);
    }

    if (a->value == -OMEGA or b->value == OMEGA)
    {
        return mkinteger(-OMEGA);
    }

    return mkinteger( a->value - b->value );
}

integer mult( integer a, integer b ) {
    // exclude OMEGAS for now
    if(a->value == OMEGA || b->value == OMEGA || a->value == -OMEGA || b->value == -OMEGA)
    {
	RT::rep->status("error in formula: invalid multiplication with oo");
        RT::rep->abort(ERROR_SYNTAX);
    }

    return mkinteger( a->value * b->value );
}

/*!
This function unfolds the FIREABLE() predicate to a conjunction that contains,
for each preplace, a term from the arc weight.
\only GreaterAtomicProposition will be performed
\param x  the id of a transition
\return  a statepredicate that is true if the given transition is fireable
*/
tStatePredicate FIREABLE_unfolder(const integer x)
{
    // start with TRUE and conjunct an atomic proposition for each preplace
    // (the TRUE predicate will be removed in later rewriting %rview tautology;)
    tStatePredicate result = AtomicProposition(True());

    // iterate preset
    for (arrayindex_t p = 0; p < Net::CardArcs[TR][PRE][x->value]; ++p)
    {
        result = Conjunction(
            result,
            AtomicProposition(
                GreaterAtomicProposition(
                    Node(mkinteger(Net::Arc[TR][PRE][x->value][p])),
                    Number(minus(mkinteger(Net::Mult[TR][PRE][x->value][p]),mkinteger(1)))
                )
            )
        );
    }

    return result;
}

/*! This fuction unfolds the Unfireable() predicate to a disjunction that contains, for each preplace, a term from the arc weight.
\only LessEqualAtomicProposition will be performed
\parm x the id of a transition
\return a statepredicate that is true if the given transition is not fireable
*/
tStatePredicate UNFIREABLE_unfolder(const integer x)
{
    // start with FALSE and disjunct an atomic proposition for each preplace
    // (the FALSE predicate will be removed in later rewriting %rview tautology;)
    tStatePredicate result = AtomicProposition(False());

    // iterate preset
    for (arrayindex_t p = 0; p < Net::CardArcs[TR][PRE][x->value]; ++p)
    {
        result = Disjunction(
            result,
            AtomicProposition(
                LessEqualAtomicProposition(
                    Node(mkinteger(Net::Arc[TR][PRE][x->value][p])),
                    Number(minus(mkinteger(Net::Mult[TR][PRE][x->value][p]),mkinteger(1)))
                )
            )
        );
    }

    return result;
}

/*!
This function unfolds the INITIAL() predicate to a conjunction that contains
for each place a term from its initial marking.

\return  a statpredicate that is exactly true in the initial marking
*/
tStatePredicate INITIAL_unfolder()
{
    // start with TRUE and conjunct an atomic proposition for each place
    // (the TRUE predicate will be removed in later rewritings)
    tStatePredicate result = AtomicProposition(True());
    
    // iterate places
    for (arrayindex_t p = 0; p < Net::Card[PL]; ++p)
    {
        result = Conjunction(
            result,
            AtomicProposition(
                EqualsAtomicProposition(
                    Node(mkinteger(p)),
                    Number(mkinteger(Marking::Initial[p]))
                )
            )
        );
    }

    return result;
}

%}

/*****************************************************
 * FORMULA REWRITING PHASE I: REMOVE SYNTACTIC SUGAR *
 *****************************************************/

/* This phase consists of several rule sets to be executed 
   sequentially. Each rule set is responsible for removing
   one of the following elements:
   - double arrows
   - single arrows
   - xor
   //- fireable
   - initial
   Applying all rule sets in the given order results in a 
   formula tree not containing any of these features.
*/

%rview goodbye_doublearrows;

// (x <=> y)    ->   (x OR !y) AND (!x OR y)
Equivalence(x, y) -> <goodbye_doublearrows: 
      Conjunction(Disjunction(x,Negation(y)), Disjunction(Negation(x),y))>;

%rview goodbye_singlearrows;

// (x => y)    ->   (!x OR y)
Implication(x, y) -> <goodbye_singlearrows: Disjunction(Negation(x), y)>;

%rview goodbye_xor;

// (x XOR y)   ->   ((x AND !y) OR (!x AND y))
ExclusiveDisjunction(x, y) -> <goodbye_xor: Disjunction(Conjunction(x, Negation(y)), Conjunction(Negation(x), y))>;

%rview goodbye_fireable;

// unfold FIREABLE predicates
AtomicProposition(Fireable(x)) -> <goodbye_fireable: FIREABLE_unfolder(x) >;

%rview goodbye_unfireable;

// unfold UNFIREABLE predicates
AtomicProposition(Unfireable(x)) -> <goodbye_unfireable: UNFIREABLE_unfolder(x) >;

%rview goodbye_initial;

// unfold INITIAL predicates
AtomicProposition(Initial()) -> <goodbye_initial: INITIAL_unfolder() >;

/*********************************************************
 * PHASE 2: Normalize comparisons in atomic propositions *
 *********************************************************/

/* This phase consists of three rule sets. It normalizes comparisions
   to the following normal form:
ruleset sides:
   - left of the comparison operator is a sum where each operand 
     is a product of multiplicity and term
   - right of the comparison operator is a single number
ruleset productlists:
   - no place occurs twice in the sum left of the comparison operator
   - If the sum left of the comparison is empty, the whole proposition is
     turned into true or false.
ruleset leq:
   - Only <= and > appear as comparison operators

   After applying these rule sets, all atomic comparison propositions will be
   in this normal form.

   The rules sets are to be applied in sequence. This way, less efforts are
   needed on = an != comparisons which would otherwise double the number
   of rule application.
*/

%rview sides;

// calculate values if possible
// This is deliberately first as it shrinks the overall formula tree

Sum(Number(x), Number(y)) -> <sides: Number(plus(x,y))>;
Sum(Number(x), Sum(Number(y), z)) -> <sides: Sum(Number(plus(x,y)), z)>;
Sum(Number(x), y) provided (x->value == 0) -> <sides: y>;
Product(x, *) provided (x->value == 0) -> <sides: Number(mkinteger(0))>;
Product(x, y=Sum(*,*)) provided (x->value == 1) -> <sides: y>;
Product(x, Number(y)) -> <sides: Number(mult(x,y))>;

// take right side to left

LessEqualAtomicProposition(x, s=Sum(*,*))     -> <sides: LessEqualAtomicProposition(Difference(x, s), Number(mkinteger(0)))>;
LessAtomicProposition(x, s=Sum(*,*))     -> <sides: LessAtomicProposition(Difference(x, s), Number(mkinteger(0)))>;
GreaterEqualAtomicProposition(x, s=Sum(*,*))     -> <sides: GreaterEqualAtomicProposition(Difference(x, s), Number(mkinteger(0)))>;
GreaterAtomicProposition(x, s=Sum(*,*))     -> <sides: GreaterAtomicProposition(Difference(x, s), Number(mkinteger(0)))>;
EqualsAtomicProposition(x, s=Sum(*,*))     -> <sides: EqualsAtomicProposition(Difference(x, s), Number(mkinteger(0)))>;
NotEqualsAtomicProposition(x, s=Sum(*,*))     -> <sides: NotEqualsAtomicProposition(Difference(x, s), Number(mkinteger(0)))>;

LessEqualAtomicProposition(x, p=Product(*,*)) -> <sides: LessEqualAtomicProposition(Difference(x, p), Number(mkinteger(0)))>;
LessAtomicProposition(x, p=Product(*,*)) -> <sides: LessAtomicProposition(Difference(x, p), Number(mkinteger(0)))>;
GreaterEqualAtomicProposition(x, p=Product(*,*)) -> <sides: GreaterEqualAtomicProposition(Difference(x, p), Number(mkinteger(0)))>;
GreaterAtomicProposition(x, p=Product(*,*)) -> <sides: GreaterAtomicProposition(Difference(x, p), Number(mkinteger(0)))>;
EqualsAtomicProposition(x, p=Product(*,*)) -> <sides: EqualsAtomicProposition(Difference(x, p), Number(mkinteger(0)))>;
NotEqualsAtomicProposition(x, p=Product(*,*)) -> <sides: NotEqualsAtomicProposition(Difference(x, p), Number(mkinteger(0)))>;

LessEqualAtomicProposition(x, n=Node(*))      -> <sides: LessEqualAtomicProposition(Difference(x, n), Number(mkinteger(0)))>;
LessAtomicProposition(x, n=Node(*))      -> <sides: LessAtomicProposition(Difference(x, n), Number(mkinteger(0)))>;
GreaterEqualAtomicProposition(x, n=Node(*))      -> <sides: GreaterEqualAtomicProposition(Difference(x, n), Number(mkinteger(0)))>;
GreaterAtomicProposition(x, n=Node(*))      -> <sides: GreaterAtomicProposition(Difference(x, n), Number(mkinteger(0)))>;
EqualsAtomicProposition(x, n=Node(*))      -> <sides: EqualsAtomicProposition(Difference(x, n), Number(mkinteger(0)))>;
NotEqualsAtomicProposition(x, n=Node(*))      -> <sides: NotEqualsAtomicProposition(Difference(x, n), Number(mkinteger(0)))>;

// take numbers back to right

LessEqualAtomicProposition(Sum(Number(x), y), Number(z)) -> <sides: LessEqualAtomicProposition(y, Number(minus(z,x)))>;
LessAtomicProposition(Sum(Number(x), y), Number(z)) -> <sides: LessAtomicProposition(y, Number(minus(z,x)))>;
GreaterEqualAtomicProposition(Sum(Number(x), y), Number(z)) -> <sides: GreaterEqualAtomicProposition(y, Number(minus(z,x)))>;
GreaterAtomicProposition(Sum(Number(x), y), Number(z)) -> <sides: GreaterAtomicProposition(y, Number(minus(z,x)))>;
EqualsAtomicProposition(Sum(Number(x), y), Number(z)) -> <sides: EqualsAtomicProposition(y, Number(minus(z,x)))>;
NotEqualsAtomicProposition(Sum(Number(x), y), Number(z)) -> <sides: NotEqualsAtomicProposition(y, Number(minus(z,x)))>;

// any node should be multiplied by 1 to have products everywhere

LessEqualAtomicProposition(n=Node(*), m=Number(*)) -> <sides: LessEqualAtomicProposition(Product(mkinteger(1), n), m)>;
LessAtomicProposition(n=Node(*), m=Number(*)) -> <sides: LessAtomicProposition(Product(mkinteger(1), n), m)>;
GreaterEqualAtomicProposition(n=Node(*), m=Number(*)) -> <sides: GreaterEqualAtomicProposition(Product(mkinteger(1), n), m)>;
GreaterAtomicProposition(n=Node(*), m=Number(*)) -> <sides: GreaterAtomicProposition(Product(mkinteger(1), n), m)>;
EqualsAtomicProposition(n=Node(*), m=Number(*)) -> <sides: EqualsAtomicProposition(Product(mkinteger(1), n), m)>;
NotEqualsAtomicProposition(n=Node(*), m=Number(*)) -> <sides: NotEqualsAtomicProposition(Product(mkinteger(1), n), m)>;

Sum(n=Node(*),y) -> <sides: Sum(Product(mkinteger(1),n),y)>;
Sum(x,n=Node(*)) -> <sides: Sum(x,Product(mkinteger(1),n))>;

// replace differences by sums

Difference(x,y) -> <sides: Sum(x, Product(mkinteger(-1), y))>;

// move numbers to left

Sum(x, n=Number(*)) -> <sides: Sum(n, x)>;
Sum(x, Sum(n=Number(*), z)) -> <sides: Sum(n, Sum(x,z))>;

// move parentheses

Sum(Sum(x,y), z) -> <sides: Sum(x, Sum(y, z))>;

// calculate nested multiplications

Product(x, Product(y, z)) -> <sides: Product(mult(x,y), z)>;
Product(x, Sum(Number(y), z)) -> <sides: Sum(Number(mult(x,y)), Product(x, z))>;
Product(x, Sum(y,z)) -> <sides: Sum(Product(x,y), Product(x,z))>;

/*
Special care has to be taken if OMEGA occurs in the formulae.
*/

// special rules to treat OMEGA 
// They appear in the end of rule set "sides" for making sure that everything
// else has been sorted out

// x < oo  ->  x <= finite
LessAtomicProposition(x,Number(y)) provided (y->value == OMEGA) -> <sides: LessEqualAtomicProposition(x, Number(mkinteger(FINITE))) >;

// x > oo   ->  FALSE
GreaterAtomicProposition(*,Number(y)) provided (y->value == OMEGA) -> <sides: False()>;

// x <= oo  ->  TRUE
LessEqualAtomicProposition(*,Number(y)) provided (y->value == OMEGA) -> <sides: True()>;

// x >= oo  ->  -x <= -oo
GreaterEqualAtomicProposition(x,Number(y)) provided (y->value == OMEGA) -> <sides: LessEqualAtomicProposition(Product(mkinteger(-1), x), Number(minus(mkinteger(0), y))) >;

// x == oo  ->  -x <= -oo
EqualsAtomicProposition(x,Number(y)) provided (y->value == OMEGA) -> <sides: LessEqualAtomicProposition(Product(mkinteger(-1), x), Number(minus(mkinteger(0), y))) >;
// x != oo  ->  x <= finite
NotEqualsAtomicProposition(x,Number(y)) provided (y->value == OMEGA) -> <sides: LessEqualAtomicProposition(x, Number(mkinteger(FINITE))) >;

// if only numbers are left, we can immediately calculate the formula's value
LessEqualAtomicProposition(Number(x), Number(y)) provided (x->value <= y->value) -> <sides: True()>;
LessEqualAtomicProposition(Number(x), Number(y)) provided (x->value > y->value) -> <sides: False()>;
LessAtomicProposition(Number(x), Number(y)) provided (x->value < y->value) -> <sides: True()>;
LessAtomicProposition(Number(x), Number(y)) provided (x->value >= y->value) -> <sides: False()>;
GreaterEqualAtomicProposition(Number(x), Number(y)) provided (x->value >= y->value) -> <sides: True()>;
GreaterEqualAtomicProposition(Number(x), Number(y)) provided (x->value < y->value) -> <sides: False()>;
GreaterAtomicProposition(Number(x), Number(y)) provided (x->value > y->value) -> <sides: True()>;
GreaterAtomicProposition(Number(x), Number(y)) provided (x->value <= y->value) -> <sides: False()>;
EqualsAtomicProposition(Number(x), Number(y)) provided (x->value == y->value) -> <sides: True()>;
EqualsAtomicProposition(Number(x), Number(y)) provided (x->value != y->value) -> <sides: False()>;
NotEqualsAtomicProposition(Number(x), Number(y)) provided (x->value != y->value) -> <sides: True()>;
NotEqualsAtomicProposition(Number(x), Number(y)) provided (x->value == y->value) -> <sides: False()>;

%rview productlists;

// collect sums of products in a product list
Sum(x,y) -> <productlists: ProductList(ConstProduct_list(x, ConstProduct_list(y, NiltProduct_list())))>;

// make sure we don't have ProductList within the list (needed for sorting)
ConstProduct_list(ProductList(x), y) -> <productlists: concat(x,y)>;

// bubble sort the product list w.r.t. the node ids
ConstProduct_list(p1=Product(*, Node(pid1)), ConstProduct_list(p2=Product(*, Node(pid2)), l)) provided (pid1->value > pid2->value) -> <productlists: ConstProduct_list(p2, ConstProduct_list(p1, l)) >;

// combine products that affect the same node id
ConstProduct_list(Product(x,Node(u)), ConstProduct_list(Product(y,Node(u)), l)) -> <productlists: ConstProduct_list(Product(plus(x,y),Node(u)), l) >;

// cope with occurring zeros
Product(x, *) provided (x->value == 0) -> <productlists: Number(mkinteger(0))>;
ConstProduct_list(Number(x), y) provided (x->value == 0) -> <productlists: y>;
ProductList(NiltProduct_list()) -> <productlists: Number(mkinteger(0)) >;

%rview leq;

// Precondition: comparisons are normalized, i.e. right operand is number.
// By the OMEGA rules in set "sides", we have y != OMEGA, y!= FINITE

// x == y   ->   x <= y && x > y-1 
AtomicProposition(EqualsAtomicProposition(x,n=Number(y))) -> <leq: Conjunction(AtomicProposition(LessEqualAtomicProposition(x,n)), AtomicProposition(GreaterAtomicProposition(x,Number(minus(y,mkinteger(1))))))>;

// x != y   ->   x <= y-1 || x > y
AtomicProposition(NotEqualsAtomicProposition(x,n=Number(y))) -> <leq: Disjunction(AtomicProposition(LessEqualAtomicProposition(x, Number(minus(y,mkinteger(1))))), AtomicProposition(GreaterAtomicProposition(x,n)))>;

// x >= y   ->   x > y-1
GreaterEqualAtomicProposition(x,Number(y)) -> <leq: GreaterAtomicProposition(x,Number(minus(y,mkinteger(1))))>;

// x < y    ->   x <= y-1
LessAtomicProposition(x,Number(y)) -> <leq: LessEqualAtomicProposition(x, Number(minus(y,mkinteger(1))))>;

// rule out constant formulas

// n <= m --> TRUE/FALSE
LessEqualAtomicProposition(Number(x), Number(y)) provided (x->value <= y->value) -> <leq: True()>;
LessEqualAtomicProposition(Number(x), Number(y)) provided (x->value > y->value) -> <leq: False()>;

// m > m --> TRUE/FALSE
GreaterAtomicProposition(Number(x), Number(y)) provided (x->value > y->value) -> <leq: True()>;
GreaterAtomicProposition(Number(x), Number(y)) provided (x->value <= y->value) -> <leq: False()>;
/*********************************************************************
 * PHASE 3: APPLY LOGICAL TAUTOLOGIES AND MOVE CTL* TO CTL OPERATORS *
 *********************************************************************/

/*
This phase consists of just one rule set since all rule applications may
make other rules applicable again. For better readability, we sort the
rules by various themes.
*/

// Theme 1: Boolean operators vs. TRUE/FALSE
// These are first since tree gets smaller
// *****************************************

%rview tautology;

// AND + TRUE/FALSE
Conjunction(a, AtomicProposition(True())) -> <tautology: a >;
Conjunction(AtomicProposition(True()), a) -> <tautology: a >;
Conjunction(*, f = AtomicProposition(False())) -> <tautology: f >;
Conjunction(f = AtomicProposition(False()), *) -> <tautology: f >;

// OR + TRUE/FALSE
Disjunction(a, AtomicProposition(False())) -> <tautology: a >;
Disjunction(AtomicProposition(False()), a) -> <tautology: a >;
Disjunction(*, t=AtomicProposition(True())) -> <tautology: t >;
Disjunction(t=AtomicProposition(True()), *) -> <tautology: t >;

// Theme 2: Temporal operators vs. TRUE/FALSE
// ******************************************

// F TRUE -> TRUE
Eventually(x=AtomicProposition(True())) -> <tautology: x>;

// F FALSE -> FALSE
Eventually(x=AtomicProposition(False())) -> <tautology: x>;

// G TRUE -> TRUE
Always(x=AtomicProposition(True())) -> <tautology: x>;

// G FALSE -> FALSE
Always(x=AtomicProposition(False())) -> <tautology: x>;

// X TRUE -> TRUE
NextState(x=AtomicProposition(True())) -> <tautology: x>;

// X FALSE -> FALSE
NextState(x=AtomicProposition(False())) -> <tautology: x>;

// E TRUE -> TRUE
ExPath(x=AtomicProposition(True())) -> <tautology: x>;

// E FALSE -> FALSE
ExPath(x=AtomicProposition(False())) -> <tautology: x>;

// A TRUE -> TRUE
AllPath(x=AtomicProposition(True())) -> <tautology: x>;

// A FALSE -> FALSE
AllPath(x=AtomicProposition(False())) -> <tautology: x>;

// x U TRUE <-> TRUE
Until(*,y=AtomicProposition(True())) -> <tautology: y>;

// TRUE U x <-> F x
Until(AtomicProposition(True()), x) -> <tautology: Eventually(x)>;

// x R TRUE -> TRUE
Release(*,y=AtomicProposition(True())) -> <tautology: y>;

// TRUE R x <-> x
Release(AtomicProposition(True()), x) -> <tautology: x>;

// x U FALSE -> FALSE
Until(*,y=AtomicProposition(False())) -> <tautology: y>;

// FALSE U x <-> x
Until(AtomicProposition(False()), x) -> <tautology: x>;

// FALSE R x <-> G x
Release(AtomicProposition(False()), x) -> <tautology: Always(x)>;

// x R FALSE <-> FALSE
Release(*,y=AtomicProposition(False())) -> <tautology: y>;

// Theme 3: Temporal operators vs. deadlock
// ****************************************

// F nodeadlock -> nodeadlock
Eventually(x=AtomicProposition(NoDeadlock())) -> <tautology: x>;

// G deadlock -> deadlock
Always(x=AtomicProposition(Deadlock())) -> <tautology: x>;

// deadlock U x -> x 
Until(AtomicProposition(Deadlock()),x) -> <tautology: x>;

// nodeadlock U x -> F x
Until(AtomicProposition(NoDeadlock()),x) -> <tautology: Eventually(x)>;

// nodeadlock R x -> x 
Release(AtomicProposition(NoDeadlock()),x) -> <tautology: x>;

// deadlock R x -> G x
Release(AtomicProposition(Deadlock()),x) -> <tautology: Always(x)>;

// A deadlock -> deadlock
AllPath(x=AtomicProposition(Deadlock())) -> <tautology: x>;

// E deadlock -> deadlock
ExPath(x=AtomicProposition(Deadlock())) -> <tautology: x>;

// A nodeadlock -> nodeadlock
AllPath(x=AtomicProposition(NoDeadlock())) -> <tautology: x>;

// E nodeadlock -> nodeadlock
ExPath(x=AtomicProposition(NoDeadlock())) -> <tautology: x>;


// Theme 4: Deadlock vs. Boolean
// *****************************

// The following collapse rules rely on a right-associative grouping 
// of AND/OR chains.
// There are rules that take care of this. These rules appear, however
// after the collapse rules. This way, collapsing gets priority over
// sorting and makes rewriting more efficient.

Disjunction(AtomicProposition(NoDeadlock()),AtomicProposition(Deadlock())) -> <tautology: AtomicProposition(True())>;
Disjunction(AtomicProposition(Deadlock()),AtomicProposition(NoDeadlock())) -> <tautology: AtomicProposition(True())>;
Disjunction(AtomicProposition(Deadlock()),x=AtomicProposition(Deadlock())) -> <tautology: x>;
Disjunction(AtomicProposition(NoDeadlock()),x=AtomicProposition(NoDeadlock())) -> <tautology: x>;

Conjunction(AtomicProposition(NoDeadlock()),AtomicProposition(Deadlock())) -> <tautology: AtomicProposition(False())>;
Conjunction(AtomicProposition(Deadlock()),AtomicProposition(NoDeadlock())) -> <tautology: AtomicProposition(False())>;
Conjunction(AtomicProposition(Deadlock()),x=AtomicProposition(Deadlock())) -> <tautology: x>;
Conjunction(AtomicProposition(NoDeadlock()),x=AtomicProposition(NoDeadlock())) -> <tautology: x>;

Disjunction(AtomicProposition(NoDeadlock()),Disjunction(AtomicProposition(Deadlock()),*)) -> <tautology: AtomicProposition(True())>;
Disjunction(AtomicProposition(Deadlock()),Disjunction(AtomicProposition(NoDeadlock()),*)) -> <tautology: AtomicProposition(True())>;
Disjunction(AtomicProposition(NoDeadlock()),x=Disjunction(AtomicProposition(NoDeadlock()),*)) -> <tautology: x>;
Disjunction(AtomicProposition(Deadlock()),x=Disjunction(AtomicProposition(Deadlock()),*)) -> <tautology: x>;

Conjunction(AtomicProposition(NoDeadlock()),Conjunction(AtomicProposition(Deadlock()),*)) -> <tautology: AtomicProposition(False())>;
Conjunction(AtomicProposition(Deadlock()),Conjunction(AtomicProposition(NoDeadlock()),*)) -> <tautology: AtomicProposition(False())>;
Conjunction(AtomicProposition(NoDeadlock()),x=Conjunction(AtomicProposition(NoDeadlock()),*)) -> <tautology: x>;
Conjunction(AtomicProposition(Deadlock()),x=Conjunction(AtomicProposition(Deadlock()),*)) -> <tautology: x>;

// Theme 5: Normalize chains of Boolean operators
// **********************************************

// (x AND y) AND z -> x AND (y AND z)
Conjunction(Conjunction(x,y),z) -> <tautology: Conjunction(x,Conjunction(y,z))>;

// (x OR y) OR z -> x OR (y OR z)
Disjunction(Disjunction(x,y),z) -> <tautology: Disjunction(x,Disjunction(y,z))>;

/* priorities are not yet implemented
// sort OR chains by "priority" of subformulas
Disjunction(x,y) provided(x->priority > y->priority) -> <tautology: Disjunction(y,x)>;
Disjunction(x,Disjunction(y,z)) provided(x->priority > y->priority) -> <tautology: Disjunction(y,Disjunction(x,z))>;

// sort AND chains by "priority" of subformulas
Conjunction(x,y) provided(x->priority > y->priority) -> <tautology: Conjunction(y,x)>;
Conjunction(x,Conjunction(y,z)) provided(x->priority > y->priority) -> <tautology: Conjunction(y,Conjunction(x,z))>;
*/

// Theme 6: Push down negations.
// In effect, resulting formulas have no negations at all.
// *******************************************************

// Negations of atomic propositions. After phase 2, only <= and > remain.

// NOT (p > n)   ->    p <= n
Negation(AtomicProposition(GreaterAtomicProposition(p,n))) -> <tautology: AtomicProposition(LessEqualAtomicProposition(p, n))>;

// NOT (p <= n)   ->    p > n
Negation(AtomicProposition(LessEqualAtomicProposition(p,n))) -> <tautology: AtomicProposition(GreaterAtomicProposition(p, n))>;

// NOT TRUE  ->  FALSE
Negation(AtomicProposition(True())) -> <tautology: AtomicProposition(False())>;

// NOT FALSE  ->  TRUE
Negation(AtomicProposition(False())) -> <tautology: AtomicProposition(True())>;

// NOT deadlock -> nodeadlock
Negation(AtomicProposition(Deadlock())) -> <tautology: AtomicProposition(NoDeadlock())>;

// NOT nodeadlock -> deadlock
Negation(AtomicProposition(NoDeadlock())) -> <tautology: AtomicProposition(Deadlock())>;

// NOT Fireable -> Unfireable
Negation(AtomicProposition(Fireable(t))) -> <tautology: AtomicProposition(Unfireable(t))>;

// NOT Unfireable -> Fireable
Negation(AtomicProposition(Unfireable(t))) -> <tautology: AtomicProposition(Fireable(t))>;

// Negations of Boolean operators

// NOT(NOT(x))  ->  x
Negation(Negation(x)) -> <tautology: x>;

// NOT (x AND y)  ->    (NOT x OR NOT y)
Negation(Conjunction(x, y)) -> <tautology: Disjunction(Negation(x), Negation(y))>;

// NOT (x OR y)   ->    (NOT x AND NOT y)
Negation(Disjunction(x, y)) -> <tautology: Conjunction(Negation(x), Negation(y))>;

// Negations and temporal operators

// NOT A   <->   E NOT
Negation(AllPath(x)) -> <tautology: ExPath(Negation(x))>;

// NOT E   <->   A NOT
Negation(ExPath(x)) -> <tautology: AllPath(Negation(x))>;

// NOT G   <->   F NOT
Negation(Always(x)) -> <tautology: Eventually(Negation(x))>;

// NOT F   <->   G NOT
Negation(Eventually(x)) -> <tautology: Always(Negation(x))>;

// NOT X   <->   X NOT
Negation(NextState(x)) -> <tautology: NextState(Negation(x))>;

// NOT (x U y) -> (NOT x R NOT y) 
Negation(Until(x,y)) -> <tautology: Release(Negation(x), Negation(y))>;

// NOT (x R y) -> (NOT x U NOT y) 
Negation(Release(x,y)) -> <tautology: Until(Negation(x), Negation(y))>;

// Theme 7: Repeated temporal operators
// ************************************

// FF a        <->  Fa
Eventually(a=Eventually(*)) -> <tautology: a>;

// AA x       <->  A x
AllPath(a=AllPath(*)) -> <tautology: a>;

// AE x       <->  E x
AllPath(a=ExPath(*)) -> <tautology: a>;

// EA x       <->  A x
ExPath(a=AllPath(*)) -> <tautology: a>;

// EE x       <->  E x
ExPath(a=ExPath(*)) -> <tautology: a>;

// FGF x       <->  GF x
Eventually(a=Always(Eventually(*))) -> <tautology: a>;

// GFG x       <->  FG x
Always(a=Eventually(Always(*))) -> <tautology: a>;

// AGAG x   <-> AG x
AllPath(Always(a=AllPath(Always(*)))) -> <tautology: a>;

// AFAF x   <-> AF x
AllPath(Eventually(a=AllPath(Eventually(*)))) -> <tautology: a>;

// EFEF x   <-> EF x
ExPath(Eventually(a=ExPath(Eventually(*)))) -> <tautology: a>;

// AGEFAGEF x <-> AGEF x
AllPath(Always(ExPath(Eventually(a=AllPath(Always(ExPath(Eventually(*)))))))) -> <tautology: a>;

// EFAGEFAG x <-> EFAG x
ExPath(Eventually(AllPath(Always(a=ExPath(Eventually(AllPath(Always(*)))))))) -> <tautology: a>;

// X (x U y) <-> (Xx U Xy)
Until(NextState(x),NextState(y)) -> <tautology: NextState(Until(x,y))>;

// (x U (x U y)) -> (x U y)
Until(x, a=Until(x,*)) -> <tautology: a>;

// Theme 8: Temporal operators vs. Boolean
// We try to push temporal operators down. This way, individual explorations
// act on simpler formulas. On top level, we can split into several 
// individual runs.
// *************************************************************************

// temporal operators need to move up, for finding a path quantifier

// (F a) OR (F b)  ->  F (a OR b)
Disjunction(Eventually(x),Eventually(y)) -> <tautology: Eventually(Disjunction(x,y))>;

// (G a) AND (G b)  ->  G (a AND b)
Conjunction(Always(x),Always(y)) -> <tautology: Always(Conjunction(x,y))>;

// (X a) AND (X b)  ->  X (a AND b)
Conjunction(NextState(x), NextState(y)) -> <tautology: NextState(Conjunction(x,y))>;

// (X a) OR (X b)   <->   X (a OR b)  
Disjunction(NextState(x), NextState(y)) -> <tautology: NextState(Disjunction(x,y))>;

// path quantifiers need to move down, for finding a temporal operator

// E (a OR b) -> (E a) OR (E b)
ExPath(Disjunction(x,y)) -> <tautology: Disjunction(ExPath(x),ExPath(y))>;

// A (a AND b) -> (A a) AND (A b)
AllPath(Conjunction(x, y)) -> <tautology: Conjunction(AllPath(x),AllPath(y))>;

// matched CTL operators can go either way. Pushing up means less recursive
//   searches on more global subformulas. Additionally, chance are that rewriting
//   in subformulas finds tautologies. Pushing down means more recursive
//   searches on more local subformulas. Current decision: push up
//   \todo: Compare performance of push up and push down (reverse rule application)

// (AG a) AND (AG b) -> AG (a AND b)
Conjunction(AllPath(Always(a)),AllPath(Always(b))) -> <tautology: AllPath(Always(Conjunction(a,b)))>;

// (EF a) OR (EF b) -> EF (a OR b)
Disjunction(ExPath(Eventually(a)),ExPath(Eventually(b))) -> <tautology: ExPath(Eventually(Disjunction(a,b)))>;

// (AX a) AND (AX b) -> AX (a AND b)
Conjunction(AllPath(NextState(a)),AllPath(NextState(b))) -> <tautology: AllPath(NextState(Conjunction(a,b)))>;

// (EX a) OR (EX b) -> EX (a OR b)
Disjunction(ExPath(NextState(a)),ExPath(NextState(b))) -> <tautology: ExPath(NextState(Disjunction(a,b)))>;

// Theme 9: Bubblesort subformulas of conjunctions and negations. This way,
// collapsing rules may become applicable. We sort according to the following
// order: deadlock=nodeadlock<A<E<X<G<F<U<R<leq=gt<con=dis.
// placing con and dis at max is essential since otherwise associativity 
// would get broken


/**************************************
 * Phase 3a: REMOVE EMPTY QUANTIFIERS *
 **************************************/

// A quantifier is "empty" if it does not contain any temporal operators.

%rview emptyquantifiers;

ExPath(x) provided (not x->containsTemporal) -> <emptyquantifiers: x>;
AllPath(x) provided (not x->containsTemporal) -> <emptyquantifiers: x>;


/*************************************************
 * Phase 4: REMOVE SINGLE ROOT TEMPORAL OPERATOR *
 *************************************************/

/*
4a. singletemporal
These rules eleminate the root temporal operator(s) to perform certain checks
with dedicated routines rather than general purpose CTL/LTL algorithms.

The following transformations are made:

- EF f -> f      (in case of reachability; algorithm: reachability)
- AG f -> NOT f  (in case of invariance; algorithm: reachability)
- AG EF f -> f   (in case of liveness; algorithm: currently only cover)
- EF AG f -> NOT f  (algorithm: liveness)
- EF AG EF f -> f  (algorithm: tscc)
- AG EF AG f -> NOT f  (algorithm: tscc)

4b. simpleneg
We need to repeat some negation rules as we may introduce negation in 4a.
These rules do not change associativity of AND/OR, so the normal form
of AND/OR chains is preserved.

4c. booleanlists
Transform Chains of AND/OR into lists
*/

%rview singletemporal;

// Liveness
// AG EF f -> f
StatePredicateFormula(AllPath(Always(ExPath(Eventually(y))))) provided(not y->containsTemporal)-> <singletemporal: StatePredicateFormula(y)>;

// Impossibility : (KW: cannot happen since negation is not there)
// AG NOT f -> NOT EF f -> f   (the outmost negation is handled by LoLA)
// StatePredicateFormula(AllPath(Always(Negation(y)))) provided(not y->containsTemporal)-> <singletemporal: StatePredicateFormula(y)>;

// Reachability
// EF f -> f
StatePredicateFormula(ExPath(Eventually(y))) provided(not y->containsTemporal)-> <singletemporal: StatePredicateFormula(y)>;

// Invariance
// AG f -> NOT EF NOT f -> NOT f   (the outmost negation is handled by LoLA)
StatePredicateFormula(AllPath(Always(y))) provided(not y->containsTemporal)-> <singletemporal: StatePredicateFormula(Negation(y))>;


// Possible Invariance
// EF AG f -> NOT AG EF NOT f -> NOT f   (the outmost negation is handled by LoLA)
StatePredicateFormula(ExPath(Eventually(AllPath(Always(y))))) provided(not y->containsTemporal)-> <singletemporal: StatePredicateFormula(Negation(y))>;

// Possible Liveness
// EF AG EF f -> f  
StatePredicateFormula(ExPath(Eventually(AllPath(Always(ExPath(Eventually(y))))))) provided(not y->containsTemporal)-> <singletemporal: StatePredicateFormula(y)>;

// Live Invariance
// AG EF AG f -> NOT EF AG EF NOT f -> NOT f  (the outmost negation is handled by LoLA)
StatePredicateFormula(AllPath(Always(ExPath(Eventually(AllPath(Always(y))))))) provided(not y->containsTemporal)-> <singletemporal: StatePredicateFormula(Negation(y))>;

%rview simpleneg;

// NOT (x AND y)  ->    (NOT x OR NOT y)
Negation(Conjunction(x, y)) -> <simpleneg: Disjunction(Negation(x), Negation(y))>;

// NOT (x OR y)   ->    (NOT x AND NOT y)
Negation(Disjunction(x, y)) -> <simpleneg: Conjunction(Negation(x), Negation(y))>;

// NOT TRUE  ->  FALSE
Negation(AtomicProposition(True())) -> <simpleneg: AtomicProposition(False())>;

// NOT FALSE  ->  TRUE
Negation(AtomicProposition(False())) -> <simpleneg: AtomicProposition(True())>;

// NOT deadlock -> nodeadlock
Negation(AtomicProposition(Deadlock())) -> <simpleneg: AtomicProposition(NoDeadlock())>;

// NOT nodeadlock -> deadlock
Negation(AtomicProposition(NoDeadlock())) -> <simpleneg: AtomicProposition(Deadlock())>;

// NOT Fireable -> Unfireable
Negation(AtomicProposition(Fireable(t))) -> <simpleneg: AtomicProposition(Unfireable(t))>;

// NOT Unfireable -> Fireable
Negation(AtomicProposition(Unfireable(t))) -> <simpleneg: AtomicProposition(Fireable(t))>;

// NOT (p > n)   ->    p <= n
Negation(AtomicProposition(GreaterAtomicProposition(p,n))) -> <simpleneg: AtomicProposition(LessEqualAtomicProposition(p, n))>;

// NOT (p <= n)   ->    p > n
Negation(AtomicProposition(LessEqualAtomicProposition(p,n))) -> <simpleneg: AtomicProposition(GreaterAtomicProposition(p, n))>;


%rview booleanlists;

// collect conjunctions in lists
Conjunction(x,y) -> <booleanlists: ConjunctionList(ConstConjunction_list(x, ConstConjunction_list(y, NiltConjunction_list())))>;
ConstConjunction_list(ConjunctionList(x), y) -> <booleanlists: concat(x,y) >;

// collect disjunctions in lists
Disjunction(x,y) -> <booleanlists: DisjunctionList(ConstDisjunction_list(x, ConstDisjunction_list(y, NiltDisjunction_list())))>;
ConstDisjunction_list(DisjunctionList(x), y) -> <booleanlists: concat(x,y) >;

/***************************************************************************
 * PHASE 5: (CTL only) path quantifier + temporal operator -> CTL operator *
 ***************************************************************************/

%rview ctloperators;

// as long as not all CTL operators have their own search classes, we
// use workarounds. Introduced negations are not replaced as this may
// cause formula blowup (and CTL model checking can handle negation) 
// \todo: remove workarounds as soon as CTL ist complete.

// workaround for: ExPath(Eventually(x)) -> <ctloperators: EF(x)>;
ExPath(Eventually(x)) -> <ctloperators: EU(AtomicProposition(True()),x)>;

ExPath(NextState(x)) -> <ctloperators: EX(x)>;

// workaround for: ExPath(Always(x)) -> <ctloperators: EG(x)>;
ExPath(Always(x)) -> <ctloperators: Negation(AU(AtomicProposition(True()),Negation(x)))>;

ExPath(Until(x,y)) -> <ctloperators: EU(x,y)>;

// workaround for: ExPath(Release(x,y)) -> <ctloperators: ER(x,y)>;
ExPath(Release(x,y)) -> <ctloperators: Negation(AU(Negation(x),Negation(y)))>;

// workaround for: AllPath(Eventually(x)) -> <ctloperators: AF(x)>;
AllPath(Eventually(x)) -> <ctloperators: AU(AtomicProposition(True()),x)>;

AllPath(NextState(x)) -> <ctloperators: AX(x)>;

// workaround for: AllPath(Always(x)) -> <ctloperators: AG(x)>;
AllPath(Always(x)) -> <ctloperators: Negation(EU(AtomicProposition(True()),Negation(x)))>;

AllPath(Until(x,y)) -> <ctloperators: AU(x,y)>;

// workaround for: AllPath(Release(x,y)) -> <ctloperators: AR(x,y)>;
AllPath(Release(x,y)) -> <ctloperators: Negation(EU(Negation(x),Negation(y)))>;


%rview rbuechi;

// expands all transition rules to triples (from,formula,to)
BuechiRule(from,TransitionRules(lrules,rrules)) -> <rbuechi: BuechiRules(BuechiRule(from,lrules),BuechiRule(from,rrules))>;
BuechiRule(from,TransitionRule(formula,to)) -> <rbuechi: ExpandedBuechiRule(from,formula,to)>;
BuechiRule(*,EmptyTransitionRules()) -> <rbuechi: EmptyBuechiRules()>;
BuechiRules(a,EmptyBuechiRules()) -> <rbuechi: a>;
BuechiRules(EmptyBuechiRules(),a) -> <rbuechi: a>;

/********************************************************
 * PHASE 6: DNF for state-equation task; needed by sara *
 ********************************************************/

%rview dnf;

// x AND (y OR z) -> (x AND y) OR (x AND z)
Conjunction(x,Disjunction(y,z)) -> 
        <dnf: Disjunction(Conjunction(x,y),Conjunction(x,z))>;

// (y OR z) AND x -> (y AND x) OR (z AND x)
Conjunction(Disjunction(y,z),x) -> 
        <dnf: Disjunction(Conjunction(y,x),Conjunction(z,x))>;

/************************************************************************************
 * PHASE 7: APPLY LOGICAL TAUTOLOGIES WHICH CAN BE INTRODUCED BY FIREABLE UNFOLDING *
 ************************************************************************************/

/*
This view or the tautology rview must be called after goodbye_fireable or
goodbye_unfireable rewrites were used.

This phase is a simple version of the tautology rview and consists of 
just one rule set since all rule applications may make other rules 
applicable again. For better readability, we sort the rules by various 
themes.
*/

// Theme 1: Boolean operators vs. TRUE/FALSE
// These are first since tree gets smaller
// *****************************************

%rview simpletautology;

// AND + TRUE/FALSE
Conjunction(a, AtomicProposition(True())) -> <simpletautology: a >;
Conjunction(AtomicProposition(True()), a) -> <simpletautology: a >;
Conjunction(*, f = AtomicProposition(False())) -> <simpletautology: f >;
Conjunction(f = AtomicProposition(False()), *) -> <simpletautology: f >;

// OR + TRUE/FALSE
Disjunction(a, AtomicProposition(False())) -> <simpletautology: a >;
Disjunction(AtomicProposition(False()), a) -> <simpletautology: a >;
Disjunction(*, t=AtomicProposition(True())) -> <simpletautology: t >;
Disjunction(t=AtomicProposition(True()), *) -> <simpletautology: t >;


// Theme 5: Normalize chains of Boolean operators
// **********************************************

// (x AND y) AND z -> x AND (y AND z)
Conjunction(Conjunction(x,y),z) -> <simpletautology: Conjunction(x,Conjunction(y,z))>;

// (x OR y) OR z -> x OR (y OR z)
Disjunction(Disjunction(x,y),z) -> <simpletautology: Disjunction(x,Disjunction(y,z))>;


