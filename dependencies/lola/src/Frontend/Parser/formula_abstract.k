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

%{ KC_TYPES_HEADER
#include <Core/Dimensions.h>
#include <Formula/StatePredicate/StatePredicate.h>
#include <Formula/LTL/BuechiAutomata.h>
#include <Formula/LTL/BuechiFromLTL.h>
#include <Formula/CTL/CTLFormula.h>
#include <cstring>
#include <map>
#include <string>
#include <Planning/Task.h>

// required, because the abstract grammar does not allow pointer types
typedef Task* Task_p;
typedef StatePredicate* StatePredicate_p;
typedef BuechiAutomata* BuechiAutomata_p;
typedef CTLFormula* CTLFormula_p;
typedef tl_Node* LTLTree_p;
typedef char* char_p;
typedef enum{UNDEF,BOOL,NUMB}checkName;
typedef void ** voidstar;
typedef kc::type type_t;

// don't use hash sets, since they are deprecated and unordered sets are not yet supported by Kimwitu++
#define DONT_USE_HASHSET
%}

tFormula:
  StatePredicateFormula(tStatePredicate)
  | ComputeBound(tAtomicProposition)
  | Compound(tFormula tFormula)
{
    formula_t type = FORMULA_MODELCHECKING;
    StatePredicate_p formula = NULL;
    CTLFormula_p ctl_formula = NULL;
    LTLTree_p ltl_tree = NULL;
    bool cannotcompute = false;
    /// A task attached to this
    Task_p task = NULL;
    int containsDeadlock = 0;
    /// contains the length of a DNF formula
    double length = 0;
    /// contains the number of OR in a DNF formula
    double number_of_or_dnf = 0;
    /// contains the number of OR in a formula
    double number_of_or = 0;
    /// contains the number of AND in a formula
    double number_of_and = 0;
    /// Are all AtomicPropositions from type FIREABLE?
    bool only_fireable = true;
}
;

tStatePredicate:
  AtomicProposition(tAtomicProposition)
| Negation(tStatePredicate)                               // removed in phase 3
| Conjunction(tStatePredicate tStatePredicate)     // removed in phase 4 for simple properties
| Disjunction(tStatePredicate tStatePredicate)     // removed in phase 4 for simple properties
| ExclusiveDisjunction(tStatePredicate tStatePredicate)   // removed in phase 1
| Implication(tStatePredicate tStatePredicate)            // removed in phase 1
| Equivalence(tStatePredicate tStatePredicate)            // removed in phase 1
| AllPath(tStatePredicate)
| ExPath(tStatePredicate)
| Always(tStatePredicate)
| Eventually(tStatePredicate)
| NextState(tStatePredicate)
| Until(tStatePredicate tStatePredicate)
| Release(tStatePredicate tStatePredicate)
| AU(tStatePredicate tStatePredicate)               // created in phase 5
| EU(tStatePredicate tStatePredicate)               // created by in phase 5
| AR(tStatePredicate tStatePredicate)               // created by in phase 5
| ER(tStatePredicate tStatePredicate)               // created by in phase 5
| AF(tStatePredicate)                               // created by in phase 5
| EF(tStatePredicate)                               // created by in phase 5
| AG(tStatePredicate)                               // created by in phase 5
| EG(tStatePredicate)                               // created by in phase 5
| AX(tStatePredicate)                               // created by in phase 5
| EX(tStatePredicate)                               // created by in phase 5
| ConjunctionList(tConjunction_list)                // created in phase 4 for simpleproperties
| DisjunctionList(tDisjunction_list)                // created in phase 4 for simpleproperties
{
    /// whether the formula contains a temporal operator
    bool containsTemporal = false;
    /// whether the formula is a valid CTL path formula
    bool validCTLPathFormula = false;
    /// whether the formula is a valid CTL state formula
    bool validCTLStateFormula = false;
    /// whether the formula is a valid LTL formula
    bool validLTLFormula = false;
    /// whether there is a subformula that cannot be handled
    bool cannotcompute = false;
    /// A task attached to this
    Task_p task = NULL;
    int priority = 0;
    formula_t type = FORMULA_MODELCHECKING;
    int containsDeadlock = 0;

    /// contains the length of a DNF formula
    double length = 0; 
    /// contains the number of OR in a DNF formula
    double number_of_or_dnf = 0;
    /// contains the number of OR in a formula
    double number_of_or = 0;
     /// contains the number of AND in a formula
    double number_of_and = 0;
    /// Are all AtomicPropositions from type FIREABLE?
    bool only_fireable = true;
}
;

tConjunction_list:
  list tStatePredicate
{
	int containsDeadlock = 0;
}
;

tDisjunction_list:
  list tStatePredicate
{
	int containsDeadlock = 0;
}
;

tAtomicProposition:
  EqualsAtomicProposition(tTerm tTerm)                // removed in phase 2
| NotEqualsAtomicProposition(tTerm tTerm)             // removed in phase 2
| GreaterAtomicProposition(tTerm tTerm)               
| GreaterEqualAtomicProposition(tTerm tTerm)          // removed in phase 2
| LessAtomicProposition(tTerm tTerm)                  // removed in phase 2
| LessEqualAtomicProposition(tTerm tTerm)
| True()
| False()
| NoDeadlock()					      // only introduced through
				                      // elemination of negation in phase 3
| Deadlock()
| Initial()                                           // removed in phase 1
| Fireable(integer)                                   
| Unfireable(integer) 
{
    /// Are all AtomicPropositions from type FIREABLE?
    bool only_fireable = true;
	int containsDeadlock = 0;
}
;

tTerm:
  Node(integer)
| Number(integer)
| Sum(tTerm tTerm)                                    // removed in phase 2 (is integrated in ProductList)
| Difference(tTerm tTerm)                             // removed in phase 2 (is integrated in ProductList) 
| Product(integer tTerm)
| ProductList(tProduct_list)                          // created in phase 2
;

tProduct_list:
  list tTerm   // in fact only Product()
;


tBuechiAutomata:
  BuechiAutomaton(tBuechiRules tAcceptingSet)
| BuechiNull()
{
	BuechiAutomata_p automata;
}
;

tBuechiRules:
  EmptyBuechiRules()
| BuechiRule(integer tTransitionRules)
| ExpandedBuechiRule(integer tFormula integer)
| BuechiRules(tBuechiRules tBuechiRules)
;

tTransitionRules:
  EmptyTransitionRules()
| TransitionRule(tFormula integer)
| TransitionRules(tTransitionRules tTransitionRules)
;

tAcceptingSet:
  EmptyAcceptingSet()
| AcceptingState(integer)
| AcceptingSet(tAcceptingSet tAcceptingSet)
;

/********************************************************************/
/*  Syntax tree of High Level Petri net                             */
/********************************************************************/


net:   /* root, the whole net */
  Net(casestring definitionsList placeblocklist marking transition)
;

definitionsList:
  EmptyDefinitionsList()
| DefinitionsList(definitionsList definitions)
;

definitions:
   Constant(type casestring arrayList expression)
|  Sort(type casestring arrayList)
|  Function(type casestring arrayList functionParametersList block)
;

type:
  TypeBool()
| TypeEnum(identList)
| TypeIdent(casestring)
| TypeIntInterval(expression expression)
| TypeStruct(structTypeList)
| TypeInt()
| TypeMultiset(type)
| TypeArray(type integer)
| TypeBlack()   
{
	bool is_finite = false;
        bool is_scalar = false;
	arrayindex_t size = 0;
}
;

optionalNumber:
  EmptyOptNumber()
| OptNumber(integer)
{
	arrayindex_t value;
	bool set;
}
;

identList:
  EmptyIdentList()
| IdentList(identList idents)
{
	arrayindex_t maxvalue;
	arrayindex_t nrvalues;
}
;

idents:
  Idents(casestring optionalNumber)
{
	arrayindex_t value;
	bool set;
}
;

structTypeList:
  EmptyStructTypeList()
| StructTypeList(structTypeList structType)
{
	bool is_finite;
	arrayindex_t size;
	arrayindex_t nrcomponents;
}
;

structType:
  StructType(type casestring arrayList)
{
	bool is_finite;
	bool is_scalar;
	arrayindex_t size;
}
;

varOrArray:
 VarOrArray(casestring arrayList)
{
	type_t inherited_type; // the type without taking into
			          // consideration array modifiers
	type_t type;  // actual type
	arrayindex_t nrdimensions;
	arrayindex_t nrelements;
}
;

arrayList:
  EmptyArrayList()
| ArrayList(arrayList expression)
{
	type_t  inherited_type; // type of the arrayed thing
				  // without considering array modifiers
	arrayindex_t nrdimensions;
	arrayindex_t nrelements;
	type_t  type;    // actual type, taking into consideration all
			   // subsequent array modifiers
}
;

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
{ checkName check = UNDEF; 
  bool is_constant = false;
  voidstar  value;
  bool is_scalar;
}
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
{
	type_t  inherited_type;
	type_t  type;
};

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
