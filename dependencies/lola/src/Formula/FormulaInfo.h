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
\author Karsten
\status new

\brief Information record for node in formula tree.

We define a data structure that is used for traversing a formula tree.
The structure contains
- a tag that determines the type of node (operator)
- the number of immediate children
- an array with pointers to the children
*/

#pragma once

class FormulaInfo;

#include <Formula/CTL/CTLFormula.h>
#include <Formula/StatePredicate/AtomicStatePredicate.h>

/*!
\todo It makes no sense to provide explicit integer values here unless the tag
is later compared to an integer. Which then would be bad style. And should be
fixed.
*/
typedef enum {  formula_ag = 1,
                formula_eg = 2,
                formula_af = 3,
                formula_ef = 4,
                formula_ax = 5,
                formula_ex = 6,
                formula_au = 7,
                formula_eu = 8,
                formula_ar = 9,
                formula_er = 10,
                formula_f = 11,
                formula_g = 12,
                formula_u = 13,
                formula_r = 14,
                formula_x = 15,
                formula_a = 16,
                formula_e = 17,
                formula_and = 18,
                formula_or = 19,
                formula_not = 20,
                formula_true = 21,
                formula_false = 22,
                formula_atomic = 23,
                formula_deadlock = 24,
                formula_nodeadlock = 25
             } FormulaNodeTagt;

class FormulaInfo
{
public:
    FormulaNodeTagt tag;
    size_t cardChildren;
    struct CTLFormula **ctlChildren;
    StatePredicate **statePredicateChildren;
    AtomicStatePredicate *f;  // only set if formula is atomic.

    FormulaInfo();
    ~FormulaInfo();
};
