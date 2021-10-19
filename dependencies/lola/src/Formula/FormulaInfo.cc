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

#include <Formula/FormulaInfo.h>
#include <Formula/CTL/CTLFormula.h>
#include <Formula/StatePredicate/StatePredicate.h>

FormulaInfo::FormulaInfo() :
    cardChildren(0), ctlChildren(0), statePredicateChildren(NULL), f(NULL)
{}

FormulaInfo::~FormulaInfo()
{
    if (ctlChildren)
    {
        delete[] ctlChildren;
    }
    if (statePredicateChildren)
    {
        delete[] statePredicateChildren;
    }
}
