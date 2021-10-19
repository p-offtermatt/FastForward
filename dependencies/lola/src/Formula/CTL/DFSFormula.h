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
\author Christian Koch
\status new
\ingroup ctl
*/

#pragma once

#include <Core/Dimensions.h>
#include <Formula/CTL/CTLFormula.h>

struct fairness_data
{
    arrayindex_t card_strong;
    arrayindex_t *strong_fairness;
    arrayindex_t *strong_backlist;
    arrayindex_t card_weak;
    arrayindex_t *weak_fairness;
    arrayindex_t *weak_backlist;

    arrayindex_t card_forbidden_transitions;
    arrayindex_t *forbidden_transitions;
};

class DFSFormula : public CTLFormula
{
public:
    /// byte index to dfs number
    arrayindex_t dfsindex;

    /// getter for the dfs number
    statenumber_t getDFS(void *payload) const;

    /// setter for the dfs number
    void setDFS(void *payload, statenumber_t dfs);

    /// returns info on a particular node in the formula tree
    virtual FormulaInfo *getInfo() const = 0;
    /// returns the number of subformulas
    virtual int countSubFormulas() const = 0;
};
