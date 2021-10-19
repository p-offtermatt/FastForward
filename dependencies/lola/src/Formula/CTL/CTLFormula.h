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

\brief Definition of the CTL base class.
*/

#pragma once

#include <Core/Dimensions.h>
#include <Stores/Store.h>
#include <Formula/FormulaInfo.h>

class NetState;
class Firelist;

enum CTLFormulaResult
{
    UNKNOWN = 0,      ///< implies white/unvisited
    IN_PROGRESS = 1,  ///< implies gray or on Tarjan stack (depending on search method)
    KNOWN_FALSE = 2,  ///< implies black/visited
    KNOWN_TRUE = 3    ///< implies black/visited
};

/*!
\todo Maybe we can use more inheritance (for functions) because here is a lot
redundancy as seen in all Files: *Formula.cc
*/
struct CTLFormula
{
    arrayindex_t index; // bit index in state payload. known flag is index, value flag is index+1
    size_t payloadsize;

    CTLFormulaResult getCachedResult(void *payload) const;

    void setCachedResult(void *payload, CTLFormulaResult result);

    virtual void initAtomics(NetState &ns) = 0;
    virtual void updateAtomics(NetState &ns, arrayindex_t t) = 0;
    virtual void revertAtomics(NetState &ns, arrayindex_t t) = 0;
    virtual bool check(Store<void *> &s, NetState &ns, Firelist &firelist,
                       std::vector<int> &witness) = 0;

    virtual void DEBUG_print() = 0;

    virtual void gatherPayloadInformation(arrayindex_t &numDFS, arrayindex_t &numCachedResults) = 0;
    virtual void setPayloadInformation(arrayindex_t cachedResultOffset, size_t payloadSize) = 0;
    /// returns info on a particular node in the formula tree
    virtual FormulaInfo *getInfo() const = 0;
    /// returns the number of subformulas
    virtual int countSubFormulas() const = 0;
};
