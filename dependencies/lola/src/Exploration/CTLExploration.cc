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
#include <Exploration/CTLExploration.h>
#include <Exploration/Firelist.h>
#include <Formula/CTL/CTLFormula.h>
#include <Net/NetState.h>
#include <Witness/Path.h>

/*!
\param formula  the CTL formula to check
\param store  the store to use to store markings
\param firelist  the firelist to use
\param ns  the netstate to use
*/
bool CTLExploration::checkProperty(CTLFormula *formula, Store<void *> &store,
                                   Firelist &firelist, NetState &ns)
{
    arrayindex_t numDFS = 0;
    arrayindex_t numCachedResults = 0;

    formula->gatherPayloadInformation(numDFS, numCachedResults);

    arrayindex_t cachedResultOffset = numDFS * SIZEOF_STATENUMBER_T;
    size_t payloadSize = cachedResultOffset + (numCachedResults * 2 + 7) / 8;

    RT::rep->status("CTL formula contains %d significant temporal operators and needs %d bytes of payload",
                    numDFS, payloadSize);

    formula->setPayloadInformation(cachedResultOffset, payloadSize);

    formula->initAtomics(ns);

    witness.clear();
    return formula->check(store, ns, firelist, witness);
}

Path CTLExploration::path()
{
    static Path p;

    if (not p.initialized)
    {
        bool containsCycle = false;
        p.initialized = true;
        for (std::vector<int>::const_iterator it = witness.begin(); it != witness.end(); ++it)
        {
            if (*it == -1)
            {
                p.beginCycle(false);
                containsCycle = true;
            }
            else
            {
                p.addTransition(*it, false);
            }
        }
        witness.clear();

        // make sure we close opened cycles
        if (containsCycle)
        {
            p.endCycle(false);
        }
    }

    return p;
}
