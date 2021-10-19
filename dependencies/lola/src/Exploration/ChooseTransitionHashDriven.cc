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

\brief Selects a transition from a fire list based on counting hash values and
a priority imposed on the fire list
*/

#include <Core/Dimensions.h>
#include <Exploration/ChooseTransition.h>
#include <Exploration/ChooseTransitionHashDriven.h>
#include <Net/Marking.h>
#include <Net/Net.h>
#include <Net/NetState.h>
#include <Net/Place.h>
#include <Net/Transition.h>

ChooseTransitionHashDriven::ChooseTransitionHashDriven() :
    table(new uint32_t[SIZEOF_MARKINGTABLE])
{}

ChooseTransitionHashDriven::~ChooseTransitionHashDriven()
{
    delete[] table;
}

arrayindex_t ChooseTransitionHashDriven::choose(NetState &ns, arrayindex_t cardfirelist,
        arrayindex_t *firelist)
{
    // Selection proceeds in two phases. In phase one, we give priority to
    // transitions that (1) enter rarely visited hash buckets and (2) are early
    // members of the fire list. If no transition is selected in phase one,
    // phase two selects a transition randomly.

    // initialize with a number that we can be sure it is not an index of a
    // transition
    arrayindex_t chosen = Net::Card[TR];

    // phase one
    for (arrayindex_t i = cardfirelist; i > 0;)
    {
        --i;
        arrayindex_t t = firelist[i];
        // compute hash value for t successor
        hash_t h = (ns.HashCurrent + Transition::DeltaHash[t]) % SIZEOF_MARKINGTABLE;
        h = (h < 0) ? h + SIZEOF_MARKINGTABLE : h;

        if ((static_cast<float>(rand()) / static_cast<float>(RAND_MAX)) <= 1.0 / (1.0 + table[h]))
        {
            chosen = t;
            break;
        }
    }

    // phase two

    // if we did not choose any transition, pick one randomly
    if (chosen == Net::Card[TR])
    {
        chosen = firelist[rand() % cardfirelist];
    }

    hash_t h = (Marking::HashCurrent + Transition::DeltaHash[chosen]) % SIZEOF_MARKINGTABLE;
    h = (h < 0) ? h + SIZEOF_MARKINGTABLE : h;
    ++(table[h]);

    return chosen;
}
