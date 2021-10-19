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

\brief Class for firelist generation. Default is firelist consisting of all
enabled transitions.
*/

#include <Exploration/Firelist.h>
#include <Net/Net.h>
#include <Net/NetState.h>
#include <Core/Runtime.h>

/*!
\param ns the net-state for which the firelist should be determined
\param[in,out] result the actual fire list
\result number of elements in fire list
*/

uint64_t * Firelist::usedAsScapegoat = NULL;
uint64_t Firelist::timestampScapegoat = 0;
Firelist::Firelist()
{
}

arrayindex_t Firelist::getFirelist(NetState &ns, arrayindex_t **result)
{
    assert(ns.CardEnabled <= Net::Card[TR]);

    *result = new arrayindex_t[ns.CardEnabled];
    arrayindex_t i = 0;
    for (arrayindex_t t = 0; t < Net::Card[TR]; ++t)
    {
        if (ns.Enabled[t])
        {
            assert(i < ns.CardEnabled);
            (*result)[i++] = t;
        }
    }
    return ns.CardEnabled;
}

/*!
This function will create a new firelist of the same type as the current one
based on the given simple property. in some cases this property will be
ignored, but if this is a stubborn-state firelist the property will be used.

This function is essential for the parallel exploration of the state space.
Each thread will have its own firelist with the current simple property.

\todo shouldnt something happen to this function? is it right that the property
doesnt get used?
*/
Firelist *Firelist::createNewFireList(SimpleProperty *)
{
    return new Firelist();
}

arrayindex_t Firelist::selectScapegoat(NetState & ns, arrayindex_t currenttransition)
{
       arrayindex_t scapegoat;
       for(arrayindex_t i = 0; i < Net::CardArcs[TR][PRE][currenttransition];i++)
       {

		arrayindex_t candidate = Net::Arc[TR][PRE][currenttransition][i];
                if(Net::Mult[TR][PRE][currenttransition][i] > ns.Current[candidate])
                {
                        scapegoat = candidate;
			if(Firelist::usedAsScapegoat[scapegoat] == Firelist::timestampScapegoat)
			{
				// this place has already been used by another transition
				return scapegoat;
			}
                }
        }
        assert(scapegoat < Net::CardArcs[TR][PRE][currenttransition]);
	// have a scapegoat that has not yet been used by another transition
	Firelist::usedAsScapegoat[scapegoat] = Firelist::timestampScapegoat;
	return scapegoat;
}

void Firelist::scapegoatNewRound()
{
	if(timestampScapegoat == UINT64_MAX)
	{
		timestampScapegoat = 1;
		for(arrayindex_t i = 0; i < Net::Card[PL]; i++)
		{
			usedAsScapegoat[i] = 0;
		}
	}
	else
	{
		++timestampScapegoat;
	}
}
