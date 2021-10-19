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
\author Christian K and Gregor B
\status unknown
 */

#include <Core/Dimensions.h>
#include <Net/Marking.h>
#include <Net/Net.h>
#include <Net/NetState.h>
#include <Net/Place.h>
#include <Net/Transition.h>

NetState *NetState::createNetStateFromInitial()
{
    NetState *ns = new NetState();
    ns->membersInitialized = true;
    // copy the current marking at its hash
    ns->Current = new capacity_t[Net::Card[PL]];
    for (arrayindex_t i = 0; i < Net::Card[PL]; i++)
    {
        ns->Current[i] = Marking::Initial[i];
    }
    ns->HashCurrent = Marking::HashInitial;

    // copy the currently enabled transitions
    ns->Enabled = new bool[Net::Card[TR]];
    for (arrayindex_t i = 0; i < Net::Card[TR]; i++)
    {
        ns->Enabled[i] = Transition::Enabled[i];
    }
    ns->CardEnabled = Transition::CardEnabled;

    return ns;
}

NetState::NetState(const NetState &ons)
{
    membersInitialized = true;
    // copy the current marking at its hash
    Current = new capacity_t[Net::Card[PL]];
    for (arrayindex_t i = 0; i < Net::Card[PL]; i++)
    {
        Current[i] = ons.Current[i];
    }
    HashCurrent = ons.HashCurrent;

    // copy the currently enabled transitions
    Enabled = new bool[Net::Card[TR]];
    for (arrayindex_t i = 0; i < Net::Card[TR]; i++)
    {
        Enabled[i] = ons.Enabled[i];
    }
    CardEnabled = ons.CardEnabled;

}

NetState &NetState::operator=(const NetState &ns)
{
    NetState tmp(ns); // copy and swap
    swap(tmp);
    return *this;
}

void NetState::swap(NetState &ns)
{
    std::swap(Current, ns.Current);
    std::swap(HashCurrent, ns.HashCurrent);
    std::swap(Enabled, ns.Enabled);
    std::swap(CardEnabled, ns.CardEnabled);
    std::swap(membersInitialized, ns.membersInitialized);
}


/*!
\todo Find out why membersInitialized is used here.
*/
NetState::~NetState()
{
    if (!membersInitialized)
    {
        return;
    }
    delete[] Current;
    delete[] Enabled;
}
