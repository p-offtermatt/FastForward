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
\author Markus
\status new
*/

#pragma once

#include <config.h>
#include <Stores/Store.h>
#include <Witness/Path.h>
#include <Exploration/Firelist.h>
#include <Exploration/SearchStack.h>
#include <Core/Dimensions.h>
#include <Exploration/DFSStackEntry.h>
#include <Exploration/SimpleProperty.h>

class ChooseTransition;
class Firelist;
class NetState;


class TSCCExploration
{
public:
    /// evaluates a given property by standard depth-first-search
    virtual statenumber_t getDFS(statenumber_t *payload) const
    {
        return *payload;
    }
    virtual void setDFS(statenumber_t *payload, statenumber_t dfs)
    {
        *payload = dfs;
    }

    virtual bool depth_first(SimpleProperty &property, NetState &ns, Store<statenumber_t> &myStore,
                             Firelist &myFirelist, int threadNumber) = 0;

    SearchStack<DFSStackEntry> dfsstack;
};

class TSCCExplorationAGEF : public TSCCExploration
{
public:
    bool depth_first(SimpleProperty &property, NetState &ns, Store<statenumber_t> &myStore,
                     Firelist &myFirelist, int threadNumber);
};

class TSCCExplorationEGAGEF : public TSCCExploration
{
public:
    bool depth_first(SimpleProperty &property, NetState &ns, Store<statenumber_t> &myStore,
                     Firelist &myFirelist, int threadNumber);
};
