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

#pragma once

#include <Core/Dimensions.h>

const int DFS_NUMBERS_PER_PNUELI = 4;
const int DFS_INITIAL_INVALID_NUMBER = -2; ///< unvisited by outermost search

/// a simple struct for a tree used as payload of the store
// the memory layout will be exactly fixed, so that we can do some dirty
// for correctness see stuff http://stackoverflow.com/questions/7793820/struct-members-memory-layout
class AutomataTree
{
public:
    arrayindex_t state;

    dfsnum_t dfs;
    arrayindex_t
    *firelist; // store the firelist in every node seems to be necessary, as the partial order reduction can differ between states with same petri but different buechi configuration
    arrayindex_t cardFirelist; // TODO really???

    AutomataTree *smaller;
    AutomataTree *bigger;

    AutomataTree();
    AutomataTree(arrayindex_t _state, bool tree);

    ~AutomataTree();
} __attribute__((packed));
