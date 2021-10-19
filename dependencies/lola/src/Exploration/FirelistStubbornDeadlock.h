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

\brief class for firelist generation. Default is firelist consisting of all
enabled transitions.
*/

#pragma once

#include <Core/Dimensions.h>

class Firelist;

/// a stubborn firelist for the search for deadlocks
class FirelistStubbornDeadlock : public Firelist
{
public:
    FirelistStubbornDeadlock();
    ~FirelistStubbornDeadlock();
    /// return value contains number of elements in fire list, argument is reference
    /// parameter for actual list
    virtual arrayindex_t getFirelist(NetState &ns, arrayindex_t **);

    virtual Firelist *createNewFireList(SimpleProperty *property);

private:
    arrayindex_t *dfsStack;
    arrayindex_t *dfs;
    arrayindex_t *lowlink;
    arrayindex_t *currentIndex;
    arrayindex_t *TarjanStack;
    arrayindex_t **mustBeIncluded;
    uint32_t *visited;
    uint32_t *onTarjanStack;
    uint32_t stamp;
    void newStamp();
};
