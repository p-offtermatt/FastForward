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
\author Harro
\status new

\brief Replacement for EmptyStore to count markings and edges (only)

This class is an extension of the class EmptyStore. It never contains
any markings. Its only purpose is to count the markings that may be
distributed over many SweepRingStores.
*/

#pragma once

#include <Core/Dimensions.h>
#include <Stores/Store.h>
#include <Stores/EmptyStore.h>

/*!
\brief Replacement for EmptyStore to count markings and edges

This class is an extension of the class EmptyStore. It never contains
any markings. Its only purpose is to count the markings that may be
distributed over many SweepRingStores.
*/
class SweepEmptyStore : public EmptyStore<void>
{
public:
    SweepEmptyStore();

    void setMarkings(int64_t count);
    void setCalls(int64_t count);
};

