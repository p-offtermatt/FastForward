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

#include <Core/Dimensions.h>
#include <Formula/StatePredicate/StatePredicate.h>

StatePredicate *StatePredicate::top = NULL;

StatePredicate *StatePredicate::copy()
{
    return copy(NULL);
}

bool StatePredicate::isOrNode() const
{
    return false;
}

// LCOV_EXCL_START
arrayindex_t StatePredicate::countUnsatisfied() const
{
    return 0;
}
// LCOV_EXCL_STOP
