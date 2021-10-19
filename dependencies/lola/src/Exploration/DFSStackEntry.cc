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

#include <Exploration/DFSStackEntry.h>

DFSStackEntry::DFSStackEntry(arrayindex_t *_fl, arrayindex_t _flIndex, void *_payload,
                             statenumber_t _lowlink)
    : fl(_fl), flIndex(_flIndex), payload(_payload), lowlink(_lowlink)
{
    assert(_fl);
}
