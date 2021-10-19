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

/*!
A struct that collects payload data for the cover store.
*/
struct CoverPayload
{
    CoverPayload *parent;
    CoverPayload *next;
    CoverPayload **sons;
    arrayindex_t **new_omegas;
    arrayindex_t card_new_omegas;
    arrayindex_t transition;
    arrayindex_t card_sons;
    arrayindex_t index;
    arrayindex_t lowlink;
    bool next_level;
    bool value;
    bool unknown;
};
