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
\author Niels
\status new

\brief Randomly selects a transition from a fire list.
*/

#include <Core/Dimensions.h>
#include <Exploration/ChooseTransition.h>
#include <Exploration/ChooseTransitionRandomly.h>

/*!
\return the index of a transition that is randomly chosen from firelist
*/
arrayindex_t ChooseTransitionRandomly::choose(NetState &, arrayindex_t cardfirelist,
        arrayindex_t *firelist)
{
    assert(cardfirelist > 0);
    assert(firelist);
    return firelist[rand() % cardfirelist];
}
