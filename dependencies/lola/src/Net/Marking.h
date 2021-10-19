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
\status approved 27.01.2012

\brief Global data for marking specific information

All data that describe attributes of markings can be found here.
*/

#pragma once

#include <Core/Dimensions.h>

/*!
 \brief collection of information related to markings
 */
struct Marking
{
public:
    /// initial marking
    static capacity_t *Initial;

    /// hash value of initial marking
    static hash_t HashInitial;

    /// current  marking
    static capacity_t *Current;

    /// hash value of current marking
    static hash_t HashCurrent;

    /// target marking
    static capacity_t *Target;

    /// Aufräumen der Knoten - Service für valgrind
    static void deleteMarkings();

    static void DEBUG__printMarking(capacity_t *marking);
};
