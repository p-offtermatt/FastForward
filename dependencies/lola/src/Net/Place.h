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

\brief Global data for place specific information

All data that describe attributes of places can be found here. General
information for a place in its role as a node, or contained in Node.*
*/

#pragma once

#include <Core/Dimensions.h>

/*!
\brief collection of information related to places
*/
struct Place
{
public:
    /// number of signifcant places; significant places are such that comparing markings on the significant places suffices for checking equality
    static arrayindex_t CardSignificant;

    /// The hash factor for a place. We compute hash values for places as \f$\sum_{p \in P} m(p)\cdot hash(p)\f$
    static hash_t *Hash;

    /// The maximum number of tokens that, according to input specification, can ever be present on this place.
    static capacity_t *Capacity;

    /// The number of bits needed for representing the numbers 0 .. Capacity[i]
    static cardbit_t *CardBits;

    /// The sum of the CardBits of all places
    static arrayindex_t SizeOfBitVector;

    /// Aufräumen der Knoten - Service für valgrind
    static void deletePlaces();

    /// Compute required nr of bits from max. nr of tokens
    static cardbit_t Capacity2Bits(capacity_t);
};
