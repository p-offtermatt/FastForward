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

\brief Useful rooutines for place specific information

All data that describe attributes of places can be found here. General
information for a place in its role as a node, ar contained in Node.*

\todo Tränsläte the comments to Inglisch pliez.
*/

#include <Core/Dimensions.h>
#include <Net/Place.h>
#include <Net/Net.h>

/*!
\brief collection of information related to places
*/
arrayindex_t Place::CardSignificant = 0;
hash_t *Place::Hash = NULL;

/*!
This is only used for calculating a dense representation of a marking, not as
a blocker for transition activation.
*/
capacity_t *Place::Capacity = NULL;

/*!
Used for handling dense representations of markings
*/
cardbit_t *Place::CardBits = NULL;
arrayindex_t Place::SizeOfBitVector = 0;

/*!
free memory for valgrind
\post Memory for Place::Hash Place::CardBits
Place::Capacity is freed
*/
void Place::deletePlaces()
{
    // allocated in Net::preprocess()
    delete[] Place::Hash;
    delete[] Place::CardBits;

    // allocated in ReadNetFile(FILE *f) and ParserPTNet::symboltable2net()
    delete[] Place::Capacity;
}

/*!
  Calculate CardBits based on Capacity
  \return the number of bits needed for the given capacity
 */
cardbit_t Place::Capacity2Bits(capacity_t cap)
{
    cardbit_t k = 0;
    while (cap)
    {
        k++;
        cap = cap >> 1;
    }
    return k;
}
