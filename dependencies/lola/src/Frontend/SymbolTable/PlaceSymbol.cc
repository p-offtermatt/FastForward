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
\status approved 25.01.2012
\ingroup g_symboltable
\brief implementation of class PlaceSymbol
*/

#include <Frontend/SymbolTable/PlaceSymbol.h>

/*!
The capacity is available in context, whereas the other ingredients are not and
are initialized with zero. cardPre and cardPost are lated initialized by
TransitionSymbol::TransitionSymbol.

\param[in] k    the name of the place
\param[in] cap  the capacity of this place (UINTMAX denotes no capacity)
*/
PlaceSymbol::PlaceSymbol(const char *k, capacity_t cap) :
    Symbol(k),
    capacity(cap),
    initialMarking(0), // temporary initialization
    cardPost(0),       // temporary initialization until transitions are parsed
    cardPre(0)         // temporary initialization until transitions are parsed
{
}

capacity_t PlaceSymbol::getCapacity() const
{
    return capacity;
}

capacity_t PlaceSymbol::getInitialMarking() const
{
    return initialMarking;
}

/*!
If several marking specifications mention this place, tokens are added.
*/
void PlaceSymbol::addInitialMarking(unsigned int newTokens)
{
    initialMarking += newTokens;
}

arrayindex_t PlaceSymbol::getCardPre() const
{
    return cardPre;
}

arrayindex_t PlaceSymbol::getCardPost() const
{
    return cardPost;
}

void PlaceSymbol::notifyPost()
{
    ++cardPost;
}

void PlaceSymbol::notifyPre()
{
    ++cardPre;
}
