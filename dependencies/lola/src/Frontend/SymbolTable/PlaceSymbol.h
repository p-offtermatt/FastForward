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

\brief definition of class PlaceSymbol
*/

#pragma once

#include <Core/Dimensions.h>
#include <Frontend/SymbolTable/Symbol.h>

/*!
\brief a symbol table entry for a place

Place symbols carry their name as key and capacity and an initial marking as
payload. While capacity is in the context of place declaration (thus, part of
constructor), initial marking is specified separately (thus, setter).
Additional information is number of pre-transitions and number of
post-transitions. This information is updated while parsing transitions.

\note The members cardPost and cardPre are used for later data structures for
arcs.
\note The capacity UINT_MAX denotes the absence of a capacity.
\note This class does not allocated dynamic memory.

\ingroup g_symboltable

\todo Dopplungen behandeln.
*/
class PlaceSymbol: public Symbol
{
public:
    PlaceSymbol(const char *k, unsigned int cap);

    /// getter for capacity
    capacity_t getCapacity() const;
    /// getter for initial marking
    capacity_t getInitialMarking() const;
    /// getter for number of pre-transitions
    arrayindex_t getCardPre() const;
    /// getter for number of post-transitions
    arrayindex_t getCardPost() const;

    /// adder for initial marking
    void addInitialMarking(capacity_t);
    /// incrementor for number of post-transitions
    void notifyPost();
    /// incrementor for number of pre-transitions
    void notifyPre();

private:
    /// the maximum number of tokens that must be representable for this place
    capacity_t capacity;
    /// the initial number of tokens on this place
    capacity_t initialMarking;
    /// the number of transitions that consume from this place
    arrayindex_t cardPost;
    /// the number of transitions that produce on this place
    arrayindex_t cardPre;
};
