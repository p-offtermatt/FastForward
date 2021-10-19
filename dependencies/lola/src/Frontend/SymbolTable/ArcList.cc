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

\brief class implementation for a symbol for a list of arcs
*/

#include <Core/Dimensions.h>
#include <Frontend/SymbolTable/ArcList.h>

PlaceSymbol *ArcList::getPlace() const
{
    return place;
}

mult_t ArcList::getMultiplicity() const
{
    return multiplicity;
}

ArcList *ArcList::getNext() const
{
    return next;
}

void ArcList::addMultiplicity(const mult_t i)
{
    multiplicity += i;
}

void ArcList::setNext(Symbol *n)
{
    next = reinterpret_cast<ArcList *>(n);
}

ArcList::ArcList(PlaceSymbol *p, mult_t m)
    : place(p), next(NULL), multiplicity(m)
{}
