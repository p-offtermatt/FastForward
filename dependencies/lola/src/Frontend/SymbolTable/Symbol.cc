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
\brief implementation of class Symbol
*/

#include <Core/Dimensions.h>
#include <Frontend/SymbolTable/Symbol.h>

arrayindex_t Symbol::getIndex() const
{
    return index;
}

const char *Symbol::getKey() const
{
    return key;
}

Symbol *Symbol::getNext() const
{
    return next;
}

void Symbol::setNext(Symbol *sym)
{
    next = sym;
}

void Symbol::setIndex(arrayindex_t i)
{
    index = i;
}

/*!
\param[in] k  the name of the symbol

\note The pointer next is lated set by SymbolTable::insert.
\note The index is later changed by ParserPTNet::symboltable2net.
*/
Symbol::Symbol(const char *const k) :
    key(k),
    next(NULL),  // intermediate initialization
    index(0)     // intermediate initialization
{}
