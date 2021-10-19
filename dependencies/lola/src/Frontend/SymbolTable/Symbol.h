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

\brief definition of class Symbol
*/

#pragma once

#include <Core/Dimensions.h>

/*!
\brief a symbol is an entry in a symbol table (class SymbolTable)

It has a string as key. For dealing with collissions in the symbol table,
symbols can be lnked as lists. Payload can be added by deriving subclasses.

\ingroup g_symboltable
*/
class Symbol
{
public:
    explicit Symbol(const char *const);
    virtual ~Symbol() {}

    /// getter for key
    const char *getKey() const;

    /// getter for next
    Symbol *getNext() const;

    /// get index of symbol in net data structures
    arrayindex_t getIndex() const;

    /// setter for next
    void setNext(Symbol *);

    /// set index of symbol in net date structures
    void setIndex(arrayindex_t);

private:
    /// the name of the symbol; used for insertion in symbol table
    const char *const key;

    /// symbols with same hash value are organized as lists
    Symbol *next;

    /// index in net data structure; set during transformation symbols --> net
    arrayindex_t index;
};
