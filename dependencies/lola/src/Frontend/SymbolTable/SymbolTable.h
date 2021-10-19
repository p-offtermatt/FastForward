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

\brief definition of class SymbolTable

\todo Eventuell ein FOREACH Makro (basierend auf first() und next()) bauen
*/

#pragma once

// forward declaration
class Symbol;

/*!
\brief A symbol table for places and transitions.

The table has strings as keys. Payload is not explicitly represented. It can be
attached by deriving from class symbol. Collissions are handled as linked lists.

\ingroup g_symboltable
*/
class SymbolTable
{
public:
    /// overall hash table collisions
    static unsigned int collisions;

public:
    SymbolTable();
    ~SymbolTable();

    /// If symbol with same key is in table: return false
    /// If symbol with same key is not in table: return true and insert it
    bool insert(Symbol *);

    /// If key is in table: return corresponding symbol
    /// If key is not in table: return NULL
    Symbol *lookup(const char *) const;

    /// initialize iteration; return NULL if table empty
    Symbol *first();
    /// continue iteration; return NULL if there is none
    Symbol *next();

    /// get number of entries in table
    unsigned int getCard() const;

private:
    /// the number of entries in table
    unsigned int card;

    /// the actual symbol table. It gets pointers as we use lists for collisions.
    Symbol **table;

    /// the index of the current element in iteration
    unsigned int currentIndex;

    /// points to the current element in iteration
    Symbol *currentSymbol;

    /// the hash function to be used
    unsigned int hash(const char *) const;
};
