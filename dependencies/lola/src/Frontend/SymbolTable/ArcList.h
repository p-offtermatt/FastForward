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

\brief class definition for a symbol for a list of arcs
*/

#pragma once

#include <Core/Dimensions.h>

// forward declarations
class Symbol;
class PlaceSymbol;

/*!
\brief arc list (intermediate representation)

This class is used as an intermediate storage between parser and the actual
Petri net data structures. It collects arcs connected in the same dircetion
from/to the same transtion.

\ingroup g_symboltable
\todo Make next and place const?
*/
class ArcList
{
public:
    ArcList(PlaceSymbol *, mult_t);

    /// getter for place
    PlaceSymbol *getPlace() const;

    /// getter for multiplicity
    mult_t getMultiplicity() const;

    /// getter for next
    ArcList *getNext() const;

    /// setter for next
    void setNext(Symbol *);

    /// adding some tokens to multplicity
    void addMultiplicity(const mult_t);

private:
    /// the place at the other end of arc (transition implicilty given)
    PlaceSymbol *place;

    /// arcs are organized as lists
    ArcList *next;

    /// multiplicity
    mult_t multiplicity;
};
