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

\brief definition of class TransitionSymbol
*/

#pragma once

#include <Core/Dimensions.h>
#include <Frontend/SymbolTable/Symbol.h>

// forward declaration
class ArcList;

/*!
\brief a symbol table entry for a transition

Transition symbols carry name as key and a list of incoming arcs, a list of
outgoing arcs, and a fairness assumption as payload. All information is
available in the context of transition definition, thus all information is
provided in the constructor

\note The members cardPost and cardPre, Post, and Pre are used for later data
structures for arcs.
\note All dynamic memory allocated by this class is deallocated in the
destructor.

\ingroup g_symboltable

\todo Dopplungen behandeln.
*/
class TransitionSymbol: public Symbol
{
public:
    TransitionSymbol(const char *, fairnessAssumption_t, ArcList *, ArcList *);
    ~TransitionSymbol();

    /// getter for number of incoming arcs
    unsigned int getCardPre() const;
    /// getter for number of outgoing arcs
    unsigned int getCardPost() const;
    /// getter for incoming arcs
    ArcList *getPre() const;
    /// getter for number of post-places
    ArcList *getPost() const;
    /// getter for fairness assumption
    fairnessAssumption_t getFairness() const;

private:
    /// the fairness assumption specified for this tarnsition
    fairnessAssumption_t fairness;

    /// the number of places that this transition produces to
    unsigned int cardPost;
    /// the number of places that this transition consumes from
    unsigned int cardPre;
    /// the outgoing arcs of this transition
    ArcList *Post;
    /// the ingoing arcs of this transition
    ArcList *Pre;
};
