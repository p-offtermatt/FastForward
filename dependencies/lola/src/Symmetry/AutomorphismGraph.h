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
\status new

\brief We translate net and formula into a graph. The automorphisms of this
graph define the feasible symmetries for symmetry reduction.
*/

#pragma once

#include <Core/Dimensions.h>

/*!
The graph automorphism problem is defined for colored graphs. In a colored
graph, both vertices and edges have colors. Colors are used for coding all
information that needs to be preserved by the symmetries:

* For places:
 - initial marking (a nonnegative integer)
* For transitions
 - fairness specification (no, weak, strong)
* For inner nodes of formula
 - operator type (G,F,U,R,X,AU,EU,AX,EX,AND,OR)
* For atomic propositions
 - Right hand side of comparison (an integer number)
* For arcs between places and transitions
 - multiplicity
* For edges from inner nodes of formula to subformula
 - order of operands for non-commutative operators (U,AU,EU)
* For edges from atomic proposition to place
 - factor for this place in left hand side of comparison
*/

class Constraint;

class Vertex
{
public:
    Vertex();
    ~Vertex();

    /// sort arcs according to their colors
    void sortArcs(Vertex **, arrayindex_t *, arrayindex_t, arrayindex_t);

    /// total number of vertices
    static arrayindex_t card;

    /// for places: the real id
    /// for transitions: the real id + card(P)
    /// for formula nodes: some consecutive number beyond card(P) + card(T)
    arrayindex_t id;

    /// inscription of the node
    arrayindex_t color;

    /// number of successors in graph
    arrayindex_t cardSucc;
    /// number of predecessors in graph
    arrayindex_t cardPre;

    /// the successors in graph
    Vertex **succ;
    /// the colors of outgoing edges
    arrayindex_t *colorSucc;

    /// the predecessors in graph
    Vertex **pre;
    /// the colors of incoming edges
    arrayindex_t *colorPre;

    /// two arrays of properties: one for this node as argument of symmetry, one as image; array index = thread using it
    int64_t *property[2];
    /// the constraint that this node is contained in (DOM vs COD), for each thread
    Constraint **inConstraint[2];
};
