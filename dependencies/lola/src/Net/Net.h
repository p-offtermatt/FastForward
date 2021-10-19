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

\brief Global data for net structure

All data that describe the net structure can be found here. Additional
information for places only, or for transition only, is deffered to files
Place.* and Transition.*
*/

#pragma once

#include <Core/Dimensions.h>

class Matrix;

/*!
\brief collection of information related to nodes

All data that describe the net structure can be found here. Additional
information for places only, or for transition only, is deffered to files
Place.* and Transition.*
*/
struct Net
{
    /// number of nodes: Card[PL] = places, Card[TR] = transitions
    static arrayindex_t Card[2];

    /// For each node, the number of nodes in pre- resp. post set.
    /// CardArcs[PL][PRE][17] is the number of transitions that produce on place 17.
    static arrayindex_t *CardArcs[2][2];

    /// For each node, the indices of nodes in pre- reps. post set.
    /// Arc[PL][POST][17][4] is the number of the 5th transition that consumes tokens from place 17.
    static arrayindex_t **Arc[2][2];

    /// for each node, the multiplicities of arcs in pre- resp. post set.
    static mult_t **Mult[2][2];

    /// names of the nodes
    /// Name[TR][15] is the name of transition 15.
    static const char **Name[2];

    // conflictcluster of all nodes
    static uf_node_t * UnionFind;

    /// free all allocated memory
    static void deleteNodes();

    /// calculates progress measure for all transitions (used by sweep line method)
    static void setProgressMeasure();

    /// sorts a concrete pair of arcs and multiplicities (used as helper method for sortAllArcs)
    static void sortArcs(arrayindex_t *arcs, mult_t *mults, const arrayindex_t from,
                         const arrayindex_t to);

    /// sorts all arcs
    static void sortAllArcs();

    /// swaps two places (does not care about ordering)
    static void swapPlaces(arrayindex_t, arrayindex_t);

    /// checks whether all arcs are properly (especially to both directions)
    static bool DEBUG__checkConsistency();

    /// checks whether all arcs are ordered properly
    static bool DEBUG__checkArcOrdering();

    /// print the net (using printf)
    static void print();

    /// assumes that raw net is read and places, transitions and the edges in-between are set properly. Computes additional net information used to speed up the simulation.
    static void preprocess();

    /// calculates the incidence matrix of the net
    static Matrix getIncidenceMatrix(node_t line_type = PL);

private:
    // calculates DeltaT and DeltaHash for each transition
    static void preprocess_organizeDeltas();

    /// calculates the set of conflicting transitions for each transition
    static void preprocess_organizeConflictingTransitions();

    // moves all elements in the range [first1,last1), that are also in [first2,last2), to result.
    // returns the number of elements moved.
    static arrayindex_t set_moveall(arrayindex_t *first1, arrayindex_t *last1,
                                    arrayindex_t *first2, arrayindex_t *last2,
                                    arrayindex_t *result);

    /// calculates all significant places and changes order of places(!)
    static void setSignificantPlaces();
};
