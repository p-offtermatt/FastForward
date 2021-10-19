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

\brief This data structure stores the generating set for all symmetries
which is in Schreier/Sims form.
Upon insertion of a symmetry, it combines it with existing ones,
to find other members of the generating set. It supports application of
the symmetries to a marking.
*/

#pragma once

#include <Core/Dimensions.h>

/*!
A generating set for the symmetries consists of several subsets, called levels.
Within one level, all elements map the first i places to themselves, for some
i, and place i not to i. That is, we store symmetries level by level, and,
within a level, as image vectors starting with place i+1. That is, if sigma is
the fifth symmetry in the second level where i = 12, we get sigma(0) = 0, ...,
sigma(11)= 11, and sigma(j) = store[1][4][j-12], for j >= 12.
*/
class GeneratingSet
{
public:
    /// create data structures
    GeneratingSet();
    ~GeneratingSet();

    /// add a generator
    void add(arrayindex_t, arrayindex_t *);

    /// transform vector of places into (approx.) canonical representative
    capacity_t *canonize(capacity_t *);

    /// realloc level array (for releasing memory)
    void condense_level(arrayindex_t);
    /// realloc store, for releasing memory
    void condense();

    /// initializes a new level
    void openLevel(arrayindex_t);
    /// debug function
    void printGeneratingSet() const;

    /// for progress report
    uint64_t possibleGenerators;
    /// for progress report
    uint64_t knownGenerators;
    /// for progress report
    uint64_t realComputed;

    /// the actual store of symmetry vectors (3-dimensional array)
    arrayindex_t ** *store;
    /// the number of levels
    arrayindex_t cardLevels;

    /// return size of symmetry group
    long double sizeofSymmetryGroup() const;

    static pthread_mutex_t mx;

private:
    /// the value of i as described above
    arrayindex_t *depth;
    // the number of elements in the level
    arrayindex_t *card;
    /// an additional vector for mapping given marking to new marking
    capacity_t *reserve_marking;

    /// build all linear combinations of knows symmetries
    void combine_all(arrayindex_t, arrayindex_t);
    /// build one linear combinations of knows symmetries
    void combine(arrayindex_t, arrayindex_t, arrayindex_t);
};
