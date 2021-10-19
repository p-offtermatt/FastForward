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

/*
\file
\author Karsten
		cast progress measure to int32 on 18.12.2012
\status approved 27.01.2012

\brief basic routines for handling nodes
*/

#include <Core/Dimensions.h>
#include <Core/Runtime.h>
#include <Frontend/Parser/ParserPTNet.h>
#include <Frontend/SymbolTable/Symbol.h>
#include <Frontend/SymbolTable/SymbolTable.h>
#include <Net/LinearAlgebra.h>
#include <Net/Marking.h>
#include <Net/Net.h>
#include <Net/Place.h>
#include <Net/Transition.h>
#include <Exploration/Firelist.h>

void    organizeConflictingTransitions();
void    organizeBackConflictingTransitions();
void    printConflicting(arrayindex_t);

arrayindex_t Net::Card[2] = {0, 0};
arrayindex_t *Net::CardArcs[2][2] = {{NULL, NULL}, {NULL, NULL}};
arrayindex_t **Net::Arc[2][2] = {{NULL, NULL}, {NULL, NULL}};
mult_t **Net::Mult[2][2] = {{NULL, NULL}, {NULL, NULL}};
const char **Net::Name[2] = {NULL, NULL};

// LCOV_EXCL_START
bool Net::DEBUG__checkConsistency()
{
    for (int type = PL; type <= TR; ++type)
    {
        node_t othertype = (type == PL) ? TR : PL;
        for (int direction = PRE; direction <= POST; direction++)
        {
            direction_t otherdirection = (direction == PRE) ? POST : PRE;
            for (arrayindex_t n = 0; n < Net::Card[type]; n ++)
            {
                for (arrayindex_t a = 0; a < Net::CardArcs[type][direction][n]; a++)
                {
                    arrayindex_t nn = Net::Arc[type][direction][n][a];
                    arrayindex_t b;
                    for (b = 0; b < Net::CardArcs[othertype][otherdirection][nn]; b++)
                    {
                        if (Net::Arc[othertype][otherdirection][nn][b] == n)
                        {
                            break;
                        }
                    }
                    if (b >= Net::CardArcs[othertype][otherdirection][nn])
                    {
                        return false;
                    }
                }
            }
        }
    }
    return true;
}

bool Net::DEBUG__checkArcOrdering()
{
    arrayindex_t curElem;
    for (arrayindex_t t = 0; t < Net::Card[TR]; ++t)
    {
        if (Net::CardArcs[TR][PRE][t])
        {
            curElem = Net::Arc[TR][PRE][t][0];
            for (arrayindex_t p = 1; p < Net::CardArcs[TR][PRE][t]; ++p)
            {
                assert(curElem < Net::Arc[TR][PRE][t][p]);
            }
        }
        if (Net::CardArcs[TR][POST][t])
        {
            curElem = Net::Arc[TR][POST][t][0];
            for (arrayindex_t p = 1; p < Net::CardArcs[TR][POST][t]; ++p)
            {
                assert(curElem < Net::Arc[TR][POST][t][p]);
            }
        }
    }
    for (arrayindex_t p = 0; p < Net::Card[PL]; ++p)
    {
        if (Net::CardArcs[PL][PRE][p])
        {
            curElem = Net::Arc[PL][PRE][p][0];
            for (arrayindex_t t = 1; t < Net::CardArcs[PL][PRE][p]; ++t)
            {
                assert(curElem < Net::Arc[PL][PRE][p][t]);
            }
        }
        if (Net::CardArcs[PL][POST][p])
        {
            curElem = Net::Arc[PL][POST][p][0];
            for (arrayindex_t t = 1; t < Net::CardArcs[PL][POST][p]; ++t)
            {
                assert(curElem < Net::Arc[PL][POST][p][t]);
            }
        }
    }
    return true;
}
// LCOV_EXCL_STOP


/*!
 Sorts array of arc (= node id) plus corresponding array of multiplicities
 in the range of from to to (not including to)
 \param arcs the arcs to be sorted
 \param mults the multiplicities belonging to the arcs to get sorted
 \param from the beginning index of the range to be sorted
 \param to the end of the range to be sorted (not included)
 \post The arcs the range [from,to) and the corresponding multiplicities are sorted by the id's
*/
void Net::sortArcs(arrayindex_t *arcs, mult_t *mults, const arrayindex_t from,
                   const arrayindex_t to)
{
    if ((to - from) < 2)
    {
        return;    // less than 2 elements are always sorted
    }

    // points to first index where element is not known < pivot
    arrayindex_t blue = from;
    // points to first index where element is not know <= pivot
    arrayindex_t white = from + 1;
    // points to last index (+1) where element is not know to be  pivot
    arrayindex_t red = to;
    const arrayindex_t pivot = arcs[from];

    assert(from < to);

    while (red > white)
    {
        if (arcs[white] < pivot)
        {
            // swap white <-> blue
            const arrayindex_t tmp_index = arcs[blue];
            const mult_t tmp_mult = mults[blue];
            arcs[blue] = arcs[white];
            mults[blue++] = mults[white];
            arcs[white] = tmp_index;
            mults[white++] = tmp_mult;
        }
        else
        {
            // there are no duplicates in arc list
            assert(arcs[white] > pivot);

            // swap white <-> red
            const arrayindex_t tmp_index = arcs[--red];
            const mult_t tmp_mult = mults[red];
            arcs[red] = arcs[white];
            mults[red] = mults[white];
            arcs[white] = tmp_index;
            mults[white] = tmp_mult;
        }
    }

    assert(blue + 1 == red);

    sortArcs(arcs, mults, from, blue);
    sortArcs(arcs, mults, red, to);
}

void clusterSortArcs(arrayindex_t *arcs, mult_t *mults, const arrayindex_t from,
                   const arrayindex_t to)
{
    if ((to - from) < 2)
    {
        return;    // less than 2 elements are always sorted
    }

    // points to first index where element is not known < pivot
    arrayindex_t blue = from;
    // points to first index where element is not know <= pivot
    arrayindex_t white = from + 1;
    // points to last index (+1) where element is not know to be  pivot
    arrayindex_t red = to;
    const arrayindex_t pivotcard = Net::CardArcs[PL][POST][arcs[from]];
    const arrayindex_t pivotindex = arcs[from];
    

    assert(from < to);

    while (red > white)
    {
        if (Net::CardArcs[PL][POST][arcs[white]] > pivotcard || (Net::CardArcs[PL][POST][arcs[white]] == pivotcard && arcs[white] < pivotindex))
        {
            // swap white <-> blue
            const arrayindex_t tmp_index = arcs[blue];
            const mult_t tmp_mult = mults[blue];
            arcs[blue] = arcs[white];
            mults[blue++] = mults[white];
            arcs[white] = tmp_index;
            mults[white++] = tmp_mult;
        }
        else
        {
            // there are no duplicates according to sort criterion

            // swap white <-> red
            const arrayindex_t tmp_index = arcs[--red];
            const mult_t tmp_mult = mults[red];
            arcs[red] = arcs[white];
            mults[red] = mults[white];
            arcs[white] = tmp_index;
            mults[white] = tmp_mult;
        }
    }

    assert(blue + 1 == red);

    clusterSortArcs(arcs, mults, from, blue);
    clusterSortArcs(arcs, mults, red, to);
}

void backClusterSortArcs(arrayindex_t *arcs, mult_t *mults, const arrayindex_t from,
                   const arrayindex_t to)
{
    if ((to - from) < 2)
    {
        return;    // less than 2 elements are always sorted
    }

    // points to first index where element is not known < pivot
    arrayindex_t blue = from;
    // points to first index where element is not know <= pivot
    arrayindex_t white = from + 1;
    // points to last index (+1) where element is not know to be  pivot
    arrayindex_t red = to;
    const arrayindex_t pivotcard = Net::CardArcs[PL][PRE][arcs[from]];
    const arrayindex_t pivotindex = arcs[from];
    

    assert(from < to);

    while (red > white)
    {
        if (Net::CardArcs[PL][PRE][arcs[white]] > pivotcard || (Net::CardArcs[PL][PRE][arcs[white]] == pivotcard && arcs[white] < pivotindex))
        {
            // swap white <-> blue
            const arrayindex_t tmp_index = arcs[blue];
            const mult_t tmp_mult = mults[blue];
            arcs[blue] = arcs[white];
            mults[blue++] = mults[white];
            arcs[white] = tmp_index;
            mults[white++] = tmp_mult;
        }
        else
        {
            // there are no duplicates according to sort criterion

            // swap white <-> red
            const arrayindex_t tmp_index = arcs[--red];
            const mult_t tmp_mult = mults[red];
            arcs[red] = arcs[white];
            mults[red] = mults[white];
            arcs[white] = tmp_index;
            mults[white] = tmp_mult;
        }
    }

    assert(blue + 1 == red);

    clusterSortArcs(arcs, mults, from, blue);
    clusterSortArcs(arcs, mults, red, to);
}

/*!
Sorts all Arcs and their mutliplicities (wraps up sortArcs)
\note: this invalidates the disabled lists and scapegoats and is therefore only allowed BEFORE calling Transition::checkEnabled_initial
\pre Transition::checkEnabled_initial is not allowed to be called before this function
\post All Arcs and their multiplicities are sorted by their ids
*/
void Net::sortAllArcs()
{
    for (int type = PL; type <= TR; ++type)
    {
        for (int direction = PRE; direction <= POST; direction++)
        {
            for (arrayindex_t n = 0; n < Net::Card[type]; n ++)
            {
                sortArcs(Net::Arc[type][direction][n], Net::Mult[type][direction][n], 0,
                         CardArcs[type][direction][n]);
            }
        }
    }
    assert(DEBUG__checkArcOrdering());
}

void clusterSortAllArcs()
{
    for (arrayindex_t n = 0; n < Net::Card[TR]; n ++)
    {
	clusterSortArcs(Net::Arc[TR][PRE][n], Net::Mult[TR][PRE][n], 0,
		 Net::CardArcs[TR][PRE][n]);
    }
    assert(DEBUG__checkArcOrdering());
}

void backClusterSortAllArcs()
{
    for (arrayindex_t n = 0; n < Net::Card[TR]; n ++)
    {
	backClusterSortArcs(Net::Arc[TR][POST][n], Net::Mult[TR][POST][n], 0,
		 Net::CardArcs[TR][POST][n]);
    }
    assert(DEBUG__checkArcOrdering());
}

/*!
Frees all allocated memory
\note memory is allocated in ParserPTNet.cc, ParserPTNet::symboltable2net()
\post the memory for the arcs,multiplicities and name are free'd
*/
void Net::deleteNodes()
{
    for (int type = PL; type <= TR; ++type)
    {
        for (int direction = PRE; direction <= POST; ++direction)
        {
            for (arrayindex_t i = 0; i < Net::Card[type]; i++)
            {
                delete[] Net::Arc[type][direction][i];
                delete[] Net::Mult[type][direction][i];
            }
            delete[] Net::CardArcs[type][direction];
            delete[] Net::Arc[type][direction];
            delete[] Net::Mult[type][direction];
        }
        for (arrayindex_t i = 0; i < Net::Card[type]; i++)
        {
            // names are allocated via strdup
            free(const_cast<char *>(Net::Name[type][i]));
        }
        delete[] Net::Name[type];
    }

}
/*!
prints the Net
*/
void Net::print()
{
    printf("Net\n===\n\n");

    printf("%u places,  %u  transitions.\n\n", Net::Card[PL], Net::Card[TR]);

    for (arrayindex_t i = 0; i < Net::Card[PL]; i++)
    {
        printf("Place %u :%s, %u tokens hash %llu capacity %u bits %u\n", i, Net::Name[PL][i],
               Marking::Initial[i], Place::Hash[i], Place::Capacity[i], Place::CardBits[i]);
        printf("From (%u):\n", Net::CardArcs[PL][PRE][i]);
        for (arrayindex_t j = 0; j < Net::CardArcs[PL][PRE][i]; j++)
        {
            printf("%s:%d ", Net::Name[TR][Net::Arc[PL][PRE][i][j]], Net::Mult[PL][PRE][i][j]);
        }
        printf("\n");
        printf("To (%u):\n", Net::CardArcs[PL][POST][i]);
        for (arrayindex_t j = 0; j < Net::CardArcs[PL][POST][i]; j++)
        {
            printf("%s:%d ", Net::Name[TR][Net::Arc[PL][POST][i][j]], Net::Mult[PL][POST][i][j]);
        }
        printf("\n");
    }

    for (arrayindex_t i = 0; i < Net::Card[TR]; i++)
    {
        printf("\nTransition %u :%s\n", i, Net::Name[TR][i]);
        switch (Transition::Fairness[i])
        {
        case NO_FAIRNESS:
            printf(" no ");
            break;
        case WEAK_FAIRNESS:
            printf(" weak ");
            break;
        case STRONG_FAIRNESS:
            printf(" strong ");
            break;
        }
        printf(" %s ", (Transition::Enabled[i] ? "enabled" : "disabled"));

        printf("From:\n");
        for (arrayindex_t j = 0; j < Net::CardArcs[TR][PRE][i]; j++)
        {
            printf("%s:%u ", Net::Name[PL][Net::Arc[TR][PRE][i][j]], Net::Mult[TR][PRE][i][j]);
        }
        printf("\n");
        printf("To:\n");
        for (arrayindex_t j = 0; j < Net::CardArcs[TR][POST][i]; j++)
        {
            printf("%s:%u ", Net::Name[PL][Net::Arc[TR][POST][i][j]], Net::Mult[TR][POST][i][j]);
        }
        printf("\n");
        printf("remove from");
        for (arrayindex_t j = 0; j < Transition::CardDeltaT[PRE][i]; j++)
        {
            printf(" %s:%u", Net::Name[PL][Transition::DeltaT[PRE][i][j]], Transition::MultDeltaT[PRE][i][j]);
        }
        printf("produce on");
        for (arrayindex_t j = 0; j < Transition::CardDeltaT[POST][i]; j++)
        {
            printf(" %s:%u", Net::Name[PL][Transition::DeltaT[POST][i][j]], Transition::MultDeltaT[POST][i][j]);
        }
        printf("\n conflicting:");
        for (arrayindex_t j = 0; j < Transition::CardConflicting[i]; j++)
        {
            printf("%s", Net::Name[TR][Transition::Conflicting[i][j]]);
        }
        printf("\n");
        for (arrayindex_t j = 0; j < Transition::CardBackConflicting[i]; j++)
        {
            printf("%s", Net::Name[TR][Transition::BackConflicting[i][j]]);
        }
    }
    printf("done\n");
}

extern ParserPTNet *symbolTables;
//TODO Comment me
void Net::swapPlaces(arrayindex_t left, arrayindex_t right)
{
    // 1. Net data structures

    int ixdLeft = symbolTables->PlaceTable->lookup(Net::Name[PL][left])->getIndex();
    symbolTables->PlaceTable->lookup(Net::Name[PL][left])->setIndex(symbolTables->PlaceTable->lookup(
                Net::Name[PL][right])->getIndex());
    symbolTables->PlaceTable->lookup(Net::Name[PL][right])->setIndex(ixdLeft);

    const char *tempname = Net::Name[PL][left];
    Net::Name[PL][left] = Net::Name[PL][right];
    Net::Name[PL][right] = tempname;

    assert(DEBUG__checkConsistency());

    for (int direction = PRE; direction <= POST; ++direction)
    {
        arrayindex_t tempindex = Net::CardArcs[PL][direction][left];
        Net::CardArcs[PL][direction][left] = Net::CardArcs[PL][direction][right];
        Net::CardArcs[PL][direction][right] = tempindex;

        arrayindex_t *tempindexpointer = Net::Arc[PL][direction][left];
        Net::Arc[PL][direction][left] = Net::Arc[PL][direction][right];
        Net::Arc[PL][direction][right] = tempindexpointer;

        mult_t *tempmultpointer = Net::Mult[PL][direction][left];
        Net::Mult[PL][direction][left] = Net::Mult[PL][direction][right];
        Net::Mult[PL][direction][right] = tempmultpointer;
    }

    // the tricky part of 1.: references to left and right in transition arc lists

    // It is tricky because both places may refer to the same transition, so we do not
    // trivially known whether a change has already been done. Scanning through all transitions
    // rather than the environments of left and right is too costly, so we chose to go
    // through all transitions in the environment of one transition twice with setting temporary indices in the
    // first run.

    // Part of the trick is that all Net:: information for place left is already at index right

    // first run left
    for (int direction = PRE; direction <= POST; ++direction)
    {
        for (arrayindex_t a = 0; a < Net::CardArcs[PL][direction][right]; ++a)
        {
            direction_t otherdirection = (direction == PRE) ? POST : PRE;
            const arrayindex_t t = Net::Arc[PL][direction][right][a];
            for (arrayindex_t b = 0; b < Net::CardArcs[TR][otherdirection][t]; b++)
            {
                if (Net::Arc[TR][otherdirection][t][b] == left)
                {
                    Net::Arc[TR][otherdirection][t][b] = Net::Card[PL];
                }
            }
        }
    }
    // only run right
    for (int direction = PRE; direction <= POST; ++direction)
    {
        for (arrayindex_t a = 0; a < Net::CardArcs[PL][direction][left]; ++a)
        {
            direction_t otherdirection = (direction == PRE) ? POST : PRE;
            const arrayindex_t t = Net::Arc[PL][direction][left][a];
            for (arrayindex_t b = 0; b < Net::CardArcs[TR][otherdirection][t]; b++)
            {
                if (Net::Arc[TR][otherdirection][t][b] == right)
                {
                    Net::Arc[TR][otherdirection][t][b] = left;
                }
            }
        }
    }
    // second run left
    for (int direction = PRE; direction <= POST; ++direction)
    {
        for (arrayindex_t a = 0; a < Net::CardArcs[PL][direction][right]; ++a)
        {
            direction_t otherdirection = (direction == PRE) ? POST : PRE;
            const arrayindex_t t = Net::Arc[PL][direction][right][a];
            for (arrayindex_t b = 0; b < Net::CardArcs[TR][otherdirection][t]; b++)
            {
                if (Net::Arc[TR][otherdirection][t][b] == Net::Card[PL])
                {
                    Net::Arc[TR][otherdirection][t][b] = right;
                }
            }
        }
    }
    assert(DEBUG__checkConsistency());

    // 2. Place data structures

    hash_t temphash = Place::Hash[left];
    Place::Hash[left] = Place::Hash[right];
    Place::Hash[right] = temphash;

    capacity_t tempcapacity = Place::Capacity[left];
    Place::Capacity[left] = Place::Capacity[right];
    Place::Capacity[right] = tempcapacity;

    cardbit_t tempcardbit = Place::CardBits[left];
    Place::CardBits[left] = Place::CardBits[right];
    Place::CardBits[right] = tempcardbit;
    // 3. Marking data structures

    capacity_t tempmarking = Marking::Initial[left];
    Marking::Initial[left] = Marking::Initial[right];
    Marking::Initial[right] = tempmarking;

    if (Marking::Current)
    {
        tempmarking = Marking::Current[left];
        Marking::Current[left] = Marking::Current[right];
        Marking::Current[right] = tempmarking;
    }

    // 4. Transition data structures

    // again, the tricky way...
    // Part of the trick is that all Net:: information for place left is already at index right

    // first run left
    for (int direction = PRE; direction <= POST; ++direction)
    {
        for (arrayindex_t a = 0; a < Net::CardArcs[PL][direction][right]; ++a)
        {
            direction_t otherdirection = (direction == PRE) ? POST : PRE;
            const arrayindex_t t = Net::Arc[PL][direction][right][a];
            for (arrayindex_t b = 0; b < Transition::CardDeltaT[otherdirection][t]; b++)
            {
                if (Transition::DeltaT[otherdirection][t][b] == left)
                {
                    Transition::DeltaT[otherdirection][t][b] = Net::Card[PL];
                }
            }
        }
    }
    // only run right
    for (int direction = PRE; direction <= POST; ++direction)
    {
        for (arrayindex_t a = 0; a < Net::CardArcs[PL][direction][left]; ++a)
        {
            direction_t otherdirection = (direction == PRE) ? POST : PRE;
            const arrayindex_t t = Net::Arc[PL][direction][left][a];
            for (arrayindex_t b = 0; b < Transition::CardDeltaT[otherdirection][t]; b++)
            {
                if (Transition::DeltaT[otherdirection][t][b] == right)
                {
                    Transition::DeltaT[otherdirection][t][b] = left;
                }
            }
        }
    }
    for (int direction = PRE; direction <= POST; ++direction)
    {
        // second run left
        for (arrayindex_t a = 0; a < Net::CardArcs[PL][direction][right]; ++a)
        {
            direction_t otherdirection = (direction == PRE) ? POST : PRE;
            const arrayindex_t t = Net::Arc[PL][direction][right][a];
            for (arrayindex_t b = 0; b < Transition::CardDeltaT[otherdirection][t]; b++)
            {
                if (Transition::DeltaT[otherdirection][t][b] == Net::Card[PL])
                {
                    Transition::DeltaT[otherdirection][t][b] = right;
                }
            }
        }
    }
    assert(DEBUG__checkConsistency());
}

/*!
Creates an equation for the given transition (index) in the provided memory

\param[in] transition the transition for which the equation should be created and saved
\param[in,out] variables a empty array of place indexes, gets set by this function regarding to the places influenced by the given transition
\param[in,out] coefficients a empty array of coefficients, the values getting set by this function regarding the transition multiplicities torwards the corresponding places
\param[in,out] size gets set to the number of elements in the row
\param[in] dual signs if transitions or places should build the rows of the matrix
\post The array variables, the array coefficients and the variable size are set (the rowparts are defined)
 */
void createTransitionEquation(arrayindex_t transition, arrayindex_t *variables,
                              int64_t *coefficients,
                              arrayindex_t &size, bool dual = false)
{
    // check if we have transition or places rows
    arrayindex_t ND(dual ? PL : TR);

    // index in new row
    size = 0;
    // for each place p in the preset of t
    for (arrayindex_t p = 0; p < Net::CardArcs[ND][PRE][transition]; ++p)
    {
        // store place index and the it's multiplicity (from p to t)
        variables[size] = Net::Arc[ND][PRE][transition][p];
        // positive numbers
        assert(Net::Mult[ND][PRE][transition][p] != 0);
        coefficients[size] = Net::Mult[ND][PRE][transition][p];
        // increase newSize
        ++size;
    }
    // for each place p in the postset of t
    for (arrayindex_t p = 0; p < Net::CardArcs[ND][POST][transition]; ++p)
    {
        const arrayindex_t pID = Net::Arc[ND][POST][transition][p];
        assert(Net::Mult[ND][POST][transition][p] != 0);

        // check whether the p is already in the new row
        // enumerate newVar till p is hit or not inside
        arrayindex_t possiblePosition = 0;
        for (; possiblePosition < size; ++possiblePosition)
        {
            if (variables[possiblePosition] >= pID)
            {
                break;
            }
        }
        // distinguish which case is true (hit or not in)
        if (variables[possiblePosition] == pID)
        {
            // p is already inside the new row, so subtract current multiplicity
            coefficients[possiblePosition] -= Net::Mult[ND][POST][transition][p];
            // new coefficient may be 0 now
            if (coefficients[possiblePosition] == 0)
            {
                // erase possiblePosition entry (possiblePosition) in both array
                memmove(&variables[possiblePosition], &variables[possiblePosition + 1],
                        (size - possiblePosition) * SIZEOF_ARRAYINDEX_T);
                memmove(&coefficients[possiblePosition], &coefficients[possiblePosition + 1],
                        (size - possiblePosition) * SIZEOF_INT64_T);
                // assumption: decreasing 0 will lead to maxInt but
                //              upcoming increase will result in 0 again
                --size;
            }
        }
        else
        {
            // p is not in new row, so add it
            // may be it is necessary to insert in between existing entrys
            memmove(&variables[possiblePosition + 1], &variables[possiblePosition],
                    (size - possiblePosition) * SIZEOF_ARRAYINDEX_T);
            memmove(&coefficients[possiblePosition + 1], &coefficients[possiblePosition],
                    (size - possiblePosition) * SIZEOF_INT64_T);
            // store place index
            variables[possiblePosition] = pID;
            // store the multiplicity (from transition to p)
            // negative numbers
            coefficients[possiblePosition] = -Net::Mult[ND][POST][transition][p];
            // increase newSize
            ++size;
        }
    }
}

/*!
calculates a incidenceMatrix which contains information about the connections between transitions and places
\param line_type the type the columns in the matrix should have (implies the rowtype)
\pre arcs must be sorted
\return a incidence matrix for the net
*/
Matrix Net::getIncidenceMatrix(node_t line_type)
{
    // the row type is the dual to the line type
    const node_t row_type = (line_type == PL) ? TR : PL;

    // get the cardinalities of lines and rows (the dimensions of the matrix)
    const arrayindex_t line_card = Net::Card[line_type];
    const arrayindex_t row_card = Net::Card[row_type];

    // arcs must be sorted
    assert(Net::DEBUG__checkArcOrdering());

    // request memory for one full row
    arrayindex_t *newVar = new arrayindex_t[line_card]();
    int64_t *newCoef = new int64_t[line_card]();
    arrayindex_t newSize;

    // create new matrix
    Matrix m(line_card);

    // load rows into matrix
    // for each transition t
    for (arrayindex_t t = 0; t < row_card; ++t)
    {
        // create equation for current transition
        createTransitionEquation(t, newVar, newCoef, newSize, (line_type == TR));
        // save current arrays as new row
        m.addRow(newSize, newVar, newCoef);

        // clear used memory
        memset(newVar, 0, newSize * SIZEOF_ARRAYINDEX_T);    // necessary?
        memset(newCoef, 0, newSize * SIZEOF_INT64_T);
    }

    // free memory
    delete[] newVar;
    delete[] newCoef;

    return m;
}

/*!
Calculates all signficant places and sorts them to the front of the place array
\pre the arcs must be sorted
\post the significant places are in the front of the place array
*/
void Net::setSignificantPlaces()
{
    RT::rep->status("finding significant places");

    // arcs must be sorted
    assert(Net::DEBUG__checkArcOrdering());

    // save number of places
    const arrayindex_t cardPL = Net::Card[PL];

    if (cardPL < 2)
    {
        // nothing to do
        Place::CardSignificant = cardPL;
        return;
    }

    // get incidence matrix (places are lines)
    Matrix m = getIncidenceMatrix(PL);

    // reduce matrix
    m.reduce();

    // gather significant places
    Place::CardSignificant = m.getSignificantColCount();
    arrayindex_t lastSignificant = cardPL - 1;
    arrayindex_t p = 0;
    while (p < Place::CardSignificant)
    {
        if (!m.isSignificant(p))
        {
            // p needs to be swapped
            // find first significant place from the right end of all places
            while (!m.isSignificant(lastSignificant))
            {
                lastSignificant--;
            }
            // swap lastSignificant with p
            Net::swapPlaces(p, lastSignificant--);
        }
        p++;
    }

    // adjust Place::SizeOfBitVector
    Place::SizeOfBitVector = 0;
    for (arrayindex_t i = 0; i < Place::CardSignificant; i++)
    {
        Place::SizeOfBitVector += Place::CardBits[i];
    }
}

/*!
Calculates the progress measure for all transitions
\pre arcs must be orderd
\post Transition::ProgressMeasure is calculated
*/
void Net::setProgressMeasure()
{
    // arcs must be sorted
    assert(Net::DEBUG__checkArcOrdering());

    // get incidence matrix (places are lines)
    Matrix m = getIncidenceMatrix(TR);

    // reduce matrix
    m.diagonalise();

    // save number of transitions
    const arrayindex_t cardTR = Net::Card[TR];

    // calculate progress measure
    int64_t *progressMeasure = new int64_t[cardTR]();
    for (arrayindex_t t = 0; t < cardTR; ++t)
        if (m.isSignificant(t))
        {
            const Matrix::Row *curRow = m.getRow(t);
            progressMeasure[curRow->variables[0]] = curRow->coefficients[0];
            for (arrayindex_t v = 1; v < curRow->varCount; ++v)
                if (!m.isSignificant(curRow->variables[v]))
                {
                    progressMeasure[curRow->variables[v]] += curRow->coefficients[v];
                }
        }

    // try for a local optimisation (reduce number of transitions with negative progress)
    while (true)
    {
        arrayindex_t tindex(ARRAYINDEX_T_MAX);
        int32_t changes(-1), ctmp;

        for (arrayindex_t t = 0; t < cardTR; ++t)
            if (m.isSignificant(t))
            {
                ctmp = 0;
                const Matrix::Row *curRow = m.getRow(t);
                for (arrayindex_t v = 0; v < curRow->varCount; ++v)
                    if (curRow->coefficients[v] < 0 && progressMeasure[curRow->variables[v]] < 0)
                    {
                        if (2 * curRow->coefficients[v] <= progressMeasure[curRow->variables[v]])
                        {
                            ++ctmp;
                        }
                    }
                    else if (curRow->coefficients[v] > 0 && progressMeasure[curRow->variables[v]] > 0)
                        if (2 * curRow->coefficients[v] > progressMeasure[curRow->variables[v]])
                        {
                            --ctmp;
                        }
                if (ctmp > changes)
                {
                    if (ctmp > 0 || curRow->coefficients[0] > 0)
                    {
                        changes = ctmp;
                        tindex = t;
                    }
                }
            }
        if (tindex == ARRAYINDEX_T_MAX)
        {
            break;
        }

        const Matrix::Row *curRow = m.getRow(tindex);
        for (arrayindex_t v = 0; v < curRow->varCount; ++v)
        {
            progressMeasure[curRow->variables[v]] -= 2 * curRow->coefficients[v];
            curRow->coefficients[v] -= curRow->coefficients[v];
        }
    }

    if (RT::args.sweeplinespread_arg > 1)
    {
        // try for another local optimisation (spread progress values better)
        // first, save the progress measures so far
        int64_t *progressCopy = new int64_t[cardTR];
        memcpy(progressCopy, progressMeasure, cardTR * SIZEOF_INT64_T);

        arrayindex_t threads(RT::args.threads_arg), tries(cardTR), fullbucket(cardTR / threads + 1),
                     maxbucket(cardTR);
        std::set<arrayindex_t> done;
        while (--tries > 0)
        {
            std::map<int64_t, arrayindex_t> pbuckets;
            arrayindex_t highbucket(1);
            for (arrayindex_t t = 0; t < cardTR; ++t)
                if (++pbuckets[progressMeasure[t]] > highbucket)
                {
                    ++highbucket;
                }
            if (fullbucket * fullbucket > cardTR)
            {
                --fullbucket;
            }
            if (highbucket <= maxbucket)
            {
                maxbucket = highbucket - 1;
            }
            if (maxbucket <= fullbucket)
            {
                break;
            }
            std::set<int64_t> tryvalues;
            for (std::map<int64_t, arrayindex_t>::iterator it = pbuckets.begin(); it != pbuckets.end(); ++it)
                if (it->second > maxbucket)
                {
                    tryvalues.insert(it->first);
                }
            /*
            		cout << "progress values with too many transitions(" << maxbucket << "," << fullbucket << "): ";
            		for(std::set<int64_t>::iterator it=tryvalues.begin(); it!=tryvalues.end(); ++it)
            			cout << *it << "(" << pbuckets[*it] << ") ";
            		cout << endl;
            */
            arrayindex_t t;
            for (t = 0; t < cardTR; ++t)
                if (m.isSignificant(t))
                {
                    if (done.find(t) != done.end())
                    {
                        continue;
                    }
                    const Matrix::Row *curRow = m.getRow(t);
                    if (tryvalues.find(curRow->coefficients[0]) == tryvalues.end())
                    {
                        continue;
                    }

                    int64_t mult;
                    for (mult = 2; mult <= RT::args.sweeplinespread_arg; ++mult)
                    {
                        int32_t ctmp = 0;
                        for (arrayindex_t v = 0; v < curRow->varCount; ++v)
                            if (curRow->coefficients[v] > 0 && progressMeasure[curRow->variables[v]] < 0)
                            {
                                if (progressMeasure[curRow->variables[v]] + (mult - 1)*curRow->coefficients[v] >= 0)
                                {
                                    ++ctmp;
                                }
                            }
                            else if (curRow->coefficients[v] < 0 && progressMeasure[curRow->variables[v]] > 0)
                                if (progressMeasure[curRow->variables[v]] + (mult - 1)*curRow->coefficients[v] < 0)
                                {
                                    --ctmp;
                                }
                        if (ctmp < 0)
                        {
                            continue;
                        }

                        int64_t toofull(0);
                        for (arrayindex_t v = 0; v < curRow->varCount; ++v)
                        {
                            if (pbuckets[progressMeasure[curRow->variables[v]]] > fullbucket)
                            {
                                --toofull;
                            }
                            if (pbuckets[progressMeasure[curRow->variables[v]] + (mult - 1)*curRow->coefficients[v]] >
                                    fullbucket)
                            {
                                ++toofull;
                            }
                        }
                        if (toofull >= 0)
                        {
                            continue;
                        }

                        done.insert(t);

                        for (arrayindex_t v = 0; v < curRow->varCount; ++v)
                        {
                            progressMeasure[curRow->variables[v]] += (mult - 1) * curRow->coefficients[v];
                            curRow->coefficients[v] *= mult;
                        }

                        break;
                    }
                    if (mult <= RT::args.sweeplinespread_arg)
                    {
                        break;
                    }
                }
            if (t == cardTR)
            {
                ++fullbucket;
            }
        }

        // check if the optimisation uses the buckets in a better way (especially more buckets)
        std::set<int64_t> oldcnt, newcnt;
        for (arrayindex_t t = 0; t < cardTR; ++t)
        {
            oldcnt.insert(progressCopy[t]);
            newcnt.insert(progressMeasure[t]);
        }
        if (newcnt.size() <= oldcnt.size())
        {
            memcpy(progressMeasure, progressCopy, cardTR * SIZEOF_INT64_T);
        }
        else
        {
            float oldrange, newrange, oldsize, newsize;
            oldrange = static_cast<float>(*(oldcnt.rbegin()) - * (oldcnt.begin()) + 1);
            newrange = static_cast<float>(*(newcnt.rbegin()) - * (newcnt.begin()) + 1);
            oldsize = static_cast<float>(oldcnt.size());
            newsize = static_cast<float>(newcnt.size());
            //cout << "oldrange=" << oldrange << " newrange=" << newrange << " oldsize=" << oldsize << " newsize=" << newsize << endl;
            if (newrange / oldrange >= newsize * newsize / (oldsize * oldsize))
            {
                memcpy(progressMeasure, progressCopy, cardTR * SIZEOF_INT64_T);
            }
            else
            {
                std::cout << "progress adapted" << std::endl;
            }
        }

        delete[] progressCopy;
    }

    // remove gcd from progress values
    int64_t gcd(0);
    for (arrayindex_t t = 0; t < cardTR; ++t)
        if (gcd == 0)
        {
            gcd = progressMeasure[t];
        }
        else if (progressMeasure[t] != 0)
        {
            gcd = ggt(gcd, progressMeasure[t]);
        }
    if (gcd < 0)
    {
        gcd = -gcd;
    }
    if (gcd != 0)
        for (arrayindex_t t = 0; t < cardTR; ++t)
        {
            progressMeasure[t] /= gcd;
        }

    //    for(arrayindex_t t=0; t<cardTR; ++t)
    //        RT::rep->status("progress[%s]=%lld", Net::Name[TR][t], progressMeasure[t]);

    // cast progress measures to 32bit
    Transition::ProgressMeasure = new int32_t[cardTR]();
    for (arrayindex_t t = 0; t < cardTR; ++t)
    {
        Transition::ProgressMeasure[t] = (int32_t) progressMeasure[t];
    }

    // free memory
    delete[] progressMeasure;
}

/*!
calculates DeltaT and DeltaHash for each transition
\post Transition::DeltaT, Transition::MultDeltaT and Transition::CardDeltaT (PRE and POST) are set for each transition
*/
void Net::preprocess_organizeDeltas()
{
    const arrayindex_t cardPL = Net::Card[PL];
    const arrayindex_t cardTR = Net::Card[TR];

    // temporarily collect places where a transition has negative token balance
    arrayindex_t *delta_pre = new arrayindex_t[cardPL]();
    // temporarily collect places where a transition has positive token balance.
    arrayindex_t *delta_post = new arrayindex_t[cardPL]();

    // same for multiplicities
    mult_t *mult_pre = new mult_t[cardPL];
    // same for multiplicities
    mult_t *mult_post = new mult_t[cardPL];

    for (arrayindex_t t = 0; t < cardTR; t++)
    {
        // initialize DeltaT structures
        arrayindex_t card_delta_pre = 0;
        arrayindex_t card_delta_post = 0;
        Net::sortArcs(Net::Arc[TR][PRE][t], Net::Mult[TR][PRE][t], 0, Net::CardArcs[TR][PRE][t]);
        Net::sortArcs(Net::Arc[TR][POST][t], Net::Mult[TR][POST][t], 0, Net::CardArcs[TR][POST][t]);

        // parallel iteration through sorted pre and post arc sets
        arrayindex_t i, j;
        for (i = 0, j = 0; (i < Net::CardArcs[TR][PRE][t])
                && (j < Net::CardArcs[TR][POST][t]); /* tricky increment */)
        {
            if (Net::Arc[TR][PRE][t][i] == Net::Arc[TR][POST][t][j])
            {
                // double arc, compare multiplicities
                if (Net::Mult[TR][PRE][t][i] == Net::Mult[TR][POST][t][j])
                {
                    // test arc, does not contribute to delta t
                }
                else
                {
                    if (Net::Mult[TR][PRE][t][i] < Net::Mult[TR][POST][t][j])
                    {
                        // positive impact --> goes to delta post
                        delta_post[card_delta_post] = Net::Arc[TR][POST][t][j];
                        mult_post[card_delta_post++] = (mult_t)(Net::Mult[TR][POST][t][j] - Net::Mult[TR][PRE][t][i]);
                    }
                    else
                    {
                        // negative impact --> goes to delta pre
                        delta_pre[card_delta_pre] = Net::Arc[TR][PRE][t][i];
                        mult_pre[card_delta_pre++] = (mult_t)(Net::Mult[TR][PRE][t][i] - Net::Mult[TR][POST][t][j]);
                    }
                }
                ++i;
                ++j;
            }
            else
            {
                if (Net::Arc[TR][PRE][t][i] < Net::Arc[TR][POST][t][j])
                {
                    // single arc goes to PRE
                    delta_pre[card_delta_pre] = Net::Arc[TR][PRE][t][i];
                    mult_pre[card_delta_pre++] = Net::Mult[TR][PRE][t][i++];
                }
                else
                {
                    // single arc goes to POST
                    delta_post[card_delta_post] = Net::Arc[TR][POST][t][j];
                    mult_post[card_delta_post++] = Net::Mult[TR][POST][t][j++];
                }
            }
        }

        // empty nonempty lists
        for (; i < Net::CardArcs[TR][PRE][t]; i++)
        {
            // single arc goes to PRE
            delta_pre[card_delta_pre] = Net::Arc[TR][PRE][t][i];
            mult_pre[card_delta_pre++] = Net::Mult[TR][PRE][t][i];
        }
        for (; j < Net::CardArcs[TR][POST][t]; j++)
        {
            // single arc goes to POST
            delta_post[card_delta_post] = Net::Arc[TR][POST][t][j];
            mult_post[card_delta_post++] = Net::Mult[TR][POST][t][j];
        }


        /*********************
        * 7a. Copy Deltas *
        **********************/

        // allocate memory for deltas
        Transition::CardDeltaT[PRE][t] = card_delta_pre;
        Transition::CardDeltaT[POST][t] = card_delta_post;
        Transition::DeltaT[PRE][t] = new arrayindex_t[card_delta_pre];
        Transition::DeltaT[POST][t] = new arrayindex_t[card_delta_post];
        Transition::MultDeltaT[PRE][t] = new mult_t[card_delta_pre];
        Transition::MultDeltaT[POST][t] = new mult_t[card_delta_post];

        // copy information on deltas
        memcpy(Transition::DeltaT[PRE][t], delta_pre, card_delta_pre * SIZEOF_ARRAYINDEX_T);
        memcpy(Transition::MultDeltaT[PRE][t], mult_pre, card_delta_pre * SIZEOF_MULT_T);
        memcpy(Transition::DeltaT[POST][t], delta_post, card_delta_post * SIZEOF_ARRAYINDEX_T);
        memcpy(Transition::MultDeltaT[POST][t], mult_post, card_delta_post * SIZEOF_MULT_T);
    }

    delete[] delta_pre;
    delete[] delta_post;
    delete[] mult_pre;
    delete[] mult_post;

    /*********************
    * 7b. Set DeltaHash *
    **********************/

    for (arrayindex_t t = 0; t < Net::Card[TR]; t++)
    {
        for (arrayindex_t i = 0; i < Transition::CardDeltaT[PRE][t]; i++)
        {
            Transition::DeltaHash[t] = (Transition::DeltaHash[t] - Transition::MultDeltaT[PRE][t][i] *
                                        Place::Hash[Transition::DeltaT[PRE][t][i]]) % SIZEOF_MARKINGTABLE;
        }
        for (arrayindex_t i = 0; i < Transition::CardDeltaT[POST][t]; i++)
        {
            Transition::DeltaHash[t] = (Transition::DeltaHash[t] + Transition::MultDeltaT[POST][t][i] *
                                        Place::Hash[Transition::DeltaT[POST][t][i]]) % SIZEOF_MARKINGTABLE;
        }
    }
}


/*!
 auxiliary comparator object needed for efficient conflict set caching
*/
struct conflictset_comparator
{
    explicit conflictset_comparator(arrayindex_t _len): len(_len) {}
    arrayindex_t len;
    //TODO comment me
    bool operator() (arrayindex_t *const &a, arrayindex_t *const &b) const
    {
        return (memcmp(a, b, len * SIZEOF_ARRAYINDEX_T) < 0);
    }
};


/*!
 copies all elements in the range [first1,last1), that are also in [first2,last2), to result.
\param first1 a arrayindex_t pointer which marks the begin (included) of the first array to be checked
\param last1 a arrayindex_t pointer which marks the (excluded) end of the second array to be checked
\param first2 a arrayindex_t pointer which marks the begin (included) of the second array to be checked
\param last2 a arrayindex_t pointer which marks the (excluded) end of the second array to be checked
\param[in,out] result a pointer where the elements get stored wich are contained in both arrays
\return returns the number of elements copied.
*/
arrayindex_t Net::set_moveall(arrayindex_t *first1, arrayindex_t *last1,
                              arrayindex_t *first2, arrayindex_t *last2,
                              arrayindex_t *result)
{
    arrayindex_t *res = result;
    arrayindex_t *retain = first1;
    arrayindex_t *lb = first1;
    while (first1 != last1 && first2 != last2)
    {
        lb = std::lower_bound(lb, last1, *first2);
        if (lb == last1)
        {
            break;
        }
        if (*lb == *first2++)
        {
            memmove(retain, first1, (lb - first1)*SIZEOF_ARRAYINDEX_T);
            retain += lb - first1;
            *res++ = *lb++;
            first1 = lb;
        }
    }
    memmove(retain, first1, (last1 - first1)*SIZEOF_ARRAYINDEX_T);
    return res - result;
}

/// calculates the set of conflicting transitions for each transition
//TODO comment me
void Net::preprocess_organizeConflictingTransitions()
{
    const arrayindex_t cardPL = Net::Card[PL];
    const arrayindex_t cardTR = Net::Card[TR];

    ////
    // allocate a bunch of temporary arrays
    ////

    // stackpos_place_done[p] states whether place p is already included in the current conflict set (the one at the current stack position)
    bool *stackpos_place_done = new bool[cardPL]();
    // stack_place_used[i] states which place has been processed at stack position i. Needed to keep stackpos_place_done updated.
    arrayindex_t *stack_place_used = new arrayindex_t[cardPL];

    // stack_conflictset[i] stores the current conflict set at stack position i
    // calloc: null-pointer tests to dynamically allocate new segments
    arrayindex_t **stack_conflictset = new arrayindex_t *[(cardPL + 1)]();
    arrayindex_t *stack_card_conflictset = new arrayindex_t[(cardPL + 1)]();

    // stack_transitions[i] stores all transitions the conflict set at stack position i applies to. Every transition appears exactly once.
    // calloc: null-pointer tests to dynamically allocate new segments
    arrayindex_t **stack_transitions = new arrayindex_t *[(cardPL + 1)]();
    arrayindex_t *stack_card_transitions = new arrayindex_t[(cardPL + 1)]();
    // index of the current transition for each stack position
    arrayindex_t *stack_transitions_index = new arrayindex_t[(cardPL + 1)]();

    // temporary array needed to do pseudo-"in-place" operations.
    arrayindex_t *tmp_array = new arrayindex_t[cardTR]();

    // initialize conflict cache array. There is a set for every possible size of the conflict set.
    std::set<arrayindex_t *, conflictset_comparator> **conflictcache = new
    std::set<arrayindex_t *, conflictset_comparator> *[cardTR + 1];
    for (arrayindex_t i = 0; i <= cardTR; i++)
    {
        conflictcache[i] = new std::set<arrayindex_t *, conflictset_comparator>(conflictset_comparator(i));
    }
    arrayindex_t num_different_conflicts = 0;
    // iterator used to temporarily store the result of a find() operation. It will point either to the cache element or the end() of the conflictcache
    std::set<arrayindex_t *, conflictset_comparator>::iterator it;

    ////
    // compute (forward-)conflicting sets
    ////

    RT::rep->status("computing forward-conflicting sets");

    // only for status output
    arrayindex_t num_finished_transitions = 0;

    /// init stack
    stack_conflictset[0] = new arrayindex_t[cardTR]();
    stack_transitions[0] = new arrayindex_t[cardTR]();
    arrayindex_t stack_index = 0;
    stack_card_transitions[0] = cardTR;

    // all transitions start at stack position 0
    for (arrayindex_t i = 0; i < cardTR; i++)
    {
        stack_transitions[0][i] = i;
    }
    while (true)
    {
        /// check if there are still transitions left the current stack position
        if (stack_transitions_index[stack_index] >= stack_card_transitions[stack_index])
        {
            /// all transitions are done, pop current position from stack
            // reset stack position
            stack_transitions_index[stack_index] = 0;
            // check if already at bottom of stack
            if (stack_index <= 0)
            {
                break;
            }
            stack_index--;
            stackpos_place_done[stack_place_used[stack_index]] = false;
            continue;
        }

        /// grab a transition
        arrayindex_t active_transition =
            stack_transitions[stack_index][stack_transitions_index[stack_index]];

        /// iterate over all its unprocessed pre-places p
        for (arrayindex_t i = 0; i < Net::CardArcs[TR][PRE][active_transition]; i++)
        {
            const arrayindex_t p = Net::Arc[TR][PRE][active_transition][i];
            if (stackpos_place_done[p])
            {
                continue;
            }

            // allocate next stack segment if not already done
            if (!stack_conflictset[stack_index + 1])
            {
                stack_conflictset[stack_index + 1] = new arrayindex_t[cardTR]();
                stack_transitions[stack_index + 1] = new arrayindex_t[cardTR]();
            }

            // compute new conflict set
            stack_card_conflictset[stack_index + 1] = std::set_union(stack_conflictset[stack_index],
                    stack_conflictset[stack_index] + stack_card_conflictset[stack_index], Net::Arc[PL][POST][p],
                    Net::Arc[PL][POST][p] + Net::CardArcs[PL][POST][p],
                    stack_conflictset[stack_index + 1]) - stack_conflictset[stack_index + 1];

            // compute all remaining transitions at the new stack position
            stack_card_transitions[stack_index + 1] = set_moveall(stack_transitions[stack_index],
                    stack_transitions[stack_index] + stack_card_transitions[stack_index], Net::Arc[PL][POST][p],
                    Net::Arc[PL][POST][p] + Net::CardArcs[PL][POST][p], stack_transitions[stack_index + 1]);
            stack_card_transitions[stack_index] -= stack_card_transitions[stack_index + 1];

            // mark p as used
            stack_place_used[stack_index] = p;
            stackpos_place_done[p] = true;

            // go to new stack position
            stack_index++;
        }

        // status output
        if (++num_finished_transitions % 1000 == 0)
        {
            RT::rep->bar(num_finished_transitions, cardTR);
            /*
            RT::rep->status("processed %d of %d transitions, pass 1/2 (%d conflict sets)",
                            num_finished_transitions,
                            cardTR, num_different_conflicts);
            */
        }

        // all pre-places of active_transition are now done, the current stack position holds the resulting conflict set
        Transition::CardConflicting[active_transition] = stack_card_conflictset[stack_index];
        // try to find conflict set in cache
        if (conflictcache[stack_card_conflictset[stack_index]]->end() != (it =
                    conflictcache[stack_card_conflictset[stack_index]]->find(stack_conflictset[stack_index])))
        {
            // success! use cached set
            Transition::Conflicting[active_transition] = *it;
            Transition::ConflictingIsOriginal[active_transition] = false;
        }
        else
        {
            // failure! allocate memory for new conflict set and add it to the cache
            Transition::Conflicting[active_transition] = new arrayindex_t[stack_card_conflictset[stack_index]];
            memcpy(Transition::Conflicting[active_transition], stack_conflictset[stack_index],
                   stack_card_conflictset[stack_index] * SIZEOF_ARRAYINDEX_T);
            conflictcache[stack_card_conflictset[stack_index]]->insert(
                Transition::Conflicting[active_transition]);
            Transition::ConflictingIsOriginal[active_transition] = true;
            num_different_conflicts++;
        }
        // move on to next transition at current stack position
        stack_transitions_index[stack_index]++;
    }

    ////
    // compute back-conflicting sets
    ////

    RT::rep->bar(100, 100);
    RT::rep->status("computing back-conflicting sets");

    num_finished_transitions = 0;

    // re-init stack
    stack_index = 0;
    stack_card_transitions[0] = cardTR;
    for (arrayindex_t i = 0; i < cardTR; i++)
    {
        stack_transitions[0][i] = i;
    }
    while (true)
    {
        /// check if there are still transitions left the current stack position
        if (stack_transitions_index[stack_index] >= stack_card_transitions[stack_index])
        {
            /// all transitions are done, pop current position from stack
            // reset stack position
            stack_transitions_index[stack_index] = 0;
            // check if already at bottom of stack
            if (stack_index <= 0)
            {
                break;
            }
            stack_index--;
            stackpos_place_done[stack_place_used[stack_index]] = false;
            continue;
        }

        /// grab a transition
        arrayindex_t active_transition =
            stack_transitions[stack_index][stack_transitions_index[stack_index]];

        /// iterate over all its unprocessed post-places p
        for (arrayindex_t i = 0; i < Net::CardArcs[TR][POST][active_transition]; i++)
        {
            const arrayindex_t p = Net::Arc[TR][POST][active_transition][i];
            if (stackpos_place_done[p])
            {
                continue;
            }

            // allocate next stack segment if not already done
            if (!stack_conflictset[stack_index + 1])
            {
                stack_conflictset[stack_index + 1] = new arrayindex_t[cardTR]();
                stack_transitions[stack_index + 1] = new arrayindex_t[cardTR]();
            }

            // compute new conflict set
            stack_card_conflictset[stack_index + 1] = std::set_union(stack_conflictset[stack_index],
                    stack_conflictset[stack_index] + stack_card_conflictset[stack_index], Net::Arc[PL][POST][p],
                    Net::Arc[PL][POST][p] + Net::CardArcs[PL][POST][p],
                    stack_conflictset[stack_index + 1]) - stack_conflictset[stack_index + 1];

            // compute all remaining transitions at the new stack position
            stack_card_transitions[stack_index + 1] = set_moveall(stack_transitions[stack_index],
                    stack_transitions[stack_index] + stack_card_transitions[stack_index], Net::Arc[PL][PRE][p],
                    Net::Arc[PL][PRE][p] + Net::CardArcs[PL][PRE][p], stack_transitions[stack_index + 1]);
            stack_card_transitions[stack_index] -= stack_card_transitions[stack_index + 1];

            // mark p as used
            stack_place_used[stack_index] = p;
            stackpos_place_done[p] = true;

            // go go new stack position
            stack_index++;
        }

        // status output
        if (++num_finished_transitions % 1000 == 0)
        {
            RT::rep->bar(num_finished_transitions, cardTR);

            /*            RT::rep->status("processed %d of %d transitions, pass 2/2 (%d conflict sets)",
                                        num_finished_transitions,
                                        cardTR, num_different_conflicts);*/
        }

        // all post-places of active_transition are now done, the current stack position holds the resulting conflict set
        Transition::CardBackConflicting[active_transition] = stack_card_conflictset[stack_index];
        // try to find conflict set in cache
        if (conflictcache[stack_card_conflictset[stack_index]]->end() != (it =
                    conflictcache[stack_card_conflictset[stack_index]]->find(stack_conflictset[stack_index])))
        {
            // success! use cached set
            Transition::BackConflicting[active_transition] = *it;
            Transition::BackConflictingIsOriginal[active_transition] = false;
        }
        else
        {
            // failure! allocate memory for new conflict set and add it to the cache
            Transition::BackConflicting[active_transition] = new
            arrayindex_t[stack_card_conflictset[stack_index]];
            memcpy(Transition::BackConflicting[active_transition], stack_conflictset[stack_index],
                   stack_card_conflictset[stack_index] * SIZEOF_ARRAYINDEX_T);
            conflictcache[stack_card_conflictset[stack_index]]->insert(
                Transition::BackConflicting[active_transition]);
            Transition::BackConflictingIsOriginal[active_transition] = true;
            num_different_conflicts++;
        }
        // move on to next transition at current stack position
        stack_transitions_index[stack_index]++;
    }

    // reset status bar
    RT::rep->bar(100, 100);

    ////
    // cleanup: free temporary arrays
    ////

    delete[] tmp_array;
    for (arrayindex_t i = 0; i <= cardTR; i++)
    {
        delete conflictcache[i];
    }
    delete[] conflictcache;
    for (arrayindex_t i = 0; i <= cardPL; i++)
    {
        delete[] stack_conflictset[i];
    }
    delete[] stack_conflictset;
    delete[] stack_card_conflictset;
    for (arrayindex_t i = 0; i <= cardPL; i++)
    {
        delete[] stack_transitions[i];
    }
    delete[] stack_transitions;
    delete[] stack_card_transitions;
    delete[] stack_transitions_index;
    delete[] stackpos_place_done;
    delete[] stack_place_used;

    RT::rep->status("%d transition conflict sets", num_different_conflicts);
    RT::data["net"]["conflict_sets"] = static_cast<int>(num_different_conflicts);
}

/*!
This function does the preprocessing for the given net. With finished preprocessing there are additional information available which are used to speed up the simulation.
\pre the raw net is read, places, transitions and edges in-between are set properly.
\post Bitvectorsize, hashindexes, conflicts, significant places, progress measure and enabledness are calculated
*/
void Net::preprocess()
{
    const arrayindex_t cardPL = Net::Card[PL];
    const arrayindex_t cardTR = Net::Card[TR];

    /************************************
    * 1. Compute bits needed for places *
    ************************************/
    Place::CardBits = new cardbit_t[cardPL];
    Place::SizeOfBitVector = 0;
    for (arrayindex_t p = 0; p < cardPL; p++)
    {
        Place::SizeOfBitVector +=
            (Place::CardBits[p] = Place::Capacity2Bits(Place::Capacity[p]));
    }

    /********************
    * 2. Compute Hashes *
    ********************/
    Place::Hash = new hash_t[cardPL];
    Marking::HashInitial = 0;
    for (arrayindex_t p = 0; p < cardPL; p++)
    {
        Place::Hash[p] = rand() % MAX_HASH;
        Marking::HashInitial = (Marking::HashInitial + Place::Hash[p] * Marking::Initial[p]) %
                               SIZEOF_MARKINGTABLE;
    }
    // set hash value for initial marking
    Marking::HashCurrent = Marking::HashInitial;

    Firelist::usedAsScapegoat = new uint64_t [Net::Card[PL]];
	for(arrayindex_t i = 0; i < Net::Card[PL];i++)
	{
		   Firelist::usedAsScapegoat[i] = 0;
	}
	Firelist::timestampScapegoat = 0;

    /*********************
    * 3. Organize Deltas *
    *********************/
    // calloc: delta hash must be initially 0
    Transition::DeltaHash = new hash_t[cardTR]();
    // allocate memory for deltas
    for (int direction = PRE; direction <= POST; direction++)
    {
        // calloc: no arcs there yet
        Transition::CardDeltaT[direction] = new arrayindex_t[cardTR]();
        Transition::DeltaT[direction] = new arrayindex_t *[cardTR];
        Transition::MultDeltaT[direction] = new mult_t *[cardTR];
    }
    Net::preprocess_organizeDeltas();


    /****************************
    * 5. Set significant places *
    *****************************/
    // test whether computation actually needed
    if (Place::CardSignificant == UINT_MAX)
    {
        if (RT::args.encoder_arg == encoder_arg_fullcopy)
        {
            RT::rep->status("significant places are not calculated (%s)",
                RT::rep->markup(MARKUP_PARAMETER, "--encoder=fullcopy").str());
            Place::CardSignificant = Net::Card[PL];
        }
        else
        {
            Net::setSignificantPlaces();
        }
    }
    RT::rep->status("%d places, %d transitions, %d significant places",
                    Net::Card[PL], Net::Card[TR], Place::CardSignificant);
    RT::data["net"]["places"] = static_cast<int>(Net::Card[PL]);
    RT::data["net"]["transitions"] = static_cast<int>(Net::Card[TR]);
    RT::data["net"]["places_significant"] = static_cast<int>(Place::CardSignificant);

    // sort all arcs. Needs to be done before enabledness check in order to not mess up the disabled lists and scapegoats, but after determining the significant places since it swaps places and destroys any arc ordering.

    /**************************************
    * 4. Organize conflicting transitions *
    **************************************/
    Transition::CardConflicting = new arrayindex_t[cardTR];
    Transition::Conflicting = new arrayindex_t *[cardTR];
    Transition::ConflictingIsOriginal = new bool[cardTR];
    Transition::CardBackConflicting = new arrayindex_t[cardTR];
    Transition::BackConflicting = new arrayindex_t *[cardTR];
    Transition::BackConflictingIsOriginal = new bool[cardTR];

    //organizeConflictingTransitions();
    //organizeBackConflictingTransitions();
    Net::preprocess_organizeConflictingTransitions();

    Net::sortAllArcs();
    /**************************
    * 6. Set progress measure *
    **************************/
    // we always calcuate a progress measure as it is reasonably cheap and beside the sweep line algorithm, also the deletion algorithm uses it
    if (RT::args.search_arg == search_arg_sweepline or RT::args.stubborn_arg == stubborn_arg_deletion)
    {
        RT::rep->status("calculating the progress measure");
        Net::setProgressMeasure();
    }

    /*******************************
    * 7. Initial enabledness check *
    *******************************/
    // use calloc: initial assumption: no transition is disabled
    Transition::Enabled = new bool[cardTR];
    // start with assumption that all transitions are enabled
    Transition::CardEnabled = cardTR;
    for (arrayindex_t t = 0; t < cardTR; t++)
    {
        Transition::Enabled[t] = true;
    }

    for (arrayindex_t t = 0; t < cardTR; t++)
    {
        Transition::checkEnabled_Initial(t);
    }
}

void unionfindsort(arrayindex_t * cl,int64_t * uf, arrayindex_t card)
{
	int64_t pivot = uf[0];
	arrayindex_t b = 0; // the first element not smaller than pivot
	arrayindex_t w = 1; // the first element not smaller or equal to pivot
	arrayindex_t r = card; // the first element larger than pivot

	// the elements between w and (including r-1) form the unknwon area
	while(w<r)
	{
		if(uf[w] < pivot)
		{
			// swap into the 0..b area
			arrayindex_t tempc = cl[b];
			int64_t tempu = uf[b];
			cl[b] = cl[w];
			uf[b] = uf[w];
			cl[w] = tempc;
			uf[w] = tempu;
			++w;
			++b;
		}
		else
		{
			if(uf[w] == pivot)
			{
				// append to b+1..w area
				++w;
			}
			else
			{
				//swap into the r..card area
				--r;
				arrayindex_t tempc = cl[w];
				int64_t tempu = uf[w];
				cl[w] = cl[r];
				uf[w] = uf[r];
				cl[r] = tempc;
				uf[r] = tempu;
			}
		}
	}
	// sort 0..b area if necessary
	if(b > 1)
	{
		unionfindsort(cl,uf,b);
	}
	// sort r..card area if necessary
	if((card-r) > 1)
	{
		unionfindsort(cl+r,uf+r,card-r);
	}
}

void clustersort(arrayindex_t * cl, arrayindex_t fr, arrayindex_t to, arrayindex_t p)
{
	// sort according to p-th pre-place of the transitions in cl between fr and to
	arrayindex_t pivot = Net::Arc[TR][PRE][cl[fr]][p];
	arrayindex_t b = fr; // the first element not smaller than pivot
	arrayindex_t w = fr+1; // the first element not smaller or equal to pivot
	arrayindex_t r = to; // the first element larger than pivot

	// the elements between w and (including r-1) form the unknwon area
	while(w<r)
	{
		if(Net::Arc[TR][PRE][cl[w]][p] < pivot)
		{
			// swap into the 0..b area
			arrayindex_t tempc = cl[b];
			cl[b] = cl[w];
			cl[w] = tempc;
			++w;
			++b;
		}
		else
		{
			if(Net::Arc[TR][PRE][cl[w]][p] == pivot)
			{
				// append to b+1..w area
				++w;
			}
			else
			{
				//swap into the r..card area
				--r;
				arrayindex_t tempc = cl[w];
				cl[w] = cl[r];
				cl[r] = tempc;
			}
		}
	}
	// sort 0..b area if necessary
	if(b -fr > 1)
	{
		clustersort(cl,fr,b,p);
	}
	// sort r..card area if necessary
	if((to-r) > 1)
	{
		clustersort(cl,r,to,p);
	}
}

void backclustersort(arrayindex_t * cl, arrayindex_t fr, arrayindex_t to, arrayindex_t p)
{
	// sort according to p-th pre-place of the transitions in cl between fr and to
	arrayindex_t pivot = Net::Arc[TR][POST][cl[fr]][p];
	arrayindex_t b = fr; // the first element not smaller than pivot
	arrayindex_t w = fr+1; // the first element not smaller or equal to pivot
	arrayindex_t r = to; // the first element larger than pivot

	// the elements between w and (including r-1) form the unknwon area
	while(w<r)
	{
		if(Net::Arc[TR][POST][cl[w]][p] < pivot)
		{
			// swap into the 0..b area
			arrayindex_t tempc = cl[b];
			cl[b] = cl[w];
			cl[w] = tempc;
			++w;
			++b;
		}
		else
		{
			if(Net::Arc[TR][POST][cl[w]][p] == pivot)
			{
				// append to b+1..w area
				++w;
			}
			else
			{
				//swap into the r..card area
				--r;
				arrayindex_t tempc = cl[w];
				cl[w] = cl[r];
				cl[r] = tempc;
			}
		}
	}
	// sort 0..b area if necessary
	if(b -fr > 1)
	{
		backclustersort(cl,fr,b,p);
	}
	// sort r..card area if necessary
	if((to-r) > 1)
	{
		backclustersort(cl,r,to,p);
	}
}

/// Sort a section of the Clusters array (see below) that contains transitions by decreasing 
/// number of conflicting transitions
void sortlength(arrayindex_t * a, arrayindex_t card)
{
	if(card<2) return;
	arrayindex_t b = 0;
	arrayindex_t pivot = Transition::CardConflicting[a[0]];
	arrayindex_t w = 1;
	arrayindex_t r = card;
	while(w<r)
	{
		if(Transition::CardConflicting[a[w]] > pivot)
		{
			// swap into the 0..b area
			arrayindex_t temp = a[b];
			a[b++] = a[w];
			a[w++] = temp;
		}
		else
		{
			if(Transition::CardConflicting[a[w]] == pivot)
			{
				// append to b+1..w area
				++w;
			}
			else
			{
				//swap into the r..card area
				arrayindex_t temp = a[w];
				a[w] = a[--r];
				a[r] = temp;
			}
		}
	}
	// sort 0..b area if necessary
	if(b > 1)
	{
		sortlength(a,b);
	}
	// sort r..card area if necessary
	if((card-r) > 1)
	{
		sortlength(a+r,card-r);
	}

}

/// local class for handling conflict set calculation
class todoConf
{
public:
	arrayindex_t start; // first transition in Clusters sharing this conflict set vector
	arrayindex_t from; // first transition in Clusters not yet finished sharing this conflict set vector
	arrayindex_t to;   // first transition in Clusters not sharing this conflict set vector
	arrayindex_t * vector; // the shared conflict set vector
	arrayindex_t vindex; // the next free entry in the shared conflict set vector
	bool * bvector; // a boolean array showing whether a transition is already included
	arrayindex_t pindex; // the next place in transition's pre place list to be inserted
	todoConf * next; // list link
};

arrayindex_t * Clusters;
arrayindex_t cardOriginal;

arrayindex_t runningclusterthreads;
pthread_mutex_t clustermutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t clustercond;
void * computeconflictthread(void * data)
{
	todoConf * todo = reinterpret_cast<todoConf *> ( data);
	todo -> next = NULL;

	do
	{
		todoConf * currentrec = todo;
		todo = todo -> next;
		// sort out finished transitions (i.e. without unprocessed pre-place)
		for(arrayindex_t i = currentrec ->from; i < currentrec->to ; i++)
		{
			arrayindex_t t = Clusters[i];
			if(currentrec -> pindex >= Net::CardArcs[TR][PRE][t])
			{
				// t finished, sort to front
				Clusters[i] = Clusters[currentrec->from];
				Clusters[(currentrec->from)++] = t;
				Transition::CardConflicting[t] = currentrec->vindex;
			}
		}
		// check if record completed
		if(currentrec->from == currentrec->to)
		{
			pthread_mutex_lock(&clustermutex);
			cardOriginal++;
			pthread_mutex_unlock(&clustermutex);
			// record completed
			// insert original conflicting vector to last transition (one that has maximum size)
			arrayindex_t t = Clusters[currentrec->to-1];
			arrayindex_t * finalvec = reinterpret_cast<arrayindex_t *> (realloc(currentrec->vector,sizeof(arrayindex_t) * currentrec-> vindex));
			Transition::Conflicting[t] = finalvec;
			Transition::ConflictingIsOriginal[t] = true;
			for(arrayindex_t j = currentrec->start; j < currentrec->to -1 ; j++)
			{
				t = Clusters[j];
				Transition::Conflicting[t] = finalvec;
				Transition::ConflictingIsOriginal[t]=false;
			}
			delete [] currentrec->bvector;
			delete currentrec;
			continue;
		}
		// at this point: record not complete
		// sort transitions according to place at position pindex
		arrayindex_t p = currentrec->pindex;
		clustersort(Clusters,currentrec->from, currentrec->to, p);
		// while there are different places at position p
		while(Net::Arc[TR][PRE][Clusters[currentrec->from]][p] != 
                      Net::Arc[TR][PRE][Clusters[currentrec->to-1]][p])
		{
			// define new subproblem
			todoConf * newrec = new todoConf();
			arrayindex_t newfrom = currentrec->to - 2;
			while(Net::Arc[TR][PRE][Clusters[newfrom]][p] == Net::Arc[TR][PRE][Clusters[currentrec->to -1 ]][p]) newfrom --;
			newfrom++;
			newrec->start = newrec -> from = newfrom;
			newrec->to = currentrec->to;
			currentrec->to = newfrom;
			newrec -> pindex = p+1; // already initialized to next value
			newrec -> vindex = currentrec->vindex;
			newrec -> vector = reinterpret_cast<arrayindex_t *> (malloc(sizeof(arrayindex_t) * Net::Card[TR]));
			memcpy(newrec->vector, currentrec->vector, sizeof(arrayindex_t) * currentrec->vindex);
			newrec -> bvector = reinterpret_cast<bool *> (malloc(sizeof(bool) * Net::Card[TR]));
			memcpy(newrec->bvector, currentrec->bvector, sizeof(bool) * Net::Card[TR]);

			// process current place pp at copied vector

			arrayindex_t pp = Net::Arc[TR][PRE][Clusters[newrec->from]][p];

			// for all post transitions of pp...

			for(arrayindex_t k = 0; k < Net::CardArcs[PL][POST][pp]; k++)
			{
				arrayindex_t tt = Net::Arc[PL][POST][pp][k];
				// insert tt if not yet included
				if(!newrec->bvector[tt])
				{
					newrec->vector[newrec->vindex++] = tt;
					newrec->bvector[tt] = true;
				}
			}
			newrec -> next = todo;
			todo = newrec;
		}
		// process remaining section in currentrec
		// process current place pp at copied vector

		arrayindex_t pp = Net::Arc[TR][PRE][Clusters[currentrec->from]][p];

		// for all post transitions of pp...

		for(arrayindex_t k = 0; k < Net::CardArcs[PL][POST][pp]; k++)
		{
			arrayindex_t tt = Net::Arc[PL][POST][pp][k];
			// insert tt if not yet included
			if(!currentrec->bvector[tt])
			{
				currentrec->vector[currentrec->vindex++] = tt;
				currentrec->bvector[tt] = true;
			}
		}
		// push currentrec to todo list
		currentrec->pindex++;
		currentrec -> next = todo;
		todo = currentrec;
	}
	while(todo != NULL);
	// check out thread
	pthread_mutex_lock(&clustermutex);
	runningclusterthreads--;
	if(runningclusterthreads == 0)
	{
		pthread_cond_signal(&clustercond);
	}
	pthread_mutex_unlock(&clustermutex);
	
	pthread_exit(NULL); // this thread finished
}

void organizeConflictingTransitions()
{
	
	cardOriginal = 0;
	runningclusterthreads = 0;
	pthread_cond_init(&clustercond,NULL);
	clusterSortAllArcs();
	arrayindex_t sectionL, sectionR;
	// 1. Compute conflict clusters using union/find algorithm

	// the semantics of the data structure: 
	// - Each element is included in exactly one set
	// - The name of the set is one of its elements
	// - If element i is the name of a set S, unionfind[i] is -card(S)
	// - If element i is not the name of a set, unionfind[i] is an element
	//   contained in the same set. Seing [i,unionfind[i]] as an edge,
	//   each set is a tree where the root is the element that lends the set
	//   its name
	// - operation find: input: i, output: name of the set containing i
	// - operation union: input i,j, result: union of the sets containing these two

	int64_t unionfind[Net::Card[TR]]; 

	// initially, all sets are singleton
	for(arrayindex_t i = 0; i < Net::Card[TR]; i++)
	{
		unionfind[i] = -1;
	}
	
	// we unify two sets if they contain two transitions that share a pre-place

	for(arrayindex_t i = 0; i < Net::Card[PL];i++) // for all places do...
	{
		// we unify set(1st post transition of p_i) with set(all other
		// post transitions of p_i)
		for(arrayindex_t j = 1; j < Net::CardArcs[PL][POST][i]; j++)
		{
			// "find" the set of post transition 0
			// this needs to be repeated as it can change

			arrayindex_t k;
			for(k = Net::Arc[PL][POST][i][0]; unionfind[k] >= 0; k = unionfind[k]);
			assert(unionfind[k] < 0);

			// "find" the set of post transition j

			arrayindex_t l;
			for(l = Net::Arc[PL][POST][i][j]; unionfind[l] >= 0; l = unionfind[l]);
			assert(unionfind[l] < 0);
			
			// perform "union": link the smaller to the larger set

			// remember the new root
			arrayindex_t m;

			if(k == l)
			{
				// already same set, nothing do do
				m = k;
			}
			else
			{
				if(unionfind[k] < unionfind[l])
				{
					// k is in larger set (remember that we compare negative numbers)
					m = k;
					unionfind[k] = unionfind[k] + unionfind[l]; // new size
					unionfind[l] = k; // l linked to k
				}
				else
				{
					m = l;
					// l is in larger set 
					unionfind[l] = unionfind[k] + unionfind[l]; // new size
					unionfind[k] = l; // k linked to l
				}
			}
			// after union, we compress the paths of both original nodes
			// thus leading to shorter paths in the future

			for(k = Net::Arc[PL][POST][i][0]; unionfind[k] >= 0; )
			{
				arrayindex_t n = unionfind[k];
				unionfind[k] = m;
				k = n;
			}
			for(l = Net::Arc[PL][POST][i][j]; unionfind[l] >= 0; )
			{
				arrayindex_t n = unionfind[l];
				unionfind[l] = m;
				l = n;
			}
		}
	}

	// here, the union find structure represents the disjoint (transition parts of)
	// all conflict clusters. For accessing it, we post process the data structure.
	// First, we link all nodes directly to their root
	
	for(arrayindex_t i = 0; i < Net::Card[TR]; i++)
	{
		arrayindex_t k;
		for(k = i; unionfind[k] >= 0; k = unionfind[k]);
		if(k != i) unionfind[i] = k;
	}

	// Second, we turn all negative entries into positive ones. After this step
	// t1 and t2 are in the same set iff unionfind[t1] == unionfind[t2].
	// At the same time, we count the sets.
	
	arrayindex_t CardConflictingSets = 0;

	for(arrayindex_t i = 0; i < Net::Card[TR];i++)
	{
		if(unionfind[i] < 0) 
		{
			unionfind[i] = i; // must have been root
			++CardConflictingSets;
		}
	}
	RT::rep->status("The net has %llu conflict clusters",CardConflictingSets);
	runningclusterthreads = CardConflictingSets; // we start a thread for each cluster

	// Third, we introduce a new array containing each transition once and
	// sort it according to the unionfind values

	Clusters = new arrayindex_t [Net::Card[TR]];
	for(arrayindex_t i = 0; i < Net::Card[TR]; i++)
	{
		Clusters[i] = i;
	}
	unionfindsort(Clusters,unionfind,Net::Card[TR]);
	
	// 2. for each conflict cluster, introduce a todo record
	arrayindex_t clStart, clEnd;


	for(clStart = 0; clStart < Net::Card[TR]; )
	{
		for(clEnd = clStart+1; clEnd < Net::Card[TR] && unionfind[clStart] == unionfind[clEnd]; clEnd++);
		todoConf * rec = new todoConf();
		rec -> start = rec -> from = clStart;
		rec -> to = clEnd;
		rec -> vector = reinterpret_cast<arrayindex_t *> (calloc(clEnd-clStart, sizeof(arrayindex_t)));
		rec -> bvector = reinterpret_cast<bool *> (calloc(Net::Card[TR], sizeof(bool)));
		rec -> pindex = 0;
		rec -> vindex = 0;
		rec -> next = NULL;

		pthread_t * mythread = new pthread_t();
		pthread_create(mythread,NULL,computeconflictthread,reinterpret_cast<void *>(rec));
		clStart = clEnd;
	}

	// now: wait for all threads to be finished
	pthread_mutex_lock(&clustermutex);
	pthread_cond_wait(&clustercond,&clustermutex);
	pthread_mutex_unlock(&clustermutex);
	RT::rep->status("computed %u orginal conflict arrays",cardOriginal);
}

void organizeBackConflictingTransitions()
{
	backClusterSortAllArcs();
	for(arrayindex_t i = 0; i < Net::Card[TR]; i++)
	{
		Clusters[i] = i;
	}
	
	// Introduce a first todo record

	todoConf * todo = new todoConf();
		todo -> start = todo -> from = 0;
		todo -> to = Net::Card[TR];
		todo -> vector = reinterpret_cast<arrayindex_t *> (calloc(Net::Card[TR], sizeof(arrayindex_t)));
		todo -> bvector = reinterpret_cast<bool *> (calloc(Net::Card[TR], sizeof(bool)));
		todo -> pindex = 0;
		todo -> vindex = 0;
		todo -> next = NULL;


	// at this point, we have a todo record for all transitions. Now we process each record.
	// If not all transitions between from and to can share the same conflict set vector, we
	// split the record and append one into todo list

	while(todo) // there is a record to be handled
	{
		todoConf * currentrec = todo;
		todo = todo -> next;

		// sort out finished transitions (i.e. without unprocessed pre-place)
		for(arrayindex_t i = currentrec ->from; i < currentrec->to ; i++)
		{
			arrayindex_t t = Clusters[i];
			if(currentrec -> pindex >= Net::CardArcs[TR][POST][t])
			{
				// t finished, sort to front
				Clusters[i] = Clusters[currentrec->from];
				Clusters[(currentrec->from)++] = t;
				Transition::CardBackConflicting[t] = currentrec->vindex;
			}
		}
		// check if record completed
		if(currentrec->from == currentrec->to)
		{
			// record completed
			// insert original conflicting vector to last transition (one that has maximum size)
			arrayindex_t t = Clusters[currentrec->to-1];
			arrayindex_t * finalvec = reinterpret_cast<arrayindex_t *> (realloc(currentrec->vector,sizeof(arrayindex_t) * currentrec-> vindex));
			Transition::BackConflicting[t] = finalvec;
			Transition::BackConflictingIsOriginal[t] = true;
			for(arrayindex_t j = currentrec->start; j < currentrec->to -1 ; j++)
			{
				t = Clusters[j];
				Transition::BackConflicting[t] = finalvec;
				Transition::BackConflictingIsOriginal[t]=false;
			}
			delete [] currentrec->bvector;
			delete currentrec;
			continue; // process next todo record
		}
		// at this point: record not complete
		// sort transitions according to place at position pindex
		arrayindex_t p = currentrec->pindex;
		backclustersort(Clusters,currentrec->from, currentrec->to, p);
		// while there are different places at position p
		while(Net::Arc[TR][POST][Clusters[currentrec->from]][p] != 
                      Net::Arc[TR][POST][Clusters[currentrec->to-1]][p])
		{
			// define new subproblem
			todoConf * newrec = new todoConf();
			arrayindex_t newfrom = currentrec->to - 2;
			while(Net::Arc[TR][POST][Clusters[newfrom]][p] == Net::Arc[TR][POST][Clusters[currentrec->to -1 ]][p]) newfrom --;
			newfrom++;
			newrec->start = newrec -> from = newfrom;
			newrec->to = currentrec->to;
			currentrec->to = newfrom;
			newrec -> pindex = p+1; // already initialized to next value
			newrec -> vindex = currentrec->vindex;
			newrec -> vector = reinterpret_cast<arrayindex_t *> (malloc(sizeof(arrayindex_t) * Net::Card[TR]));
			memcpy(newrec->vector, currentrec->vector, sizeof(arrayindex_t) * currentrec->vindex);
			newrec -> bvector = reinterpret_cast<bool *> (malloc(sizeof(bool) * Net::Card[TR]));
			memcpy(newrec->bvector, currentrec->bvector, sizeof(bool) * Net::Card[TR]);

			// process current place pp at copied vector

			arrayindex_t pp = Net::Arc[TR][POST][Clusters[newrec->from]][p];

			// for all post transitions of pp...

			for(arrayindex_t k = 0; k < Net::CardArcs[PL][POST][pp]; k++)
			{
				arrayindex_t tt = Net::Arc[PL][POST][pp][k];
				// insert tt if not yet included
				if(!newrec->bvector[tt])
				{
					newrec->vector[newrec->vindex++] = tt;
					newrec->bvector[tt] = true;
				}
			}
			// push newrec to todo list
			newrec->next = todo;
			todo = newrec;
		}
		// process remaining section in currentrec
		// process current place pp at copied vector

		arrayindex_t pp = Net::Arc[TR][POST][Clusters[currentrec->from]][p];

		// for all post transitions of pp...

		for(arrayindex_t k = 0; k < Net::CardArcs[PL][POST][pp]; k++)
		{
			arrayindex_t tt = Net::Arc[PL][POST][pp][k];
			// insert tt if not yet included
			if(!currentrec->bvector[tt])
			{
				currentrec->vector[currentrec->vindex++] = tt;
				currentrec->bvector[tt] = true;
			}
		}
		// push currentrec to todo list
		currentrec->pindex++;
		currentrec->next = todo;
		todo = currentrec;
	}
	delete [] Clusters;
}


void printConflicting(arrayindex_t i)
{
	// debug function: prints, for all transitions, the conflicting ones

		printf("%s: ",Net::Name[TR][i]);
		if(Transition::ConflictingIsOriginal[i]) printf("* ");
		for(arrayindex_t j = 0; j < Transition::CardConflicting[i]; j++)
		{
			printf("%s ",Net::Name[TR][Transition::Conflicting[i][j]]);
		}
		printf("\n");
}
