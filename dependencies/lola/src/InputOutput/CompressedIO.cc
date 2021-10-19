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
\brief implementation of compressed file input/output methods
\author Karsten
\status approved 21.02.2012
\ingroup g_io

Input and outout from/to a file in compressed format. We generate two separate
files that can be read in arbitrary order. In this version, we use an ASCII
file where data are separated by spaces and newlines.
*/

#include <config.h>
#include <Core/Dimensions.h>
#include <Frontend/Parser/ParserPTNet.h>
#include <Frontend/SymbolTable/PlaceSymbol.h>
#include <Frontend/SymbolTable/SymbolTable.h>
#include <Frontend/SymbolTable/TransitionSymbol.h>
#include <InputOutput/CompressedIO.h>
#include <Net/Marking.h>
#include <Net/Net.h>
#include <Net/Place.h>
#include <Net/Transition.h>

#define xstr(tok) str(tok)
#define str(tok) #tok
#define SCANF_MAX_WIDTH 65535
#define SCANF_MAX_WIDTH_STR xstr(SCANF_MAX_WIDTH)

/*!
\param[out] f  output file

\pre The net is properly preprocessed and the xstructure Net is filled.
\pre File f is already open to write.
\post Names are written to file f, but f is not yet closed.
*/
void WriteNameFile(FILE *const f)
{
    assert(f);

    // 1. Number of places
    fprintf(f, "%u\n", Net::Card[PL]);

    // 2. Places with one name per line, oin order of indices
    for (arrayindex_t p = 0; p < Net::Card[PL]; p++)
    {
        fprintf(f, "%s\n", Net::Name[PL][p]);
    }

    // 3. Number of transition
    fprintf(f, "%u\n", Net::Card[TR]);

    // 4. Transitions with one name per line, in order of indices
    for (arrayindex_t t = 0; t < Net::Card[TR]; t++)
    {
        fprintf(f, "%s\n", Net::Name[TR][t]);
    }
}


/*!
\param[out] f  output file
\param[in,out] symboltables  the symbol tables to write the names to

\pre File f is already open to read.
\pre Memory of Net::Name[TR] and Net::Name[PL] is already allocated.
\post Read names are written to the provided symbol tables and Net::Name.
\post Memory for symbols is allocated.

\note We assume no syntax errors. If the input file does not follow the assumed
format, this function may leave the symbol tables in an undefined state.

\todo Memory allocation for symbols should be moved into the SymbolTable class;
that is, wrapped into the insert function. This simplifies this code and makes
memory allocation local to the SymbolTable class.
*/
void ReadNameFile(FILE *const f, ParserPTNet *const symboltables)
{
    assert(f);
    assert(symboltables);
    assert(Net::Name[PL]);
    assert(Net::Name[TR]);

    char *buffer = new char[FILE_READ_BUFFER_SIZE];
    char *res = NULL;

    // 1. Number of places
    res = fgets(buffer, FILE_READ_BUFFER_SIZE, f);
    assert(res);
    const unsigned long tmp1 = std::strtoul(buffer, NULL, 10);
    Net::Card[PL] = static_cast<arrayindex_t>(tmp1);

    // 2. Places with one index, one name per line
    for (arrayindex_t p = 0; p < Net::Card[PL]; p++)
    {
        // get name of place and strip trailing newline
        res = fgets(buffer, FILE_READ_BUFFER_SIZE, f);
        assert(res);
        buffer[strlen(buffer) - 1] = '\0';

        Net::Name[PL][p] = strdup(buffer);
        PlaceSymbol *ps = new PlaceSymbol(Net::Name[PL][p], Place::Capacity[p]);
        ps->setIndex(p);
        symboltables->PlaceTable->insert(ps);
    }

    // 3. Number of transition
    res = fgets(buffer, FILE_READ_BUFFER_SIZE, f);
    assert(res);
    const unsigned long tmp2 = std::strtoul(buffer, NULL, 10);
    Net::Card[TR] = static_cast<arrayindex_t>(tmp2);

    // 4. Transitions with one index, one name per line
    for (arrayindex_t t = 0; t < Net::Card[TR]; t++)
    {
        // get name of transition and strip trailing newline
        res = fgets(buffer, FILE_READ_BUFFER_SIZE, f);
        assert(res);
        buffer[strlen(buffer) - 1] = '\0';

        Net::Name[TR][t] = strdup(buffer);
        TransitionSymbol *ts = new TransitionSymbol(Net::Name[TR][t], Transition::Fairness[t], NULL, NULL);
        ts->setIndex(t);
        symboltables->TransitionTable->insert(ts);
    }

    delete[] buffer;
}


/*!
\param[out] f  output file

\pre The net is properly preprocessed and the xstructure Net is filled.
\pre File f is already open to write.
\post The net xstructure is written to file f, but f is not yet closed.
*/
void WriteNetFile(FILE *const f)
{
    assert(f);

    // 1. Number of places and significant places
    fprintf(f, "%u %u", Net::Card[PL], Place::CardSignificant);

    // 2. For each place...
    for (arrayindex_t p = 0; p < Net::Card[PL]; p++)
    {
        // 2.a initial marking
        // 2.b capacity
        // 2.c nr of incoming arcs
        fprintf(f, "\n%u %u %u ",
                Marking::Initial[p],
                (Place::Capacity[p] == MAX_CAPACITY ? 0 : Place::Capacity[p]),
                Net::CardArcs[PL][PRE][p]);

        for (arrayindex_t i = 0; i < Net::CardArcs[PL][PRE][p]; i++)
        {
            // 2.d incoming arcs and multiplicities
            fprintf(f, "%u %u ", Net::Arc[PL][PRE][p][i], Net::Mult[PL][PRE][p][i]);
        }

        // 2.e nr of outgoing arcs
        fprintf(f, "%u ", Net::CardArcs[PL][POST][p]);

        for (arrayindex_t i = 0; i < Net::CardArcs[PL][POST][p]; i++)
        {
            // 2.f outgoing arcs and multiplicities
            fprintf(f, "%u %u ", Net::Arc[PL][POST][p][i], Net::Mult[PL][POST][p][i]);
        }
    }
    // 3. Number of transitions
    fprintf(f, "\n%u", Net::Card[TR]);

    // 2. For each transition...
    for (arrayindex_t t = 0; t < Net::Card[TR]; t++)
    {
        fprintf(f, "\n");
        // 2.a fairness
        // 2.b nr of incoming arcs
        fprintf(f, "%u %u ", Transition::Fairness[t], Net::CardArcs[TR][PRE][t]);

        for (arrayindex_t i = 0; i < Net::CardArcs[TR][PRE][t]; i++)
        {
            // 2.d incoming arcs and multiplicities
            fprintf(f, "%u %u ", Net::Arc[TR][PRE][t][i], Net::Mult[TR][PRE][t][i]);
        }

        // 2.e nr of outgoing arcs
        fprintf(f, "%u ", Net::CardArcs[TR][POST][t]);

        for (arrayindex_t i = 0; i < Net::CardArcs[TR][POST][t]; i++)
        {
            // 2.f outgoing arcs and multiplicities
            fprintf(f, "%u %u ", Net::Arc[TR][POST][t][i], Net::Mult[TR][POST][t][i]);
        }
    }
}


/*!
\param[out] f  output file

\pre File f is already open to read.
\post Read net xstructure is written to the Net xstructure.
\post Memory for the Net xstructure is allocated.

\note We assume no syntax errors. If the input file does not follow the assumed
format, this function may leave the symbol tables in an undefined state.

*/
void ReadNetFile(FILE *const f)
{
    assert(f);

    // read number of places
    unsigned int tmp1, tmp2;
    int res = fscanf(f, "%"SCANF_MAX_WIDTH_STR"u %"SCANF_MAX_WIDTH_STR"u", &tmp1, &tmp2);
    assert(res != EOF);

    Net::Card[PL] = (arrayindex_t) tmp1;
    Place::CardSignificant = (arrayindex_t) tmp2;

    // allocate place arrays
    // we set place names to NULL right now
    Net::Name[PL] = new const char *[Net::Card[PL]]();
    for (int direction = PRE; direction <= POST; direction++)
    {
        Net::CardArcs[PL][direction] = new arrayindex_t[Net::Card[PL]];
        Net::Arc[PL][direction] = new arrayindex_t *[Net::Card[PL]];
        Net::Mult[PL][direction] = new mult_t *[Net::Card[PL]];
    }

    Place::Capacity = new capacity_t[Net::Card[PL]];
    Marking::Initial = new capacity_t[Net::Card[PL]];
    Marking::Current = new capacity_t[Net::Card[PL]];

    // fill all information that is locally available in symbols, allocate node specific arrays
    for (arrayindex_t p = 0; p < Net::Card[PL]; p++)
    {
        // read initial marking
        res = fscanf(f, "%"SCANF_MAX_WIDTH_STR"u", &tmp1);
        assert(res != EOF);

        Marking::Initial[p] = (capacity_t) tmp1;
        Marking::Current[p] = Marking::Initial[p];

        // read capacity
        res = fscanf(f, "%"SCANF_MAX_WIDTH_STR"u", &tmp1);
        assert(res != EOF);

        Place::Capacity[p] = (capacity_t)(tmp1 == 0 ? MAX_CAPACITY : tmp1);

        // read number of prearcs
        res = fscanf(f, "%"SCANF_MAX_WIDTH_STR"u", &tmp1);
        assert(res != EOF);

        Net::CardArcs[PL][PRE][p] = (arrayindex_t) tmp1;
        Net::Arc[PL][PRE][p] = new arrayindex_t[Net::CardArcs[PL][PRE][p]];
        Net::Mult[PL][PRE][p] = new mult_t[Net::CardArcs[PL][PRE][p]];

        // read prearcs
        for (arrayindex_t i = 0; i < Net::CardArcs[PL][PRE][p]; i++)
        {
            res = fscanf(f, "%"SCANF_MAX_WIDTH_STR"u %"SCANF_MAX_WIDTH_STR"u", &tmp1, &tmp2);
            assert(res != EOF);

            Net::Arc[PL][PRE][p][i] = (arrayindex_t) tmp1;
            Net::Mult[PL][PRE][p][i] = (mult_t) tmp2;
        }

        // read number of postarcs
        res = fscanf(f, "%"SCANF_MAX_WIDTH_STR"u", &tmp1);
        assert(res != EOF);

        Net::CardArcs[PL][POST][p] = (arrayindex_t) tmp1;
        Net::Arc[PL][POST][p] = new arrayindex_t[Net::CardArcs[PL][POST][p]];
        Net::Mult[PL][POST][p] = new mult_t[Net::CardArcs[PL][POST][p]];

        // read postarcs
        for (arrayindex_t i = 0; i < Net::CardArcs[PL][POST][p]; i++)
        {
            res = fscanf(f, "%"SCANF_MAX_WIDTH_STR"u %"SCANF_MAX_WIDTH_STR"u", &tmp1, &tmp2);
            assert(res != EOF);

            Net::Arc[PL][POST][p][i] = (arrayindex_t) tmp1;
            Net::Mult[PL][POST][p][i] = (mult_t) tmp2;
        }
    }
    // Allocate arrays for places and transitions

    // read number of transitions
    res = fscanf(f, "%"SCANF_MAX_WIDTH_STR"u", &tmp1);
    assert(res != EOF);

    Net::Card[TR] = (arrayindex_t) tmp1;

    // transition names are set to NULL here
    Net::Name[TR] = new const char *[Net::Card[TR]]();
    for (int direction = PRE; direction <= POST; direction++)
    {
        Net::CardArcs[TR][direction] = new arrayindex_t[Net::Card[TR]];
        Net::Arc[TR][direction] = new arrayindex_t *[Net::Card[TR]];
        Net::Mult[TR][direction] = new mult_t *[Net::Card[TR]];
    }

    Transition::Fairness = new fairnessAssumption_t[Net::Card[TR]];

    for (arrayindex_t t = 0; t < Net::Card[TR]; t++)
    {
        // read fairness
        res = fscanf(f, "%"SCANF_MAX_WIDTH_STR"u", &tmp1);
        assert(res != EOF);

        Transition::Fairness[t] = (fairnessAssumption_t)tmp1;

        // read number of prearcs
        res = fscanf(f, "%"SCANF_MAX_WIDTH_STR"u", &tmp1);
        assert(res != EOF);

        Net::CardArcs[TR][PRE][t] = (arrayindex_t) tmp1;
        Net::Arc[TR][PRE][t] = new arrayindex_t[Net::CardArcs[TR][PRE][t]];
        Net::Mult[TR][PRE][t] = new mult_t[Net::CardArcs[TR][PRE][t]];

        // read prearcs
        for (arrayindex_t i = 0; i < Net::CardArcs[TR][PRE][t]; i++)
        {
            res = fscanf(f, "%"SCANF_MAX_WIDTH_STR"u %"SCANF_MAX_WIDTH_STR"u", &tmp1, &tmp2);
            assert(res != EOF);

            Net::Arc[TR][PRE][t][i] = (arrayindex_t) tmp1;
            Net::Mult[TR][PRE][t][i] = (mult_t) tmp2;
        }

        // read number of postarcs
        res = fscanf(f, "%"SCANF_MAX_WIDTH_STR"u", &tmp1);
        assert(res != EOF);

        Net::CardArcs[TR][POST][t] = (arrayindex_t) tmp1;
        Net::Arc[TR][POST][t] = new arrayindex_t[Net::CardArcs[TR][POST][t]];
        Net::Mult[TR][POST][t] = new mult_t[Net::CardArcs[TR][POST][t]];

        // read postarcs
        for (arrayindex_t i = 0; i < Net::CardArcs[TR][POST][t]; i++)
        {
            res = fscanf(f, "%"SCANF_MAX_WIDTH_STR"u %"SCANF_MAX_WIDTH_STR"u", &tmp1, &tmp2);
            assert(res != EOF);

            Net::Arc[TR][POST][t][i] = (arrayindex_t) tmp1;
            Net::Mult[TR][POST][t][i] = (mult_t) tmp2;
        }
    }
}
