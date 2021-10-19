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
\author Karsten
\file
\status approved 21.02.2012
\ingroup g_frontend g_symboltable

\brief implementation of class ParserPTNet

\todo Mal den new-Operator in Bezug auf Exceptions ansehen.
*/

#include <config.h>
#include <Core/Dimensions.h>
#include <Core/Runtime.h>
#include <Frontend/Parser/ParserPTNet.h>
#include <Frontend/SymbolTable/ArcList.h>
#include <Frontend/SymbolTable/PlaceSymbol.h>
#include <Frontend/SymbolTable/Symbol.h>
#include <Frontend/SymbolTable/SymbolTable.h>
#include <Frontend/SymbolTable/TransitionSymbol.h>
#include <Net/Marking.h>
#include <Net/Net.h>
#include <Net/Place.h>
#include <Net/Transition.h>

/*!
\post Memory is allocated for the symbol tables. It is released by the
destructor.
*/
ParserPTNet::ParserPTNet() :
    PlaceTable(new SymbolTable()), TransitionTable(new SymbolTable())
{
}

/*!
\post Memory of the symbol tables is deallocated.
*/
ParserPTNet::~ParserPTNet()
{
    delete PlaceTable;
    delete TransitionTable;
}

/*!
\todo comment me
*/
void ParserPTNet::symboltable2net()
{
    /*********************************************
    * 1. Allocate memory for basic net structure *
    *********************************************/

    // 1.1 set cardinalities
    const arrayindex_t cardPL = PlaceTable->getCard();
    const arrayindex_t cardTR = TransitionTable->getCard();
    Net::Card[PL] = cardPL;
    Net::Card[TR] = cardTR;
    Place::CardSignificant = UINT_MAX;  // mark as "still to be computed"

    // 1.2 allocate arrays for node (places and transitions) names, arcs, and multiplicities
    for (int type = PL; type <= TR; type ++)
    {
        Net::Name[type] = new const char *[Net::Card[type]];
        for (int direction = PRE; direction <= POST; direction ++)
        {
            Net::CardArcs[type][direction] = new arrayindex_t[Net::Card[type]];
            Net::Arc[type][direction] = new arrayindex_t *[Net::Card[type]];
            Net::Mult[type][direction] = new mult_t *[Net::Card[type]];
        }
    }


    /********************************
    * 2. Allocate memory for places *
    *********************************/

    Place::Capacity = new capacity_t[cardPL];


    /**********************************
    * 3. Allocate memory for markings *
    ***********************************/

    Marking::Initial = new capacity_t[cardPL];
    Marking::Current = new capacity_t[cardPL];


    /***********************************************
    * 4. Copy data from the symbol table to places *
    ************************************************/

    // fill all information that is locally available in symbols, allocate node specific arrays
    PlaceSymbol *ps;
    arrayindex_t i;
    for ((ps = reinterpret_cast<PlaceSymbol *>(PlaceTable->first())), (i = 0); ps;
            ps = reinterpret_cast<PlaceSymbol *>(PlaceTable->next()), i++)
    {
        const arrayindex_t tempCardPre = ps->getCardPre();
        const arrayindex_t tempCardPost = ps->getCardPost();

        // we take care of the place name (not destructed by SymbolTable)
        Net::Name[PL][i] = ps->getKey();
        Net::CardArcs[PL][PRE][i] = tempCardPre;
        Net::CardArcs[PL][POST][i] = tempCardPost;
        ps->setIndex(i);

        // allocate memory for place's arcs (is copied later with transitions)
        Net::Arc[PL][PRE][i] = new arrayindex_t[tempCardPre];
        Net::Arc[PL][POST][i] = new arrayindex_t[tempCardPost];
        Net::Mult[PL][PRE][i] = new mult_t[tempCardPre];
        Net::Mult[PL][POST][i] = new mult_t[tempCardPost];

        // capacity
        Place::Capacity[i] = ps->getCapacity();

        // set initial marking and calculate hash
        // note: seems like overkill, but will be needed for reversibility etc.
        Marking::Initial[i] = Marking::Current[i] = ps->getInitialMarking();
    }

    /*************************************
    * 5. Allocate memory for transitions *
    **************************************/

    // allocate memory for static data
    Transition::Fairness = new fairnessAssumption_t[cardTR];


    /****************************************************
    * 6. Copy data from the symbol table to transitions *
    *****************************************************/

    // current_arc is used for filling in arcs and multiplicities of places
    // calloc: no arcs there yet
    arrayindex_t *current_arc_post = new arrayindex_t[cardPL]();
    arrayindex_t *current_arc_pre = new arrayindex_t[cardPL]();

    TransitionSymbol *ts;
    for (ts = reinterpret_cast<TransitionSymbol *>(TransitionTable->first()), i = 0; ts;
            ts = reinterpret_cast<TransitionSymbol *>(TransitionTable->next()), i++)
    {
        const arrayindex_t tempCardPre = ts->getCardPre();
        const arrayindex_t tempCardPost = ts->getCardPost();

        // we need to take care of the name (not destructed by SymbolTable)
        Net::Name[TR][i] = ts->getKey();
        Net::CardArcs[TR][PRE][i] = tempCardPre;
        Net::CardArcs[TR][POST][i] = tempCardPost;
        ts->setIndex(i);

        // allocate memory for transition's arcs
        Net::Arc[TR][PRE][i] = new arrayindex_t[tempCardPre];
        Net::Arc[TR][POST][i] = new arrayindex_t[tempCardPost];
        Net::Mult[TR][PRE][i] = new mult_t[tempCardPre];
        Net::Mult[TR][POST][i] = new mult_t[tempCardPost];

        Transition::Fairness[i] = ts->getFairness();

        // copy arcs (for transitions AND places)
        ArcList *al;
        arrayindex_t j;
        for (al = ts->getPre(), j = 0; al; al = al->getNext(), j++)
        {
            const arrayindex_t k = al->getPlace()->getIndex();
            Net::Arc[TR][PRE][i][j] = k;
            Net::Arc[PL][POST][k][current_arc_post[k]] = i;
            Net::Mult[PL][POST][k][(current_arc_post[k])++] = Net::Mult[TR][PRE][i][j] = al->getMultiplicity();
        }
        for (al = ts->getPost(), j = 0; al; al = al->getNext(), j++)
        {
            const arrayindex_t k = al->getPlace()->getIndex();
            Net::Arc[TR][POST][i][j] = k;
            Net::Arc[PL][PRE][k][current_arc_pre[k]] = i;
            Net::Mult[PL][PRE][k][(current_arc_pre[k])++] = Net::Mult[TR][POST][i][j] = al->getMultiplicity();
        }
    }

    //RT::rep->message("Names %s %s %s %s", Net::Name[PL][0],Net::Name[PL][1],Net::Name[PL][2],Net::Name[PL][3]);
    delete[] current_arc_pre;
    delete[] current_arc_post;
}
