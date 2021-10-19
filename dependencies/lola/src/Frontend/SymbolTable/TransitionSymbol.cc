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
\ingroup g_frontend g_symboltable

\brief implementation of class TransitionSymbol
*/

#include <Frontend/SymbolTable/ArcList.h>
#include <Frontend/SymbolTable/PlaceSymbol.h>
#include <Frontend/SymbolTable/TransitionSymbol.h>

/*!
When transitions this constructor is called, all arcs of this transition are
known. As places are parsed earlier, we now update their information on ingoing
and outgoing arcs.

\param[in] k   the name of the transition
\param[in] f   the fairness assumption for the transition
\param[in] pr  the arcs to preplaces
\param[in] po  the arcs to postplaces
*/
TransitionSymbol::TransitionSymbol(const char *k, fairnessAssumption_t f,
                                   ArcList *pr, ArcList *po) :
    Symbol(k),
    fairness(f),
    cardPost(0), // temporary initialization until constructor finishes
    cardPre(0),  // temporary initialization until constructor finishes
    Post(po),
    Pre(pr)
{
    // count incoming arcs at transition and at places
    for (ArcList *a = Pre; a; a = a->getNext())
    {
        ++cardPre;
        a->getPlace()->notifyPost();
    }

    // count outgoing arcs at transition and at places
    for (ArcList *a = Post; a; a = a->getNext())
    {
        ++cardPost;
        a->getPlace()->notifyPre();
    }
}

unsigned int TransitionSymbol::getCardPre() const
{
    return cardPre;
}

unsigned int TransitionSymbol::getCardPost() const
{
    return cardPost;
}

ArcList *TransitionSymbol::getPre() const
{
    return Pre;
}

ArcList *TransitionSymbol::getPost() const
{
    return Post;
}

fairnessAssumption_t TransitionSymbol::getFairness() const
{
    return fairness;
}

TransitionSymbol::~TransitionSymbol()
{
    while (Pre)
    {
        ArcList *tmp = Pre;
        Pre = Pre->getNext();
        delete tmp;
    }
    while (Post)
    {
        ArcList *tmp = Post;
        Post = Post->getNext();
        delete tmp;
    }
}
