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
\author Markus
\status new
\brief Class for firelist generation. Generates a firelist based on Stubborn
sets created by upsets and downsets
*/

#include <Core/Dimensions.h>
#include <Exploration/Firelist.h>
#include <Exploration/FirelistStubbornTsccUpset.h>
#include <Net/Net.h>
#include <Net/NetState.h>
#include <Net/Transition.h>
#include <Exploration/SimpleProperty.h>
#include <Exploration/StatePredicateProperty.h>
#include <Formula/StatePredicate/StatePredicate.h>

FirelistStubbornTsccUpset::FirelistStubbornTsccUpset(SimpleProperty *simpleproperty) :
    sp(static_cast<StatePredicateProperty *>(simpleproperty)),
    dfsStack(new arrayindex_t[Net::Card[TR]]),
    onStack(new bool[Net::Card[TR]]()),
    dl(new FirelistStubbornDeadlock())
{}

FirelistStubbornTsccUpset::~FirelistStubbornTsccUpset()
{
    delete[] dfsStack;
    delete[] onStack;
}


/*!
  \param ns the net-state for which the firelist should be determined
  \param[in,out] result the actual fire list (contains the enabled transitions for this netstate)
  \result number of elements in fire list
  */
arrayindex_t FirelistStubbornTsccUpset::getFirelist(NetState &ns, arrayindex_t **result)
{
    scapegoatNewRound();
    //if property is true then upset is empty
    // other cases for upset calulation:
    // Property := State Property
    // M(p)     := Marking (eg. ns)
    // k        := the natural number defined by property that
    //             creates a border to M(p)
    //
    //
    // Property == M(p) <= k : up(M)
    // Property == M(p) = k : up(M)
    // Property == M(p) >= k : up(M)
    // Property == M(p) != k : up(M)
    arrayindex_t cardEnabled = 0;
    arrayindex_t stackpointer = 0;
    //get the upset
    bool needEnabled;
    if (!sp->getPredicate()->value)
    {
        stackpointer = sp->getPredicate()->getUpSet(dfsStack, onStack, &needEnabled);
    }
    else
    {
        //do bruteforce firelist or just set upset empty?
        //stackpointer = 0;
        // for now use bruteforce
        assert(ns.CardEnabled <= Net::Card[TR]);

        *result = new arrayindex_t[ns.CardEnabled];
        arrayindex_t i = 0;
        for (arrayindex_t t = 0; t < Net::Card[TR]; ++t)
        {
            if (ns.Enabled[t])
            {
                assert(i < ns.CardEnabled);
                (*result)[i++] = t;
            }
        }
        return ns.CardEnabled;
    }

    //fprintf(stderr,"stackp: %d\n",stackpointer);

    // loop until all stack elements processed
    for (arrayindex_t firstunprocessed = 0; firstunprocessed < stackpointer; ++firstunprocessed)
    {
        arrayindex_t currenttransition = dfsStack[firstunprocessed];
        arrayindex_t *mustbeincluded;
        arrayindex_t cardmustbeincluded;
        if (ns.Enabled[currenttransition])
        {
            ++cardEnabled;
            mustbeincluded = Transition::Conflicting[currenttransition];
            cardmustbeincluded = Transition::CardConflicting[currenttransition];
        }
        else
        {
	    // select scapegoat
	    arrayindex_t scapegoat = Firelist::selectScapegoat(ns,currenttransition);

            mustbeincluded = Net::Arc[PL][PRE][scapegoat];
            cardmustbeincluded = Net::CardArcs[PL][PRE][scapegoat];
        }
        for (arrayindex_t i = 0; i < cardmustbeincluded; ++i)
        {
            const arrayindex_t t = mustbeincluded[i];
            if (!onStack[t])
            {
                dfsStack[stackpointer++] = t;
                onStack[t] = true;
            }
        }
    }
    if (cardEnabled || !needEnabled)
    {
        // an up set for deadlock atomic propositions is any
        // enabled transition. If, through the other up sets,
        // an eabled transition is there, everything is ok.
        arrayindex_t size = cardEnabled;
        * result = new arrayindex_t [cardEnabled];
        for (arrayindex_t i = 0; i < stackpointer; ++i)
        {
            const arrayindex_t t = dfsStack[i];
            if (ns.Enabled[t])
            {
                (*result)[--cardEnabled] = t;
            }
            onStack[t] = false;
        }
        return size;
    }
    else
    {
        // If the other up sets to not yield an enabled transition,
        // we need to return a stubborn set with an enabled
        // transition. This is exactly any deadlock preserving
        // stubborn set.
        for (arrayindex_t i = 0; i < stackpointer; ++i)
        {
            onStack[dfsStack[i]] = false;
        }
        return dl->getFirelist(ns, result);
    }
}


Firelist *FirelistStubbornTsccUpset::createNewFireList(SimpleProperty *property)
{
    return new FirelistStubbornTsccUpset(property);
}
