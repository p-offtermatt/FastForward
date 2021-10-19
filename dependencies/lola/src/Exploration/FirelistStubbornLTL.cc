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
 * \file
 * \author Torsten
 * \status new
 * \brief A class for firelist generation. Default is firelist consisting of all
 *        enabled transitions.
 */

#include <Core/Dimensions.h>
#include <Core/Runtime.h>
#include <Exploration/Firelist.h>
#include <Exploration/FirelistStubbornLTL.h>
#include <Formula/LTL/BuechiAutomata.h>
#include <Formula/StatePredicate/StatePredicate.h>
#include <Exploration/StatePredicateProperty.h>
#include <Net/Net.h>
#include <Net/NetState.h>
#include <Net/Transition.h>

/*!
 * \brief A constructor for stubborn sets (firelists) using the algorithm
 *        from Koch.
 */
FirelistStubbornLTL::FirelistStubbornLTL(BuechiAutomata *bauto) : bauto(bauto),
    dfsStack(new arrayindex_t[Net::Card[TR]]), 
    onStack(new bool[Net::Card[TR]]())
{
}

/*!
 * \brief Destructor.
 */
FirelistStubbornLTL::~FirelistStubbornLTL()
{
    delete[] dfsStack;
    delete[] onStack;
}
         
/*!
 * \brief A function to construct a stubborn set (firelist) for a given marking.
 * \param[in] ns The netstate for which the stubborn set (firelist) should be 
 *               build.
 * \param[in,out] result A pointer to NULL, which will be replaced by a pointer 
 *                       to an array containing the stubborn set (firelist).
 * \return The number of elements in the stubborn set (firelist).
 */
arrayindex_t FirelistStubbornLTL::getFirelist(NetState &ns,
        arrayindex_t **result)
{
    //RT::rep->message("Firelist is used...");
    
    // Assertions
    assert(ns.CardEnabled <= Net::Card[TR]);
    
    // Check for deadlock
    if (ns.CardEnabled == 0)
    {
        // found a deadlock - return empty firelist
        * result = new arrayindex_t[1];
        return 0;
    }
    scapegoatNewRound();    
    //---- 1. Step: Get the UP sets from all phi_i, which are leaving the current 
    // Buechi state q. 
    // Point 4 of the theorem.
    
    //RT::rep->message("Current Buechi state is %i", ns.currentAutomatonState);
    arrayindex_t stackPointer = 0;
    bool needEnabled = false;
    StatePredicate *spFormula;
    // Iterate through all phi_i, which can leave the current Buechi state
    uint32_t psiTransition = 0;
    
    for (int i = 0; i < bauto->cardTransitions[ns.currentAutomatonState]; i++)
    {
        // Check, if phi_i is not psi and not true. Only transitions are 
        // considered, which are leaving the current Buechi state and aren't true
        //RT::rep->message("Get the UP sets of all phi_i");
        if (bauto->transitions[ns.currentAutomatonState][i][1] != ns.currentAutomatonState)
        {
            // Check if phi_i is fale to get the UP set for this phi_i
            if (bauto->atomicPropositions[bauto->transitions[ns.currentAutomatonState][i][0]]->getPredicate()->value == 0)
            {
                // Get the phi (predicate)
                spFormula = bauto->atomicPropositions[bauto->transitions[ns.currentAutomatonState][i][0]]->getPredicate();
                // Get the UP set - transition of the UP set, which are already on
                // the dfsStack are ignored -> dfsStack holds all transitions of
                // all UP sets
                stackPointer = spFormula->getUpSet(dfsStack, onStack, &needEnabled);
            }
            //else
            // TODO check what must be done if phi_i is true (value == 1) in 
            // current Buechi state - can happen in the initial state
        }
        else
        {
            // Get the number of the edge (transition) which is the loop edge
            // of the current Buechi state - called psi 
            // Needed for the semi-invisibility check and done here to avoid 
            // another loop through cardTransitions
            psiTransition = bauto->transitions[ns.currentAutomatonState][i][1];
        }
    }
    
    // Check if stackPointer is empty
    if (stackPointer == 0)
    {   
//        RT::rep->message("No UP set is found. Use all Transitions.");
        // No UP set is found
        // TODO check if there is a better way than to return all transtions
        // We return all transitions
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
    
    //----- 2. Check semi-invisibility
    // Point 2 of the theorem
//    RT::rep->message("Check semi-invisibility.");
    
    // Get the psi (predicate)
    spFormula = bauto->atomicPropositions[bauto->transitions[ns.currentAutomatonState][psiTransition][0]]->getPredicate(); 
    
    //----- 3. Build the closure to the transitions we got from the phi_i 
    // UP sets and from the semi-invisibility check
    // Point 1 and 5 of the theorem
//    RT::rep->message("Build the closure to transitions we got from the the"
//    " phi_i UP sets and from the semi-invisibility check.");

    arrayindex_t cardEnabled = 0;
    // loop until all stack elements processed
    for (arrayindex_t firstUnprocessed = 0; firstUnprocessed < stackPointer; ++firstUnprocessed)
    {
        arrayindex_t currenttransition = dfsStack[firstUnprocessed];
        arrayindex_t *mustbeincluded;
        arrayindex_t  cardmustbeincluded;
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
                dfsStack[stackPointer++] = t;
                onStack[t] = true;
            }
        }
    }  
            
    // TODO Check if code must be changed to the FirelistStubbornLTL situation
    if (cardEnabled || !needEnabled)
    {
        // if up set for deadlock AP is required but there is an enabled
        // transition so far, this enabled transition serves as a valid
        // up set
        arrayindex_t size = cardEnabled;
        * result = new arrayindex_t [cardEnabled];
        for (arrayindex_t i = 0; i < stackPointer; ++i)
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
        // if no enabled transition is in the stubborn set yet, all transitions
        // are returned
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
    
    //----- 4. Fire all t of the firelist and check, if new marking holds psi
    // Point 3 of the theorem
    // Do this in LTLExploration
}

Firelist *FirelistStubbornLTL::createNewFireList(BuechiAutomata *bauto)
{
    return new FirelistStubbornLTL(bauto);
}
