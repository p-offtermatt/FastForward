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
\status approved 27.01.2012,
		changed progress measure to int32 on 18.12.2012
\brief Useful routines for transition specific information

All data that describe attributes of transitions can be found here. General
information for a transition in its role as a node, ar contained in Node.*
*/

#include <config.h>
#include <Core/Dimensions.h>
#include <Core/Runtime.h>
#include <Net/Marking.h>
#include <Net/Net.h>
#include <Net/NetState.h>
#include <Net/Place.h>
#include <Net/Transition.h>

fairnessAssumption_t *Transition::Fairness = NULL;
bool *Transition::Enabled = NULL;
arrayindex_t Transition::CardEnabled = 0;
hash_t *Transition::DeltaHash = NULL;
arrayindex_t *Transition::CardDeltaT[2] = {NULL};
arrayindex_t **Transition::DeltaT[2] = {NULL};
mult_t **Transition::MultDeltaT[2] = {NULL};
arrayindex_t *Transition::CardConflicting = NULL;
bool *Transition::ConflictingIsOriginal = NULL;
arrayindex_t **Transition::Conflicting = NULL;
arrayindex_t *Transition::CardBackConflicting = NULL;
arrayindex_t **Transition::BackConflicting = NULL;
bool *Transition::BackConflictingIsOriginal = NULL;
int32_t *Transition::ProgressMeasure = NULL;

/*!
\brief clean up transitions for valgrind
\post Transition data is deleted
*/
void Transition::deleteTransitions()
{
    // allocated in ParserPTNet::symboltable2net()
    delete[] Transition::Fairness;

    // allocated in Net::preprocess()
    delete[] Transition::Enabled;
    delete[] Transition::DeltaHash;

    // allocated in Net::preprocess_organizeDeltas()
    for (int direction = PRE; direction <= POST; direction++)
    {
        for (arrayindex_t i = 0; i < Net::Card[TR]; i++)
        {
            delete[] Transition::MultDeltaT[direction][i];
            delete[] Transition::DeltaT[direction][i];
        }
        delete[] Transition::DeltaT[direction];
        delete[] Transition::MultDeltaT[direction];
        // allocated in Net::preprocess()
        delete[] Transition::CardDeltaT[direction];
    }

    // allocated in Net::preprocess()
    delete[] Transition::CardConflicting;
    for (arrayindex_t i = 0; i < Net::Card[TR]; i++)
    {
        if (Transition::ConflictingIsOriginal[i])
        {
            delete[] Transition::Conflicting[i];
        }
        if (Transition::BackConflictingIsOriginal[i])
        {
            delete[] Transition::BackConflicting[i];
        }
    }
    delete[] Transition::Conflicting;
    delete[] Transition::BackConflicting;
    delete[] Transition::CardBackConflicting;
    delete[] Transition::ConflictingIsOriginal;
    delete[] Transition::BackConflictingIsOriginal;

    // allocated in Net::setProgressMeasure()
    delete[] Transition::ProgressMeasure;
}


/*!
 Check transition for activation
 1. scan through pre-places for testing enabledness
 \todo swap of values to be done with XOR (ineffektiv(er) bei heutigen Compilern! Außerdem haben wir zwei Swaps verschränkt...)
*/
void Transition::checkEnabled(NetState &ns, arrayindex_t t)
{
    // scan through all pre-places
    for (arrayindex_t i = 0; i < Net::CardArcs[TR][PRE][t]; ++i)
    {
        if (ns.Current[Net::Arc[TR][PRE][t][i]] < Net::Mult[TR][PRE][t][i])
        {
            // transition is disabled
            if (ns.Enabled[t])
            {
                // enabled --> disabled
                ns.Enabled[t] = false;
                --ns.CardEnabled;

            }
            return;
        }
    }
    // for loop completed: we did not find an insufficiently marked place
    // => transition enabled
    if (!ns.Enabled[t])
    {
        // disabled-->enabled
        ns.Enabled[t] = true;
        ++ns.CardEnabled;
    }
}

/*!
 This function checks the enabledness of a netstate and starts at index t
\param[in] t index where the check for enabledness should start
 */
void Transition::checkEnabled_Initial(arrayindex_t t)
{
    NetState ns;
    ns.Current = Marking::Current;
    ns.HashCurrent = Marking::HashCurrent;
    ns.Enabled = Transition::Enabled;
    ns.CardEnabled = Transition::CardEnabled;
    checkEnabled(ns, t);
    Marking::HashCurrent = ns.HashCurrent;
    Transition::CardEnabled = ns.CardEnabled;
}


/*!
 fire a transition 
 \param[in,out] ns a netstate on which the transition must be enabled
 \param[in] t index of the to fire transition
 \pre the Transition on place t of the Netstate ns must be enabled
 \post Transition was fired -> Markings(Deltas) and Hashes are updated
 */
void Transition::fire(NetState &ns, arrayindex_t t)
{
    // Don't even think about firing a disabled transition!
    assert(ns.Enabled[t]);

    // 1. Update current marking
    for (arrayindex_t i = 0; i < Transition::CardDeltaT[PRE][t]; i++)
    {
        // there should be enough tokens to fire this transition
        assert(ns.Current[Transition::DeltaT[PRE][t][i]] >= Transition::MultDeltaT[PRE][t][i]);

        ns.Current[Transition::DeltaT[PRE][t][i]] -= Transition::MultDeltaT[PRE][t][i];
    }
    for (arrayindex_t i = 0; i < Transition::CardDeltaT[POST][t]; i++)
    {
        ns.Current[Transition::DeltaT[POST][t][i]] += Transition::MultDeltaT[POST][t][i];
    }
    // 2. update hash value
    ns.HashCurrent += Transition::DeltaHash[t];
    ns.HashCurrent %= SIZEOF_MARKINGTABLE;
    while (UNLIKELY(ns.HashCurrent < 0))
    {
        // just safety belt, if % returns negative value
        ns.HashCurrent += SIZEOF_MARKINGTABLE; // LCOV_EXCL_LINE
    }
}

/*!
 check if there is a cycle
 \param[in] t arrayindex_t to use for checking
 \return true if the NetState contains a cycle  else false
 */
bool Transition::isCycle(arrayindex_t t)
{
    return (Transition::CardDeltaT[PRE][t] == 0 && Transition::CardDeltaT[POST][t] == 0);
}

/// update enabledness information after having fired a transition
void Transition::updateEnabled(NetState &ns, arrayindex_t t)
{
    // 1. check conflicting enabled transitions (tt) for enabledness
    for (arrayindex_t i = 0; i < Transition::CardConflicting[t]; i++)
    {
        const arrayindex_t tt = Transition::Conflicting[t][i];
        if (ns.Enabled[tt])
        {
            checkEnabled(ns, tt);
        }
    }

    // 2. check back-conflicting (those that have a pre-place which is a 
    // post-place of this transition) disabled transitions
    for (arrayindex_t i = 0; i < Transition::CardBackConflicting[t]; i++)
    {
        const arrayindex_t tt = Transition::BackConflicting[t][i];
        if (!ns.Enabled[tt])
        {
            checkEnabled(ns, tt);
        }
    }
    //for (arrayindex_t i = 0; i < Transition::CardDeltaT[POST][t]; i++)
    //{
     //   // one place that got new tokens
      //  const arrayindex_t p = Transition::DeltaT[POST][t][i];
      //  for (arrayindex_t j = 0; j < ns.CardDisabled[p]; /* tricky increment handling */)
       // {
        //    const arrayindex_t tt = ns.Disabled[p][j];
         //   checkEnabled(ns, tt);
          //  if (ns.Disabled[p][j] == tt)
           // {
            //    j++; /* tricky increment handling */
            //}
        //}
    //}
}

/// fire a transition in reverse direction (for backtracking) and update enabledness of all transitions
void Transition::backfire(NetState &ns, arrayindex_t t)
{
    // 1. Update current marking
    for (arrayindex_t i = 0; i < Transition::CardDeltaT[PRE][t]; i++)
    {
        ns.Current[Transition::DeltaT[PRE][t][i]] += Transition::MultDeltaT[PRE][t][i];
    }
    for (arrayindex_t i = 0; i < Transition::CardDeltaT[POST][t]; i++)
    {
        // there should be enough tokens to backfire this transition
        assert(ns.Current[Transition::DeltaT[POST][t][i]] >= Transition::MultDeltaT[POST][t][i]);

        ns.Current[Transition::DeltaT[POST][t][i]] -= Transition::MultDeltaT[POST][t][i];
    }
    // 2. update hash value
    ns.HashCurrent -= Transition::DeltaHash[t];
    ns.HashCurrent %= SIZEOF_MARKINGTABLE;
    while (ns.HashCurrent < 0)
    {
        ns.HashCurrent += SIZEOF_MARKINGTABLE;
    }
}

/// update enabledness after having backfired a transition
void Transition::revertEnabled(NetState &ns, arrayindex_t t)
{
    // 1. check backward conflicting enabled transitions for enabledness
    for (arrayindex_t i = 0; i < Transition::CardBackConflicting[t]; i++)
    {
        const arrayindex_t tt = Transition::BackConflicting[t][i];
        if (ns.Enabled[tt])
        {
            checkEnabled(ns, tt);
        }
    }

    // 2. check conflicting disabled transitions for enabledness
    for (arrayindex_t i = 0; i < Transition::CardConflicting[t]; i++)
    {
        const arrayindex_t tt = Transition::Conflicting[t][i];
        if (!ns.Enabled[tt])
        {
            checkEnabled(ns, tt);
        }
    }
    //// 2. check those transitions where the scapegoat received additional tokens
  //  for (arrayindex_t i = 0; i < Transition::CardDeltaT[PRE][t]; i++)
   // {
    //    const arrayindex_t p = Transition::DeltaT[PRE][t][i]; // one place that got new tokens
     //   for (arrayindex_t j = 0; j < ns.CardDisabled[p]; /* tricky increment handling */)
      //  {
       //     const arrayindex_t tt = ns.Disabled[p][j];
        //    checkEnabled(ns, tt);
         //   if (ns.Disabled[p][j] == tt)
          //  {
           //     j++; /* tricky increment handling */
           // }
        //}
    //}
}

// LCOV_EXCL_START
bool DEBUG__testEnabled(NetState &ns, arrayindex_t t)
{
    if (ns.Enabled[t])
    {
        for (arrayindex_t i = 0; i < Net::CardArcs[TR][PRE][t]; i++)
        {
            arrayindex_t p = Net::Arc[TR][PRE][t][i];
            if (ns.Current[p] < Net::Mult[TR][PRE][t][i])
            {
                return false;
            }
        }
    }
    else
    {
        arrayindex_t p = Net::Arc[TR][PRE][t][0];
        if (ns.Current[p] >= Net::Mult[TR][PRE][t][0])
        {
            return false;
        }
    }
    return true;
}
// LCOV_EXCL_STOP
