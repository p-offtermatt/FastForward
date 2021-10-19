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
\author Christian Koch
\status new
\ingroup ctl

\brief Implementation of the CTL EU operator.
*/

#include <Formula/CTL/EUFormula.h>
#include <Exploration/DFSStackEntry.h>
#include <Exploration/Firelist.h>
#include <Exploration/SearchStack.h>
#include <Net/Transition.h>

EUFormula::EUFormula(CTLFormula *phi, CTLFormula *psi) : phi(phi), psi(psi)
{
    assert(phi);
    assert(psi);
}

void EUFormula::initAtomics(NetState &ns)
{
    phi->initAtomics(ns);
    psi->initAtomics(ns);
}

void EUFormula::updateAtomics(NetState &ns, arrayindex_t t)
{
    phi->updateAtomics(ns, t);
    psi->updateAtomics(ns, t);
}

void EUFormula::revertAtomics(NetState &ns, arrayindex_t t)
{
    phi->revertAtomics(ns, t);
    psi->revertAtomics(ns, t);
}

// LCOV_EXCL_START
void EUFormula::DEBUG_print()
{
    printf("[%u,%u,%zu]E(", index, dfsindex, payloadsize);
    phi->DEBUG_print();
    printf(")U(");
    psi->DEBUG_print();
    printf(")");
}
// LCOV_EXCL_STOP

/*!
\param[in,out] numDFS  todo
\param[in,out] numCachedResults  todo
*/
void EUFormula::gatherPayloadInformation(arrayindex_t &numDFS,
        arrayindex_t &numCachedResults)
{
    dfsindex = (numDFS++) * SIZEOF_STATENUMBER_T;
    index = (numCachedResults++) * 2;
    phi->gatherPayloadInformation(numDFS, numCachedResults);
    psi->gatherPayloadInformation(numDFS, numCachedResults);
}

void EUFormula::setPayloadInformation(arrayindex_t cachedResultOffset,
                                      size_t payloadSize)
{
    index += cachedResultOffset * 8;
    payloadsize = payloadSize;
    phi->setPayloadInformation(cachedResultOffset, payloadSize);
    psi->setPayloadInformation(cachedResultOffset, payloadSize);
}

/*!
Implements the check whether E(phi U psi) holds in the current state. The
algorithm closely follows the pseudocode given in the CGV lecture (slide 125).

\param s  the store for the markings with their payload
\param ns  the current net state (may be updated)
\param firelist  the current firelist (may be updated)
\param[in,out] witness  the witness path

\return Whether E(phi U psi) holds in the current state. In case the formula is
proved true, witness contains a witness path.

\post during the check, sucessor states may be checked recursively
*/
bool EUFormula::check(Store<void *> &s, NetState &ns, Firelist &firelist,
                      std::vector<int> &witness)
{
    //==================================
    // (1) prepare marking and payload
    //==================================

    // check if marking has already been stored
    void **pInitialPayload;
    if (!s.searchAndInsert(ns, &pInitialPayload, 0))
    {
        // The marking is new: initialize the payload with 0 (the starting
        // value) for all entries.
        *pInitialPayload = ::operator new(payloadsize);
        // Memset is required to mimick a calloc on a void* in C++.
        memset(*pInitialPayload, 0, payloadsize);
    }
    void *payload = *pInitialPayload;

    // => the payload is now initialized


    //==========================
    // (2) check cached result
    //==========================

    // check if we already know the result: check cached result
    switch (getCachedResult(payload))
    {
    case UNKNOWN:
        break;
    case KNOWN_TRUE:
        return true;
    case KNOWN_FALSE:
        return false;
    case IN_PROGRESS:
        assert(false); // impossible for first state
    }

    // => we do not know the value of the formula, so we need to calculate it


    //=========================================
    // (3) quick check 1: psi => E(phi U psi)
    //=========================================

    // check psi
    if (psi->check(s, ns, firelist, witness))
    {
        // We know that psi holds, so E(phi U psi) holds by definition. We
        // hence found a witness for E(phi U psi) and return.
        setCachedResult(payload, KNOWN_TRUE);
        return true;
    }

    // reset the witness
    witness.clear();

    // => We know that psi does not hold.


    //======================================================
    // (4) quick check 2: (!phi AND !psi) => !E(phi U psi)
    //======================================================

    // check phi
    if (not phi->check(s, ns, firelist, witness))
    {
        // We know that neither phi nor psi hold, so E(phi U psi) does not hold
        // by definition. We hence could not prove E(phi U psi) and return.
        setCachedResult(payload, KNOWN_FALSE);
        return false;
    }

    // => We know that phi holds, but phi does not hold yet.


    //===================================
    // (5) prepare a depth-first search
    //===================================

    // The DFS stack will contain all gray nodes.
    SearchStack<DFSStackEntry> dfsStack;

    // The Tarjan stack will contain all _black_ nodes the SCC of which is not
    // yet finished. An explanation can be found in the doc/Tarjan folder.
    SearchStack<void *> tarjanStack;

    // Initialize DFS number and the lowlink value; mark state to be on Tarjan
    // stack. The DFS starts with 1 to leave 0 for recognizing uninitialized
    // values.
    statenumber_t currentDFSNumber = 1;
    setDFS(payload, currentDFSNumber);
    statenumber_t currentLowlink = currentDFSNumber;

    // Mark this state to be in progress.
    setCachedResult(payload, IN_PROGRESS);


    //=================================
    // (6) check the successor states
    //=================================

    // fetch firelist for current state
    arrayindex_t *currentFirelist;
    arrayindex_t currentFirelistIndex = firelist.getFirelist(ns, &currentFirelist);

    // A flag to signal whether enabledness and atomics need to be reverted
    // after the check in (8b).
    bool revertEnabledNeeded = false;

    // The followin loop with the nested if is ugly, but the only way to avoid
    // goto jumps.
    while (true)
    {
        // Note currentFirelistIndex can be updated within the loop.
        if (currentFirelistIndex--)
        {
            // fire thre current transition
            Transition::fire(ns, currentFirelist[currentFirelistIndex]);

            // We don't update the enabledness and the atomics yet, because
            // this may not be needed at all if we find the result for the new
            // state on the cache. If such update is required, we later set
            // revertEnabledNeeded to true.

            // check if marking has already been stored
            void **pNewPayload;
            if (!s.searchAndInsert(ns, &pNewPayload, 0))
            {
                // The marking is new: initialize the payload with 0 (the
                // starting value) for all entries.
                *pNewPayload = ::operator new(payloadsize);
                // Memset is required to mimick a calloc on a void* in C++.
                memset(*pNewPayload, 0, payloadsize);
            }
            void *newpayload = *pNewPayload;


            // check if we already know the result: check cached result
            const CTLFormulaResult newCachedResult = getCachedResult(newpayload);
            if (newCachedResult == UNKNOWN)
            {
                // update enabledness and atomic propositions for current state
                // (needed for further checking)
                Transition::updateEnabled(ns, currentFirelist[currentFirelistIndex]);
                updateAtomics(ns, currentFirelist[currentFirelistIndex]);

                // reset the witness
                witness.clear();


                //==========================================
                // (6a) quick check 1: psi => E(phi U psi)
                //==========================================

                // check psi
                if (psi->check(s, ns, firelist, witness))
                {
                    // We know that psi holds, so E(phi U psi) holds by
                    // definition. We hence found a witness for E(phi U psi) and
                    // break out of the loop and continue at (8b).
                    setCachedResult(newpayload, KNOWN_TRUE);

                    // we updated the enabledness above, so it needs to be
                    // reverted in (8b)
                    revertEnabledNeeded = true;

                    // break; set all nodes to true in (8b)
                    break;
                }

                // reset the witness
                witness.clear();

                //=======================================================
                // (6b) quick check 2: (!phi AND !psi) => !E(phi U psi)
                //=======================================================

                // check phi
                if (not phi->check(s, ns, firelist, witness))
                {
                    // We know that neither phi nor psi hold, so we could not
                    // prove E(phi U psi) for this state and hence backtrack
                    // and continue.
                    setCachedResult(newpayload, KNOWN_FALSE);

                    // backtrack (backfire and revert)
                    Transition::backfire(ns, currentFirelist[currentFirelistIndex]);
                    Transition::revertEnabled(ns, currentFirelist[currentFirelistIndex]);
                    revertAtomics(ns, currentFirelist[currentFirelistIndex]);

                    // continue with next transition
                    continue;
                }


                //========================
                // (7a) recursive decent
                //========================

                // Store the current fire list, the current state's payload and
                // the current low link on the DFS stack.
                new (dfsStack.push()) DFSStackEntry(currentFirelist,
                                                    currentFirelistIndex,
                                                    payload, currentLowlink);

                // replace the payload and the firelist
                payload = newpayload;
                currentFirelistIndex = firelist.getFirelist(ns, &currentFirelist);

                // organize DFS stack
                setDFS(newpayload, ++currentDFSNumber);
                currentLowlink = currentDFSNumber;
                setCachedResult(newpayload, IN_PROGRESS);

                // continue with next transition (first of new firelist)
                continue;
            }
            else if (newCachedResult == KNOWN_TRUE)
            {
                // The result of the new state is known to be true. We hence
                // found a witness for E(phi U psi) and can abort the search by
                // breaking out of the loop to (8b) where we organize the
                // witness and set states' results to true. The enabledness
                // and atomics weren't updated, so no revert needed.
                break;
            }
            else if (newCachedResult == IN_PROGRESS)
            {
                // The result of the new state is not yet known and a check is
                // in progress. We hence cannot prove E(phi U psi) yet and
                // hence backtrack and continue. The enabledness and atomics
                // weren't updated, so no revert needed.
                Transition::backfire(ns, currentFirelist[currentFirelistIndex]);

                // update lowlink
                const statenumber_t newdfs = getDFS(newpayload);
                if (newdfs < currentLowlink)
                {
                    currentLowlink = newdfs;
                }

                // continue with next transition
                continue;
            }
            else // (newCachedResult == KNOWN_FALSE)
            {
                // The result of the new state is known to be false. We hence
                // could not prove E(phi U psi) for this state and hence
                // backtrack and continue. The enabledness and atomics weren't
                // updated, so no revert needed.
                Transition::backfire(ns, currentFirelist[currentFirelistIndex]);

                // continue with next transition
                continue;
            }
        }
        else
        {
            //===================================
            // (7b) no more transitions to fire
            //===================================

            // => we need to tidy up the stacks and return false

            // free memory for finished firelist
            delete[] currentFirelist;

            // check if SCC is finished
            const statenumber_t dfs = getDFS(payload);
            if (dfs == currentLowlink)
            {
                // SCC is finished: all elements on the Tarjan stack that have
                // a higher DFS number then ours belong to the finished SCC
                // => we can set the formula to false.
                while (tarjanStack.StackPointer and getDFS(tarjanStack.top()) > dfs)
                {
                    setCachedResult(tarjanStack.top(), KNOWN_FALSE);
                    tarjanStack.pop();
                }
                setCachedResult(payload, KNOWN_FALSE);
            }
            else
            {
                // SCC not yet finished, push self onto Tarjan stack
                assert(dfs > currentLowlink);
                *tarjanStack.push() = payload;
            }

            // check if there are any states to backtrack to
            if (dfsStack.StackPointer)
            {
                //=======================================================
                // (7c) we can backtrack => continue search for witness
                //=======================================================

                // restore firelist, payload, and lowlink
                currentFirelist = dfsStack.top().fl;
                currentFirelistIndex = dfsStack.top().flIndex;
                payload = dfsStack.top().payload;
                if (currentLowlink > dfsStack.top().lowlink)
                {
                    // propagate lowlink to parent
                    currentLowlink = dfsStack.top().lowlink;
                }
                dfsStack.pop();

                // backtrack
                Transition::backfire(ns, currentFirelist[currentFirelistIndex]);
                Transition::revertEnabled(ns, currentFirelist[currentFirelistIndex]);
                revertAtomics(ns, currentFirelist[currentFirelistIndex]);
            }
            else
            {
                //===========================================================
                // (8a) we did not found a witness -> E(phi U psi) is false
                //===========================================================

                // tarjan stack empty
                assert(!tarjanStack.StackPointer);
                // first node is always start of SCC
                assert(dfs == currentLowlink);
                // returned to initial state
                assert(*pInitialPayload == payload);

                // reset the witness
                witness.clear();

                return false;
            }
        }
    }


    //=============================================================
    // (8b) we aborted the search without finding a counterexample
    //=============================================================

    // => E(phi U psi) holds - we only need to organize the witness

    // Revert the transition that brought us to the witness state.
    Transition::backfire(ns, currentFirelist[currentFirelistIndex]);

    // Possibly revert enabledness and the atomics - this was skipped in the
    // loop before.
    if (revertEnabledNeeded)
    {
        Transition::revertEnabled(ns, currentFirelist[currentFirelistIndex]);
        revertAtomics(ns, currentFirelist[currentFirelistIndex]);
    }

    // Add the last transition to witness path.
    witness.push_back(currentFirelist[currentFirelistIndex]);

    // The current state can reach witness state => the formula is known to be
    // true.
    setCachedResult(payload, KNOWN_TRUE);

    // All elements that are still on the Tarjan stack can reach this state
    // => for these states, the formula is known to be true.
    while (tarjanStack.StackPointer)
    {
        setCachedResult(tarjanStack.top(), KNOWN_TRUE);
        tarjanStack.pop();
    }

    // free memory for current firelist
    delete[] currentFirelist;

    // All elements that are still on DFS stack (and hence on Tarjan stack) can
    // reach this state => for these states, the formula is known to be true.
    // We need to revert all the transitions to restore original net state.
    while (dfsStack.StackPointer)
    {
        setCachedResult(dfsStack.top().payload, KNOWN_TRUE);
        Transition::backfire(ns, dfsStack.top().fl[dfsStack.top().flIndex]);
        Transition::revertEnabled(ns, dfsStack.top().fl[dfsStack.top().flIndex]);
        revertAtomics(ns, dfsStack.top().fl[dfsStack.top().flIndex]);

        witness.push_back(dfsStack.top().fl[dfsStack.top().flIndex]);

        // free memory for stacked firelist
        delete[] dfsStack.top().fl;

        dfsStack.pop();
    }

    // => the witness path is complete

    // E(phi U psi) holds and we found a witness path
    return true;
}

FormulaInfo *EUFormula::getInfo() const
{
    FormulaInfo *Info = new FormulaInfo();
    Info->tag = formula_eu;
    Info->cardChildren = 2;
    Info->ctlChildren = new CTLFormula * [2];
    Info->ctlChildren[0] = phi;
    Info->ctlChildren[1] = psi;
    return Info;
}

int EUFormula::countSubFormulas() const
{
    return 1 + phi->countSubFormulas() + psi->countSubFormulas();
}
