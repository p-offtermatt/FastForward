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

\brief Implementation of the CTL AX operator.
*/

#include <Exploration/Firelist.h>
#include <Formula/CTL/AXFormula.h>
#include <Net/Transition.h>

/*!
\param phi  the inner formula for AX phi
*/
AXFormula::AXFormula(CTLFormula *phi) : phi(phi)
{
    assert(phi);
}

void AXFormula::initAtomics(NetState &ns)
{
    phi->initAtomics(ns);
}

void  AXFormula::updateAtomics(NetState &ns, arrayindex_t t)
{
    phi->updateAtomics(ns, t);
}

void AXFormula::revertAtomics(NetState &ns, arrayindex_t t)
{
    phi->revertAtomics(ns, t);
}

/*!
\param s  the store to use
\param ns  the current state of the net
\param firelist  a list of activated transitions (possibly only a subset)
\param[in,out] witness  a witness path to which we may add transitions

\post In case the function returns false, witness holds a counterexample for
AX phi, consisting of a transition that leads to a state that does not satisfy
phi.
*/
bool AXFormula::check(Store<void *> &s, NetState &ns, Firelist &firelist,
                      std::vector<int> &witness)
{
    // get a list of transitions to fire
    arrayindex_t *fl;
    arrayindex_t cardfl = firelist.getFirelist(ns, &fl);

    // fire each transition and check whether all successor states satisfy phi
    while (cardfl--)
    {
        // fire the transition and evaluate the our formula's state
        const arrayindex_t currentTransition = fl[cardfl];
        Transition::fire(ns, currentTransition);
        Transition::updateEnabled(ns, currentTransition);
        updateAtomics(ns, currentTransition);

        // store the value of the phi
        const bool result = phi->check(s, ns, firelist, witness);

        // restore the state before firing
        Transition::backfire(ns, currentTransition);
        Transition::revertEnabled(ns, currentTransition);
        revertAtomics(ns, currentTransition);

        if (not result)
        {
            // the successor state does not satisfiy phi: AX phi = false -
            // add the current transition to the counterexample and abort search
            witness.push_back(currentTransition);
            return false;
        }

        // the successor state does satisfies phi: continue - we have not yet
        // found a counterexample for AX phi
        witness.clear();
    }

    // all successor states satisfy phi: AX phi = true
    return true;
}

// LCOV_EXCL_START
void AXFormula::DEBUG_print()
{
    printf("AX(");
    phi->DEBUG_print();
    printf(")");
}
// LCOV_EXCL_STOP

void AXFormula::gatherPayloadInformation(arrayindex_t &numDFS, arrayindex_t &numCachedResults)
{
    phi->gatherPayloadInformation(numDFS, numCachedResults);
}

void AXFormula::setPayloadInformation(arrayindex_t cachedResultOffset, size_t payloadSize)
{
    phi->setPayloadInformation(cachedResultOffset, payloadSize);
}

FormulaInfo *AXFormula::getInfo() const
{
    FormulaInfo *Info = new FormulaInfo();
    Info->tag = formula_ax;
    Info->cardChildren = 1;
    Info->ctlChildren = new CTLFormula * [1];
    Info->ctlChildren[0] = phi;
    return Info;
}

int AXFormula::countSubFormulas() const
{
    return 1 + phi->countSubFormulas();
}
