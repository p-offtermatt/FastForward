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

\brief Implementation of the CTL EX operator.
*/

#include <Exploration/Firelist.h>
#include <Formula/CTL/EXFormula.h>
#include <Net/Transition.h>

/*!
\param phi  the inner formula for EX phi
*/
EXFormula::EXFormula(CTLFormula *phi) : phi(phi)
{
    assert(phi);
}

void EXFormula::initAtomics(NetState &ns)
{
    phi->initAtomics(ns);
}

void EXFormula::updateAtomics(NetState &ns, arrayindex_t t)
{
    phi->updateAtomics(ns, t);
}

void EXFormula::revertAtomics(NetState &ns, arrayindex_t t)
{
    phi->revertAtomics(ns, t);
}

/*!
\param s  the store to use
\param ns  the current state of the net
\param firelist  a list of activated transitions (possibly only a subset)
\param[in,out] witness  a witness path to which we may add transitions

\post In case the function returns true, witness holds a witness for EX phi,
consisting of a transition that leads to a state that satisfies phi.
*/
bool EXFormula::check(Store<void *> &s, NetState &ns, Firelist &firelist,
                      std::vector<int> &witness)
{
    // get a list of transitions to fire
    arrayindex_t *fl;
    arrayindex_t cardfl = firelist.getFirelist(ns, &fl);

    // fire each transition and check whether a successor state satisfies phi
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

        if (result)
        {
            // the successor state satisfies phi: EX phi = true - add the
            // current transition to the witness and return
            witness.push_back(currentTransition);
            return true;
        }

        // the successor state does not satisfies phi: continue - we have not
        // yet found a witness for EX phi
        witness.clear();
    }

    // no successor state satisfies phi: EX phi = false
    return false;
}

// LCOV_EXCL_START
void EXFormula::DEBUG_print()
{
    printf("EX(");
    phi->DEBUG_print();
    printf(")");
}
// LCOV_EXCL_STOP

void EXFormula::gatherPayloadInformation(arrayindex_t &numDFS, arrayindex_t &numCachedResults)
{
    phi->gatherPayloadInformation(numDFS, numCachedResults);
}

void EXFormula::setPayloadInformation(arrayindex_t cachedResultOffset, size_t payloadSize)
{
    phi->setPayloadInformation(cachedResultOffset, payloadSize);
}

FormulaInfo *EXFormula::getInfo() const
{
    FormulaInfo *Info = new FormulaInfo();
    Info->tag = formula_ex;
    Info->cardChildren = 1;
    Info->ctlChildren = new CTLFormula * [1];
    Info->ctlChildren[0] = phi;
    return Info;
}

int EXFormula::countSubFormulas() const
{
    return 1 + phi->countSubFormulas();
}
