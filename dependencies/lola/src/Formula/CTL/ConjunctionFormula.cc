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

\brief Implementation of conjunctions for CTL.
*/

#include <Formula/CTL/CTLFormula.h>
#include <Formula/CTL/ConjunctionFormula.h>

/*!
\param _subs  a vector containing pointers to subformulae
\post allocates memory for the array subs
*/
ConjunctionFormula::ConjunctionFormula(std::vector<CTLFormula *> _subs)
    : subs(new CTLFormula*[_subs.size()]()), cardSubs(_subs.size())
{
    assert(subs);
    for (size_t i = 0; i < cardSubs; ++i)
    {
        assert(_subs[i]);
        subs[i] = _subs[i];
    }
}

void ConjunctionFormula::initAtomics(NetState &ns)
{
    for (arrayindex_t i = 0; i < cardSubs; i++)
    {
        subs[i]->initAtomics(ns);
    }
}

void ConjunctionFormula::updateAtomics(NetState &ns, arrayindex_t t)
{
    for (arrayindex_t i = 0; i < cardSubs; i++)
    {
        subs[i]->updateAtomics(ns, t);
    }
}

void ConjunctionFormula::revertAtomics(NetState &ns, arrayindex_t t)
{
    for (arrayindex_t i = 0; i < cardSubs; i++)
    {
        subs[i]->revertAtomics(ns, t);
    }
}

bool ConjunctionFormula::check(Store<void *> &s, NetState &ns,
                               Firelist &firelist, std::vector<int> &witness)
{
    // check every subformula
    for (arrayindex_t i = 0; i < cardSubs; i++)
    {
        if (not subs[i]->check(s, ns, firelist, witness))
        {
            // we found an unsatisfied subformula -> conjunction is false
            return false;
        }

        // the current subformula is true - it does not provide information for
        // a counterexample
        witness.clear();
    }

    // we did not find a counterexample -> conjunction is true
    return true;
}

// LCOV_EXCL_START
void ConjunctionFormula::DEBUG_print()
{
    printf("AND(");
    for (arrayindex_t i = 0; i < cardSubs; i++)
    {
        subs[i]->DEBUG_print();
    }
    printf(")");
}
// LCOV_EXCL_STOP

void ConjunctionFormula::gatherPayloadInformation(arrayindex_t &numDFS,
        arrayindex_t &numCachedResults)
{
    for (arrayindex_t i = 0; i < cardSubs; i++)
    {
        subs[i]->gatherPayloadInformation(numDFS, numCachedResults);
    }
}

void ConjunctionFormula::setPayloadInformation(arrayindex_t cachedResultOffset,
        size_t payloadSize)
{
    for (arrayindex_t i = 0; i < cardSubs; i++)
    {
        subs[i]->setPayloadInformation(cachedResultOffset, payloadSize);
    }
}

FormulaInfo *ConjunctionFormula::getInfo() const
{
    FormulaInfo *Info = new FormulaInfo();
    Info->tag = formula_and;
    Info->cardChildren = cardSubs;
    Info->ctlChildren = new CTLFormula * [cardSubs];
    for (arrayindex_t i = 0; i < cardSubs; i++)
    {
        Info->ctlChildren[i] = subs[i];
    }
    return Info;
}

int ConjunctionFormula::countSubFormulas() const
{
    int sum = 1; // one for root node
    for (arrayindex_t i = 0; i < cardSubs; i++)
    {
        sum += subs[i]->countSubFormulas();
    }
    return sum;
}

