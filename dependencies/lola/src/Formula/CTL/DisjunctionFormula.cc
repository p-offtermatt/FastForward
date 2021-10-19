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

\brief Implementation of disjunctions for CTL.
*/

#include <Formula/CTL/DisjunctionFormula.h>

/*!
\param _subs  a vector containing pointers to subformulae
\post allocates memory for the array subs
*/
DisjunctionFormula::DisjunctionFormula(std::vector<CTLFormula *> _subs)
    : subs(new CTLFormula*[_subs.size()]()), cardSubs(_subs.size())
{
    assert(subs);
    for (size_t i = 0; i < cardSubs; ++i)
    {
        assert(_subs[i]);
        subs[i] = _subs[i];
    }
}

void DisjunctionFormula::initAtomics(NetState &ns)
{
    for (arrayindex_t i = 0; i < cardSubs; i++)
    {
        subs[i]->initAtomics(ns);
    }
}

void DisjunctionFormula::updateAtomics(NetState &ns, arrayindex_t t)
{
    for (arrayindex_t i = 0; i < cardSubs; i++)
    {
        subs[i]->updateAtomics(ns, t);
    }
}

void DisjunctionFormula::revertAtomics(NetState &ns, arrayindex_t t)
{
    for (arrayindex_t i = 0; i < cardSubs; i++)
    {
        subs[i]->revertAtomics(ns, t);
    }
}

bool DisjunctionFormula::check(Store<void *> &s, NetState &ns,
                               Firelist &firelist, std::vector<int> &witness)
{
    // check every subformula
    for (arrayindex_t i = 0; i < cardSubs; i++)
    {
        if (subs[i]->check(s, ns, firelist, witness))
        {
            // we found a satisfied subformula -> conjunction is true
            return true;
        }

        // the current subformula is false - it does not provide information for
        // a witness path
        witness.clear();
    }

    // we did not find a witness -> disjunction is false
    return false;
}

// LCOV_EXCL_START
void DisjunctionFormula::DEBUG_print()
{
    printf("OR(");
    for (arrayindex_t i = 0; i < cardSubs; i++)
    {
        subs[i]->DEBUG_print();
    }
    printf(")");
}
// LCOV_EXCL_STOP

void DisjunctionFormula::gatherPayloadInformation(arrayindex_t &numDFS,
        arrayindex_t &numCachedResults)
{
    for (arrayindex_t i = 0; i < cardSubs; i++)
    {
        subs[i]->gatherPayloadInformation(numDFS, numCachedResults);
    }
}

void DisjunctionFormula::setPayloadInformation(arrayindex_t cachedResultOffset,
        size_t payloadSize)
{
    for (arrayindex_t i = 0; i < cardSubs; i++)
    {
        subs[i]->setPayloadInformation(cachedResultOffset, payloadSize);
    }
}

FormulaInfo *DisjunctionFormula::getInfo() const
{
    FormulaInfo *Info = new FormulaInfo();
    Info->tag = formula_or;
    Info->cardChildren = cardSubs;
    Info->ctlChildren = new CTLFormula * [cardSubs];
    for (arrayindex_t i = 0; i < cardSubs; i++)
    {
        Info->ctlChildren[i] = subs[i];
    }
    return Info;
}

int DisjunctionFormula::countSubFormulas() const
{
    int sum = 1; // count root node
    for (arrayindex_t i = 0; i < cardSubs; i++)
    {
        sum += subs[i]->countSubFormulas();
    }
    return sum;
}
