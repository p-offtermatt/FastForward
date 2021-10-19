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

\brief Implementation of negations for CTL.
*/

#include <Formula/CTL/NotFormula.h>

NotFormula::NotFormula(CTLFormula *inner) : inner(inner)
{
    assert(inner);
}

void NotFormula::initAtomics(NetState &ns)
{
    inner->initAtomics(ns);
}

void NotFormula::updateAtomics(NetState &ns, arrayindex_t t)
{
    inner->updateAtomics(ns, t);
}

void NotFormula::revertAtomics(NetState &ns, arrayindex_t t)
{
    inner->revertAtomics(ns, t);
}

bool NotFormula::check(Store<void *> &s, NetState &ns, Firelist &firelist,
                       std::vector<int> &witness)
{
    return not inner->check(s, ns, firelist, witness);
}

// LCOV_EXCL_START
void NotFormula::DEBUG_print()
{
    printf("NOT(");
    inner->DEBUG_print();
    printf(")");
}
// LCOV_EXCL_STOP

void NotFormula::gatherPayloadInformation(arrayindex_t &numDFS, arrayindex_t &numCachedResults)
{
    inner->gatherPayloadInformation(numDFS, numCachedResults);
}

void NotFormula::setPayloadInformation(arrayindex_t cachedResultOffset, size_t payloadSize)
{
    inner->setPayloadInformation(cachedResultOffset, payloadSize);
}

FormulaInfo *NotFormula::getInfo() const
{
    FormulaInfo *Info = new FormulaInfo();
    Info->tag = formula_not;
    Info->cardChildren = 1;
    Info->ctlChildren = new CTLFormula * [1];
    Info->ctlChildren[0] = inner;
    return Info;
}

int NotFormula::countSubFormulas() const
{
    return 1 + inner->countSubFormulas();
}
