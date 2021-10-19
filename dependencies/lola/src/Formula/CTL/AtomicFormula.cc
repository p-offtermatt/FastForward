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

\brief Implementation of a CTL atomic formulae (state predicate).
*/

#include <Formula/StatePredicate/StatePredicate.h>
#include <Exploration/StatePredicateProperty.h>
#include <Formula/CTL/AtomicFormula.h>

void AtomicFormula::initAtomics(NetState &ns)
{
    inner->initProperty(ns);
}

void AtomicFormula::updateAtomics(NetState &ns, arrayindex_t t)
{
    inner->checkProperty(ns, t);
}

void AtomicFormula::revertAtomics(NetState &ns, arrayindex_t t)
{
    inner->updateProperty(ns, t);
}

bool AtomicFormula::check(Store<void *> &, NetState &, Firelist &, std::vector<int> &)
{
    return inner->getPredicate()->value;
}

// LCOV_EXCL_START
void AtomicFormula::DEBUG_print()
{
    printf("ATOMIC");
}
// LCOV_EXCL_STOP

void AtomicFormula::gatherPayloadInformation(arrayindex_t &, arrayindex_t &)
{}

void AtomicFormula::setPayloadInformation(arrayindex_t, size_t)
{}

FormulaInfo *AtomicFormula::getInfo() const
{
    return (inner->getPredicate())->getInfo();
}


int AtomicFormula::countSubFormulas() const
{
    return (inner->getPredicate())->countSubFormulas();
}
