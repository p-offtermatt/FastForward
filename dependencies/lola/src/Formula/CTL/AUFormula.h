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

\brief Definition of the CTL AU operator.
*/

#pragma once

#include <Core/Dimensions.h>
#include <Formula/CTL/DFSFormula.h>

class AUFormula : public DFSFormula
{
public:
    AUFormula(CTLFormula *phi, CTLFormula *psi);
    /// returns info on a particular node in the formula tree
    FormulaInfo *getInfo() const;
    /// returns the number of subformulas
    int countSubFormulas() const;

private:
    void initAtomics(NetState &ns);
    void updateAtomics(NetState &ns, arrayindex_t t);
    void revertAtomics(NetState &ns, arrayindex_t t);

    /// check whether A(phi U psi) holds
    bool check(Store<void *> &s, NetState &ns, Firelist &firelist,
               std::vector<int> &witness);

    void DEBUG_print();

    void gatherPayloadInformation(arrayindex_t &numDFS, arrayindex_t &numCachedResults);
    void setPayloadInformation(arrayindex_t cachedResultOffset, size_t payloadSize);

    bool getFairWitness(Store<void *> &s, NetState &ns, Firelist &firelist,
                        std::vector<int> &witness, fairness_data &fairness);
    bool subdivideFairnessCheck(Store<void *> &s, NetState &ns,
                                Firelist &firelist, std::vector<int> &witness,
                                fairness_data &fairness);
    bool fairSCC(Store<void *> &s, NetState &ns, Firelist &firelist,
                 std::vector<int> &witness, fairness_data &fairness);
    void constructWitness(Store<void *> &s, NetState &ns, Firelist &firelist,
                          std::vector<int> &witness, fairness_data &fairness,
                          bool *enabled_strong);
    void produceWitness(Store<void *> &s, NetState &ns, Firelist &firelist,
                        std::vector<int> &witness, fairness_data &fairness,
                        void *initialPayload, statenumber_t initialDFS,
                        bool *fulfilled_weak, bool *fulfilled_strong,
                        arrayindex_t initial_fulfilled_conditions);
    void findWitnessPathTo(Store<void *> &s, NetState &ns, Firelist &firelist,
                           std::vector<int> &witness, void *destinationPayload,
                           statenumber_t initialDFS, statenumber_t myDFS);

    /// the first inner formula for A(phi U psi)
    CTLFormula *phi;
    /// the second inner formula for A(phi U psi)
    CTLFormula *psi;
};
