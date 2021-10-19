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

\brief Global data for transition specific information

All data that describe attributes of transitions can be found here. General
information for a transition in its role as a node, ar contained in Node.*
*/

#pragma once

#include <Core/Dimensions.h>

class NetState;

/*!
 \brief collection of information related to transitions
 */
struct Transition
{
public:
    /// fairness specification
    static fairnessAssumption_t *Fairness;

    /// Activation status
    static bool *Enabled;

    /// number of enabled transitions
    static arrayindex_t CardEnabled;

    /// When transition i fires from m to m', DeltaHash[i] is hash(m') - hash(m). This value is independent of m and m'
    static hash_t *DeltaHash;

    /// That many places need to be changed when transition i fires CardDeltaT[PRE] for places that lose tokens, CardDeltaT[POST] for those that gain tokens
    static arrayindex_t *CardDeltaT[2];

    /// These are the places that change their marking when transition i fires DeltaT[PRE] for places that lose tokens, DeltaT[POST] for those that gain tokens
    static arrayindex_t **DeltaT[2];

    /// This is the amount of change
    static mult_t **MultDeltaT[2];

    /// After firing transition i, that many transitions must be checked for enabledness if they have been enabled before: \f$card(DeltaT[PRE]^\bullet)\f$
    static arrayindex_t *CardConflicting;

    /// After firing transition i, these transitions must be checked for enabledness if they have been enabled before: \f$(DeltaT[PRE]^\bullet)\f$
    static arrayindex_t **Conflicting;
    /// Determines whether a conflicting set is an original or just a reference to some other (identical) set. Important to avoid double-freeing the memory.
    static bool *ConflictingIsOriginal;

    /// After reverse firing transition i, that many transitions must be checked for enabledness if they have been enabled before: \f$card(DeltaT[POST]^\bullet)\f$
    static arrayindex_t *CardBackConflicting;

    /// After reverse firing transition i, these transitions must be checked for enabledness if they have been enabled before: \f$(DeltaT[POST]^\bullet)\f$
    static arrayindex_t **BackConflicting;
    /// Determines whether a conflicting set is an original or just a reference to some other (identical) set. Important to avoid double-freeing the memory.
    static bool *BackConflictingIsOriginal;

    /// The progress measure for the sweep line method
    static int32_t *ProgressMeasure;

    /// Check transition for activation
    static void checkEnabled(NetState &ns, arrayindex_t t);

    /// Check transition for activation
    /// wrapper function for the paring phase of the net, when no NetState object has been created yet.
    static void checkEnabled_Initial(arrayindex_t t);

    /// fire a transition
    static void fire(NetState &ns, arrayindex_t t);

    /// update enabledness of all transitions
    static void updateEnabled(NetState &ns, arrayindex_t t);

    /// fire a transition in reverse direction (for backtracking)
    static void backfire(NetState &ns, arrayindex_t t);

    /// update enabledness of all transitions after backfiring
    static void revertEnabled(NetState &ns, arrayindex_t t);

    /// Aufräumen der Knoten - Service für valgrind
    static void deleteTransitions();

    static bool isCycle(arrayindex_t t);
};
