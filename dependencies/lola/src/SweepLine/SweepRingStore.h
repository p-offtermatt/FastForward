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
\author Harro
\status new

\brief Store for transient markings

Each front in the SweepLine method needs one array of stores for transient markings.
Since markings need to be retrieved from this array, the only possible encoding
for this array is the FullCopyEncoder. Multithreading is disregarded as at most
one thread can access one store at any time.
*/

#pragma once

#include <Core/Dimensions.h>
#include <Stores/VectorStores/HashingWrapperStore.h>
#include <Stores/VectorStores/PrefixTreeStore.h>
#include <Stores/VectorStores/VSTLStore.h>
#include <Stores/NetStateEncoder/NetStateEncoder.h>
#include <SweepLine/SweepListStore.h>

class FullCopyEncoder;
class NetState;

/*!
\brief Store for transient markings of the SweepLine method

Each front in the SweepLine method needs one array of stores for transient markings.
Since markings need to be retrieved from this array, the only possible encoding
for this array is the FullCopyEncoder.
*/
template <class T>
class SweepRingStore
{
public:
    /// constructor with size of the store and maximal positive progress
    SweepRingStore(arrayindex_t _size, arrayindex_t _front_offset, arrayindex_t _transient_offset,
                   FullCopyEncoder *fce, NetStateEncoder *nse, arrayindex_t _nr_of_threads = 1,
                   hash_t _nr_of_buckets = 1);
    /// destructor
    ~SweepRingStore();

    /// check if a state is in the store at progress offset, insert it if not
    bool searchAndInsert(NetState &ns, int32_t offset, T **payload, arrayindex_t thread);
    /// get a state at active progress and relocate it to swap or permanent storage
    bool popState(NetState &ns, arrayindex_t thread);
    /// advance the active progress by one
    bool advanceProgress();
    /// initialise the store with the persistent states
    void init(SweepListStore<T> *oldpstates, SweepListStore<T> *newpstates);
    /// delete all transient states and check if there aren't new persistent ones at all
    bool clear();
    /// check for a new persistent state in the bucket with the lowest progress value
    bool checkNewPersistent();
    /// check for a new persistent state in buckets with the lowest progress values
    int32_t checkNewPersistent(int32_t);
    /// check if the last inserted state was made persistent
    bool insertedIsNewPersistent(arrayindex_t thread);
    /// get the number of deleted transient states during the last progress advance
    int64_t getNumberOfDeletedStates();

    VectorStore<T> *createSweepStore();

    void printState(NetState &ns);
private:
    /// size of the store ring for transient states
    arrayindex_t size;
    /// maximal progress of a single transition
    arrayindex_t front_offset;
    /// store element for the current progress value
    arrayindex_t active;
    /// progress offset at which transient state are forgotten
    arrayindex_t transient_offset;
    /// maximal number of threads to access this store simultaneously
    threadid_t nr_of_threads;
    /// number of hash values (buckets per progress value and store mode)
    hash_t nr_of_buckets;
    /// size of the last deleted store bucket
    int64_t deleted_store_size;
    /// encoder for all transient and new persistent states
    FullCopyEncoder *fullencoder;
    /// encoder for old persistent states
    NetStateEncoder *sigpencoder;
    /// state counter for transient store
    int64_t *count;
    /// ring of stores for transient states
    VectorStore<T> **store;
    /// swap space for states with computed successors
    VectorStore<T> *samevalue;
    /// connectors from the store ring to the stores of old persistent states with the same progress values
    SweepListStore<T> **oldpersistent;
    /// connectors from the store ring to the stores of new persistent states with the same progress values
    SweepListStore<T> **newpersistent;
    /// flag indicating if the list of new persistent states is empty
    bool new_persistent_empty;
    /// flag indicating whether the last inserted state is persistent or transient
    bool *inserted_persistent;
};

#include <SweepLine/SweepRingStore.inc>
