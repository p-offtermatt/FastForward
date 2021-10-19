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

\brief VectorStore implementation using binary prefix trees. Based on BinStore.
Relies on the assumption that different input vectors (possibly of different
length) are not prefix of another.
*/

#pragma once

#include <Core/Dimensions.h>
#include <Stores/VectorStores/VectorStore.h>

template <typename T>
class PrefixTreeStore : public VectorStore<T>
{
public:
    /// constructor (parameter needed only if elements can be retrieved from the store)
    explicit PrefixTreeStore(threadid_t number_of_threads = 1);
    /// destructor
    virtual ~PrefixTreeStore();

    /// searches for a vector and inserts if not found
    /// @param in vector to be seached for or inserted
    /// @param bitlen length of vector
    /// @param payload pointer to be set to the place where the payload of this state will be held
    /// @param noinsert if set to true only a search is done
    /// @return true, if the marking was found in the store, otherwise false.
    virtual bool searchAndInsert(const vectordata_t *in, bitarrayindex_t bitlen, hash_t, T **payload,
                                 threadid_t, bool noinsert = false);

    /// gets and removes a vector from the store
    /// @param out place where the returned vector will be written to
    /// @param threadIndex the index of the thread that requests this call.
    /// @return false, if the store was already empty, otherwise true
    virtual bool popVector(vectordata_t *&out, threadid_t threadIndex = 0);

    /// check if the store is empty
    /// @return true, if the store is empty
    virtual bool empty();

private:
    size_t getPayloadSize();

    /// a binary decision node
    class Decision
    {
    public:
        /// constructor
        /// @param b index of first bit different from previous vector
        explicit Decision(bitarrayindex_t b);
        /// index of first bit different from previous vector
        bitarrayindex_t bit;
        /// remaining vector after first differing bit
        vectordata_t *vector;
        /// decision nodes differing later than this one
        Decision *nextold;
        /// decision nodes differing from the remaining vector of this node
        Decision *nextnew;
        /// destructor
        ~Decision();
    };

    // first branch in decision tree; NULL as long as less than two elements
    Decision *branch;

    // the read-write mutex
    pthread_rwlock_t rwlock;

    // first vector; null as long as empty
    vectordata_t *firstvector;

    // full vector will be reconstructed here when popping vectors
    vectordata_t **popVectorCache;

    threadid_t number_of_threads;
};

// forward declaration to register function specialization
template<>
size_t PrefixTreeStore<void>::getPayloadSize();


#include <Stores/VectorStores/PrefixTreeStore.inc>
