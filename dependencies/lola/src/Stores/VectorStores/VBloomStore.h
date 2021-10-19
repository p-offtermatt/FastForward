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

#pragma once

#include <Core/Dimensions.h>
#include <Stores/VectorStores/VectorStore.h>

template<size_t SIZE>
class VBloomStore : public VectorStore<void>
{
private:
    inline uint32_t hash_sdbm(const vectordata_t *in, size_t len) const;
    inline uint32_t hash_fnv(const vectordata_t *in, size_t len) const;

    pthread_mutex_t mutex;

    uint32_t *hash_values;
    const size_t hash_functions;
    std::bitset<SIZE> *filter;

public:
    /// constructor
    explicit VBloomStore(threadid_t num_threads, size_t = 2);
    /// destructor
    ~VBloomStore();

    /// searches for a vector and inserts if not found
    /// @param in vector to be seached for or inserted
    /// @param bitlen length of vector
    /// @param hash of current NetState
    /// @param payload pointer to be set to the place where the payload of this state will be held
    /// @param threadIndex the index of the thread that requests this call. Values will range from 0 to (number_of_threads - 1). Used to allow using thread-local auxiliary data structures without locking any variables.
    /// @param noinsert if set to true only a search is done
    /// @return true, if the marking was found in the store, otherwise false.
    bool searchAndInsert(const vectordata_t *in, bitarrayindex_t bitlen, hash_t hash, void **payload,
                         threadid_t threadIndex, bool noinsert = false);

    /// gets and removes a vector from the store (yields an error for this store)
    /// @param out place where the returned vector will be written to
    /// @param threadIndex the index of the thread that requests this call.
    /// @return false, if the store was already empty, otherwise true
    virtual bool popVector(vectordata_t *&out, threadid_t threadIndex = 0);

    /// check if the store is empty (yields an error for this store)
    /// @return true, if the store is empty
    virtual bool empty();
};

#include <Stores/VectorStores/VBloomStore.inc>
