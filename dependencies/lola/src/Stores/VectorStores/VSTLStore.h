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
\author Niels, Christian Koch
\status new
*/

#pragma once

#include <Core/Dimensions.h>
#include <Stores/VectorStores/VectorStore.h>

/// prepended "V" to avoid name clash
template <typename T>
class VSTLStore : public VectorStore<T>
{
private:
    /// mutex
    pthread_rwlock_t rwlock;

    /// actual store
    std::map<std::vector<vectordata_t>, T> store;
    /// one cache for each thread
    std::vector<vectordata_t> *intermediate;

    /// whether we only have one thread
    bool singleThreaded;


public:
    /// constructor
    /// @param num_threads the maximum number of threads this store has to work with
    explicit VSTLStore(threadid_t num_threads);
    /// destructor
    virtual ~VSTLStore();

    /// searches for a vector and inserts if not found
    /// @param in vector to be seached for or inserted
    /// @param bitlen length of vector
    /// @param hash of current NetState
    /// @param payload pointer to be set to the place where the payload of this state will be held
    /// @param threadIndex the index of the thread that requests this call. Values will range from 0 to (number_of_threads - 1). Used to allow using thread-local auxiliary data structures without locking any variables.
    /// @param noinsert if set to true only a search is done
    /// @return true, if the marking was found in the store, otherwise false.
    virtual bool searchAndInsert(const vectordata_t *in, bitarrayindex_t bitlen, hash_t hash,
                                 T **payload,
                                 threadid_t threadIndex, bool noinsert = false);

    /// gets and removes a vector from the store
    /// @param out place where the returned vector will be written to
    /// @param threadIndex the index of the thread that requests this call.
    /// @return false, if the store was already empty, otherwise true
    virtual bool popVector(vectordata_t *&out, threadid_t threadIndex = 0);

    /// check if the store is empty
    /// @return true, if the store is empty
    virtual bool empty();
};

#include <Stores/VectorStores/VSTLStore.inc>

template <>
class VSTLStore<void> : public VectorStore<void>
{
private:
    /// mutex
    pthread_rwlock_t rwlock;

    /// actual store
    std::set<std::vector<vectordata_t> > store;
    /// one cache for each thread
    std::vector<vectordata_t> *intermediate;

    /// whether we only have one thread
    bool singleThreaded;


public:
    /// constructor
    /// @param num_threads the maximum number of threads this store has to work with
    explicit VSTLStore(threadid_t num_threads);
    /// destructor
    virtual ~VSTLStore();

    /// searches for a vector and inserts if not found
    /// @param in vector to be seached for or inserted
    /// @param bitlen length of vector
    /// @param hash of current NetState
    /// @param payload pointer to be set to the place where the payload of this state will be held
    /// @param threadIndex the index of the thread that requests this call. Values will range from 0 to (number_of_threads - 1). Used to allow using thread-local auxiliary data structures without locking any variables.
    /// @param noinsert if set to true only a search is done
    /// @return true, if the marking was found in the store, otherwise false.
    virtual bool searchAndInsert(const vectordata_t *in, bitarrayindex_t bitlen, hash_t hash,
                                 void **payload,
                                 threadid_t threadIndex, bool noinsert);

    /// gets and removes a vector from the store
    /// @param out place where the returned vector will be written to
    /// @param threadIndex the index of the thread that requests this call.
    /// @return false, if the store was already empty, otherwise true
    virtual bool popVector(vectordata_t *&out, threadid_t threadIndex = 0);

    /// check if the store is empty
    /// @return true, if the store is empty
    virtual bool empty();
};
