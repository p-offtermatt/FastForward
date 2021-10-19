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

\brief VectorStore implementation using binary suffix trees. Based on BinStore.
Relies on the assumption that different input vectors (possibly of different
length) are not prefix of another.
*/

#pragma once

#include <Core/Dimensions.h>
#include <Stores/VectorStores/VectorStore.h>

/// A VectorStoreCreator encapsulates a method that, when invoked, creates a new VectorStore.
/// It is used by the HashingWrapperStore to create VectorStores for new hash buckets on demand.
template<typename P>
class VectorStoreCreator
{
public:
    virtual VectorStore<P> *operator() (void) const = 0;
    virtual ~VectorStoreCreator() {}
};

/// VectorStoreCreator implementation template for VectorStores with zero constructor arguments
template<typename P, typename T>
class NullaryVectorStoreCreator : public VectorStoreCreator<P>
{
public:
    VectorStore<P> *operator() (void) const
    {
        return new T();
    }
};

/// VectorStoreCreator implementation template for VectorStores with one constructor argument
template<typename P, typename T, typename A1>
class UnaryVectorStoreCreator : public VectorStoreCreator<P>
{
    A1 arg1;
public:
    explicit UnaryVectorStoreCreator(A1 _arg1)
    {
        arg1 = _arg1;
    }
    VectorStore<P> *operator() (void) const
    {
        return new T(arg1);
    }
};

/// VectorStoreCreator implementation template for VectorStores with two constructor arguments
template<typename P, typename T, typename A1, typename A2>
class BinaryVectorStoreCreator : public VectorStoreCreator<P>
{
    A1 arg1;
    A2 arg2;
public:
    BinaryVectorStoreCreator(A1 _arg1, A2 _arg2)
    {
        arg1 = _arg1;
        arg2 = _arg2;
    }
    VectorStore<P> *operator() (void) const
    {
        return new T(arg1, arg2);
    }
};


/// A HashingWrapperStore provides a hash based bucketing mechanism and can be combined with any underlying VectorStore.
/// It creates a fixed number of buckets, each with its own VectorStore, and redirects the incoming searchAndInsert calls to one of them depending on the hash value of the given NetState.
template <typename T>
class HashingWrapperStore : public VectorStore<T>
{
public:
    /// constructor (optional parameter _number_of_threads necessary if elements should be retrievable by popVector())
    HashingWrapperStore(VectorStoreCreator<T> *_storeCreator,
                        arrayindex_t _number_of_buckets = SIZEOF_MARKINGTABLE, threadid_t _number_of_threads = 1);
    /// destructor
    virtual ~HashingWrapperStore();

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

    /// return the hash value of the last marking returned by popVector()
    virtual hash_t getLastHash(threadid_t threadIndex = 0);

private:
    VectorStore<T> **buckets;

    arrayindex_t *currentPopBucket;
    arrayindex_t number_of_buckets;
};

#include <Stores/VectorStores/HashingWrapperStore.inc>
