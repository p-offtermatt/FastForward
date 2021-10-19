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

#include <Net/Marking.h>
#include <Net/Net.h>
#include <Net/Place.h>
#include <Stores/VectorStores/VSTLStore.h>

template <typename T>
VSTLStore<T>::VSTLStore(threadid_t num_threads)
{
    singleThreaded = (num_threads == 1);
    intermediate = new std::vector<vectordata_t>[num_threads];
    pthread_rwlock_init(&rwlock, NULL);
}

template <typename T>
VSTLStore<T>::~VSTLStore()
{
    pthread_rwlock_destroy(&rwlock);
    delete[] intermediate;
}

template <typename T>
bool VSTLStore<T>::searchAndInsert(const vectordata_t *in, bitarrayindex_t bitlen, hash_t,
                                   T **payload, threadid_t threadIndex, bool noinsert)
{
    arrayindex_t vectorLength = (bitlen + (VECTOR_WIDTH - 1)) / VECTOR_WIDTH;

    std::vector<vectordata_t> &ci = intermediate[threadIndex];

    ci.resize(vectorLength);
    // copy current marking to vector
    std::copy(in, in + vectorLength, ci.begin());

    // shift away unused bits to make sure they are zero
    bitlen -= vectorLength * VECTOR_WIDTH;
    if (bitlen)
    {
        (ci[vectorLength - 1] >>= (VECTOR_WIDTH - bitlen)) <<= (VECTOR_WIDTH - bitlen);
    }

    int ret;
    // avoid locking overhead if working in single-threaded mode
    if (singleThreaded)
    {
        ret = store.count(ci);
        if (!noinsert)
        {
            *payload = &store[ci];
        }
        return ret;
    }

    // test if already inserted
    pthread_rwlock_rdlock(&rwlock);
    ret = store.count(ci);
    pthread_rwlock_unlock(&rwlock);
    if (ret)
    {
        return true;
    }
    else if (noinsert)
    {
        return false;
    }

    // add vector to marking store
    pthread_rwlock_wrlock(&rwlock);
    ret = store.count(ci);
    *payload = &store[ci];
    pthread_rwlock_unlock(&rwlock);

    return ret;
}

/*!
 * \brief  gets and removes a vector from the store
 */
template <typename T>
bool VSTLStore<T>::popVector(vectordata_t *&out, threadid_t threadIndex)
{
    pthread_rwlock_wrlock(&rwlock);

    // get result
    const bool result = not store.empty();

    // when store not empty, set out pointer to vector of this state
    if (result)
    {
        __typeof(store.begin()) it = store.begin(); // get iterator of first element
        std::vector<vectordata_t> &ci = intermediate[threadIndex] = it->first; // get its vector
        store.erase(it); // remove this iterator from the store
        out = &ci[0]; // return underlying array
    }

    pthread_rwlock_unlock(&rwlock);

    return result;
}

/*!
 * \brief Check if the store is empty
 * \note This function does not ensure thread safety
 * \return If the store is empty
 */
template <typename T>
bool VSTLStore<T>::empty()
{
    return store.empty();
}
