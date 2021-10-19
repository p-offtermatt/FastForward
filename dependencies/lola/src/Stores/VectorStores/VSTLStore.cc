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

#include <Core/Dimensions.h>
#include <Net/Marking.h>
#include <Net/Net.h>
#include <Net/Place.h>
#include <Stores/VectorStores/VSTLStore.h>

VSTLStore<void>::VSTLStore(threadid_t num_threads)
{
    singleThreaded = (num_threads == 1);
    intermediate = new std::vector<vectordata_t>[num_threads];
    pthread_rwlock_init(&rwlock, NULL);
}

VSTLStore<void>::~VSTLStore()
{
    pthread_rwlock_destroy(&rwlock);
    delete[] intermediate;
}

bool VSTLStore<void>::searchAndInsert(const vectordata_t *in, bitarrayindex_t bitlen,
                                      hash_t, void **, threadid_t threadIndex, bool noinsert)
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

    // avoid locking overhead if working in single-threaded mode
    if (singleThreaded)
    {
        if (noinsert)
        {
            return store.count(ci);
        }
        else
        {
            return !store.insert(ci).second;
        }
    }

    // test if already inserted
    pthread_rwlock_rdlock(&rwlock);
    int found = store.count(ci);
    pthread_rwlock_unlock(&rwlock);
    if (found)
    {
        return true;
    }
    else if (noinsert)
    {
        return false;
    }

    // add vector to marking store
    pthread_rwlock_wrlock(&rwlock);
    bool ret = !store.insert(ci).second;
    pthread_rwlock_unlock(&rwlock);

    return ret;
}

/*!
 * \brief  gets and removes a vector from the store
 */
bool VSTLStore<void>::popVector(vectordata_t *&out, threadid_t threadIndex)
{
    pthread_rwlock_wrlock(&rwlock);

    // get result
    const bool result = !store.empty();

    // when store not empty, set out pointer to vector of this state
    if (result)
    {
        std::vector<vectordata_t> &ci = intermediate[threadIndex] = *store.begin(); // get first vector
        store.erase(store.begin()); // remove this vector from the store
        out = &ci[0]; // return underlying array
    }

    pthread_rwlock_unlock(&rwlock);

    return result;
}

/*!
 * \brief Check if the store is empty
 * \return If the store is empty
 */
bool VSTLStore<void>::empty()
{
    // lock the store to ensure thread safety
    pthread_rwlock_rdlock(&rwlock);
    if (store.empty())
    {
        pthread_rwlock_unlock(&rwlock);
        return true;
    }
    else
    {
        pthread_rwlock_unlock(&rwlock);
        return false;
    }
}
