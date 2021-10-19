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

#include <Core/Dimensions.h>
#include <Core/Runtime.h>
#include <Net/Marking.h>
#include <Net/Net.h>
#include <Net/Place.h>
#include <Stores/VectorStores/VBloomStore.h>

template<size_t SIZE>
VBloomStore<SIZE>::VBloomStore(threadid_t num_threads, size_t hashes) :
    hash_values(new uint32_t[hashes *num_threads]),
    hash_functions(hashes), filter(new std::bitset<SIZE>())
{
    pthread_mutex_init(&mutex, NULL);
}

template<size_t SIZE>
VBloomStore<SIZE>::~VBloomStore()
{
    delete filter;
    delete[] hash_values;
    pthread_mutex_destroy(&mutex);
}

template<size_t SIZE>
inline uint32_t VBloomStore<SIZE>::hash_sdbm(const vectordata_t *in, size_t len) const
{
    uint32_t hash = 0;

    while (len--)
    {
        hash = *(in++) + (hash << 6) + (hash << 16) - hash;
    }

    return hash;
}

template<size_t SIZE>
inline uint32_t VBloomStore<SIZE>::hash_fnv(const vectordata_t *in, size_t len) const
{
    const unsigned int fnv_prime = 0x811C9DC5;
    uint32_t hash = 0;

    while (len--)
    {
        hash *= fnv_prime;
        hash ^= *(in++);
    }

    return hash;
}

template<size_t SIZE>
bool VBloomStore<SIZE>::searchAndInsert(const vectordata_t *in, bitarrayindex_t bitlen, hash_t,
                                        void **, threadid_t threadIndex, bool noinsert)
{
    uint32_t *cur_hashes = hash_values + (threadIndex * hash_functions);
    size_t len = (bitlen + VECTOR_WIDTH - 1) / VECTOR_WIDTH;

    /*************************
     * calculate hash values *
     *************************/

    // the first two hash functions are given explicitly
    const uint32_t hash_0 = hash_sdbm(in, len);
    cur_hashes[0] = hash_0 % SIZE;

    const uint32_t hash_1 = hash_fnv(in, len);
    cur_hashes[1] = hash_1 % SIZE;

    // the other hash functions can be derived: h_i(x) = h_1(x) + i * h_2(x)
    for (size_t h = 2; h < hash_functions; ++h)
    {
        cur_hashes[h] = (hash_0 + h * hash_1) % SIZE;
    }


    /****************************************
     * check whether vector has been stored *
     ****************************************/

    for (size_t h = 0; h < hash_functions; ++h)
    {
        // found an unset bit -> vector is new and must be stored
        if ((*filter)[cur_hashes[h]] == 0)
        {
            if (noinsert)
            {
                return false;    // check if we want to insert at all
            }

            pthread_mutex_lock(&mutex); // some rare occasion where double-checked locking actually works
            if ((*filter)[cur_hashes[h]] == 0)
            {
                // set this and all subsequent bits to 1 (previous bits were already 1)
                while (h < hash_functions)
                {
                    (*filter)[cur_hashes[h++]] = 1;
                }
                pthread_mutex_unlock(&mutex);
                return false;
            }
            pthread_mutex_unlock(&mutex);
        }
    }

    // all bits were 1 -> vector was probably stored already
    return true;
}

// gets and removes a vector from the store
template<size_t SIZE>
bool VBloomStore<SIZE>::popVector(vectordata_t *&, threadid_t)
{
    RT::rep->status("this store cannot return states");
    RT::rep->abort(ERROR_COMMANDLINE);
    return false;
}

/*!
* \brief Check if the store is empty
* \return If the store is empty
*/
template<size_t SIZE>
bool VBloomStore<SIZE>::empty()
{
    RT::rep->status("this store cannot return states");
    RT::rep->abort(ERROR_COMMANDLINE);
    return false;
}
