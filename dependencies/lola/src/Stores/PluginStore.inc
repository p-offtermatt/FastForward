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

\brief general-purpose base store consisting of two components: a
NetStateEncoder, that converts the current state into a input vector, and a
VectorStore, that stores the input vectors and checks whether they are already
known. Implementations for both components can be combined arbitrarily.
Specific components may have some functional or compatibility limitations,
though.
*/

#include <Core/Dimensions.h>
#include <Net/Marking.h>
#include <Net/Net.h>
#include <Net/Place.h>

template<typename T>
PluginStore<T>::PluginStore(NetStateEncoder *_netStateEncoder, VectorStore<T> *_vectorStore,
                            threadid_t _number_of_threads)
    : Store<T>(_number_of_threads)
{
    netStateEncoder = _netStateEncoder;
    vectorStore = _vectorStore;
}

template<typename T>
PluginStore<T>::~PluginStore()
{
    delete netStateEncoder;
    delete vectorStore;
}

template<typename T>
bool PluginStore<T>::searchAndInsert(NetState &ns, T **payload, threadid_t threadIndex,
                                     bool)
{
    // count calls
    ++(this->calls[threadIndex]);

    // fetch input vector
    bitarrayindex_t bitlen;
    vectordata_t *input = netStateEncoder->encodeNetState(ns, bitlen, threadIndex);

    // check input vector in vector store
    bool ret = vectorStore->searchAndInsert(input, bitlen, ns.HashCurrent, payload, threadIndex);
    if (!ret)
    {
        ++(this->markings[threadIndex]);
    }
    return ret;
}

/*!
 * \brief gets and removes a state from the store
 * \param ns NetState where the removed state will be written to
 * \return false, if store was already empty
 */
template<typename T>
bool PluginStore<T>::popState(NetState &ns)
{
    // pointer to be set to popped vector
    vectordata_t *vec = NULL;

    // try to get a vector from the store
    const bool result = vectorStore->popVector(vec);
    if (result)
    {
        // decode vector to state and write to ns
        netStateEncoder->decodeNetState(ns, vec);
    }
    return result;
}
