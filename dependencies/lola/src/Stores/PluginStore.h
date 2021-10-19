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
NetStateEncoder, that converts the current state into an input vector, and a
VectorStore, that stores the input vectors and checks whether they are already
known. Implementations for both components can be combined arbitrarily.
Specific components may have some functional or compatibility limitations,
though.
*/

#pragma once

#include <Core/Dimensions.h>
#include <Stores/NetStateEncoder/NetStateEncoder.h>
#include <Stores/Store.h>
#include <Stores/VectorStores/VectorStore.h>

template <typename T> class PluginStore : public Store<T>
{
public:
    /// creates new Store using the specified components. The given components are assumed to be used exclusively and are freed once the PluginStore gets destructed.
    PluginStore(NetStateEncoder *_netStateEncoder, VectorStore<T> *_vectorStore,
                threadid_t _number_of_threads);

    /// frees both components
    ~PluginStore();

    bool searchAndInsert(NetState &ns, T **payload, threadid_t threadIndex, bool noinsert = false);

    /// gets and removes a state from the store
    /// @param ns NetState where the removed state will be written to
    /// @return false, if store was already empty
    virtual bool popState(NetState &ns);

private:
    /// used NetStateEncoder (given in constructor)
    NetStateEncoder *netStateEncoder;
    /// used VectorStore (given in constructor)
    VectorStore<T> *vectorStore;
};

#include <Stores/PluginStore.inc>
