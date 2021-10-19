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
\author Gregor Behnke
\status new

\brief a meta-store to test, whether a new store works correctly
*/

#pragma once

#include <Core/Dimensions.h>
#include <Stores/Store.h>

template <typename T> class CompareStore : public Store<T>
{
public:
    /// creates new Compare-Store using the two given Stores
    CompareStore(Store<T> *correct, Store<T> *test, threadid_t _number_of_threads);

    /// frees both components
    ~CompareStore();

    bool searchAndInsert(NetState &ns, T **payload, threadid_t threadIndex, bool noinsert = false);

    /// gets and removes a state from the store
    /// @param ns NetState where the removed state will be written to
    /// @return false, if store was already empty
    virtual bool popState(NetState &ns);

private:
    /// correct store
    Store<T> *correctStore;
    /// store to be tested
    Store<T> *testStore;
};

#include <Stores/CompareStore.inc>
