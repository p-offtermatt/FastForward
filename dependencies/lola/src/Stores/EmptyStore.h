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
\author Niels
\author Max Görner
\author Christian Koch
\status new

\brief Store implementation that is always empty. All calls to searchAndInsert() will return false.
*/

#pragma once

#include <Core/Dimensions.h>
#include <Stores/Store.h>

template <typename T>
class EmptyStore : public Store<T>
{
public:
    int tries;

    explicit EmptyStore(threadid_t number_of_threads);
    bool searchAndInsert(NetState &ns, T **payload, threadid_t thread, bool noinsert = false);
};

template <typename T>
EmptyStore<T>::EmptyStore(threadid_t number_of_threads) : Store<T>(number_of_threads), tries(0)
{
}

template <typename T>
inline bool EmptyStore<T>::searchAndInsert(NetState &, T **payload,
        threadid_t thread, bool)
{
    if (payload)
    {
        *payload = NULL;
    }
    ++this->calls[thread]; //We're using "this" since access to protected attributes is harder when using templates than when not.
    return false;
}
