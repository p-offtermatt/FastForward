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
\status new

\brief A wrapper for the cycle reduction that internally uses another store and
only manipulates the searchAndInsert function.
*/

#pragma once

#include <Core/Runtime.h>
#include <Net/LinearAlgebra.h>
#include <Net/Net.h>
#include <Net/Transition.h>
#include <Stores/Store.h>

template <typename T>
class CycleStore : public Store<T>
{
public:
    ~CycleStore();
    CycleStore(threadid_t number_of_threads, Store<T> *actualStore, int k);
    bool searchAndInsert(NetState &ns, T **payload, threadid_t thread, bool noinsert = false);
    bool empty();

private:
    /// the actual store
    Store<T> *actualStore;

    /// an array storing which transitions cover the cycles
    bool *u;

    /// a heuristic parameter
    int k;
};

template <typename T>
CycleStore<T>::~CycleStore()
{
    delete[] u;
    delete actualStore;
}

template <typename T>
CycleStore<T>::CycleStore(threadid_t number_of_threads, Store<T> *actualStore, int k) :
    Store<T>(number_of_threads), actualStore(actualStore),
    u(new bool[Net::Card[TR]]), k(k)
{
    // required to avoid an assertion error (TODO: Why?)
    Net::sortAllArcs();

    // get and reduce incidence matrix (transitions are lines)
    Matrix m = Net::getIncidenceMatrix(TR);
    m.reduce();

    size_t count = 0;
    for (arrayindex_t i = 0; i < Net::Card[TR]; i++)
    {
        u[i] = !m.isSignificant(i);
        if (u[i])
        {
            ++count;
        }
    }
    RT::rep->status("found %d transitions to cover the cycles", count);
    RT::rep->status("cycle heuristic: %d (%s)", k, RT::rep->markup(MARKUP_PARAMETER,
                    "--cycleheuristic").str());

    // to avoid two concurrent reporters, silence one
    actualStore->silence();
}

template <typename T>
inline bool CycleStore<T>::searchAndInsert(NetState &ns, T **payload, threadid_t thread,
        bool noinsert)
{
    // count calls
    ++(this->calls[thread]);

    // check whether state should be saved
    noinsert = true;

    for (arrayindex_t t = 0; t < Net::Card[TR]; ++t)
    {
        if (u[t] and ns.Enabled[t])
        {
            noinsert = false;
            break;
        }
    }

    const bool ret = actualStore->searchAndInsert(ns, payload, thread, noinsert);
    if (!ret && !noinsert)
    {
        ++(this->markings[thread]);
    }

    return ret;
}

template <typename T>
inline bool CycleStore<T>::empty()
{
    return actualStore->empty();
}
