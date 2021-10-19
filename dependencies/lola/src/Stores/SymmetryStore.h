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
\author Karsten
\status new

\brief A wrapper for the symmetry reduction that transforms a marking into
its canonical representative and passes it to another store.
*/

#pragma once

#include <Core/Runtime.h>
#include <Net/Net.h>
#include <Net/Transition.h>
#include <Stores/Store.h>
#include <Symmetry/Symmetry.h>


template <typename T>
class SymmetryStore : public Store<T>
{
public:
    ~SymmetryStore();
    SymmetryStore(threadid_t number_of_threads, Store<T> *actualStore);
    bool searchAndInsert(NetState &ns, T **payload, threadid_t thread, bool noinsert = false);
    bool empty();
    Store<T> * setGeneratingSet(GeneratingSet * GG){ G = GG; if(!G || G->knownGenerators == 0)
	{
		return actualStore;
	}
	else
	{
		return this;
	}
    }

private:
    /// the actual store
    Store<T> *actualStore;

    /// The generating set of symmetries to be used
    GeneratingSet *G;
};

template <typename T>
SymmetryStore<T>::~SymmetryStore()
{
    delete G;
    delete actualStore;
}

template <typename T>
SymmetryStore<T>::SymmetryStore(threadid_t number_of_threads, Store<T> *actualStore):
    Store<T>(number_of_threads), actualStore(actualStore)
{
    // required to avoid an assertion error (TODO: Why?)
    Net::sortAllArcs();

    // to avoid two concurrent reporters, silence one
    this->silence();
}

template <typename T>
inline bool SymmetryStore<T>::searchAndInsert(NetState &ns, T **payload, threadid_t thread,
        bool noinsert)
{
    // the statistics are queried here, so we need to keep them here as well
    ++(this->calls[thread]);

    // canonize
    capacity_t *canrep = new capacity_t [Net::Card[PL]];
    // make a copy of current marking - TODO: this can be faster when we use memcpy
    for (arrayindex_t i = 0; i < Net::Card[PL]; i++)
    {
        canrep[i] = ns.Current[i];
    }

    canrep = G->canonize(canrep);
    capacity_t *rememberthemilk = ns.Current;
    ns.Current = canrep; // pretend that canrep is current marking

    // pass on to actual store
    const bool ret = actualStore->searchAndInsert(ns, payload, thread, noinsert);

    // reset current marking
    delete[] ns.Current;
    ns.Current = rememberthemilk;

    // finish (statistics)
    if (!ret && !noinsert)
    {
        ++(this->markings[thread]);
    }

    return ret;
}

template <typename T>
inline bool SymmetryStore<T>::empty()
{
    return actualStore->empty();
}
