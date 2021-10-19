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
\author Harro
\status new

\brief List of Stores for persistent markings in the SweepLine method

This class realizes one store in a list of stores, sorted by progress
measure and containing known persistent markings, either old or new ones.
*/

#pragma once

#include <Core/Dimensions.h>
#include <Stores/VectorStores/PrefixTreeStore.h>
#include <Stores/VectorStores/VSTLStore.h>
#include <Stores/VectorStores/HashingWrapperStore.h>

/*!
\brief List of Stores for persistent markings in the SweepLine method

This class realizes one store in a list of stores, sorted by progress
measure and containing known persistent markings, either old or new ones.
*/
template <class T>
class SweepListStore
{
public:
    /// constructor for a single element of a list
    SweepListStore(arrayindex_t nr_of_threads = 1, hash_t nr_of_buckets = 1);

    /// constructor for a list of stores of given length
    SweepListStore(arrayindex_t nr_of_threads, hash_t nr_of_buckets, arrayindex_t size);

    /// destructor
    ~SweepListStore();

    /// check if there is a next store in the list and return it (or NULL)
    SweepListStore<T> *checkNext();

    /// check for and possibly create a next store in the list
    SweepListStore<T> *getNext();

    /// set the next store in the list
    void setNext(SweepListStore<T> *sls);

    VectorStore<T> *createSweepStore();

    /// the actual store for this list element
    VectorStore<T> *store;

private:
    /// the next store in the list
    SweepListStore<T> *next;

    /// maximal number of threads to access this store simultaneously
    threadid_t nr_of_threads;

    /// number of hash values (buckets per progress value and store mode)
    hash_t nr_of_buckets;
};

#include <SweepLine/SweepListStore.inc>
