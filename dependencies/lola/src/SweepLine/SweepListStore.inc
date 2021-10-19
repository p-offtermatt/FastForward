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

/*
\file
\author Harro
\status new

\brief List of Stores for persistent markings in the SweepLine method

This class realizes one store in a list of stores, sorted by progress
measure and containing known persistent markings, either old or new ones.
*/

#include <Core/Dimensions.h>
#include <Core/Runtime.h>
#include <SweepLine/SweepListStore.h>
#include <Planning/Task.h>

/*!
 * \brief Constructor for one store in a list of stores
 */
template <class T>
SweepListStore<T>::SweepListStore(arrayindex_t _nr_of_threads, hash_t _nr_of_buckets)
    : next(NULL), nr_of_threads(_nr_of_threads), nr_of_buckets(_nr_of_buckets)
{
    store = createSweepStore();
}

/*!
 * \brief Constructor for a list of stores, returns pointer to the first store
 * \param size The number of stores in the list
 */
template <class T>
SweepListStore<T>::SweepListStore(arrayindex_t _nr_of_threads, hash_t _nr_of_buckets,
                                  arrayindex_t size)
    : next(size > 0 ? new SweepListStore(_nr_of_threads, _nr_of_buckets, size - 1) : NULL),
      nr_of_threads(_nr_of_threads), nr_of_buckets(_nr_of_buckets)
{
    store = createSweepStore();
}

/*!
 * \brief Destructor for the list starting at this store
 */
template <class T>
SweepListStore<T>::~SweepListStore()
{
    delete store;
    delete next;
}

/*!
 * \brief Check if there exists a store after this one in the list
 * \return The next store if it exists, NULL otherwise
 */
template <class T>
SweepListStore<T> *SweepListStore<T>::checkNext()
{
    return next;
}

/*!
 * \brief Check if there exists a store after this one in the list
 *        and create one if it does not exist
 * \return The next store in the list, possibly just created
 */
template <class T>
SweepListStore<T> *SweepListStore<T>::getNext()
{
    if (!next)
    {
        next = new SweepListStore<T>(nr_of_threads, nr_of_buckets);
    }
    return next;
}

/*!
 * \brief Manually set the next store in the list
 * \param sls The store that should be the next after this one
 */
template <class T>
void SweepListStore<T>::setNext(SweepListStore<T> *sls)
{
    next = sls;
}

template <class T>
VectorStore<T> *SweepListStore<T>::createSweepStore()
{
    switch (RT::args.store_arg)
    {
    case store_arg_prefix:
        if (nr_of_buckets > 1)
        {
            return new HashingWrapperStore<T>(new UnaryVectorStoreCreator<T, PrefixTreeStore<T>, arrayindex_t>
                                              (nr_of_threads), nr_of_buckets, nr_of_threads);
        }
        else
        {
            return new PrefixTreeStore<T>(nr_of_threads);
        }
    case store_arg_stl:
        /*
                    if (nr_of_buckets>1)
        				return new PluginStore<T>(enc, new HashingWrapperStore<T>(new UnaryVectorStoreCreator<T,VSTLStore<T>,arrayindex_t>(number_of_threads)), number_of_threads);
                    else
        */
        return new VSTLStore<T>(nr_of_threads);
    default:
        RT::rep->message(RT::rep->markup(MARKUP_BAD, "specified store does not fit the given task").str());
        RT::rep->abort(ERROR_COMMANDLINE);
        // LCOV_EXCL_START
        return NULL;
        // LCOV_EXCL_STOP
    }
}

