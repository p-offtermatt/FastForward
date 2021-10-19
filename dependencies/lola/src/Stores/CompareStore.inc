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

#include <Core/Dimensions.h>
#include <Net/Marking.h>
#include <Net/Net.h>
#include <Net/Place.h>

template<typename T>
CompareStore<T>::CompareStore(Store<T> *correct, Store<T> *test,
                              threadid_t _number_of_threads) : Store<T>(_number_of_threads)
{
    correctStore = correct;
    testStore = test;
}

template<typename T>
CompareStore<T>::~CompareStore()
{
    //TODO creates a segfault when correctStore gets freed..
    //delete correctStore;
    delete testStore;
}

template<typename T>
bool CompareStore<T>::searchAndInsert(NetState &ns, T **payload, threadid_t threadIndex,
                                      bool)
{
    bool correctResult = correctStore->searchAndInsert(ns, payload, threadIndex);
    bool testResult = testStore->searchAndInsert(ns, payload, threadIndex);

    if (correctResult == testResult)
    {
        return correctResult;
    }

    assert(false);
    return true;  // cannot happen - only to silence compiler  LCOV_EXCL_LINE
}

/*!
 * \brief gets and removes a state from the store
 * \param ns NetState where the removed state will be written to
 * \return false, if store was already empty
 */
template<typename T>
bool CompareStore<T>::popState(NetState &ns)
{
    testStore->popState(ns);
    return correctStore->popState(ns);
}
