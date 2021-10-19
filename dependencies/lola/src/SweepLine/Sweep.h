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

\brief Global data for SweepLine method

All general data used in the SweepLine method can be found here.
*/

#pragma once

#include <Core/Dimensions.h>
#include <Exploration/Firelist.h>
#include <Exploration/SimpleProperty.h>
#include <Net/NetState.h>
#include <SweepLine/SweepEmptyStore.h>
#include <SweepLine/SweepListStore.h>
#include <SweepLine/SweepRingStore.h>

/*!
\brief Collection of information related SweepLine method

All general data used in the SweepLine method can be found here.
*/
template <class T>
class Sweep
{
public:
    /// Constructor
    Sweep(SimpleProperty &property, NetState &ns, SweepEmptyStore &st, Firelist &firelist,
          arrayindex_t number_of_fronts, arrayindex_t number_of_threads);

    /// Destructor
    ~Sweep();

    /// Run sweepline method
    bool run();

    /// Run sweepline method multithreaded
    bool runThreads();

    char * getStatus();

private:
    /// transfer struct for the start of a parallel search front
    struct tpSweepArguments
    {
        /// the initial or current net state
        NetState *ns;
        /// flag indicating whether this thread is the front thread
        bool *frontrunner;
        /// mutex for the frontrunner flag (for the predecessor front)
        pthread_mutex_t *frontMutex;
        /// mutex for the frontrunner flag (for the successor front)
        pthread_mutex_t *backMutex;
        /// mutex condition (for the predecessor front)
        pthread_cond_t *frontCond;
        /// mutex condition (for the successor front)
        pthread_cond_t *backCond;
        /// semaphore value (for the predecessor front)
        arrayindex_t *frontSemaphore;
        /// semaphore value (for the successor front)
        arrayindex_t *backSemaphore;
        /// the ID number of the front the current thread belongs to
        arrayindex_t frontID;
        /// the global ID of the current thread
        int globalThreadID;
        /// number of transient states in the local store
        int64_t *transient_count;
        /// number of persistent states added by the local store
        int64_t *persistent_count;
        /// number of calls to the local store (edges)
        int64_t *call_count;
        /// the SweepLine object with further parameters
        Sweep<T> *sweep;
    };

    /// starting point for the sweepline method (single- or multithreaded)
    static void *frontSweep(void *container);

    /// starting point for a helper thread of the sweepline method (multithreaded only)
    static void *threadSweep(void *container);

    /// initialise the sweepline front for a thread
    bool initFront(tpSweepArguments &args, SweepRingStore<T> &store, int32_t &back_progress,
                   int32_t &sem_countdown);

    /// run the front until the end, return a state fulfilling the property or NULL if none
    NetState *runFront(SweepRingStore<T> &store, Firelist &fl, SimpleProperty &sp,
                       tpSweepArguments &args, int32_t &bp, int32_t &semcnt);

    /// finalise the front
    void deinitFront(SweepRingStore<T> &store, tpSweepArguments &args);

    /// check all states with the current progress measure and calculate their successors
    bool checkStates(SweepRingStore<T> &store, Firelist &fl, SimpleProperty &sp,
                     tpSweepArguments &args);

    /// extend the lists of persistent states to before the minimal progress measure
    void extendLeft(int32_t &bp);

    /// update enabledness information for the current state if the marking has been replaced
    static void updateState(NetState &ns);

    /// forward the state count to the dummy store for later printing
    void forwardStateCount(bool show = false);

    void initThreadMutex(pthread_mutex_t *mutex, pthread_cond_t *cond, arrayindex_t threadID);
    void terminateThreadMutex(pthread_mutex_t *mutex, pthread_cond_t *cond);
    void checkCall(tpSweepArguments &args, bool draw = false);

    /// the number of fronts running over the search space
    arrayindex_t nr_of_fronts;

    /// the total number of threads running over the search space
    arrayindex_t nr_of_threads;

    /// the number of threads per front
    arrayindex_t threads_per_front;

    /// the number of hash values (buckets per progress value and store type)
    hash_t nr_of_buckets;

    /// the property to check for
    SimpleProperty &prop;

    /// the initial state of the net
    NetState &start;

    /// the firelist for the property
    Firelist &fl;

    /// Dummy store for counting markings and calls
    SweepEmptyStore &count_store;

    /// the list of new persistent states (starting point, minimal progress measure)
    SweepListStore<T> *start_new_persistent;

    /// the list of old persistent states (starting point, minimal progress measure)
    SweepListStore<T> *start_old_persistent;

    /// counters for persistent states
    int64_t ** *persistent_count;

    /// counters for transient states
    int64_t ** *transient_count;

    /// counters for edges (calls)
    int64_t ** *call_count;

    /// maximal number of transient states
    int64_t max_transient_count;

    /// the number of buckets the store for transient states must contain
    arrayindex_t store_size;

    /// the maximal progress measure of a single transition
    arrayindex_t front_offset;

    /// the negative progress measure offset at which transient states are forgotten
    arrayindex_t transient_offset;

    /// pointers to the stores of all fronts
    SweepRingStore<T> **store;

    /// mutex used in case a thread becomes idle
    pthread_mutex_t **idleMutex;

    /// mutex to lock on if a thread becomes idle
    pthread_cond_t **idleCond;

    /// flag if a thread is idle, not lock-protected
    bool **idle;

    /// the front a helper works for at this time, not lock-protected
    arrayindex_t **frontptr;

    /// flag if a thread should return to its own front, not lock-protected
    bool **call;

    /// number of helpers active at a given front
    arrayindex_t **helpers;

    /// number of threads that request termination
    arrayindex_t running;
    arrayindex_t terminating;
    pthread_mutex_t runMutex;
    pthread_cond_t runCond;

    /// encoder for all transient and new persistent states
    FullCopyEncoder *fullencoder;
    /// encoder for old persistent states
    NetStateEncoder *sigpencoder;

    /// a flag telling all threads to terminate
    bool exit;
};

#include <SweepLine/Sweep.inc>
