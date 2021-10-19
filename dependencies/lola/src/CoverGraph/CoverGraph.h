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

\brief Global data for Coverability Graph construction
*/

#pragma once

#include <Core/Dimensions.h>
#include <CoverGraph/CoverPayload.h>
#include <Stores/Store.h>
#include <Witness/Path.h>

class NetState;
class Firelist;
class SimpleProperty;

// use the maximal capacity of a place as omega (unboundedly many) tokens
#define OMEGA (static_cast<int>(MAX_CAPACITY>>1))
// compare value for "less than OMEGA"
#define FINITE (OMEGA-1)

/*!
\brief Collection of information related to the Coverability Graph construction
*/
class CoverGraph
{
public:
    /// Constructor
    CoverGraph(SimpleProperty &property, NetState &ns, Store<CoverPayload> &st,
               Firelist &firelist, Path &_path, arrayindex_t number_of_threads);

    /// Destructor
    ~CoverGraph();

    /// Run breadth first method
    ternary_t run();

    /// Run breadth first method multithreaded
    ternary_t runThreads();

    //  static void printState(NetState& ns);

    /// switch to terminal SCC check (between constructor call and run)
    void setAGEF();

private:
    /// transfer struct for the start of a parallel search
    struct tpCoverArguments
    {
        /// the initial or current net state
        NetState *ns;
        /// the global ID of the current thread
        int globalThreadID;
        /// the CoverGraph object with further parameters
        CoverGraph *obj;
        /// the first payload of this thread (starting point for freeing memory)
        CoverPayload *root;
        /// the payload of the current root
        CoverPayload *payload;
    };

    /// starting point for the coverability search (single- or multithreaded)
    static void *coverThread(void *container);

    /// init the local mutex of a thread and make it globally known
    void initThreadMutex(pthread_mutex_t *mutex, pthread_cond_t *cond, arrayindex_t threadID);

    /// get a helper thread to work on a given state
    bool getHelper(NetState &ns, CoverPayload *payload);

    /// tell the other threads that this thread is becoming idle
    void goIdle(arrayindex_t threadID);

    /// deinit the local mutex of a thread
    void terminateThreadMutex(pthread_mutex_t *mutex, pthread_cond_t *cond, NetState *&ns,
                              CoverPayload *pay);

    /// fire a transition
    static void fire(NetState &ns, arrayindex_t t);

    /// fire a transition in reverse direction (for backtracking)
    static void backfire(NetState &ns, arrayindex_t t);

    /// set the token number of a place to omega
    static void createOmega(NetState &ns, arrayindex_t p);

    /// set the token number of a place to the former value
    static void removeOmega(NetState &ns, arrayindex_t t, arrayindex_t p, capacity_t tokens);

    /// find the places on which an omega must be introduced
    arrayindex_t **checkOmega(NetState &ns, arrayindex_t t, CoverPayload *payload, arrayindex_t &count,
                              arrayindex_t threadID);

    /// release a list of omega places
    static void revertOmega(NetState &ns, arrayindex_t **omegas, arrayindex_t count);

    /// release all omega lists create by one thread
    static void releaseOmegas(CoverPayload *pay);

    /// terminate the thread
    NetState *terminateThread(pthread_mutex_t *mutex, pthread_cond_t *cond, SimpleProperty *sp,
                              Firelist *fl, CoverPayload *root, NetState *ns, CoverPayload *pay, bool unknown);

    /// create a witness path
    void createPath(NetState *&ns, CoverPayload *pay);

    /// search the tsccs
    CoverPayload *tarjan(CoverPayload *root);

    /// get a witness state after the tscc search has failed
    NetState *getWitnessState(CoverPayload *pay);

    /// global information for all threads
    tpCoverArguments *threadInfo;

    /// the total number of threads running over the search space
    arrayindex_t nr_of_threads;

    /// the property to check for
    SimpleProperty &prop;

    /// the initial state of the net
    NetState &start;

    /// the firelist for the property
    Firelist &fl;

    /// the store to which states are written
    Store<CoverPayload> &store;

    /// mutexes used in case a thread becomes idle
    pthread_mutex_t **idleMutex;

    /// conditions to lock on if a thread becomes idle
    pthread_cond_t **idleCond;

    /// flag if a thread is idle (under above lock)
    bool **idle;

    /// total number of idle threads (under runMutex)
    arrayindex_t idle_count;

    /// a set of idle threads (by ID, under runMutex)
    std::set<arrayindex_t> idle_set;

    /// number of threads that request running
    arrayindex_t running;

    /// number of threads that request termination
    arrayindex_t terminating;

    /// shared mutex for idle list, init, and termination
    pthread_mutex_t runMutex;

    /// shared condition to lock on when initialising or terminating
    pthread_cond_t runCond;

    /// a flag telling all threads to terminate
    bool exit;

    /// set when a property if unknown for some state
    bool global_unknown;

    /// whether to check terminal sccs
    bool scc;

    /// the witness path to return
    Path &path;

    /// space for calculating marking differences when introducing omegas
    capacity_t **difference;

    /// signums of marking differences for diff
    char **signum;

    /// list of omegas introduced on some path
    arrayindex_t **pomega;

    /// global index for tarjan
    arrayindex_t index;

    /// last tscc found
    arrayindex_t ltscc;

    /// last (potential) true value in tarjan
    arrayindex_t last_true;

    /// last questionable value in tarjan
    arrayindex_t last_unknown;
};
