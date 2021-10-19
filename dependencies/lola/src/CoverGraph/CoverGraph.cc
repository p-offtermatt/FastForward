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

\brief basic routines for Coverability Graph search
*/

#include <config.h>
#include <Core/Dimensions.h>
#include <CoverGraph/CoverGraph.h>
#include <Exploration/Firelist.h>
#include <Exploration/SimpleProperty.h>
#include <InputOutput/Reporter.h>
#include <Net/Marking.h>
#include <Net/Net.h>
#include <Net/NetState.h>
#include <Net/Place.h>
#include <Net/Transition.h>

/*!
* \brief Constructor for the Coverability Graph search
* \param property The property to check for
* \param ns The initial state of the net
* \param st The store where states are saved
* \param firelist A firelist applicable to the property
* \param _path A place to store a witness path
* \param number_of_threads The number of threads running over the state space simultaneously.
*/
CoverGraph::CoverGraph(SimpleProperty &property, NetState &ns, Store<CoverPayload> &st,
                       Firelist &firelist,
                       Path &_path, arrayindex_t number_of_threads)
    : nr_of_threads(number_of_threads), prop(property), start(ns), fl(firelist),
      store(st), exit(false), global_unknown(false), scc(false), path(_path),
      index(0), ltscc(0), last_true(0), last_unknown(0)
{
    // reserve memory for thread-local mutexes for the idle state change
    idleMutex = new pthread_mutex_t *[nr_of_threads];
    idleCond = new pthread_cond_t *[nr_of_threads];

    // each thread must show its idle status
    idle = new bool *[nr_of_threads]();
    // we count the number of idle threads to know when to terminate
    idle_count = 0;
    // we count the running and the terminating threads
    terminating = running = nr_of_threads;

    // information that must be transferred to the threads
    threadInfo = new tpCoverArguments[nr_of_threads];

    // get a mutex and condition for thread synchronisation upon termination
    // LCOV_EXCL_START
    if (UNLIKELY(pthread_mutex_init(&runMutex, NULL)))
    {
        RT::rep->status("mutexes could not be created");
        RT::rep->abort(ERROR_THREADING);
    }
    if (UNLIKELY(pthread_cond_init(&runCond, NULL)))
    {
        RT::rep->status("mutex conditions could not be created");
        RT::rep->abort(ERROR_THREADING);
    }
    // LCOV_EXCL_STOP

    // reserve memory for omega calculations
    difference = new capacity_t *[nr_of_threads];
    signum = new char *[nr_of_threads];
    pomega = new arrayindex_t *[nr_of_threads];
    for (arrayindex_t i = 0; i < nr_of_threads; ++i)
    {
        difference[i] = new capacity_t[Net::Card[PL]]();
        signum[i] = new char[Net::Card[PL]]();
        pomega[i] = new arrayindex_t[Net::Card[PL]]();
    }
}

/*!
 * \brief Destructor.
 */
CoverGraph::~CoverGraph()
{
    // free memory for omega calculations
    for (arrayindex_t i = 0; i < nr_of_threads; ++i)
    {
        delete[] difference[i];
        delete[] signum[i];
        delete[] pomega[i];
    }
    delete[] difference;
    delete[] signum;
    delete[] pomega;

    // destroy mutexes
    int mutex_destruction_status = 0;
    mutex_destruction_status |= pthread_cond_destroy(&runCond);
    mutex_destruction_status |= pthread_mutex_destroy(&runMutex);
    // LCOV_EXCL_START
    if (UNLIKELY(mutex_destruction_status))
    {
        RT::rep->status("mutexes could not be destroyed");
        RT::rep->abort(ERROR_THREADING);
    }
    // LCOV_EXCL_STOP

    delete[] idleMutex;
    delete[] idleCond;
    delete[] idle;
    delete[] threadInfo;
}

/*!
 * \brief Run the Coverability Graph search.
 * \return If a state fulfilling the property could be found. In the positive case,
 *         the netstate given to the constructor will contain such a state.
 */
ternary_t CoverGraph::run()
{
    // check if we need a multithreaded run (just in case)
    if (nr_of_threads > 1)
    {
        // LCOV_EXCL_START
        return runThreads();
        // LCOV_EXCL_STOP
    }

    // gather the arguments for the run using the main thread only
    threadInfo[0].globalThreadID = 0;
    threadInfo[0].obj = this;
    threadInfo[0].ns = new NetState(start);
    threadInfo[0].root = NULL;

    // run the Coverability Graph search and get the result
    void *return_value(coverThread(reinterpret_cast<void *>(threadInfo)));
    prop.value = false;
    if (return_value)
    {
        prop.value = true;
        start = *reinterpret_cast<NetState *>(return_value);
        delete reinterpret_cast<NetState *>(return_value);
    }

    // return the result according to the formula type (AGEF or EF)
    if (prop.value)
    {
        return (scc ? TERNARY_FALSE : TERNARY_TRUE);
    }
    if (global_unknown)
    {
        return TERNARY_UNKNOWN;
    }
    return (scc ? TERNARY_TRUE : TERNARY_FALSE);
}

/*!
 * \brief Run the Coverability Graph search in multithreaded mode
 * \return If a state fulfilling the property could be found. In the positive case,
 *         the netstate given to the constructor will contain such a state.
 */
ternary_t CoverGraph::runThreads()
{
    // thread objects
    pthread_t *runner_thread = new pthread_t[nr_of_threads]();

    // set the arguments for all threads
    for (arrayindex_t i = 0; i < nr_of_threads; i++)
    {
        // initialise data structure for threads
        threadInfo[i].globalThreadID = i;
        threadInfo[i].obj = this;
        threadInfo[i].ns = NULL;
        threadInfo[i].root = NULL;
    }
    // the first thread gets the start state
    threadInfo[0].ns = new NetState(start);

    // create the threads
    for (arrayindex_t i = 0; i < nr_of_threads; i++)
        if (UNLIKELY(pthread_create(runner_thread + i, NULL, coverThread,
                                    threadInfo + i)))
        {
            // LCOV_EXCL_START
            RT::rep->status("threads could not be created");
            RT::rep->abort(ERROR_THREADING);
            // LCOV_EXCL_STOP
        }


    //// THREADS ARE RUNNING AND SEARCHING


    // wait for all threads to finish
    prop.value = false;
    for (arrayindex_t i = 0; i < nr_of_threads; i++)
    {
        void *return_value;
        if (UNLIKELY(pthread_join(runner_thread[i], &return_value)))
        {
            // LCOV_EXCL_START
            RT::rep->status("threads could not be joined");
            RT::rep->abort(ERROR_THREADING);
            // LCOV_EXCL_STOP
        }
        if (return_value)
        {
            prop.value = true;
            start = *reinterpret_cast<NetState *>(return_value);
        }
    }

    // free the allocated memory
    delete[] runner_thread;
    for (arrayindex_t i = 0; i < nr_of_threads; ++i)
    {
        delete threadInfo[i].ns;
    }

    // propagate the result according to the formula type (AGEF or EF)
    if (prop.value)
    {
        return (scc ? TERNARY_FALSE : TERNARY_TRUE);
    }
    if (global_unknown)
    {
        return TERNARY_UNKNOWN;
    }
    return (scc ? TERNARY_TRUE : TERNARY_FALSE);
}


/*!
 * \brief main function for a thread (also used for single-threaded runs)
 * \param container Arguments needed by the thread
 * \return Pointer to a state fulfilling the property or NULL if none exists
 */
void *CoverGraph::coverThread(void *container)
{
    // obtain the global information structure and our thread ID
    tpCoverArguments *arguments = reinterpret_cast<tpCoverArguments *>(container);
    CoverGraph *cg(arguments->obj);
    threadid_t threadID(arguments->globalThreadID);
    Store<CoverPayload> &store(cg->store);

    // for idle checks by helpers and the thread itself
    bool idle(true);
    cg->idle[threadID] = &idle;

    // Make copies of SimpleProperty and FireList for this thread
    SimpleProperty *sp = cg->prop.copy();
    Firelist *myFirelist = cg->fl.createNewFireList(sp);
    // obtain a firelist generator that delivers all enabled transitions (AGEF only)
    Firelist simpleFirelist;

    // get the starting state
    NetState *ns(arguments->ns);

    // flag if we encountered a state with unknown property value in this thread
    bool unknown(false);

    // Payload Pointers
    CoverPayload *ipay;  // a temporary pointer
    CoverPayload *unlinked;  // the last node without a next-pointer
    CoverPayload *currentPayload;  // the node from which we fire transitions

    // allocate mutex and wait for init of all threads
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    cg->initThreadMutex(&mutex, &cond, threadID);

    /// the thread has initialized and can start searching now

    if (threadID == 0)
    {
        // the first thread gets the initial state to start with
        store.searchAndInsert(*ns, &ipay, 0);

        // set the payload of the initial state
        ipay->parent = NULL; // parent that created this node
        ipay->next = NULL; // link list for nodes created by this thread
        ipay->card_new_omegas = 0; // number of new omegas introduced at this state
        ipay->transition = 0; // the transition fired to get to this state from its parent
        ipay->next_level =
            true; // if we are the first node on a new level of the subtree created by this thread
        ipay->unknown =
            false; // if the predicate's outcome was unclear (e.g. comparison of omega with a constant)
        ipay->index = 0; // index for tarjan's TSCC search (AGEF only)

        // remember the starting point for freeing memory later
        arguments->root = ipay;

        // remember the last state that has no "next state" in the
        // breadth first search YET
        unlinked = ipay;

        // remember the current state info
        currentPayload = ipay;
    }
    else
    {
        if (arguments->ns)
        {
            // other threads may get a state later
            ns = arguments->ns;
            currentPayload = arguments->payload;
            unlinked = currentPayload;
            arguments->root = currentPayload;
        }
        else
        {
            // or they do not get a state at all (direct termination)
            return cg->terminateThread(&mutex, &cond, sp, myFirelist, arguments->root, NULL, NULL, false);
        }
    }

    // reserve memory to store dynamic partial firing sequence (of indeterminate length)
    std::vector<arrayindex_t> tseq;
    tseq.reserve(8);

    // whether we have reached a new level in our (local) search tree
    bool new_level(false);

    // we work until we get a termination notice
    while (!cg->exit)
    {
        // first, check the property, terminate only if we get TRUE for an EF formula
        if ((currentPayload->value = sp->checkOmegaProperty(*ns)) && !cg->scc)
        {
            // if the property holds in the start state, we can terminate
            return cg->terminateThread(&mutex, &cond, sp, myFirelist, arguments->root, ns, currentPayload,
                                       unknown);
        }
        // if the result cannot be trusted, remember this, for the node and for the thread
        if (sp->isUnknown())
        {
            unknown = true;
            currentPayload->unknown = true;
        }

        // if we have reached the next level of the tree
        // the first child node we encounter is also on a(nother) new level
        if (currentPayload->next_level)
        {
            new_level = true;
        }

        // get the firelist: stubborn set for false-nodes, all enabled transitions otherwise (AGEF only)
        arrayindex_t *currentFirelist;
        arrayindex_t currentEntry;
        if (currentPayload->value)
        {
            currentEntry = simpleFirelist.getFirelist(*ns, &currentFirelist);
        }
        else
        {
            currentEntry = myFirelist->getFirelist(*ns, &currentFirelist);
        }
        if (cg->scc)
        {
            // in the AGEF case we must save the edges so we can make a tarjan TSCC search later
            currentPayload->sons = new CoverPayload *[currentEntry];
            currentPayload->card_sons = currentEntry;
        }

        // create all the child nodes (breadth first)
        for (arrayindex_t i = 0; i < currentEntry; ++i)
        {
            // fire a transition
            CoverGraph::fire(*ns, currentFirelist[i]);

            // then check if new omegas must be introduced
            arrayindex_t count;
            arrayindex_t **omegas = cg->checkOmega(*ns, currentFirelist[i], currentPayload, count, threadID);

            // check if the state (with new omegas) is already in the store
            if (store.searchAndInsert(*ns, &ipay, threadID))
            {
                // State exists! -->backtracking to previous state
                CoverGraph::revertOmega(*ns, omegas, count);
                CoverGraph::backfire(*ns, currentFirelist[i]);
            }
            else
            {
                // State does not exist!
                Transition::updateEnabled(*ns, currentFirelist[i]);

                // update the payloads (parent, sideways link, fired transition, new omegas)
                ipay->parent = currentPayload;
                ipay->next = NULL;
                ipay->sons = NULL;
                ipay->transition = currentFirelist[i];
                ipay->card_new_omegas = count;
                ipay->new_omegas = omegas;
                ipay->unknown = false;
                ipay->index = 0;

                // check if we can delegate this node to another thread
                // but never transfer the first son - it might be the only
                // node on this tree level that we can reach
                if (i > 0 && cg->getHelper(*ns, ipay))
                {
                    // another thread uses this node as its new root
                    // we must not link it into our subtree
                }
                else
                {
                    // check if last added node and this child are on different tree levels
                    ipay->next_level = new_level;
                    new_level = false;
                    // add the missing sideways link from the last added node
                    unlinked->next = ipay;
                    unlinked = ipay;
                }

                // revert changes so we can fire the next transition
                for (arrayindex_t j = 0; j < ipay->card_new_omegas; ++j)
                {
                    CoverGraph::removeOmega(*ns, ipay->transition, ipay->new_omegas[j][0], ipay->new_omegas[j][1]);
                }
                CoverGraph::backfire(*ns, currentFirelist[i]);
                Transition::revertEnabled(*ns, currentFirelist[i]);
            }

            // save edges for scc detection (AGEF only)
            if (cg->scc)
            {
                currentPayload->sons[i] = ipay;
            }
        }

        // we are done with the stubborn set
        delete[] currentFirelist;

        // find the next brother/cousin node in breadth first
        CoverPayload *cousin(currentPayload->next);

        // if there are no further cousins we may stop
        // (and possibly restart somewhere else if multi-threading)
        if (!cousin)
        {
            // forget the state and become idle
            delete ns;
            arguments->ns = NULL;
            cg->goIdle(threadID);

            // check if we start at a new subtree/state
            if (arguments->ns)
            {
                // get the new state to start from
                ns = arguments->ns;
                currentPayload = arguments->payload;
                unlinked->next = currentPayload;
                unlinked = currentPayload;

                // we have a new state to start with, so continue our work
                continue;
            }

            // if no new state was given to us, we can terminate
            break;
        }

        // follow the paths from cousin and currentPayload to their common ancestor
        // revert the state according to the path to currentPayload and save
        // the path to cousin for now
        ipay = currentPayload;
        // if currentPayload and cousin are on different tree levels
        bool diff_level(cousin->next_level);
        // space for the new firing sequence (to cousin)
        tseq.clear();
        // reserved memory for omega places on the new path (to cousin)
        arrayindex_t *pomega(cg->pomega[threadID]);
        arrayindex_t countOmega(0);
        while (ipay != cousin)
        {
            // if our cousin is on the next tree level, we only move rootwards on the cousin path
            // thus we reach the common ancestor at the same time on both paths
            if (diff_level)
            {
                diff_level = false;
            }
            else
            {
                // move one step towards the root on our path
                // and adapt the state accordingly
                for (arrayindex_t i = 0; i < ipay->card_new_omegas; ++i)
                {
                    CoverGraph::removeOmega(*ns, ipay->transition, ipay->new_omegas[i][0], ipay->new_omegas[i][1]);
                }
                CoverGraph::backfire(*ns, ipay->transition);
                Transition::revertEnabled(*ns, ipay->transition);
                ipay = ipay->parent;
            }

            // remember the cousin path so we can follow it later in the opposite direction
            tseq.push_back(cousin->transition);
            for (arrayindex_t i = 0; i < cousin->card_new_omegas; ++i)
            {
                pomega[countOmega++] = cousin->new_omegas[i][0];
            }
            cousin = cousin->parent;
        }

        // now follow the path to the cousin forward, introduce omegas, and do the state changes
        for (arrayindex_t i = 0; i < countOmega; ++i)
        {
            CoverGraph::createOmega(*ns, pomega[i]);
        }
        for (arrayindex_t i = tseq.size(); i > 0; --i)
        {
            CoverGraph::fire(*ns, tseq[i - 1]);
            Transition::updateEnabled(*ns, tseq[i - 1]);
        }

        // after having obtained the next state to operate on, also get its payload
        currentPayload = currentPayload->next;
    }

    // no more states to process, we may terminate
    return cg->terminateThread(&mutex, &cond, sp, myFirelist, arguments->root, NULL, NULL, unknown);
}


/*!
 * \brief init the thread mutex or idle checks and let all threads wait
 *        until all of them have initialised.
 * \param mutex The mutex for idle checks.
 * \param cond The condition the threads sleeps upon when idle.
 * \param threadID The ID of the thread.
 */
void CoverGraph::initThreadMutex(pthread_mutex_t *mutex, pthread_cond_t *cond,
                                 arrayindex_t threadID)
{
    // make the location of mutex and condition known to other threads
    idleMutex[threadID] = mutex;
    idleCond[threadID] = cond;

    // initialise the mutex
    // LCOV_EXCL_START
    if (UNLIKELY(pthread_mutex_init(mutex, NULL)))
    {
        exit = true;
        RT::rep->status("mutexes could not be created");
        RT::rep->abort(ERROR_THREADING);
    }
    if (UNLIKELY(pthread_cond_init(cond, NULL)))
    {
        exit = true;
        RT::rep->status("mutex conditions could not be created");
        RT::rep->abort(ERROR_THREADING);
    }
    // LCOV_EXCL_STOP

    // wait until all threads have initialized
    pthread_mutex_lock(&runMutex);
    if (!--running)
    {
        // the last thread initializing starts the thread with ID zero
        pthread_mutex_unlock(&runMutex);
        pthread_mutex_lock(idleMutex[0]);
        *idle[0] = false;
        pthread_cond_signal(idleCond[0]);
        pthread_mutex_unlock(idleMutex[0]);
    }
    else
    {
        pthread_mutex_unlock(&runMutex);
    }

    // we wait until we get a state from which to start our first search
    // or a termination notice
    pthread_mutex_lock(mutex);
    while (*idle[threadID] && !exit)
    {
        pthread_cond_wait(cond, mutex);
    }
    pthread_mutex_unlock(mutex);

    // the thread with ID zero makes known that all other threads are idle at first
    if (!threadID)
    {
        pthread_mutex_lock(&runMutex);
        for (arrayindex_t i = 1; i < nr_of_threads; ++i)
        {
            idle_set.insert(i);
            ++idle_count;
        }
        pthread_mutex_unlock(&runMutex);
    }
}

/*!
 * \brief delegate a state to another thread.
 * \param ns The state.
 * \param payload The payload belonging to the state.
 * \return If a helper thread could be obtained.
 */
bool CoverGraph::getHelper(NetState &ns, CoverPayload *payload)
{
    // check if there are idle threads (without locking mechanism)
    if (!idle_count)
    {
        return false;
    }

    // a helper could be available, check again with locking to be sure
    pthread_mutex_lock(&runMutex);
    if (!idle_count)
    {
        // LCOV_EXCL_START
        pthread_mutex_unlock(&runMutex);
        return false;
        // LCOV_EXCL_STOP
    }

    // we found a helper
    arrayindex_t helper(*(idle_set.begin()));
    idle_set.erase(idle_set.begin());
    --idle_count;
    pthread_mutex_unlock(&runMutex);

    // make the state known to the helper thread
    threadInfo[helper].ns = new NetState(ns);
    threadInfo[helper].payload = payload;
    // it's a new root state for the helper, so it's on a new tree level
    payload->next_level = true;

    // signal the helper that it's not idle anymore
    pthread_mutex_lock(idleMutex[helper]);
    *idle[helper] = false;
    pthread_cond_signal(idleCond[helper]);
    pthread_mutex_unlock(idleMutex[helper]);
    return true;
}

/*!
 * \brief we have no more states to search locally
 *        so we go idle and wait until a new state is given to us.
 * \param threadID The ID of this thread.
 */
void CoverGraph::goIdle(arrayindex_t threadID)
{
    // make ourselves idle
    pthread_mutex_lock(idleMutex[threadID]);
    *idle[threadID] = true;

    // and tell other threads that we want work
    pthread_mutex_lock(&runMutex);
    idle_set.insert(threadID);

    // if all threads are idle now
    if (++idle_count == terminating)
    {
        // we tell them all to terminate by setting the exit flag
        pthread_mutex_unlock(&runMutex);
        pthread_mutex_unlock(idleMutex[threadID]);
        exit = true;
        return;
    }
    pthread_mutex_unlock(&runMutex);

    // otherwise we wait until we get a new state or are signalled to exit
    while (*idle[threadID] && !exit)
    {
        pthread_cond_wait(idleCond[threadID], idleMutex[threadID]);
    }
    pthread_mutex_unlock(idleMutex[threadID]);
}


/*!
 * \brief fire a transition respecting omega entries in the state
 * \param ns The current state.
 * \param t The transition to be fired.
 */
void CoverGraph::fire(NetState &ns, arrayindex_t t)
{
    if (!ns.Enabled[t])
    {
        // LCOV_EXCL_START
        RT::rep->message("=================ERROR==================");
        RT::rep->message("TRY TO FIRE %s", Net::Name[TR][t]);
        RT::rep->message("current marking %s:%d %s:%d %s:%d %s:%d", Net::Name[PL][0], ns.Current[0],
                         Net::Name[PL][1], ns.Current[1], Net::Name[PL][2], ns.Current[2], Net::Name[PL][3], ns.Current[3]);
        // LCOV_EXCL_STOP
    }

    //  Don't even think about firing a disabled transition!
    assert(ns.Enabled[t]);

    // 1. Update current marking
    for (arrayindex_t i = 0; i < Transition::CardDeltaT[PRE][t]; i++)
    {
        // there should be enough tokens to fire this transition
        assert(ns.Current[Transition::DeltaT[PRE][t][i]] >= Transition::MultDeltaT[PRE][t][i]);

        // omega entries must remain unchanged
        if (ns.Current[Transition::DeltaT[PRE][t][i]] < OMEGA)
        {
            ns.Current[Transition::DeltaT[PRE][t][i]] -= Transition::MultDeltaT[PRE][t][i];
        }
    }
    for (arrayindex_t i = 0; i < Transition::CardDeltaT[POST][t]; i++)
    {
        // omega entries must remain unchanged
        if (ns.Current[Transition::DeltaT[POST][t][i]] < OMEGA)
        {
            ns.Current[Transition::DeltaT[POST][t][i]] += Transition::MultDeltaT[POST][t][i];
        }
    }

    // 2. update hash value
    ns.HashCurrent += Transition::DeltaHash[t];
    ns.HashCurrent %= SIZEOF_MARKINGTABLE;
    if (UNLIKELY(ns.HashCurrent < 0))
    {
        // just safety belt, if % returns negative value
        // LCOV_EXCL_START
        ns.HashCurrent += SIZEOF_MARKINGTABLE;
        // LCOV_EXCL_STOP
    }
}

/*!
 * \brief fire a transition backwards respecting omega entries in the state
 * \param ns The current state.
 * \param t The transition to be reverse-fired.
 */
void CoverGraph::backfire(NetState &ns, arrayindex_t t)
{
    // 1. Update current marking
    for (arrayindex_t i = 0; i < Transition::CardDeltaT[PRE][t]; i++)
    {
        // omega entries remain unchanged
        if (ns.Current[Transition::DeltaT[PRE][t][i]] < OMEGA)
        {
            ns.Current[Transition::DeltaT[PRE][t][i]] += Transition::MultDeltaT[PRE][t][i];
        }
    }
    for (arrayindex_t i = 0; i < Transition::CardDeltaT[POST][t]; i++)
    {
        // there should be enough tokens to backfire this transition
        assert(ns.Current[Transition::DeltaT[POST][t][i]] >= Transition::MultDeltaT[POST][t][i]);

        // omega entries remain unchanged
        if (ns.Current[Transition::DeltaT[POST][t][i]] < OMEGA)
        {
            ns.Current[Transition::DeltaT[POST][t][i]] -= Transition::MultDeltaT[POST][t][i];
        }
    }

    // 2. update hash value
    ns.HashCurrent -= Transition::DeltaHash[t];
    ns.HashCurrent %= SIZEOF_MARKINGTABLE;
    while (ns.HashCurrent < 0)
    {
        ns.HashCurrent += SIZEOF_MARKINGTABLE;
    }
}

/*!
 * \brief update enabledness information after introducing an omega
 * \param ns The current state.
 * \param p A place where an omega must be introduced.
 */
void CoverGraph::createOmega(NetState &ns, arrayindex_t p)
{
    // set the place to omega
    ns.Current[p] = OMEGA;

    // update the enabledness info for all disabled post-transitions of p
     for(arrayindex_t i = 0; i < Net::CardArcs[PL][POST][p];i++)
     {
	const arrayindex_t tt = Net::Arc[PL][POST][p][i];
	if(!ns.Enabled[tt])
	{
		Transition::checkEnabled(ns,tt);
	}
     }
//    for (arrayindex_t j = 0; j < ns.CardDisabled[p]; /* tricky increment handling */)
//    {
//        const arrayindex_t tt = ns.Disabled[p][j];
//        Transition::checkEnabled(ns, tt);
//        if (ns.Disabled[p][j] == tt)
//        {
//            j++; /* tricky increment handling */
//        }
//    }
}

/*!
 * \brief update enabledness information after removing an omega
 * \param ns The current state.
 * \param t The transition that has just reverse-fired such that an omega is eliminated from its preset.
 * \param p The place where the omega must be removed.
 * \param tokens The original number of tokens on p before the omega was introduced.
 */
void CoverGraph::removeOmega(NetState &ns, arrayindex_t t, arrayindex_t p, capacity_t tokens)
{
    // remove the omega and replace it with the original number of tokens
    ns.Current[p] = tokens;

    // update the enabledness information for the place p
    for (arrayindex_t i = 0; i < Transition::CardConflicting[t]; i++)
    {
        const arrayindex_t tt = Transition::Conflicting[t][i];
        if (ns.Enabled[tt])
        {
            Transition::checkEnabled(ns, tt);
        }
    }
}

/* usable in case of debugging
void CoverGraph::printState(NetState& ns)
{
    for (int i = 0; i < Net::Card[PL]; i++)
    {
        std::cout << Net::Name[PL][i] << ":";
        if (ns.Current[i] == OMEGA) std::cout << "w ";
        else std::cout << ns.Current[i] << " ";
    }
    std::cout << std::endl;
}
*/

/*!
 * \brief calculate where to introduce omegas after firing a transition
 * \param ns The current state.
 * \param t The transition that has just fired.
 * \param payload The payload of the current state.
 * \param count The number of omegas to introduce (return value).
 * \param threadID The ID of the calling thread.
 * \return A list of places where to introduce omegas, including the original number of tokens
 *         and the distances to the covered states.
 */
arrayindex_t **CoverGraph::checkOmega(NetState &ns, arrayindex_t t, CoverPayload *payload,
                                      arrayindex_t &count,
                                      arrayindex_t threadID)
{
    // get reserved memory for the following calculations and clear it
    capacity_t *diff(difference[threadID]);
    char *sign(signum[threadID]);
    memset(diff, 0, Net::Card[PL] * SIZEOF_CAPACITY_T);
    memset(sign, 0, Net::Card[PL] * SIZEOF_CHAR);

    // we count how many places have more/less tokens than in the compared (=current) state
    arrayindex_t plus(0), minus(0), loops(1);
    count = 0;

    // we backfire transition t now, calculating the plus/minus-effect it would have on a state
    while (true)
    {
        // first undo the adding of tokens done by the postset
        for (arrayindex_t i = 0; i < Transition::CardDeltaT[POST][t]; ++i)
        {
            // get a place from the postset
            arrayindex_t p(Transition::DeltaT[POST][t][i]);
            // but don't do anything if it has omega tokens
            if (ns.Current[p] == OMEGA)
            {
                continue;
            }

            // update the differences between current and former state
            arrayindex_t n(Transition::MultDeltaT[POST][t][i]);
            if (sign[p] >= 0)
            {
                diff[p] += n;
                if (sign[p] == 0)
                {
                    ++plus;
                    sign[p] = 1;
                }
            }
            else
            {
                if (diff[p] >= n)
                {
                    diff[p] -= n;
                    if (diff[p] == 0)
                    {
                        --minus;
                        sign[p] = 0;
                    }
                }
                else
                {
                    diff[p] = n - diff[p];
                    sign[p] = 1;
                    ++plus;
                    --minus;
                }
            }
        }

        // now do the same for the preset (undo removal of tokens)
        for (arrayindex_t i = 0; i < Transition::CardDeltaT[PRE][t]; ++i)
        {
            arrayindex_t p(Transition::DeltaT[PRE][t][i]);
            if (ns.Current[p] == OMEGA)
            {
                continue;
            }
            arrayindex_t n(Transition::MultDeltaT[PRE][t][i]);
            if (sign[p] <= 0)
            {
                diff[p] += n;
                if (sign[p] == 0)
                {
                    ++minus;
                    sign[p] = -1;
                }
            }
            else
            {
                if (diff[p] >= n)
                {
                    diff[p] -= n;
                    if (diff[p] == 0)
                    {
                        --plus;
                        sign[p] = 0;
                    }
                }
                else
                {
                    diff[p] = n - diff[p];
                    sign[p] = -1;
                    --plus;
                    ++minus;
                }
            }
        }

        // check if the current state covers the former one
        // if so, we introduce omegas according to this former state
        if (plus && !minus)
        {
            break;
        }

        // check if we have reached the root of the tree
        if (!payload->parent)
        {
            // no state has been covered, give up, no new omegas
            return NULL;
        }

        // get the next transition to reverse-fire on the path to the root
        t = payload->transition;
        payload = payload->parent;

        // measure the distance between current and former state
        ++loops;
    }

    // we introduce new omegas
    arrayindex_t **omegas = new arrayindex_t *[plus]();
    for (arrayindex_t i = 0; i < plus; ++i)
    {
        omegas[i] = new arrayindex_t[3]();
    }
    arrayindex_t current(0);

    // check which places have been truly covered
    for (arrayindex_t i = 0; i < Net::Card[PL]; ++i)
    {
        if (sign[i] > 0)
        {
            // and introduce an omega there
            omegas[current][0] = i;
            // save the former number of tokens (for reverse-firing)
            omegas[current][1] = ns.Current[i];
            ns.Current[i] = OMEGA;
            // save the distance between current and covered state (for witness paths)
            omegas[current][2] = loops;
            ++current;
        }
    }

    // return the list of new omegas
    count = plus;
    return omegas;
}

/*!
 * \brief remove omegas from the current state and free the list of omegas
 * \param ns The current state.
 * \param omegas The list of omegas to be removed from the state and to be freed.
 * \param count The number of omegas to be removed from the state.
 */
void CoverGraph::revertOmega(NetState &ns, arrayindex_t **omegas, arrayindex_t count)
{
    if (!count)
    {
        return;
    }
    for (arrayindex_t i = 0; i < count; ++i)
    {
        ns.Current[omegas[i][0]] = omegas[i][1];
        delete[] omegas[i];
    }
    delete[] omegas;
}

/*!
 * \brief release all omega-lists in payloads created by this thread from memory.
 * \param pay The first payload ever created by this thread (NULL if none).
 */
void CoverGraph::releaseOmegas(CoverPayload *pay)
{
    // walk through the created subtrees along the next pointers
    // and delete all the omega-lists from the payloads of the store
    while (pay)
    {
        if (pay->card_new_omegas)
        {
            for (arrayindex_t i = 0; i < pay->card_new_omegas; ++i)
            {
                delete[] pay->new_omegas[i];
            }
            delete[] pay->new_omegas;
        }
        // in case we saved the sons, delete that memory too
        if (pay->sons)
        {
            delete[] pay->sons;
        }
        pay = pay->next;
    }
}

/*!
 * \brief free the local mutex and condition, create the witness path, make scc check, and terminate
 * \param mutex The mutex to be freed.
 * \param cond The condition to be freed.
 * \param ns The witness state.
 * \param pay Payload of the witness state.
 */
void CoverGraph::terminateThreadMutex(pthread_mutex_t *mutex, pthread_cond_t *cond, NetState *&ns,
                                      CoverPayload *pay)
{
    // signal all threads to terminate (in case it hasn't been done yet)
    exit = true;
    if (nr_of_threads > 1)
        for (arrayindex_t i = 0; i < nr_of_threads; ++i)
        {
            pthread_mutex_lock(idleMutex[i]);
            pthread_cond_signal(idleCond[i]);
            pthread_mutex_unlock(idleMutex[i]);
        }

    // wait until request for termination is accepted
    pthread_mutex_lock(&runMutex);

    // create a witness path in the simple case
    if (!scc)
    {
        createPath(ns, pay);
    }

    // are we the last thread?
    if (!--terminating)
    {
        // we are the last thread to reach this point
        // mutexes are not required anymore (all others are waiting)

        // we search the tsccs now in case of an AGEF-formula
        if (scc)
        {
            CoverPayload *result(tarjan(threadInfo[0].root));
            if (result)
            {
                global_unknown = false;
                ns = getWitnessState(result);
                createPath(ns, result);
            }
        }

        // all threads may continue beyond this point now.
        pthread_cond_broadcast(&runCond);
    }
    else while (terminating)
        {
            // wait until the last thread reaches this point
            pthread_cond_wait(&runCond, &runMutex);
        }
    pthread_mutex_unlock(&runMutex);

    // now we may destroy the mutexes
    int mutex_destruction_status = pthread_cond_destroy(cond);
    mutex_destruction_status |= pthread_mutex_destroy(mutex);
    // LCOV_EXCL_START
    if (UNLIKELY(mutex_destruction_status))
    {
        RT::rep->status("mutexes could not be destroyed");
        RT::rep->abort(ERROR_THREADING);
    }
    // LCOV_EXCL_STOP
}

/*!
 * \brief Terminate a thread.
 * \param mutex The idle mutex of the thread.
 * \param cond The idle condition of the thread.
 * \param sp The copied property to be checked by the thread.
 * \param fl The firelist of the thread.
 * \param root The first payload ever created by the thread (NULL if none).
 * \param ns A state fulfilling the property or NULL.
 * \param pay The payload of the witness state ns.
 * \param unknown If the property check for this thread led to the overall result 'unknown'.
 */
NetState *CoverGraph::terminateThread(pthread_mutex_t *mutex, pthread_cond_t *cond,
                                      SimpleProperty *sp, Firelist *fl, CoverPayload *root, NetState *ns, CoverPayload *pay, bool unknown)
{
    // kill the local mutex and condition
    // check the tsccs (in case of AGEF)
    // create a witness path
    terminateThreadMutex(mutex, cond, ns, pay);

    // transfer the unknown status to the global thread
    if (unknown && !scc)
    {
        pthread_mutex_lock(&runMutex);
        global_unknown = true;
        pthread_mutex_unlock(&runMutex);
    }

#ifndef USE_PERFORMANCE
    // free the memory allocated for the payloads
    CoverGraph::releaseOmegas(root);
#endif

    delete fl;
    delete sp;
    return ns;
}

void CoverGraph::createPath(NetState *&ns, CoverPayload *pay)
{
    // at this point we have a global lock
    // got a positive result in this thread?
    if (ns)
    {
        // check if we have to create the witness path
        if (!path.initialized)
        {
            // prevent other threads from creating the witness path
            path.initialized = true;

            // walk backwards through the witness path
            while (pay->parent)
            {
                // check if an omega was introduced here
                arrayindex_t jump(0);
                for (arrayindex_t i = 0; i < pay->card_new_omegas; ++i)
                {
                    // if omega was introduced check if the cycle is new
                    if (pay->new_omegas[i][2] == jump)
                    {
                        continue;
                    }
                    jump = pay->new_omegas[i][2];

                    // print the cycle to the path variable
                    path.endCycle(true);
                    CoverPayload *tmp(pay);
                    for (arrayindex_t j = 0; j < jump; ++j, tmp = tmp->parent)
                    {
                        path.addTransition(tmp->transition, true);
                    }
                    path.beginCycle(true);
                }

                // print the transition used to reach this state
                path.addTransition(pay->transition, true);

                // move towards the root of the path
                pay = pay->parent;
            }
        }
        else
        {
            // LCOV_EXCL_START
            // in the rare case when two threads find a positive answer
            // at the same time, delete the found state that does not
            // belong to the constructed path
            delete ns;
            ns = NULL;
            // LCOV_EXCL_STOP
        }
    }
}

/*!
 * \brief Make the CoverGraph routines check the terminal SCCs
          instead of all states.
 */
void CoverGraph::setAGEF()
{
    scc = true;
}

CoverPayload *CoverGraph::tarjan(CoverPayload *root)
{
    // a simple tarjan algorithm without a stack
    // we are interested in tsccs only

    root->index = index;
    root->lowlink = index++;

    if (root->unknown)
    {
        last_unknown = index;
    }
    else if (root->value)
    {
        last_true = index;
    }

    for (arrayindex_t i = 0; i < root->card_sons; ++i)
    {
        if (root->sons[i]->index == 0)
        {
            CoverPayload *witness(tarjan(root->sons[i]));
            if (witness)
            {
                return witness;
            }
            if (root->lowlink > root->sons[i]->lowlink)
            {
                root->lowlink = root->sons[i]->lowlink;
            }
        }
        else
        {
            if (root->lowlink > root->sons[i]->index)
            {
                root->lowlink = root->sons[i]->index;
            }
        }
    }

    if (root->lowlink == root->index && ltscc <= root->index)
    {
        // found a tscc
        ltscc = root->index;   // index/lowlink of the last tscc
        if (last_unknown > root->index)
        {
            global_unknown = true;
        }
        else if (last_true <= root->index)
        {
            return root;
        }
    }
    return NULL;
}

NetState *CoverGraph::getWitnessState(CoverPayload *pay)
{
    // push the path on a stack (last element pushed is first transition to fire)
    std::vector<CoverPayload *> payloads;
    for (; pay->parent; pay = pay->parent)
    {
        payloads.push_back(pay);
    }

    // compute the state we reach after the path
    NetState *ns = new NetState(start);
    while (!payloads.empty())
    {
        // create the necessary omegas
        for (arrayindex_t i = 0; i < payloads.back()->card_new_omegas; ++i)
        {
            CoverGraph::createOmega(*ns, payloads.back()->new_omegas[i][0]);
        }

        // fire the transition
        CoverGraph::fire(*ns, payloads.back()->transition);

        // update enabledness
        Transition::updateEnabled(*ns, payloads.back()->transition);

        // move on to next payload
        payloads.pop_back();
    }
    return ns;
}

