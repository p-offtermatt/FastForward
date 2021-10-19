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

#include <Core/Dimensions.h>
#include <Core/Runtime.h>
#include <Witness/Path.h>
#include <Planning/Task.h>
#include <Planning/ParallelTask.h>
#include <Core/Handlers.h>

/*!
\brief the verification task

This class handles parallel execution of subtasks
 */


// the body of the subtask threads

void * executeSubTask(void * arg)
{
    int last_state, last_type;
    pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, &last_type);
    pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, &last_state);
    // extract argument = parent
    ParallelTask * parent = reinterpret_cast<ParallelTask *> (arg);

    // find out which subtask I am
    int my_index = 0;
    if (pthread_equal(parent -> SubTask[1], pthread_self()) != 0)
    {
        // I am right task
        my_index = 1;
    }
    else
    {
        // I am left task
        assert(pthread_equal(parent -> SubTask[0], pthread_self()) != 0);
    }

    // actual subtask execution
    ternary_t result = my_index ? parent -> second -> getResult() :
            parent -> first -> getResult();

    // announce result to main task

    // lock mutex
    pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &last_state);
    pthread_mutex_lock(&(parent -> subtaskmutex));

    // change result variables
    parent -> has_finished[my_index] = true;
    parent -> subresult[my_index] = result;

    // check condition and signal parent
    if ((parent -> has_finished[0] && parent -> has_finished[1])
            || (parent -> subresult[0] != TERNARY_UNKNOWN) 
            || (parent->subresult[1] != TERNARY_UNKNOWN))
    {
        pthread_cond_signal(&(parent -> subtaskcondition));
    }

    pthread_mutex_unlock(&(parent -> subtaskmutex));
    pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, &last_state);

    // finish
    sleep(100000);
    return NULL;
}

// getResult from parent task
ternary_t ParallelTask::getResult()
{
    bool bool_result(false);
    ternary_t result(TERNARY_UNKNOWN);

    has_finished[0] = has_finished[1] = false;
    subresult[0] = subresult[1] = TERNARY_UNKNOWN;

    // init mutex & condition
    pthread_mutex_init(&subtaskmutex, NULL);
    pthread_cond_init(&subtaskcondition, NULL);

    // start threads for subtasks
    const int ret1 = pthread_create(&(SubTask[0]), NULL, executeSubTask,
            reinterpret_cast<void *> (this));
    const int ret2 = pthread_create(&(SubTask[1]), NULL, executeSubTask,
            reinterpret_cast<void *> (this));
    if (UNLIKELY(ret1 != 0 || ret2 != 0))
    {
        RT::rep->status("thread could not be created");
        RT::rep->abort(ERROR_THREADING);
    }

    // wait for reply by subtasks
    pthread_mutex_lock(&subtaskmutex);
    if (!((has_finished[0] && has_finished[1])
            || subresult[0] != TERNARY_UNKNOWN 
            || subresult[1] != TERNARY_UNKNOWN))
    {
        pthread_cond_wait(&subtaskcondition, &subtaskmutex);
    }
    // arriving here means that exit condition is satisfied.
    assert((has_finished[0] && has_finished[1])
            || subresult[0] != TERNARY_UNKNOWN 
            || subresult[1] != TERNARY_UNKNOWN);
    
    // kill running threads
    for (int i = 0; i < 2; i++)
    {
        if (!has_finished[i])
        {
            pthread_cancel(SubTask[i]);
        }
    }
    pthread_mutex_unlock(&subtaskmutex);

    // kill sara if running
    if (RT::saraPID > 0)
    {
        kill(RT::saraPID, SIGKILL);
    }

    // determine result
    if (subresult[0] == TERNARY_UNKNOWN)
    {
        winner = TERNARY_TRUE;
        return subresult[1];
    }
    winner = TERNARY_FALSE;
    return subresult[0];
}

ParallelTask::ParallelTask(Task * f, Task * s)
{
    first = f;
    second = s;
    has_finished[0] = has_finished[1] = false;
    subresult[0] = subresult[1] = TERNARY_UNKNOWN;
}

char * ParallelTask::getStatus(uint64_t elapsed)
{
    char * r1 = first->getStatus(elapsed);
    char * r2 = second->getStatus(elapsed);
    if (!r1) return r2;
    if (!r2) return r1;
    sprintf(r1 + strlen(r1), " || %s", r2);
    delete[] r2;
    return r1;
}
