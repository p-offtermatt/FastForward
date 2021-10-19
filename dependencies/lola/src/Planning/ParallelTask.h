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

#pragma once

#include <Core/Dimensions.h>
#include <Witness/Path.h>
#include <Planning/Task.h>

/*!
\brief the verification task

This class provides a task that consists of the parallel composition of
two other tasks. It can be used, e.g. for combining a structural with a
state space method.

 */

class ParallelTask : public Task {
public:
    ternary_t winner;
    Task * first;
    Task * second;
    pthread_t SubTask[2];
    pthread_mutex_t subtaskmutex;
    pthread_cond_t subtaskcondition;
    bool has_finished[2];
    ternary_t subresult[2];
    ParallelTask(Task* f, Task *s);
    ~ParallelTask()
    {
	// cannot delete tasks since threads may already be killed
        //delete first;
        //delete second;
    }

    /// run the actual verification algorithm
    ternary_t getResult();
    /// interprete and display the result
    void interpreteResult(ternary_t result)
    {
        if (winner == TERNARY_FALSE)
        {
            first -> interpreteResult(result);
        }
        else
        {
            second -> interpreteResult(result);
        }
    }

    /// return the witness path
    virtual Path getWitnessPath()
    {
        if (winner == TERNARY_FALSE)
        {
            return first -> getWitnessPath();
        }
        else
        {
            return second -> getWitnessPath();
        }

    }
    /// return the target marking
    virtual capacity_t *getMarking()
    {
        if (winner == TERNARY_FALSE)
        {
            return first -> getMarking();
        }
        else
        {
            return second -> getMarking();
        }

    }

    /// return the number of stored markings
    virtual void getStatistics()
    {
        if (winner == TERNARY_FALSE)
        {
            first -> getStatistics();
        }
        else
        {
            second -> getStatistics();
        }

    }
    static ParallelTask * buildTask(Task * f, Task * s)
    {
        return new ParallelTask(f, s);
    }

    char * getStatus(uint64_t);
    char * early_abortion()
    {
        char * c = first->early_abortion();
        if (c)
        {
            return c;
        }
        return second -> early_abortion();
    }
};
