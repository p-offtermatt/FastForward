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

This class provides a task that consists of the sequential composition of
two other tasks. It can be used, e.g. for combining a structural with a
state space method.

*/

class SequentialTask: public Task
{
public:
     bool second_started;
     ternary_t intermediate;
     Task * first;
     Task * second;
     SequentialTask(Task* f, Task *s){ first = f; second = s; second_started = false;
                                        intermediate = TERNARY_UNKNOWN;}
    ~SequentialTask(){delete first; delete second;}

    /// run the actual verification algorithm
    ternary_t getResult() {
	intermediate = first -> getResult();
	if(intermediate != TERNARY_UNKNOWN)
	{
		return intermediate;
	}
 	first -> getStatistics();
	second_started = true;
	return second -> getResult();
    }

    /// interprete and display the result
    void interpreteResult(ternary_t result){
	if(!second_started)
	{
		first -> interpreteResult(result);
	} 
	else
	{
		second -> interpreteResult(result);
	}
    }

    /// return the witness path
    virtual Path getWitnessPath() {
	if(!second_started)
	{
		return first -> getWitnessPath();
	} 
	else
	{
		return second -> getWitnessPath();
	}
	
    }
    /// return the target marking
    virtual capacity_t *getMarking() {
	if(!second_started)
	{
		return first -> getMarking();
	} 
	else
	{
		return second -> getMarking();
	}
	
    }

    /// return the number of stored markings
    virtual void getStatistics() {
	if(!second_started)
	{
		first -> getStatistics();
	} 
	else
	{
		second -> getStatistics();
	}
	
    }

    static SequentialTask * buildTask(Task * f,Task * s) {return new SequentialTask(f,s);}
    char * getStatus(uint64_t elapsed){ if(!second_started)
			{ 
				return first -> getStatus( elapsed);
			}
			else
			{ 
				return second -> getStatus( elapsed);
			}
	}
    char * early_abortion(){if(!second_started)
				{	
					return first->early_abortion();
				}
				return second->early_abortion();
			   }
};
