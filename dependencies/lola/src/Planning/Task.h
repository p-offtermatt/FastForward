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
#include <Net/Net.h>


/*!
\brief the verification task

This class hierarchy encapsulates all information on how to approach the given
problem. An object is built after analysis of the given formula. 
Construction is controlled by several command line parameters.
The idea is that, in the end, there is one task to be executed. However,
some subclasses can wrap several subtasks to executed in parallel or
sequentielly. This way, a property can be explored by combining incomplete
(e.g. structural or findpath) with complete methods. 
The task object is configured and evaluated in the main() method.

In the lifecycle of a task object, the methods will be called
in the order of their specification below.

*/

class Task
{
public:
    virtual ~Task(){}

    /// run the actual verification algorithm
    virtual ternary_t getResult() = 0;
    /// interprete and display the result
    virtual void interpreteResult(ternary_t result) = 0;

    /// return the witness path
    virtual Path getWitnessPath() = 0;
    /// return the target marking
    virtual capacity_t *getMarking() = 0;

    /// return the number of stored markings
    virtual void getStatistics() = 0;
    static Task * buildTask();
    static threadid_t number_of_threads;
    virtual char * getStatus(uint64_t) = 0;
    virtual char * early_abortion() = 0;

 	static void setFormula();
        static void outputFormulaAsProcessed();
};
