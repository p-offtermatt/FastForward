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
#include <Symmetry/Constraints.h>


/*!
\brief the verification task

An empty Task is a task that does nothing. It implements the --check=none
option.

*/

class EmptyTask : public Task
{
public:
    ~EmptyTask(){};

    ternary_t getResult() { 

		// compute symmetries, just for fun...
		if (RT::args.symmetry_given)
		    {
			SymmetryCalculator *SC = NULL;
			SC = new SymmetryCalculator();
			assert(SC);
			SC->ComputeSymmetries();
			delete SC;
		    }

		
		return TERNARY_UNKNOWN;}
    /// interprete and display the result
    void interpreteResult(ternary_t result) {}
    // the following will never be called
    // LCOV_EXCL_START
    /// return the witness path
    Path getWitnessPath() {assert(false); return * (new Path);}
    // LCOV_EXCL_STOP

    /// return the target marking
    capacity_t *getMarking() {assert(false); return NULL;}

    /// return the number of stored markings
    void getStatistics() {}
    static Task * buildTask() { return new EmptyTask();}
    char * getStatus(uint64_t){return NULL;}
    char * early_abortion(){return NULL;}
};
