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
#include <Exploration/CTLExploration.h>

struct CTLFormula;

/*!
\brief the verification task

This class collects all information for executing a CTL model checking
procedure.

*/

class CTLTask: public Task
{
public:
    CTLFormula * ctlFormula;
    Store<void *> *ctlStore;
    Firelist * fl;
    NetState * ns;
    CTLExploration * ctlExploration;
    CTLTask();
    ~CTLTask();

    /// run the actual verification algorithm
    ternary_t getResult();
    /// interprete and display the result
    void interpreteResult(ternary_t result);

    /// return the witness path
    Path getWitnessPath() ;
    /// return the target marking
    capacity_t *getMarking() ;

    /// return the number of stored markings
    void getStatistics();
    static Task * buildTask(){ return new CTLTask();}
    char * getStatus(uint64_t);
    uint64_t previousNrOfMarkings;
    char * early_abortion(){return ctlStore->check_abortion();}
     bool goStatus;
};
