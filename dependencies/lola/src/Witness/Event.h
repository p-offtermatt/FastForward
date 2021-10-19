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
\author Niels
\status new

\brief Definition of a class to store events of a distributed run.
*/

#pragma once

#include <Core/Dimensions.h>

/*!
\brief an event of a distributed run

\note This class assumes safe Petri nets!
\todo Move the code of this class to a class modeling distributed runs.
*/
class Event
{
private:
    /// all events of the distributed run
    static std::vector<Event *> events;

    /// the last stored event for a given transition
    static std::map<arrayindex_t, Event *> current;

    /// the transition this event is associated to
    const arrayindex_t transition;

public:
    /// whether this event is associated with a target transition
    bool target;

    /// create an event given a transition
    explicit Event(const arrayindex_t);

    /// print the events in Graphviz dot notation
    static void dot(FILE *const);
};
