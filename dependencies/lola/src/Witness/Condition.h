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

\brief Definition of a class to store conditions of a distributed run.
*/

#pragma once

#include <Core/Dimensions.h>

class Event;

/*
\brief a condition of a distributed run
\todo Move the code of this class to a class modeling distributed runs.
*/
class Condition
{
private:
    /// all conditions of the distributed run
    static std::vector<Condition *> conditions;

    /// the place this condition is associated to
    const arrayindex_t place;

public:
    /// the last stored condition for a given place
    static std::map<arrayindex_t, Condition *> current;

    /// the input event of this condition
    Event *in;
    /// the output event for this condition
    Event *out;

    /// construct a condition given a place and an optional input event
    Condition(const arrayindex_t, Event * = NULL);

    /// print the conditions in Graphviz dot notation
    static void dot(FILE *const);
};
