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

\brief Definition of a class to organize witness paths.
*/

#pragma once

#include <Core/Dimensions.h>

class JSON;

/*!
\brief a witness path / couterexample

This class is used as unified data structure for all verification tasks to store
witness paths (if applicable). Thereby, a witness path is a sequence of
transition indices. Furthermore, cycles are supported to model lasso paths.

\note The object is initially uninitialized (modeled by member variable initialized).
*/
class Path
{
public:
    Path();

    /// print witness path to given file
    void print(FILE *const) const;

    /// print witness path as distributed run to given file
    void printRun(FILE *const) const;

    /// return the path in JSON format
    JSON json() const;

    /// add a transition to the path
    void addTransition(const arrayindex_t transition, bool add_to_front = false);

    /// mark the begin of a cycle on the path
    void beginCycle(bool add_to_front = false);

    /// mark the end of a cycle on the path
    void endCycle(bool add_to_front = false);

    /// whether the path was initialized
    bool initialized;

private:
    /// the transition indices of the path
    std::list<arrayindex_t> transition_sequence;
};
