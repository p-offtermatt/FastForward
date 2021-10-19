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
\author Karsten
\status approved 18.04.2012

\brief derives deadlock checking from SimpleProperty
*/

#pragma once

#include <Exploration/SimpleProperty.h>
#include <Witness/Path.h>

class Firelist;

/// derives deadlock checking from SimpleProperty
class DeadlockExploration : public SimpleProperty
{
public:
    DeadlockExploration() {}
    Path _p;

private:
    /// prepare for search
    virtual bool initProperty(NetState &ns);

    /// check property in Marking::Current, use after fire. Argument is transition just fired.
    virtual bool checkProperty(NetState &ns, arrayindex_t);

    /// check property in Marking::Current, use after backfire. Argument is transition just backfired.
    bool updateProperty(NetState &, arrayindex_t);

    /// copy contructor for use in parallel evaluation
    virtual SimpleProperty *copy();
};
