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

\brief property to find deadlocks
*/

#include <Core/Dimensions.h>
#include <Net/NetState.h>
#include <Exploration/DeadlockExploration.h>

bool DeadlockExploration::initProperty(NetState &ns)
{
    return !ns.CardEnabled;
}

bool DeadlockExploration::checkProperty(NetState &ns, arrayindex_t)
{
    return !ns.CardEnabled;
}

bool DeadlockExploration::updateProperty(NetState &, arrayindex_t)
{
    return false;
}

SimpleProperty *DeadlockExploration::copy()
{
    return new DeadlockExploration();
}
