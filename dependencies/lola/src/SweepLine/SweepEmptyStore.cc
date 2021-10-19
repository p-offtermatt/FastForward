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

/*
\file
\author Harro
\status new

\brief List of Stores for persistent markings in the SweepLine method

This class realizes one store in a list of stores, sorted by progress
measure and containing known persistent markings, either old or new ones.
*/

#include <SweepLine/SweepEmptyStore.h>

/*!
 * \brief Constructor for one store in a list of stores
 */
SweepEmptyStore::SweepEmptyStore() : EmptyStore<void>(1)
{
}

/*!
 * \brief Set the total number of markings
 * \param count  The number of markings
 */
void SweepEmptyStore::setMarkings(int64_t count)
{
    this->markings[0] = count;
}

/*!
 * \brief Set the total number of calls
 * \param count  The number of calls
 */
void SweepEmptyStore::setCalls(int64_t count)
{
    this->calls[0] = count;
}

