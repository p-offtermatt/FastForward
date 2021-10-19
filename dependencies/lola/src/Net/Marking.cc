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
\status approved 27.01.2012

\brief Global data for marking specific information

All data that describe attributes of markings can be found here.
*/

#include <Core/Dimensions.h>
#include <Net/Marking.h>
#include <Net/Net.h>

capacity_t *Marking::Initial = NULL;
hash_t Marking::HashInitial = 0;
capacity_t *Marking::Current = NULL;
hash_t Marking::HashCurrent = 0;

/// collection of information related to markings

/*!
 * Free the Memory of Initial and Current Marking
 \pre Memory for Marking::Initial and Marking::Current was allocated in ParserPTNet::symboltable2net()
 \post Marking::Initial and Marking::Current are freed
 */
void Marking::deleteMarkings()
{
    // memory for initial and current marking is allocated in ParserPTNet::symboltable2net()
    delete[] Marking::Initial;
    delete[] Marking::Current;
}

// LCOV_EXCL_START
void Marking::DEBUG__printMarking(capacity_t *marking)
{
    printf("PRINTING MARKING %p:\n", reinterpret_cast<void *>(marking));
    for (arrayindex_t i = 0; i < Net::Card[PL]; i++)
    {
        printf("\t%s:%d\n", Net::Name[PL][i], marking[i]);
    }
    printf("==============\n");
}
// LCOV_EXCL_STOP
