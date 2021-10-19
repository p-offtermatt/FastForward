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
\author Christian Koch
\status new

\brief VectorStore implementation using binary suffix trees. Based on BinStore.
Relies on the assumption that different input vectors (possibly of different
length) are not prefix of another.
*/

#include <Stores/VectorStores/PrefixTreeStore.h>

template<>
size_t PrefixTreeStore<void>::getPayloadSize()
{
    return 0;
}
