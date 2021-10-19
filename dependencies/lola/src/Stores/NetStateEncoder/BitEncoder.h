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

\brief NetStateEncoder implementation that copies the marking bit-perfect with
no compression (analogous to BitStore).
*/

#pragma once

#include <Core/Dimensions.h>
#include <Stores/PluginStore.h>

class BitEncoder : public NetStateEncoder
{
public:
    /// constructor; initialize auxiliary data structure (namely "inputs").
    /// @param numThreads maximum number of threads that may work with this NetStateEncoder concurrently.
    explicit BitEncoder(int numThreads);

    /// destructor; frees all memory used for auxiliary data structure (namely "inputs").
    ~BitEncoder();

    vectordata_t *encodeNetState(NetState &ns, bitarrayindex_t &bitlen, arrayindex_t threadIndex);
private:
    /// vector of input vectors that are returned from encodeNetState. Each thread has its own vector to avoid conflicts.
    vectordata_t **inputs;

    /// number of words in input vector (fixed for all threads)
    arrayindex_t insize;

    /// memcpy can be used only if
    /// - input and suffix tree vectors use the same data type
    /// - the alignment is right (offset is 0 bits)
    /// - all bits are significant for the used places
    ///
    /// to simplify, memcpy is used only for the first couple of places where all conditions are met. memcpylen states the number of such leading places
    arrayindex_t memcpylen;
};

