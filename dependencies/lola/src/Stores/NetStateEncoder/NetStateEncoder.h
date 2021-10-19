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

\brief NetStateEncoder provides an interface for converting NetState markings
into input vectors.
*/

#pragma once

#include <Core/Dimensions.h>

class NetState;

class NetStateEncoder
{
protected:
    /// the maximum number of threads this NetStateEncoder has to work with.
    /// The value is used to create thread-local auxiliary data structures if needed.
    int numThreads;

public:
    /// constructor; initialize auxiliary data structures if needed.
    /// @param _numThreads maximum number of threads that may work with this NetStateEncoder concurrently.
    explicit NetStateEncoder(int _numThreads) : numThreads(_numThreads) {}

    /// destructor; frees all memory used for auxiliary data structures
    virtual ~NetStateEncoder() {}

    /// converts the given NetState marking into an input vector.
    /// @param ns input: NetState that needs to be converted
    /// @param bitlen output: reference to length of input vector (in bits). Will be filled by the method.
    /// @param threadIndex input: the index of the thread that requests this call. Values will range from 0 to (numThreads - 1). Used to allow using thread-local auxiliary data structures without locking any variables.
    /// @return the final input vector.
    virtual vectordata_t *encodeNetState(NetState &ns, bitarrayindex_t &bitlen,
                                         arrayindex_t threadIndex) = 0;

    /// decodes a given encoded state and sets the netstate appropriately
    /// @param ns NetState the decoded state will be written to
    /// @param data vector to be decoded
    virtual void decodeNetState(NetState &ns, vectordata_t *data);
};
