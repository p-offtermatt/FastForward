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

\brief NetStateEncoder implementation that copies the marking, while ignoring
capacity limitations. The copy operation isn't done at all if possible (just
passing the marking pointer), otherwise memcpy is used
*/

#pragma once

#include <Core/Dimensions.h>
#include <Stores/PluginStore.h>

class CopyEncoder : public NetStateEncoder
{
public:
    explicit CopyEncoder(int numThreads);
    ~CopyEncoder();

    vectordata_t *encodeNetState(NetState &ns, bitarrayindex_t &bitlen, arrayindex_t threadIndex);

private:
    /// number of words in input vector
    arrayindex_t insize;

    /// instead of copying the data, the marking pointer can be casted directly into the input vector pointer if:
    /// a) the marking bit width is divisble by the input vector data width
    ///    or
    /// b) the marking vector has at least as many bytes as the needed input vector
    /// (a) will be checked at compile time whereas (b) will be checked at runtime. Only if both tests fail memcpy is actually used.

    /// test codition (a). If the condition is met, these variables are not necessary.
#if SIZEOF_CAPACITY_T % SIZEOF_VECTORDATA_T != 0
    /// states whether condition (b) is met. Will be determined once in the constructor.
    bool nocopy;

    /// vector of input vectors that are returned from encodeNetState. Each thread has its own vector to avoid conflicts. Only needed if memcpy is actually used.
    vectordata_t **inputs;
#endif
};

