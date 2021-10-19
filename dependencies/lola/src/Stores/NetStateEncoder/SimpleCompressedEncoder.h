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
\author Max Görner
\status new

\brief NetStateEncoder implementation that offers a simple form of compression,
based on the observation that small marking values occur much more frequently
than large ones.
*/

#pragma once

#include <Core/Dimensions.h>
#include <Stores/PluginStore.h>

/// The marking values are converted based on a fixed prefix encoding.
/// Each code word consists of (n-1) 0s, followed by a 1, followed by n arbitrary bits (resulting in a total length of 2n bits).
///
/// Example: For n=1 the two possible code words are 10 and 11, for n=2 the four possible code words are 0100, 0101, 0110 and 0111.
///
/// marking values are assigned to the code words in ascending order, so that small numbers are mapped to short code words.
/// For example, the first few mappings are as follows:
/// - 0 => 10
/// - 1 => 11
/// - 2 => 0100
/// - 3 => 0101
/// - 4 => 0110
/// - 5 => 0111
/// - 6 => 001000
/// - 7 => 001001
/// - and so on.

class SimpleCompressedEncoder : public NetStateEncoder
{
public:
    /// constructor; initialize auxiliary data structure (namely "inputs").
    /// @param numThreads maximum number of threads that may work with this NetStateEncoder concurrently.
    explicit SimpleCompressedEncoder(int numThreads);

    /// destructor; frees all memory used for auxiliary data structure (namely "inputs").
    ~SimpleCompressedEncoder();

    vectordata_t *encodeNetState(NetState &ns, bitarrayindex_t &bitlen, arrayindex_t threadIndex);
private:
    /// adds the numbits least significant bits of val to the input vector
    void addToInput(capacity_t val, bitarrayindex_t numbits, vectordata_t *&pInput,
                    bitarrayindex_t &input_bitstogo);

    /// vector of input vectors that are returned from encodeNetState. Each thread has its own vector to avoid conflicts.
    vectordata_t **inputs;

    /// maximum number of words necessary in input vector (fixed for all threads)
    arrayindex_t insize;
};

