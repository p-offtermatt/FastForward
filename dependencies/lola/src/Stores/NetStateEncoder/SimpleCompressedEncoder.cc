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
based on the observation that small marking counts occur much more frequently
than large ones.
*/

#include <Core/Dimensions.h>
#include <Net/Marking.h>
#include <Net/Net.h>
#include <Net/NetState.h>
#include <Net/Place.h>
#include <Stores/NetStateEncoder/SimpleCompressedEncoder.h>

SimpleCompressedEncoder::SimpleCompressedEncoder(int numThreads) : NetStateEncoder(numThreads)
{
    // calculate worst case input vector length (assuming that all markings need maximal length)
    insize = ((2 * Place::CardSignificant * PLACE_WIDTH + VECTOR_WIDTH - 1) / VECTOR_WIDTH) * sizeof(
                 vectordata_t);

    // allocate auxiliary input vectors.
    inputs = new vectordata_t *[numThreads];
    for (int i = 0; i < numThreads; i++)
    {
        inputs[i] = new vectordata_t[insize];
    }
}

SimpleCompressedEncoder::~SimpleCompressedEncoder()
{
    // free auxiliary input vectors.
    for (int i = 0; i < numThreads; i++)
    {
        delete[] inputs[i];
    }
    delete[] inputs;
}

void SimpleCompressedEncoder::addToInput(capacity_t val, bitarrayindex_t numbits,
        vectordata_t *&pInput,
        bitarrayindex_t &input_bitstogo)
{
    // repeat as long as there are bits to copy (may happen when remaining bits of current input vector word are not sufficient to hold all the bits)
    while (numbits)
    {
        // fill all bits into the current input vector word (as long as there are enough bits).
        // uses preprocessor to optimize operation (since this is the most time consuming step)
#if PLACE_WIDTH >= VECTOR_WIDTH
        *pInput |= vectordata_t((capacity_t(val << (PLACE_WIDTH - numbits)) >>
                                 (PLACE_WIDTH - input_bitstogo)));
#else
        *pInput |= vectordata_t(vectordata_t(val) << (VECTOR_WIDTH - numbits)) >>
                   (VECTOR_WIDTH - input_bitstogo);
#endif
        if (input_bitstogo <= numbits)
        {
            // current input vector word is full, continue with the next one.
            numbits -= input_bitstogo;
            pInput++;
            input_bitstogo = VECTOR_WIDTH;
        }
        else
        {
            // there are bits left in the current input vector word.
            input_bitstogo -= numbits;
            return;
        }
    }
}

vectordata_t *SimpleCompressedEncoder::encodeNetState(NetState &ns, bitarrayindex_t &bitlen,
        arrayindex_t threadIndex)
{
    // initialize number of bits. Will be increased as bits are added to the input vector.
    bitlen = 0;

    // fetch input vector for this thread
    vectordata_t *pCurThreadInput = inputs[threadIndex];

    // current position in input vector (will move forward as the vector is filled with data)
    vectordata_t *pInput = pCurThreadInput;
    // number of bits not yet filled with data.
    bitarrayindex_t input_bitstogo = VECTOR_WIDTH;

    // clear input vector (binary or operations will be used in the upcoming steps)
    memset(pCurThreadInput, 0, insize);

    // process each marking value sequentially and individually.
    for (arrayindex_t place_index = 0; place_index < Place::CardSignificant; place_index++)
    {
        // current marking value to store
        capacity_t curMarking = ns.Current[place_index];

        //Calculating number of needed bits (i.e., the number n of the code word)
        arrayindex_t prefix_length = 1; // this is n
        capacity_t num_elems_with_prefix = 2; // number of different code words of length 2n
        // curMarking is now interpreted as the number of values that are smaller than than the current marking value and still need to be assigned to a code word.
        // As long as there are at least as many smaller values as different code words with current prefix length n, some of the smaller values are mapped to the code words of length n (and are therefore subtracted from curMarking) and n is incremented.
        while (curMarking >= num_elems_with_prefix)
        {
            prefix_length++, curMarking -= num_elems_with_prefix, num_elems_with_prefix <<= 1;
        }

        // write prefix (n bits, (n-1) 0s and one 1).
        addToInput(1, prefix_length, pInput, input_bitstogo);
        // write payload (curMarking, exactly n bits)
        addToInput(curMarking, prefix_length, pInput, input_bitstogo);
        // increase bitlen by 2n
        bitlen += prefix_length << 1;
    }
    return pCurThreadInput;
}
