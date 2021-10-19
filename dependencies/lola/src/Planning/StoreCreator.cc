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

#include <config.h>
#include <Core/Dimensions.h>
#include <CoverGraph/CoverPayload.h>
#include <Planning/StoreCreator.h>
#include <InputOutput/InputOutput.h>
#include <Stores/Store.h>
#include <SweepLine/SweepEmptyStore.h>
#include <Symmetry/Symmetry.h>

/*!
\ingroup g_globals
\todo Is this mapping actually needed or was it this just added for debugging
purposes.
*/


/// special store creation for stores without payload support (e.g. BloomStore)
template<>
Store<void> *StoreCreator<void>::createSpecializedStore(threadid_t number_of_threads)
{
    NetStateEncoder *enc = 0;
    switch (RT::args.encoder_arg)
    {
    case encoder_arg_bit:
        enc = new BitEncoder(number_of_threads);
        break;
    case encoder_arg_copy:
        enc = new CopyEncoder(number_of_threads);
        break;
    case encoder_arg_simplecompressed:
        enc = new SimpleCompressedEncoder(number_of_threads);
        break;
    case encoder_arg_fullcopy:
        enc = new FullCopyEncoder(number_of_threads);
        break;
    case encoder__NULL:
        assert(false);
    }

    Store<void> * st;
    switch (RT::args.store_arg)
    {
    case store_arg_bloom:
        RT::rep->status("using Bloom filter with %lu bit (%lu MB)", (unsigned long)
                        BLOOM_FILTER_SIZE, BLOOM_FILTER_SIZE / 1048576);
        RT::rep->status("using Bloom filter with %d hash functions (%s)",
                        RT::args.hashfunctions_arg,
                        RT::rep->markup(MARKUP_PARAMETER, "--hashfunctions").str());

        if (RT::args.bucketing_given)
        {
            st = new PluginStore<void>(enc,
                                         new HashingWrapperStore<void>(new BinaryVectorStoreCreator < void,
                                                 VBloomStore < (unsigned long)  BLOOM_FILTER_SIZE / SIZEOF_MARKINGTABLE + 1 > , arrayindex_t,
                                                 size_t > (number_of_threads, RT::args.hashfunctions_arg)), number_of_threads);
        }
        else
        {
            st = new PluginStore<void>(enc,
                                         new VBloomStore<BLOOM_FILTER_SIZE>(number_of_threads,
                                                 RT::args.hashfunctions_arg), number_of_threads);
        }
	break;

    default:
        storeCreationError();
        return NULL;
    }
 // symmetry reduction: wrap created store
        if (RT::args.symmetry_given)
        {
            // wrap current store in CycleStore object
            st = new SymmetryStore<void>(number_of_threads, st);
        }
        return st;

}

