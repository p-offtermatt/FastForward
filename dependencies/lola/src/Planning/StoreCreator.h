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

#pragma once

#include <Core/Dimensions.h>
#include <Core/Runtime.h>
#include <Stores/CompareStore.h>
#include <Stores/CycleStore.h>
#include <Stores/EmptyStore.h>
#include <Stores/NetStateEncoder/BitEncoder.h>
#include <Stores/NetStateEncoder/CopyEncoder.h>
#include <Stores/NetStateEncoder/FullCopyEncoder.h>
#include <Stores/NetStateEncoder/SimpleCompressedEncoder.h>
#include <Stores/PluginStore.h>
#include <Stores/Store.h>
#include <Stores/SymmetryStore.h>
#include <Stores/VectorStores/HashingWrapperStore.h>
#include <Stores/VectorStores/PrefixTreeStore.h>
#include <Stores/VectorStores/VBloomStore.h>
#include <Stores/VectorStores/VSTLStore.h>
#include <Symmetry/Constraints.h>
#include <Witness/Path.h>

// forward declarations
class AutomataTree;
class BuechiAutomata;
class LTLExploration;
struct CTLFormula;
class ChooseTransition;
class DFSExploration;
class TSCCExploration;
class Firelist;
class NetState;
class SimpleProperty;
class StatePredicate;
class CTLExploration;
struct CoverPayload;
class SymmetryCalculator;


/// generic store creator, basically a templatarized namespace for static creation methods.
/// Resolves the code duplication problem with using different templates in Task::setStore
template<typename T>
class StoreCreator
{
public:
    /// creates a new store based on the specified template and the command line arguments
    static Store<T> *createStore(threadid_t number_of_threads)
    {
	RT::rep->indent(-2);
	RT::rep->status("STORE");
	RT::rep->indent(2);
        // create an encode according --encoder
        NetStateEncoder *enc = NULL;
        switch (RT::args.encoder_arg)
        {
        case encoder_arg_bit:
        {
            RT::rep->status("using a bit-perfect encoder (%s)", RT::rep->markup(MARKUP_PARAMETER,
                            "--encoder=bit").str());
            RT::data["store"]["encoder"] = "bit-perfect";
            enc = new BitEncoder(number_of_threads);
            break;
        }

        case encoder_arg_copy:
        {
            RT::rep->status("using a copy encoder (%s)", RT::rep->markup(MARKUP_PARAMETER, "--encoder=copy").str());
            RT::data["store"]["encoder"] = "copy";
            enc = new CopyEncoder(number_of_threads);
            break;
        }

        case encoder_arg_simplecompressed:
        {
            RT::rep->status("using a simple compression encoder (%s)", RT::rep->markup(MARKUP_PARAMETER,
                            "--encoder=simplecompressed").str());
            RT::data["store"]["encoder"] = "simple compression";
            enc = new SimpleCompressedEncoder(number_of_threads);
            break;
        }

        case encoder_arg_fullcopy:
        {
            RT::rep->status("using a full copy encoder (%s)", RT::rep->markup(MARKUP_PARAMETER,
                            "--encoder=fullcopy").str());
            RT::data["store"]["encoder"] = "full copy";
            enc = new FullCopyEncoder(number_of_threads);
            break;
        }

        case encoder__NULL:
        {
            // cannot happen
            assert(false);
        }
        }

        // create a store according to --store
        Store<T> *st = NULL;

        switch (RT::args.store_arg)
        {
        case store_arg_comp:
        {
            RT::rep->status("using a compare store (%s)", RT::rep->markup(MARKUP_PARAMETER, "--store=comp").str());
            RT::data["store"]["type"] = "compare";
            st = new CompareStore<T>(
                new PluginStore<T>(enc, new PrefixTreeStore<T>(), number_of_threads),
                new PluginStore<T>(enc, new VSTLStore<T>(number_of_threads), number_of_threads),
                number_of_threads);
            break;
        }

        case store_arg_prefix:
        {
            RT::rep->status("using a prefix tree store (%s)", RT::rep->markup(MARKUP_PARAMETER, "--store=prefix").str());
            RT::data["store"]["type"] = "prefix";
            if (RT::args.bucketing_given)
            {
                st = new PluginStore<T>(enc, new HashingWrapperStore<T>(new
                                        NullaryVectorStoreCreator<T, PrefixTreeStore<T> >()), number_of_threads);
            }
            else
            {
                st = new PluginStore<T>(enc, new PrefixTreeStore<T>(), number_of_threads);
            }
            break;
        }

        case store_arg_stl:
        {
            RT::rep->status("using an STL store (%s)", RT::rep->markup(MARKUP_PARAMETER, "--store=stl").str());
            RT::data["store"]["type"] = "stl";
            if (RT::args.bucketing_given)
            {
                st = new PluginStore<T>(enc, new HashingWrapperStore<T>(new
                                        UnaryVectorStoreCreator<T, VSTLStore<T>, arrayindex_t>(number_of_threads)), number_of_threads);
            }
            else
            {
                st = new PluginStore<T>(enc, new VSTLStore<T>(number_of_threads), number_of_threads);
            }
            break;
        }

        case store_arg_bloom:
        {
            RT::rep->status("using a Bloom filter store (%s)", RT::rep->markup(MARKUP_PARAMETER,
                            "--store=bloom").str());
            RT::data["store"]["type"] = "bloom";
            st = createSpecializedStore(number_of_threads);
            break;
        }

        case store__NULL:
        {
            // cannot happen
            assert(false);
        }
        }

        // cycle reduction: wrap created store
        if (RT::args.cycle_given)
        {
            RT::rep->status("using the cycle reduction (%s)", RT::rep->markup(MARKUP_PARAMETER,
                            "--cycle").str());
            // wrap current store in CycleStore object
            st = new CycleStore<T>(number_of_threads, st, RT::args.cycleheuristic_arg);
        }

        // symmetry reduction: wrap created store
        if (RT::args.symmetry_given)
        {
            // wrap current store in CycleStore object
            st = new SymmetryStore<T>(number_of_threads, st);
        }

        return st;
    }

private:
    /// creates specialized stores for different payload types.
    /// the default implementation raises an error, but template specializations of it may create stores with restricted payload type compatibility.
    static Store<T> *createSpecializedStore(threadid_t)
    {
        storeCreationError();
        return NULL;
    }
    /// auxiliary method that raises an error. Will be called by the different specializations of createSpecializedStore.
    static void storeCreationError()
    {
        RT::rep->message(RT::rep->markup(MARKUP_BAD, "specified store does not fit the given task").str());
        RT::rep->abort(ERROR_COMMANDLINE);
    }
};

/// forward declaration of special store creation for stores without payload support (e.g. BloomStore)
template<>
Store<void> *StoreCreator<void>::createSpecializedStore(threadid_t number_of_threads);


