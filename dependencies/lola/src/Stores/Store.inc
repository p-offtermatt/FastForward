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
\author Niels
\author Max Görner
\author Christian Koch
\status new
*/

#include <Core/Dimensions.h>
#include <Core/Runtime.h>
#include <Stores/EmptyStore.h>
#include <Core/Runtime.h>

template <typename T>
char *  Store<T>:: check_abortion()
{
	// NULL =  no abortion
	// !=NULL = abortion + abort message
        if (RT::args.markinglimit_given
                and get_number_of_markings() > (unsigned)RT::args.markinglimit_arg)
        {
	    char * text = new char[strlen("marking limit reached - aborting")+1];
	    strcpy(text,"marking limit reached - aborting");
            return(text);
        }
	else
	{
		return NULL;
	}
	
}

template <typename T>
void *Store<T>::reporter_internal(void)
{
    //static unsigned int intervals = 0;
    //uint64_t last_markings = 0;
//
    //while (true)
    //{
        //sleep(REPORT_FREQUENCY);
//
        //const uint64_t last_period = get_number_of_markings() - last_markings;
        //const unsigned int time_elapsed = intervals * REPORT_FREQUENCY;
        //last_markings = get_number_of_markings();
        //++intervals;
//
        //// correct the number of calls to avoid integer overflows
        //uint64_t calls = get_number_of_calls();
        //if (calls > 0)
        //{
            //--calls;
        //}
//
        //if (RT::args.search_arg == search_arg_findpath)
        //{
            //RT::rep->status("%10llu tries, %10llu fired transitions, %5d secs", ((EmptyStore<T> *) this)->tries,
                            //calls, time_elapsed);
        //}
        //else
        //{
            //RT::rep->status("%10llu markings, %10llu edges, %8.0f markings/sec, %5d secs",
                            //get_number_of_markings(),
                            //calls, (last_period / (float)REPORT_FREQUENCY), time_elapsed);
//
            //// report probability of a false positive in the Bloom filter
            //if (RT::args.store_arg == store_arg_bloom)
            //{
                //const double n = static_cast<double>(get_number_of_markings());
                //static const double k = static_cast<double>(RT::args.hashfunctions_arg);
                //static const double m = static_cast<double>(BLOOM_FILTER_SIZE);
                //RT::rep->status("%10llu hash table size      false positive probability: %.10lf",
                                //BLOOM_FILTER_SIZE, pow((1.0 - exp((-k * n) / m)), k));
            //}
        //}
//
        //// early abortion
        //if (RT::args.markinglimit_given
                //and get_number_of_markings() > (unsigned)RT::args.markinglimit_arg)
        //{
            //RT::rep->status(RT::rep->markup(MARKUP_IMPORTANT, "marking limit reached - aborting").str());
            //kill(getpid(), SIGUSR2);
        //}
    //}
//
    //// line is never reached, but silences the compiler
    return NULL;
}

template <typename T>
void *Store<T>::reporter_helper(void *context)
{
    // required indirection to mix C/C++ - see http://stackoverflow.com/questions/1151582/pthread-function-from-a-class for more information
    return ((Store *)context)->reporter_internal();
}

template <typename T>
Store<T>::Store(threadid_t _number_of_threads) :
    number_of_threads(_number_of_threads), markings(0), calls(0)
{
    calls = new uint64_t[number_of_threads]();
    markings = new uint64_t[number_of_threads]();
//    const int ret = pthread_create(&reporter_thread, NULL, reporter_helper, this);
//    if (UNLIKELY(ret != 0))
//    {
//        RT::rep->status("thread could not be created");
//        RT::rep->abort(ERROR_THREADING);
//    }
}

template <typename T>
void Store<T>::silence()
{
    void *void_pointer;
//    int ret = pthread_cancel(reporter_thread);
//    ret |= pthread_join(reporter_thread, &void_pointer);
//    if (LIKELY(ret == 0))
//    {
//        RT::rep->status("killed reporter thread");
//    }
}

template <typename T>
Store<T>::~Store()
{
    silence();
    delete[] calls;
    delete[] markings;
}

template <typename T>
uint64_t Store<T>::get_number_of_markings()
{
    uint64_t result = 0;
    for (uint32_t thread_number = 0; thread_number < number_of_threads; thread_number++)
    {
        result += markings[thread_number];
    }
    return result;
}

template <typename T>
uint64_t Store<T>::get_number_of_calls()
{
    uint64_t result = 0;
    for (uint32_t thread_number = 0; thread_number < number_of_threads; thread_number++)
    {
        result += calls[thread_number];
    }
    // First call is for initial state, it does not count for number of fired transitions....
    if(result) 
    {
	--result;
    }
    return result;
}

template <typename T>
bool Store<T>::popState(NetState &)
{
    RT::rep->status("popState method not yet implemented for this store");
    RT::rep->abort(ERROR_COMMANDLINE);

    // line is never reached, but silences the compiler
    return false;
}
