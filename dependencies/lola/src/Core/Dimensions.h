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
\ingroup g_runtime

\brief collection of arbitrarily chosen but fixed constants
*/

#pragma once

// the macro _CONFIGURING is set while executing the configure script to avoid
// inclusion of config.h before creating it
#ifndef _CONFIGURING
#include <config.h>

// make sure this bug will never happen again
#if SIZEOF_ARRAYINDEX_T == 0 || SIZEOF_MULT_T == 0 || SIZEOF_CAPACITY_T == 0 || SIZEOF_CARDBIT_T == 0 || SIZEOF_HASH_T == 0 || SIZEOF_FAIRNESSASSUMPTION_T == 0 || SIZEOF_SOCKADDR_IN == 0
#error The size of a user-defined data type could not be determined by the configure script.
#endif
#endif

/* C99 says to define __STDC_LIMIT_MACROS before including stdint.h,
 * if you want the limit (max/min) macros for int types. 
 */

#ifdef HAVE_STDINT_H
#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS 1
#endif
#include <stdint.h>
#endif

#ifdef HAVE_INDEX_T
#include <sys/types.h>
#endif

#include <algorithm>
#include <bitset>
#include <cassert>
#include <cctype>
#include <cerrno>
#include <climits>
#include <cmath>
#include <csignal>
#include <cstdarg>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <errno.h>
#include <fcntl.h>
#include <fstream>
#include <inttypes.h>
#include <iostream>
#include <libgen.h>
#include <limits>
#include <list>
#include <map>
#include <math.h>
#include <new>
#include <pthread.h>
#include <semaphore.h>
#include <set>
#include <signal.h>
#include <sstream>
#include <stack> 
#include <stdexcept>
#include <stdio.h>
#include <stdlib.h>
#include <streambuf>
#include <string>
#include <string.h>
#include <sys/resource.h>
#include <unistd.h>
#include <utility>
#include <vector>

#ifdef WIN32
#include <winsock.h>
#include <windows.h>
#include <stdint.h>
typedef uint32_t socklen_t;
#else
#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/socket.h>
#endif

// allow us to use "nullptr" everywhere
#include <cstddef>
#ifndef nullptr
#define nullptr NULL
#endif

/// number of entries in a symbol table
#define SIZEOF_SYMBOLTABLE 0x10000

/// number of entries in a hash table for markings
#define SIZEOF_MARKINGTABLE 0x100000

/// number of stack frames in one chunk of the search stack, better be power of two
#define SIZEOF_STACKCHUNK 0x1000

// Some useful constants, can be used as indices in arrays

typedef enum
{
    PL = 0,  ///< Index for "Places" in arrays
    TR = 1   ///< Index for "Transitions" in arrays
} node_t;

typedef enum
{
    PRE = 0,  ///< Index for "Pre Set" in arrays
    POST = 1  ///< Index for "Post Set" in arrays
} direction_t;

// a struct for saving the conflict clusters
typedef struct uf_node{
    int index;
    uf_node * parent;
    int type;
} uf_node_t;


// Some type definitions for dimensioning index structures

/// Type for node indices, limit the number of places/ transitions
typedef uint32_t arrayindex_t;

/// the maximal value for arrayindex_t (aka 32 bit unsigned integer)
#define ARRAYINDEX_T_MAX 4294967295UL

#ifndef INTMAX_MAX
/// the maximal value for 64 bit unsigned integers
#define INTMAX_MAX 9223372036854775807LL
#endif

/// type for numbering/counting the states of the petri net while processing
typedef uint32_t statenumber_t;

/// Type for bitvector indices, should be large enough for card(places) * log maxcapacity
typedef uint32_t bitarrayindex_t;

/// Type for multiplicities, limits the max. multiplicity at any arc
typedef uint16_t mult_t;

/// Type for capacities, limits the max. capacity of a place (to
/// sizeof(used type) -1, since max(type) is needed for representing
/// unboundedness in coverability graphs
typedef uint32_t capacity_t;
#define MAX_CAPACITY ((capacity_t)-1)

/// Type for hash values (32 bit plus enough space to calculate)
typedef int64_t hash_t;
#define MAX_HASH (1 << 31)

/// Type for the number of bits required to store a place's marking
typedef uint8_t cardbit_t;

/// type for thread ids and thread counts
typedef uint8_t threadid_t;

/// size of string buffer in Socket class
#define UDP_BUFFER_SIZE 1024

/*!
The exit codes of LoLA:
0   everything went fine and LoLA came to a normal end
1   any kind of error (syntax error, wrong command line, ...) occurred
2   premature termination (ctrl+C, kill, remote termination, ...)
*/
typedef enum
{
    EXIT_NORMAL      = 0,  ///< normal exit
    EXIT_ERROR       = 1,  ///< error
    EXIT_TERMINATION = 2   ///< premature termination
} exit_t;

/*!
The most special type of a formula, so LoLA can use dedicated routines.

\note Individual integer values must not be given -- formula_t should only be
compared against symbolic values.
*/
typedef enum
{
    FORMULA_REACHABLE,      ///< EF
    FORMULA_INVARIANT,      ///< AG
    FORMULA_LIVENESS,       ///< AGEF
    FORMULA_FAIRNESS,       ///< GF
    FORMULA_STABILIZATION,  ///< FG
    FORMULA_EVENTUALLY,     ///< F
    FORMULA_INITIAL,        ///< no temporal operator
    FORMULA_LTL,            ///< LTL formulae
    FORMULA_CTL,            ///< CTL formulae
    FORMULA_MODELCHECKING,  ///< CTL* formulae
    FORMULA_DEADLOCK,       ///< deadlock
    FORMULA_EFAG,           ///< EFAG
    FORMULA_EGAGEF,         ///< EGAGEF
    FORMULA_AGEFAG,         ///< AGEFAG
    FORMULA_NODEADLOCK,     ///< deadlock
    FORMULA_BOUND,          ///< compute a bound
    FORMULA_BOOLEAN,        ///< Boolean operator is top (and contains temporal)
    FORMULA_COMPOUND        ///< collection of formulas
} formula_t;

/// three-valued logic
typedef enum
{
    TERNARY_FALSE = 0,
    TERNARY_TRUE = 1,
    TERNARY_UNKNOWN = 2
} ternary_t;

/// number of seconds between report outputs
#define REPORT_FREQUENCY 5

/// the size of the bloom filter (2 GB)
#define BLOOM_FILTER_SIZE 2147483648UL

/////////////
// types used in SuffixTreeStore and in BinStore2
typedef uint32_t vectordata_t;

// maximum size (in bits) of a place
#define PLACE_WIDTH (SIZEOF_CAPACITY_T * 8)

// maximum size (in bits) of a vector item
#define VECTOR_WIDTH (SIZEOF_VECTORDATA_T * 8)


//File Read Buffer Size
#define FILE_READ_BUFFER_SIZE 10000

// maximal length of printed formulae in characters
#define FORMULA_PRINT_SIZE 400


/////////////
// types for Tarjan
typedef uint64_t dfsnum_t;

/////////////
// types used for symmetry reduction

// bucket size for bucketized counting sort
#define BUCKET_SIZE 3000

// Comparison
typedef enum
{
    CMP_LT = 0,
    CMP_EQ = 1,
    CMP_GT = 2
} cmp_t;

/*!
This is just a type definition for the possible fairness assumptions that can
be specified for each transition. We consider weak and strong fairness as well
as the absence of fairness.
*/
typedef enum
{
    NO_FAIRNESS     = 0,   ///< no fairness
    WEAK_FAIRNESS   = 1,   ///< weak fairness
    STRONG_FAIRNESS = 2    ///< strong fairness
}
fairnessAssumption_t;


// Nr of bytes reserved for a single status message

#define STATUSLENGTH 1024
