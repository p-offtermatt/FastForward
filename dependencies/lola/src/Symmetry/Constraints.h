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
\status new

\brief A constraint is a pair (A,B) where A and B are sets of nodes. It is
satisfied by a bijection pi of the nodes iff pi(A) = B. This file defines data
structures for handling constraints and the main calculation procedures that
use it

Starting with three constraints (P,P), (T,T), and (F,F) (where F is the set of
nodes in the formula tree), we develop symmetries by an iterative process of
splitting constraints. If, at any time, card(A) != card(B), we know that no
bijection can satisfy (A,B). Hence, our data structure relies on card(A) =
card(B). Further, it relies on the fact, that, all left sides of constraints
are a partition of P+T+F, and all right sides of constraints form a partition
of P+T+F. Up to a certain point in computation, we have A=B for all
constraints. Before that point, only A is stored. Symmetries are computed by
stepwise splitting constraints into smaller ones until all As and Bs are
singletons. The resulting set of constraints represents a bijection.

The basic idea of the data structure is as follows: There is one array
Element[DOM] that contains all vertices. For each constraint (A,B), the
elements of A form a consecutive area in this array. Another array,
Element[COD], contains all vertices where all elements of B form a consecutive
area. A third array, Constraints, contains one entry for each constraint. Its
constituents A and B are defined by the range in the Elements arrays that
contains the elements of A and B, respectively. Since we have the assertion
card(A)=card(B) for all constraints, the indices for start and end of the
ranges are the same for A as for B. Example: Let Element[DOM] = [1,3,2,5,4],
Element[COD] = [1,2,3,4,5], and Constraint = [0..1, 2..3,4..4]. We represent
the three constraints [{1,3},{1,2}], [{2,5},{3,4}], and [{4},{5}].

Elements and Constraints arrays exist in several copies, once for each thread
participating in computation.
*/

#pragma once

#include <Core/Dimensions.h>
#include <Symmetry/AutomorphismGraph.h>
#include <Symmetry/Symmetry.h>

struct CTLFormula;
class StatePredicate;

// indices in the SymmetryCalculator::elements and Vertex::pos
#define DOM 0
#define COD 1

/*!
\brief an entry in the SymmetryCalculator::constraints array
*/
class Constraint
{
public:
    Constraint();

    /// index of first element of A in Elements[DOM] resp. first element of B in elements[COD]
    arrayindex_t from;
    /// index of last element of A in Elements[DOM] resp. last element of B in elements[COD]
    arrayindex_t to;

    /// constraint where this one has been split from (used for backtracking)
    Constraint *parent;

    // signals whether this constraint still needs to be processed in refinement process
    bool unprocessed;
    bool touched;

    /// a linked (todo) list for all unprocess constraints
    Constraint *next;
    Constraint *nextTouched;
};

/*! We want to use threads for exploring symmetries. To this end, we hand
a ticket to each thread that contains its index and a bit of other useful
information. Through this index, the
thread finds all its information for processing part of the search tree.
In the class SymmetryCalculator, we maintain a list of free tickets. This
is used as condition variable to block creation of too many threads.
*/

class ThreadTicket
{
public:
    arrayindex_t idx;  // the index for access to my copy of the main
    // data structures
    arrayindex_t depth;  // the depth in the search tree where I have been
    // forked off
    arrayindex_t c_min;  // the constraint for which images are tried
    ThreadTicket *next;  // next ticket in the free tickets list
};

/*!
a class to encapsulate the symmetry calculation

\todo What about LTL?
\todo Make ComputeSymmetries() void.
*/
class SymmetryCalculator
{
public:
    /// the number of worker threads, taken from command line argument
    static arrayindex_t max_threads;

    /// a mutex variable for securing the management of running worker threads
    static pthread_mutex_t rt_mutex;

    /// a mutex variable for securing the update of statistical information
    static pthread_mutex_t db_mutex;

    /// construct a calculator in case no formula is given (deadlock/full)
    SymmetryCalculator();

    /// construct a calculator for a CTL formula
    explicit SymmetryCalculator(const CTLFormula *const);

    /// construct a calculator for a state predicate
    explicit SymmetryCalculator(const StatePredicate *const);

    ~SymmetryCalculator();

    /// two copies of an elements array (DOM/COD), for each thread
    static Vertex ** **elements;

    /// an array of constraints, for each thread
    static Constraint **constraints;

    /// the number of constraints, for each thread
    static arrayindex_t *cardConstraints;

    /// a linked list of constraints that need to be refined, for each thread
    static Constraint **todo;

    /// the number of dead branches visited during calculation
    static uint64_t deadBranches;

    /// sorts an element array by property (given DOM/COD)
    /// arg1: thread number, arg2: DOM or COD, arg3: from index, arg4: to index
    static void sort(arrayindex_t, int, arrayindex_t, arrayindex_t);

    /// the main computation
    void ComputeSymmetries();

    /// set to true if you want to stop symmetry calculation
    static bool stop;

    /// the result generating set
    static GeneratingSet *G;


    /// a list with one record for each thread that is available for running
    /// invariant: card(running threads) + card(records in list) = max_threads
    static ThreadTicket *freeThreads;

private:
    /// the number for formula nodes in the graph
    static arrayindex_t sizeFormula;

    /// the CTL formula
    const CTLFormula *const F;

    /// the state predicate
    const StatePredicate *const S;

    /// constructs a graph from net and formula
    void initGraph();

    /// creates an initial refinement refinement from (P->P, T->T, F->F)
    void initialRefinement();

    /// initial split of a constraint (only on one array)
    bool initialSplit(Constraint *const);

    /// general split of a constraint (on both arrays)
    static bool split(arrayindex_t, Constraint *const);

    /// refine by number of neighbors (within initial refinement)
    void initialArcRefine();

    /// general refine by number of neighbors
    static bool refine(arrayindex_t);

    /// computer all generators in a given level
    void ExploreLevel(arrayindex_t);

    /// find one symmetry consistent with the constraint
    static bool ExploreSymmetry(arrayindex_t);

    /// debug output of constraints
    void printConstraints();
    /// debug output of initial constraints
    void printInitialConstraints();
    /// debug output of elements
    void printElements();
    /// debug output of initial elements
    void printInitialElements();
    void printGraph();
    void printId(arrayindex_t);

    /// anchor for a linked list containing all constraints touched by considered arcs, once for each thread
    static Constraint **firstTouched;

    /// signal handler
    static void signalHandler(int);

    /// progress reporter
    static void *report_progress(void *);

    /// start procedure for worker threads
    static void *ExploreSymmetryThread(void *);

    /// condition variable for managing running threads
    static pthread_cond_t thread_cond;
};
