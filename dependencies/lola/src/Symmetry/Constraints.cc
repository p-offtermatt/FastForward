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
structures for handling constraints.

Starting with three constraints (P,P), (T,T), and (F,F) (where F is the set of
nodes in the formula tree), we develop symmetries by an iterative process of
splitting constraints. If, at any time, card(A) != card(B), we know that no
bijection can satisfy (A,B). Hence, our data structure relies on card(A) =
card(B). Further, it relies on the fact, that, all left sides of constraints
are a partition of P+T+F, and all right sides of constraints form a partition
of P+T+F. Up to a certain point in computation, we have A=B for all
constraints. Before that point, only A is stored. Symmetries are coputed by
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
Element[COD] = [1,2,3,4,5], and Constraint = [ 0..1, 2..3,4..4]. We represent
the three constraints [{1,3},{1,2}], [{2,5},{3,4}], and [{4},{5}].
*/

#include <Core/Dimensions.h>
#include <Core/Handlers.h>
#include <Core/Runtime.h>
#include <Formula/CTL/CTLFormula.h>
#include <Formula/StatePredicate/StatePredicate.h>
#include <Net/Marking.h>
#include <Net/Net.h>
#include <Net/Place.h>
#include <Net/Transition.h>
#include <Symmetry/AutomorphismGraph.h>
#include <Symmetry/Constraints.h>

Constraint::Constraint() {}

uint64_t SymmetryCalculator::deadBranches = 0;
arrayindex_t SymmetryCalculator::max_threads = 0;
pthread_mutex_t SymmetryCalculator::rt_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t SymmetryCalculator::db_mutex = PTHREAD_MUTEX_INITIALIZER;
GeneratingSet *SymmetryCalculator::G = NULL;
arrayindex_t *SymmetryCalculator::cardConstraints = NULL;
arrayindex_t SymmetryCalculator::sizeFormula = 0;
bool SymmetryCalculator::stop = false;
pthread_cond_t SymmetryCalculator::thread_cond;
Vertex ** **SymmetryCalculator::elements = NULL;
Constraint **SymmetryCalculator::constraints = NULL;
Constraint **SymmetryCalculator::firstTouched = NULL;
Constraint **SymmetryCalculator::todo = NULL;
ThreadTicket *SymmetryCalculator::freeThreads = NULL;

SymmetryCalculator::SymmetryCalculator() : F(NULL), S(NULL)
{
}

SymmetryCalculator::SymmetryCalculator(const CTLFormula *const FF) : F(FF), S(NULL)
{}

SymmetryCalculator::SymmetryCalculator(const StatePredicate *const SP) : F(NULL), S(SP)
{}

// LCOV_EXCL_START
// cannot test abortion signalling
/*!
\param signum The signal to react to.
\note This function is only called for those signals that have been registered
by calling signal() first.

\post The symmetry calculation is aborted.
*/
void SymmetryCalculator::signalHandler(int signum)
{
    RT::rep->message("caught signal %s - aborting symmetry calculation",
                     RT::rep->markup(MARKUP_WARNING, strsignal(signum)).str());

    // reset signal handler
    signal(SIGINT, SIG_DFL);
    signal(SIGINT, Handlers::signalTerminationHandler);

    stop = true;
}

// LCOV_EXCL_STOP

void *SymmetryCalculator::report_progress(void *)
{
    unsigned int intervals = 0;
    while (true)
    {
        sleep(REPORT_FREQUENCY);
        unsigned int time_elapsed = intervals++ * REPORT_FREQUENCY;
        unsigned int percent;

        if (G->possibleGenerators == 0)
        {
            percent = 100;
        }
        else
        {
            percent = (G->knownGenerators * 100) / G->possibleGenerators;
        }
        RT::rep->status("%8llu generators;%8llu candidates remain (%2u%%), %5d secs",
                        SymmetryCalculator::G->knownGenerators,
                        SymmetryCalculator::G->possibleGenerators - SymmetryCalculator::G->knownGenerators, percent,
                        time_elapsed);
        if (RT::args.symmtimelimit_given && time_elapsed > (unsigned) RT::args.symmtimelimit_arg)
        {
            if (!stop)
            {
                RT::rep->status("time limit for symmetry computation exceeded - aborting");
                stop = true;
            }
        }
    }
    // unreachable
    return NULL;
}

void SymmetryCalculator::ComputeSymmetries()
{
    // announce start of computation

    RT::rep->status("computing symmetries (%s)", RT::rep->markup(MARKUP_PARAMETER, "--symmetry").str());
    RT::data["limits"]["symmetrytime"] = JSON::null;
    if (RT::args.symmtimelimit_given)
    {
        RT::data["limits"]["symmetrytime"] = RT::args.symmtimelimit_arg;
        RT::rep->status("time limit for symmetry computation of %u seconds given (%s)",
                        RT::args.symmtimelimit_arg, RT::rep->markup(MARKUP_PARAMETER, "--symmtimelimit").str());
    }

    // initialize main data

    stop = false;
    G = new GeneratingSet();

    // initialize tickets for worker threads (different from report threads)

    max_threads = RT::args.threads_arg;
    for (arrayindex_t i = 0; i < max_threads; i++)
    {
        ThreadTicket *T = new ThreadTicket();
        T -> next = freeThreads;
        freeThreads = T;
        // indices start at 1 (0 is reserved for initial thread)
        T -> idx = i + 1;
    }
    int x = pthread_cond_init(&thread_cond, NULL);
    if(x != 0) RT::rep->status("cond init error");

    // spawn progress report thread

    pthread_t symmreporter;
    const int ret = pthread_create(&symmreporter, NULL, SymmetryCalculator::report_progress, NULL);
    // LCOV_EXCL_START
    if (UNLIKELY(ret != 0))
    {
        RT::rep->status("thread could not be created");
        RT::rep->abort(ERROR_THREADING);
    }
    // LCOV_EXCL_STOP

    // listen to interrupt signal (ctrl+c)

    signal(SIGINT, signalHandler);

    // retrieve graph from net && formula, initialize constraints data structure

    initGraph();

    // split constraints according to
    // - node color
    // - fan-in, fan-out
    // - number of edges with given color
    // - neighboured constraints

    initialRefinement();

    // actual search for symmetries

    ExploreLevel(0);

    // wait for worker threads to return; implemented as:
    // wait for all tickets to be returned to freeTickets list.
    for (arrayindex_t i = 0; i < max_threads; i++)
    {
        // wait until list not empty (i.e. some thread returned)
        int x = pthread_mutex_lock(&rt_mutex);
	if(x != 0) RT::rep->status("mutex lock error");
        if (freeThreads == NULL)
        {
            pthread_cond_wait(&thread_cond, &rt_mutex);
        }
        // release ticket
        ThreadTicket *T = freeThreads;
        freeThreads = freeThreads -> next;
        delete T;
        pthread_mutex_unlock(&rt_mutex);
    }

    // post-process generating set

    G->condense();

    // kill reporter thread
    pthread_cancel(symmreporter);

    // reset signal handler
    signal(SIGINT, SIG_DFL);
    signal(SIGINT, Handlers::signalTerminationHandler);

    // final announdements

    if (stop)
    {
        RT::rep->status("symmetry computation interrupted: continue with partial generating set");
    }

    RT::rep->status("computed %llu generators (%llu in search tree, %llu by composition)",
                    G->knownGenerators, G->realComputed, G->knownGenerators - G->realComputed);
    RT::rep->status("representing %LG symmetries", G->sizeofSymmetryGroup());
    RT::rep->status("%u dead branches visited in search tree", deadBranches);
    RT::data["symmetries"]["generators"] = static_cast<double>(G->knownGenerators);
    RT::data["symmetries"]["dead_branches"] = static_cast<double>(deadBranches);
    RT::data["symmetries"]["represented"] = static_cast<double>(G->sizeofSymmetryGroup());
}


void SymmetryCalculator::initGraph()
{
    assert(max_threads > 0);
    // fill symmetry calculator && create automorphism graph

    // 1. Initialization

    // 1.1 Get size of graph
    // #vertices = #P + #T +#F where is is number of nodes in formula tree

    if (S)
    {
        sizeFormula = (S->countSubFormulas());
    }
    if (F)
    {
        sizeFormula = (F->countSubFormulas());
    }

    // 1.2 allocate memory for calculator data structures

    elements = new Vertex *** [max_threads + 1]; // one per thread, one (0) for descent to identity
    elements[0] = new Vertex **[2];  // for identity: one array for DOM, one for COD
    // (entries for other threads are initialized when forked)

    elements[0][DOM] = new Vertex * [Net::Card[PL] + Net::Card[TR] + sizeFormula];
    elements[0][COD] = new Vertex * [Net::Card[PL] + Net::Card[TR] + sizeFormula];
    constraints = new Constraint * [max_threads + 1]; // one per thread, one (0) for identity
    constraints[0] = new Constraint[Net::Card[PL] + Net::Card[TR] + sizeFormula];
    // constraints arrays for other threads are initialized when forked

    cardConstraints = new arrayindex_t [max_threads + 1];
    firstTouched = new Constraint * [max_threads + 1];
    todo = new Constraint * [max_threads + 1];

    // 1.3 fill places into elements array and create graph nodes for places

    // check list
    // have: nodes  arcs
    //       P: no      to T: no    from T: no      from F: no
    //       T: no      to P: no    from P: no
    //       F: no      to F: no    from F: no      to P: no

    for (arrayindex_t i = 0; i < Net::Card[PL]; i++) // for all places do
    {
        // create vertex
        Vertex *v = new Vertex();
        v->id = i;
        v->color = Marking::Initial[i];
        v->cardSucc = Net::CardArcs[PL][POST][i];
        v->cardPre = Net::CardArcs[PL][PRE][i];

        // the actual arc arrays will be handled later, since
        // at this time, it is unclear how many formula nodes
        // connect to the place.
        elements[0][DOM][i] = v;
    }

    // check list
    // have: nodes  arcs
    //       P: yes     to T: no    from T: no      from F: no
    //       T: no      to P: no    from P: no
    //       F: no      to F: no    from F: no      to P: no

    // 1.4 fill transitions into elements array and create graph nodes
    for (arrayindex_t i = 0; i < Net::Card[TR]; i++)
    {
        // create vertex
        Vertex *v = new Vertex();
        v->id = i + Net::Card[PL];
        v->color = Transition::Fairness[i];
        v->cardSucc = Net::CardArcs[TR][POST][i];
        v->cardPre = Net::CardArcs[TR][PRE][i];

        // Transitions are only connected to places. Hence,
        // actual Pre + Succ arrays can be filled now.

        v->succ = new Vertex * [v->cardSucc];
        v->colorSucc = new arrayindex_t[v->cardSucc];
        v->pre = new Vertex * [v->cardPre];
        v->colorPre = new arrayindex_t[v->cardPre];
        for (arrayindex_t j = 0; j < v->cardSucc; j++)
        {
            v->succ[j] = elements[0][DOM][Net::Arc[TR][POST][i][j]];
            v->colorSucc[j] = Net::Mult[TR][POST][i][j];
        }
        for (arrayindex_t j = 0; j < v->cardPre; j++)
        {
            v->pre[j] = elements[0][DOM][Net::Arc[TR][PRE][i][j]];
            v->colorPre[j] = Net::Mult[TR][PRE][i][j];
        }
        elements[0][DOM][i + Net::Card[PL]] = v;
    }

    // check list
    // have: nodes  arcs
    //       P: yes     to T: no    from T: no      from F: no
    //       T: yes     to P: yes   from P: yes
    //       F: no      to F: no    from F: no      to P: no

    FormulaInfo **info = NULL;
    if (sizeFormula) // if there is a formula...
    {
        // 1.5 collect all formula infos;

        info = new FormulaInfo * [sizeFormula];
        if (S)
        {
            info[0] = S->getInfo();
        }
        if (F)
        {
            info[0] = F-> getInfo();
        }
        arrayindex_t nextfree = 1; // next free slot for filling another
        // formula node
        for (arrayindex_t i = 0; i < sizeFormula; i++)
        {
            if (info[i]->statePredicateChildren)
            {
                for (arrayindex_t j = 0; j < info[i]->cardChildren; j++)
                {
                    info[nextfree++] = info[i]->statePredicateChildren[j]->getInfo();
                }
            }
            if (info[i]->ctlChildren)
            {
                for (arrayindex_t j = 0; j < info[i]->cardChildren; j++)
                {
                    info[nextfree++] = info[i]->ctlChildren[j]->getInfo();
                }
            }
            assert(nextfree <= sizeFormula);
        }

        // 1.6 fill formula nodes into elements array and create graph nodes
        for (arrayindex_t i = 0; i < sizeFormula; i++)
        {
            Vertex *v = new Vertex();
            v->id = i + Net::Card[PL] + Net::Card[TR];
            elements[0][DOM][v->id] = v;
        }

        // 1.7 add graph edges for formula nodes

        arrayindex_t nextsucc = 1;
        // formula root does not have predecessor edges
        elements[0][DOM][Net::Card[PL] + Net::Card[TR]]->cardPre = 0;
        elements[0][DOM][Net::Card[PL] + Net::Card[TR]]->pre = NULL;

        // each node adds its own successors and its successor's predecessor
        for (arrayindex_t i = 0; i < sizeFormula; i++)
        {
            Vertex *current = elements[0][DOM][i + Net::Card[PL] + Net::Card[TR]];
            current->color = info[i]->tag;
            if (current->color == formula_atomic)
            {
                AtomicStatePredicate *f = info[i]->f;
                // an atomic formula has places as successors
                current->cardSucc = f->cardPos + f->cardNeg;
                current->succ = new Vertex * [current->cardSucc];
                current->colorSucc = new arrayindex_t[current->cardSucc];
                for (arrayindex_t j = 0; j < f->cardPos; j++)
                {
                    current->succ[j] = elements[0][DOM][f->posPlaces[j]];
                    current->colorSucc[j] = f->posMult[j];
                    // predecessors of place only counted
                    // since only after having scanned
                    // all atomic formulas the number
                    // of predecessors is determined and
                    // the arrays for p can be allocated
                    // --> there will be second pass
                    ++(current->succ[j]->cardPre);
                }
                for (arrayindex_t j = 0; j < f->cardNeg; j++)
                {
                    current->succ[f->cardPos + j] = elements[0][DOM][f->negPlaces[j]];
                    current->colorSucc[f->cardPos + j] = - f->negMult[j];
                    // predecessors of place only counted
                    // since only after having scanned
                    // all atomic formulas the number
                    // of predecessors is determined and
                    // the arrays for p can be allocated
                    // --> there will be second pass
                    ++(current->succ[f->cardPos + j]->cardPre);
                }
            }
            else if (current->color == formula_deadlock ||
                     current->color == formula_nodeadlock)
            {
                current->cardSucc = 0;
                current->succ = NULL;
                current->colorSucc = NULL;
            }
            else
            {
                // node is not atomic --> successors are
                // formula nodes
                current->cardSucc = info[i]->cardChildren;
                current->succ = new Vertex * [current->cardSucc];
                current->colorSucc = new arrayindex_t[current->cardSucc];
                if (current->color == formula_u ||
                        current->color == formula_r ||
                        current->color == formula_au ||
                        current->color == formula_eu ||
                        current->color == formula_ar ||
                        current->color == formula_er )
                {
                    for (arrayindex_t j = 0; j < current->cardSucc; j++)
                    {
                        Vertex *s =  elements[0][DOM][Net::Card[PL] + Net::Card[TR] + nextsucc++];
                        current->succ[j] =  s;
                        // until/release formula edges are consecutively numbered
                        // so the subformulas are not symmetric
                        current->colorSucc[j] = j + 1;
                        s->pre = new Vertex * [1];
                        s->colorPre = new arrayindex_t[1];
                        s->cardPre = 1;
                        s->pre[0] = current;
                        s->colorPre[0] = j + 1;
                    }
                }
                else
                {
                    for (arrayindex_t j = 0; j < current->cardSucc; j++)
                    {
                        Vertex *s =  elements[0][DOM][Net::Card[PL] + Net::Card[TR] + nextsucc++];
                        current->succ[j] =  s;
                        // formula edges (other than until/release) do not have color: either only one successor or successors can be symmetric
                        current->colorSucc[j] = 0;
                        s->pre = new Vertex * [1];
                        s->colorPre = new arrayindex_t[1];
                        s->cardPre = 1;
                        s->pre[0] = current;
                        s->colorPre[0] = 0;
                    }
                }
            }
        }
        assert(nextsucc == sizeFormula);
    }

    // check list
    // have: nodes  arcs
    //       P: yes     to T: no    from T: no      from F: no
    //       T: yes     to P: yes   from P: yes
    //       F: yes     to F: yes   from F: yes     to P: yes

    // 1.8 allocate edges for places and connect to transitions

    {
        for (arrayindex_t i = 0; i < Net::Card[PL]; i++)
        {
            Vertex *v = elements[0][DOM][i];
            v->succ = new Vertex * [v->cardSucc];
            v->colorSucc = new arrayindex_t[v->cardSucc];
            v->pre = new Vertex * [v->cardPre];
            v->colorPre = new arrayindex_t[v->cardPre];
            for (arrayindex_t j = 0; j < Net::CardArcs[PL][POST][i]; j++)
            {
                v->succ[j] = elements[0][DOM][Net::Arc[PL][POST][i][j] + Net::Card[PL]];
                v->colorSucc[j] = Net::Mult[PL][POST][i][j];
            }
            for (arrayindex_t j = 0; j < Net::CardArcs[PL][PRE][i]; j++)
            {
                v->pre[j] = elements[0][DOM][Net::Arc[PL][PRE][i][j] + Net::Card[PL]];
                v->colorPre[j] = Net::Mult[PL][PRE][i][j];
            }
            // cardPre is used as current index for subsequent
            // insertion of incoming formula edges
            v->cardPre = Net::CardArcs[PL][PRE][i];
        }
    }

    // check list
    // have: nodes  arcs
    //       P: yes     to T: yes   from T: yes     from F: no
    //       T: yes     to P: yes   from P: yes
    //       F: yes     to F: yes   from F: yes     to P: yes

    // 1.9 insert remaining arcs

    if (sizeFormula) // there is a formula
    {
        // scan through formula edges and insert backward edges from
        // places to atomic formulas
        arrayindex_t nextsucc = 1;

        for (arrayindex_t i = 0; i < sizeFormula; i++)
        {
            Vertex *current = elements[0][DOM][i + Net::Card[PL] + Net::Card[TR]];
            if (current->color == formula_atomic)
            {
                for (arrayindex_t j = 0; j < current->cardSucc; j++)
                {
                    Vertex *p = current->succ[j];
                    p->pre[p->cardPre] = current;
                    p->colorPre[p->cardPre++] = current->colorSucc[j];
                }
                // color of atomic formula node is now set
                // to the threshold of the comparison.
                // This may be ambigous to other tags but this
                // is not a problem: as atomic formulas have
                // places as successors while other formula
                // nodes have formulas as successors, our
                // algorithm will quickly separate atomic from
                // other formulas.

                current->color = info[i]->f->threshold;
            }
            else
            {
                nextsucc += info[i]->cardChildren;
            }
        }
    }

    // check list
    // have: nodes  arcs
    //       P: yes     to T: yes   from T: yes     from F: yes
    //       T: yes     to P: yes   from P: yes
    //       F: yes     to F: yes   from F: yes     to P: yes

    // 1.10 sort all arc arrays by increasing colors

    for (arrayindex_t i = 0; i < Net::Card[PL] + Net::Card[TR] + sizeFormula; i++)
    {
        Vertex *v = elements[0][DOM][i];
        if (v->cardSucc)
        {
            v->sortArcs(v->succ, v->colorSucc, 0, v->cardSucc);
        }
        if (v->cardPre)
        {
            v->sortArcs(v->pre, v->colorPre, 0, v->cardPre);
        }
    }

    cardConstraints[0] = 0;
    // 2. initialize constraints
    // We start with three constraints: (P,P),(T,T) and (F,F)
    if (Net::Card[PL])
    {
        // there are places

        // fst index: number of thread (here:initial), snd index: number of constraint
        constraints[0][0].from = 0;
        constraints[0][0].to = Net::Card[PL] - 1;
        constraints[0][0].unprocessed = true;
        constraints[0][0].parent = NULL;
        for (arrayindex_t i = 0; i < Net::Card[PL]; i++)
        {
            elements[0][DOM][i]->inConstraint[DOM][0] = constraints[0] /* = &(constraints[0][0]) */ ;
        }
        cardConstraints[0]++;
    }
    if (Net::Card[TR])
    {
        // if there are transitions
        constraints[0][cardConstraints[0]].from = Net::Card[PL];
        constraints[0][cardConstraints[0]].to = Net::Card[PL] + Net::Card[TR] - 1;
        constraints[0][cardConstraints[0]].unprocessed = true;
        constraints[0][cardConstraints[0]].parent = NULL;
        for (arrayindex_t i = 0; i < Net::Card[TR]; i++)
        {
            elements[0][DOM][Net::Card[PL] + i]->inConstraint[DOM][0] = constraints[0] +
                    cardConstraints[0] /* = &(constraints[0][cardConstraints[0]]) */;
        }
        cardConstraints[0]++;
    }
    if (sizeFormula)
    {
        // if there is a formula
        constraints[0][cardConstraints[0]].from = Net::Card[PL] + Net::Card[TR];
        constraints[0][cardConstraints[0]].to = Net::Card[PL] + Net::Card[TR] + sizeFormula - 1;
        constraints[0][cardConstraints[0]].unprocessed = true;
        constraints[0][cardConstraints[0]].parent = NULL;
        for (arrayindex_t i = 0; i < sizeFormula; i++)
        {
            elements[0][DOM][Net::Card[PL] + Net::Card[TR] + i]->inConstraint[DOM][0] = constraints[0] +
                    cardConstraints[0];
        }
        cardConstraints[0]++;
        for (arrayindex_t i = 0; i < sizeFormula; i++)
        {
            delete info[i];
        }
        delete[] info;
    }
}

void SymmetryCalculator::sort(arrayindex_t thr, int direction, arrayindex_t from, arrayindex_t to)
{
    arrayindex_t blue = from;
    arrayindex_t white = from + 1;
    arrayindex_t red = to;

    int64_t pivot = elements[thr][direction][from]->property[direction][thr];
    while (red >= white)
    {
        if (elements[thr][direction][white]->property[direction][thr] < pivot)
        {
            // sort into blue area
            Vertex *tmp = elements[thr][direction][white];
            elements[thr][direction][white++] = elements[thr][direction][blue];
            elements[thr][direction][blue++] = tmp;
        }
        else
        {
            if (elements[thr][direction][white]->property[direction][thr] == pivot)
            {
                // sort into white area
                ++white;
            }
            else
            {
                //sort into red area
                Vertex *tmp = elements[thr][direction][white];
                elements[thr][direction][white] = elements[thr][direction][red];
                elements[thr][direction][red--] = tmp;
            }
        }
    }
    // sort blue area
    if (blue - from >= 2)
    {
        sort(thr, direction, from, blue - 1);
    }
    // sort red area
    if (to - red >= 2)
    {
        sort(thr, direction, red + 1, to);
    }
}

bool SymmetryCalculator::initialSplit(Constraint *const c)
{
    // separates an existing initial constraint into many,
    // defined by the entry in the property field of the
    // Element data structure

    // 1. sort elements according to property
    sort(0, DOM, c->from, c->to);

    // 2. while exist differences: append new constraint

    // return value: actual split happened
    arrayindex_t return_value = false;
    arrayindex_t fst = c->from;
    while (elements[0][DOM][fst]->property[DOM][0] != elements[0][DOM][c->to]->property[DOM][0])
    {
        return_value = true;
        // one more split required

        // search split point
        arrayindex_t i;
        for (i = fst + 1; elements[0][DOM][i]->property[DOM][0] == elements[0][DOM][fst]->property[DOM][0];
                i++)
        {}

        // add new constraint
        assert(i <= c->to);
        constraints[0][cardConstraints[0]].from = fst;
        constraints[0][cardConstraints[0]].to = i - 1;
        constraints[0][cardConstraints[0]].parent = NULL;
        for (arrayindex_t j = constraints[0][cardConstraints[0]].from;
                j <= constraints[0][cardConstraints[0]].to; j++)
        {
            elements[0][DOM][j]->inConstraint[DOM][0] = constraints[0] + cardConstraints[0];
        }
        constraints[0][cardConstraints[0]].touched = false;
        constraints[0][cardConstraints[0]++].unprocessed = true;
        firstTouched[0] = NULL;


        // update old constraint
        c->from = i;
        fst = i;
    }
    c -> touched = false;
    return return_value;
}

bool SymmetryCalculator::split(arrayindex_t thr, Constraint *const c)
{
    // separates an existing constraint into many,
    // defined by the entry in the property field of the
    // Element data structure

    // 1. sort elements according to property
    sort(thr, DOM, c->from, c->to);
    sort(thr, COD, c->from, c->to);

    // if extremal property values are different, split fails
    if (elements[thr][COD][c->from]->property[COD][thr] !=
            elements[thr][DOM][c->from]->property[DOM][thr] ||
            elements[thr][COD][c->to]->property[COD][thr] != elements[thr][DOM][c->to]->property[DOM][thr])
    {
        // inconsistency
        return false;
    }

    // 2. while exist differences: append new constraint

    arrayindex_t fst = c->from;

    while (elements[thr][DOM][fst]->property[DOM][thr] != elements[thr][DOM][c->to]->property[DOM][thr])
    {
        // by previous tests, we know that extremal values are the same
        assert(elements[thr][DOM][fst]->property[DOM][thr] == elements[thr][COD][fst]->property[COD][thr]
               && elements[thr][DOM][c->to]->property[DOM][thr] == elements[thr][COD][c->to]->property[COD][thr]);

        // search split point
        arrayindex_t i;

        for (i = fst + 1;
                elements[thr][DOM][i]->property[DOM][thr] == elements[thr][DOM][fst]->property[DOM][thr]; i++)
        {}

        assert(i <= c->to);

        // check if co-domain has same split point
        if (elements[thr][COD][i - 1]->property[COD][thr] != elements[thr][DOM][i - 1]->property[DOM][thr]
                ||
                elements[thr][COD][i]->property[COD][thr] != elements[thr][DOM][i]->property[DOM][thr])
        {
            // inconsistency
            return false;
        }
        // passing the previous test means that entry assertion
        // will be true in next round

        // add new constraint
        constraints[thr][cardConstraints[thr]].from = fst;
        constraints[thr][cardConstraints[thr]].to = i - 1;
        constraints[thr][cardConstraints[thr]].parent = c;
        for (arrayindex_t j = constraints[thr][cardConstraints[thr]].from;
                j <= constraints[thr][cardConstraints[thr]].to; j++)
        {
            elements[thr][DOM][j]->inConstraint[DOM][thr] = constraints[thr] + cardConstraints[thr];
            elements[thr][COD][j]->inConstraint[COD][thr] = constraints[thr] + cardConstraints[thr];
        }
        constraints[thr][cardConstraints[thr]].touched = false;
        constraints[thr][cardConstraints[thr]++].unprocessed = true;


        // update old constraint
        c->from = i;
        fst = i;
    }
    // split successful
    c->touched = false;
    return true;
}

void SymmetryCalculator::initialArcRefine()
{
    // for all constraints c' and all edge colors x, we
    // count, for all nodes n in c', how many incoming (outgoing)
    // edges n has with source (target) in c and color x. If that number
    // is different for two nodes in c', they are separated into
    // different constraints as no symmetry can map one to the other.

    // We rely that, by prior computation,
    // - all nodes in c have the same number of incoming/outgoing edges
    // - for all nodes in c, the color of the i-th edge in the Pre/post
    //   list is the same.
    // - the Pre/Post arrays are sorted by color

    Constraint *c = todo[0];
    todo[0] = todo[0]->next; // this constraint will be processed in the end.
    c->unprocessed = false;

    Vertex *reference = elements[0][DOM][c->from];  // a typical vertex in c

reentry: // used for restart if c itself has been changed

    // process all incoming edges;
    for (arrayindex_t j = 0; j < reference->cardPre;) // for all edge colors do
    {
        firstTouched[0] = NULL;
        arrayindex_t color = reference ->colorPre[j];
        // now we start a counting process
        do // for all edges with this color
        {
            if (stop)
            {
                return;  // LCOV_EXCL_LINE
            }
            for (arrayindex_t k = c->from; k <= c->to; k++)
            {
                ++(elements[0][DOM][k]->pre[j]->property[DOM][0]);
                Constraint *cc = elements[0][DOM][k]->pre[j]->inConstraint[DOM][0];
                if (!cc->touched)
                {
                    cc->touched = true;
                    cc->nextTouched = firstTouched[0];
                    firstTouched[0] = cc;
                }
            }
        }
        while ((++j) < reference->cardPre && reference->colorPre[j] == color);

        // check effect of counting for all constraints
        arrayindex_t currentCardConstraint = cardConstraints[0];
        for (Constraint *l = firstTouched[0]; l;  l = l -> nextTouched)
        {
            l->touched = false;
            bool splitted = initialSplit(l);
            if (splitted)
            {
                // append l to
                // unprocessed list
                if (!(l->unprocessed))
                {
                    l->unprocessed = true;
                    if (l != c)
                    {
                        l->next = todo[0];
                        todo[0] = l;
                    }
                }
            }
        }

        // append new constraints to unprocessed list
        for (arrayindex_t m = currentCardConstraint; m < cardConstraints[0]; m++)
        {
            constraints[0][m].next = todo[0];
            todo[0] = constraints[0] + m;
            constraints[0][m].unprocessed = true;
        }

        // next few lines removed: in a constraint, all elements have same property,
        // so not resetting it does not harm (except eventual overflow)

        //        for (arrayindex_t n = 0; n < Net::Card[PL] + Net::Card[TR] + sizeFormula; n++)
        //        {
        //            elements[0][DOM][n]->property[DOM][0] = 0;
        //        }
        if (c->unprocessed)
        {
            c -> unprocessed = false;
            goto reentry;
        }
    }

    // process all outgoing edges;

    for (arrayindex_t j = 0; j < reference->cardSucc;)
    {
        firstTouched[0] = NULL;
        arrayindex_t color = reference->colorSucc[j];
        // now we start a counting process
        do // for all edges with this color
        {
            if (stop)
            {
                return;  // LCOV_EXCL_LINE
            }
            for (arrayindex_t k = c->from; k <= c->to; k++)
            {
                ++(elements[0][DOM][k]->succ[j]->property[DOM][0]);
                Constraint *cc = elements[0][DOM][k]->succ[j]->inConstraint[DOM][0];
                if (!cc->touched)
                {
                    cc -> touched = true;
                    cc -> nextTouched = firstTouched[0];
                    firstTouched[0] = cc;
                }
            }
        }
        while ((++j) < reference->cardSucc && reference->colorSucc[j] == color);

        // check effect of counting for all constraints
        arrayindex_t currentCardConstraint = cardConstraints[0];
        for (Constraint *l = firstTouched[0]; l ; l = l->nextTouched)
        {
            l->touched = false;
            bool splitted = initialSplit(l);
            if (splitted)
            {
                // append l to
                // unprocessed list
                if (!(l->unprocessed))
                {
                    l->unprocessed = true;
                    if (l != c)
                    {
                        l->next = todo[0];
                        todo[0] = l;
                    }
                }
            }
        }

        // append new constraints to unprocessed list
        for (arrayindex_t m = currentCardConstraint; m < cardConstraints[0]; m++)
        {
            constraints[0][m].next = todo[0];
            todo[0] = constraints[0] + m;
            constraints[0][m].unprocessed = true;
        }
        // not resetting property does not harm as it is unique in constraint
        //        for (arrayindex_t n = 0; n < Net::Card[PL] + Net::Card[TR] + sizeFormula; n++)
        //        {
        //            elements[0][DOM][n]->property[DOM][0] = 0;
        //        }
        if (c->unprocessed)
        {
            c -> unprocessed = false;
            goto reentry;
        }
    }
}


bool SymmetryCalculator::refine(arrayindex_t thr)
{
    // for all constraints c' and all edge colors x, we
    // count, for all nodes n in c', how many incoming (outgoing)
    // edges n has with source (target) in c and color x. If that number
    // is different for two nodes in c', they are separated into
    // different constraints as no symmetry can map one to the other.

    // We rely that, by prior computation,
    // - all nodes in c have the same number of incoming/outgoing edges
    // - for all nodes in c, the color of the i-th edge in the Pre/post
    //   list is the same.
    // - the Pre/Post arrays are sorted by color


    Constraint *c = todo[thr];
    todo[thr] = todo[thr]->next; // this constraint will be processed in the end.
    c->unprocessed = false;
    // process all incoming edges;

    Vertex *reference = elements[thr][DOM][c->from];  // a typical vertex in c

reentry: // used for restart if c itself has been changed

    for (arrayindex_t j = 0; j < reference->cardPre;)
    {
        firstTouched[thr] = NULL;
        arrayindex_t color = reference ->colorPre[j];
        // now we start a counting process
        do // for all edges with this color
        {
            if (stop)
            {
                return false;  // LCOV_EXCL_LINE
            }
            for (arrayindex_t k = c->from; k <= c->to; k++)
            {
                ++(elements[thr][DOM][k]->pre[j]->property[DOM][thr]);
                Constraint *cc = elements[thr][DOM][k]->pre[j]->inConstraint[DOM][thr];
                if (!cc->touched)
                {
                    cc -> touched = true;
                    cc -> nextTouched = firstTouched[thr];
                    firstTouched[thr] = cc;
                }
                ++(elements[thr][COD][k]->pre[j]->property[COD][thr]);
                cc = elements[thr][COD][k]->pre[j]->inConstraint[COD][thr];
                if (!cc->touched)
                {
                    cc -> touched = true;
                    cc -> nextTouched = firstTouched[thr];
                    firstTouched[thr] = cc;
                }
            }
        }
        while ((++j) < reference->cardPre && reference->colorPre[j] == color);

        // check effect of counting for all constraints
        arrayindex_t currentCardConstraint = cardConstraints[thr];
        for (Constraint *l = firstTouched[thr]; l ; l = l->nextTouched)
        {
            l->touched = false;
            arrayindex_t thisCardConstraint = cardConstraints[thr];
            if (!split(thr, l))
            {
                //for (arrayindex_t n = 0; n < Net::Card[PL] + Net::Card[TR] + sizeFormula; n++)
                //{
                //elements[thr][DOM][n]->property[DOM][thr] = 0;
                //elements[thr][DOM][n]->property[COD][thr] = 0;
                //}
                return false;
            }
            if (thisCardConstraint != cardConstraints[thr])
            {
                // something changed
                // append c[l] to
                // unprocessed list
                if (!(l->unprocessed))
                {
                    l->unprocessed = true;
                    if (l != c)
                    {
                        l->next = todo[thr];
                        todo[thr] = l;
                    }
                }
            }
        }

        // append new constraints to unprocessed list
        for (arrayindex_t m = currentCardConstraint; m < cardConstraints[thr]; m++)
        {
            constraints[thr][m].unprocessed = true;
            constraints[thr][m].next = todo[thr];
            todo[thr] = constraints[thr] + m;
        }
        // property entry unique, so no reset necessary
        //
        //        for (arrayindex_t n = 0; n < Net::Card[PL] + Net::Card[TR] + sizeFormula; n++)
        //        {
        //            assert(n < Net::Card[PL] + Net::Card[TR] + sizeFormula);
        //            elements[thr][DOM][n]->property[DOM][thr] = 0;
        //            elements[thr][DOM][n]->property[COD][thr] = 0;
        //        }
        if (c->unprocessed)
        {
            c -> unprocessed = false;
            goto reentry;
        }
    }

    // process all outgoing edges;

    for (arrayindex_t j = 0; j < reference->cardSucc;)
    {
        firstTouched[thr] = NULL;
        arrayindex_t color = reference->colorSucc[j];
        // now we start a counting process
        do // for all edges with this color
        {
            if (stop)
            {
                return false;  // LCOV_EXCL_LINE
            }
            for (arrayindex_t k = c->from; k <= c->to; k++)
            {
                ++(elements[thr][DOM][k]->succ[j]->property[DOM][thr]);
                Constraint *cc = elements[thr][DOM][k]->succ[j]->inConstraint[DOM][thr];
                if (!cc->touched)
                {
                    cc -> touched = true;
                    cc -> nextTouched = firstTouched[thr];
                    firstTouched[thr] = cc;
                }
                ++(elements[thr][COD][k]->succ[j]->property[COD][thr]);
                cc = elements[thr][COD][k]->succ[j]->inConstraint[COD][thr];
                if (!cc->touched)
                {
                    cc -> touched = true;
                    cc -> nextTouched = firstTouched[thr];
                    firstTouched[thr] = cc;
                }
            }
        }
        while ((++j) < reference->cardSucc && reference->colorSucc[j] == color);

        // check effect of counting for all constraints
        arrayindex_t currentCardConstraint = cardConstraints[thr];
        for (Constraint *l = firstTouched[thr]; l ; l = l->nextTouched)
        {
            l->touched = false;
            arrayindex_t thisCardConstraint = cardConstraints[thr];
            if (!split(thr, l))
            {
                //for (arrayindex_t n = 0; n < Net::Card[PL] + Net::Card[TR] + sizeFormula; n++)
                //{
                //elements[thr][DOM][n]->property[DOM][thr] = 0;
                //elements[thr][DOM][n]->property[COD][thr] = 0;
                //}
                return false; // refinement leads to incosistency
            }
            if (cardConstraints[thr] != thisCardConstraint)
            {
                // split has added new constraint
                // append l to
                // unprocessed list
                if (!(l->unprocessed))
                {
                    l->unprocessed = true;
                    if (l != c)
                    {
                        l->next = todo[thr];
                        todo[thr] = l;
                    }
                }
            }
        }

        // append new constraints to unprocessed list
        for (arrayindex_t m = currentCardConstraint; m < cardConstraints[thr]; m++)
        {
            constraints[thr][m].next = todo[thr];
            todo[thr] = constraints[thr] + m;
            constraints[thr][m].unprocessed = true;
        }
        // not necessary
        //        for (arrayindex_t n = 0; n < Net::Card[PL] + Net::Card[TR] + sizeFormula; n++)
        //
        //        {
        //            elements[thr][DOM][n]->property[DOM][thr] = 0;
        //            elements[thr][DOM][n]->property[COD][thr] = 0;
        //        }
        if (c->unprocessed)
        {
            c -> unprocessed = false;
            goto reentry;
        }
    }
    return true;
}

void SymmetryCalculator::initialRefinement()
{
    // the original constraint (P,P),(T,T),(F,F) is further split
    // according to
    // - colors of nodes
    // - number of incoming/outgoing edges
    // - number of incoming/outgoing edges with same color
    // All processes will necessarily end up in constraints where
    // left and right side coincide. Hence, the refinement process
    // will be executed on the DOM part of the data structure only.
    // During the whole process, the pos entry of the Vertex record
    // will not be used. It will thus be updated only in the end of
    // the refinement prcedure.

    // 1. split according to node color

    // 1.1. load property field with node color

    for (arrayindex_t i = 0; i < Net::Card[PL] + Net::Card[TR] + sizeFormula; i++)
    {
        elements[0][DOM][i]->property[DOM][0] = elements[0][DOM][i]->color;
    }

    // 1.2 split all existing constraints
    arrayindex_t currentCardConstraints = cardConstraints[0];
    for (arrayindex_t i = 0; i < currentCardConstraints; i++)
    {
        initialSplit(constraints[0] + i);
        if (stop)
        {
            return;  // LCOV_EXCL_LINE
        }
    }

    // 2. split according to number of incoming arcs
    // 2.1. load property field with number of incoming arcs

    for (arrayindex_t i = 0; i < Net::Card[PL] + Net::Card[TR] + sizeFormula; i++)
    {
        elements[0][DOM][i]->property[DOM][0] = elements[0][DOM][i]->cardPre;
    }

    // 2.2 split all existing constraints
    currentCardConstraints = cardConstraints[0];
    for (arrayindex_t i = 0; i < currentCardConstraints; i++)
    {
        initialSplit(constraints[0] + i);
        if (stop)
        {
            return;  // LCOV_EXCL_LINE
        }
    }

    // 3. split according to number of outgoing arcs
    // 3.1. load property field with number of outgoing arcs

    for (arrayindex_t i = 0; i < Net::Card[PL] + Net::Card[TR] + sizeFormula; i++)
    {
        elements[0][DOM][i]->property[DOM][0] = elements[0][DOM][i]->cardSucc;
        if (stop)
        {
            return;  // LCOV_EXCL_LINE
        }
    }

    // 3.2 split all existing constraints
    currentCardConstraints = cardConstraints[0];
    for (arrayindex_t i = 0; i < currentCardConstraints; i++)
    {
        initialSplit(constraints[0] + i);
        if (stop)
        {
            return;  // LCOV_EXCL_LINE
        }
    }
    if (stop)
    {
        return;  // LCOV_EXCL_LINE
    }

    // 4. Split according to number of edges with same color
    // We rely on the following observations for the present
    // data structures:
    // (a) By step 2 and 3, all nodes inside one constraint
    //     have the same number of outgoing and incoming edges
    // (b) Edge lists are sorted by colors. Hence, two nodes
    //     have the same number of edges with same color (for
    //     all colors) if and only if for every j, the color of
    //     the j-th edge is the same for both nodes.

    // We have three nested loops:
    // - For all currently present constraints C do (implemented by
    //   the index i from 0 to currentCardConstraints)
    // - For all positions in the arc list (implemented by index j)
    // - for all constraints that have resulted from C (implemented
    //   through the next pointer in the Constraint record)

    // for all currently existing constraints
    currentCardConstraints = cardConstraints[0];
    for (arrayindex_t i = 0; i < currentCardConstraints; i++)
    {
        arrayindex_t fanin = elements[0][DOM][constraints[0][i].from]->cardPre;
        constraints[0][i].next = NULL;
        todo[0] = constraints[0] + i;
        for (arrayindex_t j = 0; j < fanin; j++)
        {
            if (stop)
            {
                return;  // LCOV_EXCL_LINE
            }
            arrayindex_t firstnewconstraint = cardConstraints[0];
            for (Constraint *c = todo[0]; c ; c = c->next)
            {
                // fill property field with color of j-th edge
                for (arrayindex_t k = c->from;  k <= c->to; k++)
                {
                    elements[0][DOM][k]->property[DOM][0] =
                        elements[0][DOM][k]->colorPre[j];
                }
                // split
                initialSplit(c);
            }
            if (firstnewconstraint != cardConstraints[0])
            {
                for (arrayindex_t l = firstnewconstraint; l < cardConstraints[0] - 1; l++)
                {
                    constraints[0][l].next = constraints[0] + (l + 1);
                }
                constraints[0][cardConstraints[0] - 1].next = todo[0];
                todo[0] = constraints[0] + firstnewconstraint;
            }
        }
    }

    // Now, we prepare for the (still initial, hence symmetric) refinement
    // according to the relation between two constraints.
    // To this end, we need to initialize the todo/next link.

    for (arrayindex_t i = 0; i < cardConstraints[0] - 1; i++)
    {
        constraints[0][i].next = constraints[0] + (i + 1);
        constraints[0][i].unprocessed = true;
        constraints[0][i].touched = false;
    }
    firstTouched[0] = NULL;
    todo[0] = constraints[0];
    constraints[0][cardConstraints[0] - 1].next = NULL;
    constraints[0][cardConstraints[0] - 1].touched = false;
    //    for (arrayindex_t i = 0; i < Net::Card[PL] + Net::Card[TR] + sizeFormula; i++)
    //    {
    //        elements[0][DOM][i]->property[DOM][0] = 0;
    //    }
    while (todo[0])
    {
        initialArcRefine();
        if (stop)
        {
            return;  // LCOV_EXCL_LINE
        }
    }
}

void SymmetryCalculator::ExploreLevel(arrayindex_t startlevel)
{
    // 1. find the smallest place in a non-singleton constraint
    arrayindex_t c_min = Net::Card[PL];
    arrayindex_t e_min = Net::Card[PL];
    arrayindex_t id_min = Net::Card[PL];
    for (arrayindex_t c = 0; c < cardConstraints[0]; c++)
    {
        if (constraints[0][c].from == constraints[0][c].to)
        {
            // singleton constraint
            continue;
        }
        if (elements[0][DOM][constraints[0][c].from]->id >= Net::Card[PL])
        {
            // constraint contains transitions or formula nodes
            continue;
        }
        for (arrayindex_t e = constraints[0][c].from; e <= constraints[0][c].to; e++)
        {
            if (elements[0][DOM][e]-> id < id_min)
            {
                id_min = elements[0][DOM][e]-> id;
                c_min = c;
                e_min = e;
            }
        }
    }
    // in all levels between start level and id_min, we now know that
    // no generators will be found there
    for (arrayindex_t i = startlevel; i < id_min; i++)
    {
        G->possibleGenerators -= Net::Card[PL] - i - 1;
    }

    if (id_min == Net::Card[PL])
    {
        // nothing remains to explore
        return;
    }
    assert(id_min < Net::Card[PL]);

    // now, id_min is the place with smallest id that is contained
    // in a nontrivial constraint (c_min) at position e_min.
    // split c_min into id_min and all other elements

    // only images in codomain of constraint c_min remain possible for id_min
    G->possibleGenerators -= /*old*/ (Net::Card[PL] - id_min - 1)
                                     - /*new*/(constraints[0][c_min].to - constraints[0][c_min].from);

    // swap id_min to first position inside its constraint
    arrayindex_t first = constraints[0][c_min].from;
    Vertex *swap = elements[0][DOM][e_min];
    elements[0][DOM][e_min] = elements[0][DOM][first];
    elements[0][DOM][first] = swap;

    // exclude id_min from its constraint
    ++(constraints[0][c_min].from);

    // form new singleton constraint for id_min
    constraints[0][cardConstraints[0]].from = first;
    constraints[0][cardConstraints[0]].to = first;
    constraints[0][cardConstraints[0]].parent = NULL;
    constraints[0][cardConstraints[0]].unprocessed = true;
    constraints[0][cardConstraints[0]].touched = false;

    // build unprocessed list consisting of the two resulting constraints
    constraints[0][cardConstraints[0]].next = constraints[0] + c_min;
    elements[0][DOM][first] -> inConstraint[DOM][0] = constraints[0] + cardConstraints[0];
    todo[0] = constraints[0] + (cardConstraints[0]++);
    constraints[0][c_min].unprocessed = true;
    constraints[0][c_min].next = NULL;

    // This is the point where we fork a thread to check all images but identity
    // After forking, we dive further down to identity

    // get ticket for a thread

    pthread_mutex_lock(&rt_mutex);
    if (!freeThreads)
    {
        pthread_cond_wait(&thread_cond, &rt_mutex);
    }

    ThreadTicket *MyTicket = freeThreads;
    freeThreads = freeThreads -> next;
    pthread_mutex_unlock(&rt_mutex);
    arrayindex_t thr = MyTicket -> idx;

    // allocate thread's copy of main data structures
    elements[thr] = new Vertex **[2];
    elements[thr][DOM] = new Vertex * [Net::Card[PL] + Net::Card[TR] + sizeFormula];
    elements[thr][COD] = new Vertex * [Net::Card[PL] + Net::Card[TR] + sizeFormula];
    constraints[thr] = new Constraint [Net::Card[PL] + Net::Card[TR] + sizeFormula];


    // fill thread's elements arrays
    for (arrayindex_t i = 0; i < Net::Card[PL] + Net::Card[TR] + sizeFormula; i++)
    {
        elements[thr][DOM][i] = elements[thr][COD][i] = elements[0][DOM][i];
    }

    // fill thread's constraint array
    for (arrayindex_t i = 0; i < cardConstraints[0]; i++)
    {
        constraints[thr][i] = constraints[0][i];
        // connect elements to constraints
        for (arrayindex_t j = constraints[thr][i].from; j <= constraints[thr][i].to; j++)
        {
            elements[thr][DOM][j]->inConstraint[DOM][thr] =
                elements[thr][COD][j]->inConstraint[COD][thr] = constraints[thr] + i;
        }
    }
    cardConstraints[thr] = cardConstraints[0];
    // to do list must be created in thread itself
    todo[thr] = NULL;
    // touching did not start
    firstTouched[thr] = NULL;

    MyTicket -> depth = id_min;
    MyTicket -> c_min = c_min;

    // this is the information for the thread:
    // - record found symmetries a depth id_min
    // - test all possible images (taken from constraint #c_min)...
    // - ... for the argument in the singleton constraint at cardConstraints[thr]-1
    pthread_t my_thread;

    // in case the calculation of symmetries is aborted, do not start the
    // prepared thread, but return here
    if (stop)
    {
        // check out this thread
        pthread_mutex_lock(&rt_mutex);
        MyTicket -> next = freeThreads;
        freeThreads = MyTicket;
        assert(freeThreads != NULL);
        // wake up main thread that may wait for thread to complete
        pthread_cond_signal(&thread_cond);
        pthread_mutex_unlock(&rt_mutex);

        return;
    }

    pthread_create(&my_thread, NULL, ExploreSymmetryThread, reinterpret_cast<void *>(MyTicket));

    // Now, dive down to identity

    initialRefinement();
    ExploreLevel(id_min + 1);
}


/*!
\return  whether a symmetry has been found
*/
bool SymmetryCalculator::ExploreSymmetry(arrayindex_t thr)
{
    // 1. refine until nothing changes

    while (todo[thr])
    {
        if (!refine(thr))
        {
            pthread_mutex_lock(&db_mutex);
            ++deadBranches;
            pthread_mutex_unlock(&db_mutex);
            return false;  // refine leads to inconsistency
        }
    }

    // 2. find the first node in a non-singleton constraint
    arrayindex_t c_min;  // index, will represent the non-singleton constraint
    arrayindex_t e_min = Net::Card[PL] + Net::Card[TR] +
                         sizeFormula;  // index of an element of constraint #c
    arrayindex_t id_min = Net::Card[PL] + Net::Card[TR] + sizeFormula;  // id of that element
    for (c_min = 0; c_min < cardConstraints[thr]; c_min++)
    {
        if (constraints[thr][c_min].from != constraints[thr][c_min].to)
        {
            // There exists a non-singleton constraint
            break;
        }
    }
    if (c_min >= cardConstraints[thr])
    {
        // There is no non-singleton constraint
        // --> symmetry found
        return true;
    }
    e_min = constraints[thr][c_min].from;

    // now, a place that is contained
    // in a nontrivial constraint (c_min) at position e_min.

    // We want to make sure that elements[thr][COD][e_min] does
    // not contain the smallest element

    assert(e_min + 1 < Net::Card[PL] + Net::Card[TR] + sizeFormula);
    assert(e_min + 1 <= constraints[thr][c_min].to);  // c not singleton
    if (id_min < elements[thr][COD][e_min + 1]->id)

    {
        Vertex *swap = elements[thr][COD][e_min + 1];
        elements[thr][COD][e_min + 1] = elements[thr][COD][e_min];
        elements[thr][COD][e_min] = swap;
    }

    // split c_min into id_min and all other elements

    ++(constraints[thr][c_min].from);
    constraints[thr][cardConstraints[thr]].from = e_min;
    constraints[thr][cardConstraints[thr]].to = e_min;
    constraints[thr][cardConstraints[thr]].parent = constraints[thr] + c_min;
    constraints[thr][cardConstraints[thr]].unprocessed = true;
    constraints[thr][cardConstraints[thr]].touched = false;
    constraints[thr][cardConstraints[thr]].next = constraints[thr] + c_min;
    elements[thr][DOM][e_min]->inConstraint[DOM][thr] = constraints[thr] + cardConstraints[thr];
    elements[thr][COD][e_min]->inConstraint[COD][thr] = constraints[thr] + cardConstraints[thr];
    todo[thr] = constraints[thr] + cardConstraints[thr]++;
    constraints[thr][c_min].unprocessed = true;
    constraints[thr][c_min].next = NULL;

    // This is the status we need to return to when backtracking

    arrayindex_t currentCardConstraints = cardConstraints[thr];

    /*
    Now, we need to try all possible images for id_min. The candidates are the
    nodes in constraint c_min. We process them in ascending order. We need to
    care about the following things:

    1. After every descend, the elements in c_min can be in different order, so
    the index in the elements array is not a good iterator. Instead, we look,
    in every re-iteration, for the smallest node index in c_min that is larger
    than the previously used one.

    2. When we add generators, additional generators may be inferred by
    composing known ones (this is faster than the calculation procedure. Such
    images need to be skipped.
    */

    while (true)  // for all images do...
    {
        // set back to status before recursive descent
        // subsequent levels have duplicated elements structure

        for (arrayindex_t i = 0; i < Net::Card[PL] + Net::Card[TR] + sizeFormula; i++)
        {
            elements[thr][DOM][i]->property[DOM][thr] = 0;
            elements[thr][DOM][i]->property[COD][thr] = 0;
        }
        while (cardConstraints[thr] > currentCardConstraints)
        {
            Constraint *c = constraints[thr] + --(cardConstraints[thr]);
            Constraint *p = c->parent;
            if (c->from < p->from)
            {
                p->from = c->from;
            }
            if (c->to > p->to)
            {
                p->to = c->to;
            }
            for (arrayindex_t i = c->from; i <= c->to ; i++)
            {
                elements[thr][DOM][i]->inConstraint[DOM][thr] = p;
                elements[thr][COD][i]->inConstraint[COD][thr] = p;
            }
        }
        for (arrayindex_t c = 0; c < cardConstraints[thr]; c++)
        {
            constraints[thr][c].unprocessed = false;
            constraints[thr][c].touched = false;
        }
        firstTouched[thr] = NULL;
        constraints[thr][c_min].unprocessed = true;
        constraints[thr][cardConstraints[thr] - 1].unprocessed = true;
        constraints[thr][cardConstraints[thr] - 1].next = constraints[thr] + c_min;
        constraints[thr][c_min].next = NULL;
        todo[thr] = constraints[thr] + (cardConstraints[thr] - 1);

        // select next image to be processed, in ascending order of id's
        // Since the singleton constraint at cardConstraints-1
        // initially does not contain the smallest id, we do not oversee
        // any element.

        Constraint *ourConstraint = constraints[thr] + c_min;
        arrayindex_t previous_image = 0;
        arrayindex_t next_index = 0;
        arrayindex_t next_id = Net::Card[PL] + Net::Card[TR] + sizeFormula;
        assert(ourConstraint->from <= ourConstraint->to);
        for (arrayindex_t i = ourConstraint->from; i <= ourConstraint->to; i++)
        {
            arrayindex_t current_id = elements[thr][COD][i]->id;
            if (current_id >= previous_image  // not yet processed
                    && current_id < next_id)  // smaller than other
                // candidates
            {
                next_index = i;
                next_id = current_id;
            }
        }
        if (next_id >= Net::Card[PL] + Net::Card[TR] + sizeFormula)
        {
            // no image remains
            return false;
        }

        // rearrange constraint structure such that current_id
        // becomes image for id_min

        assert(e_min < Net::Card[PL] + Net::Card[TR] + sizeFormula);
        assert(next_index < Net::Card[PL] + Net::Card[TR] + sizeFormula);
        Vertex *swap = elements[thr][COD][e_min];
        elements[thr][COD][e_min] = elements[thr][COD][next_index];
        elements[thr][COD][next_index] = swap;
        elements[thr][COD][e_min]->inConstraint[COD][thr] = constraints[thr] + (cardConstraints[thr] - 1);
        elements[thr][COD][next_index]->inConstraint[COD][thr] = ourConstraint;
        previous_image = next_id + 1;

        // dive down to find a generator
        if (ExploreSymmetry(thr))
        {
            return true;
        }
        if (stop)  // set externally
        {
            return false;  // LCOV_EXCL_LINE
        }
    }
}


//void SymmetryCalculator::printConstraints()
//{
//    for (arrayindex_t c = 0; c < cardConstraints; c++)
//    {
//        std::cout << std::endl << "==========================================" << std::endl;
//        for (arrayindex_t j = constraints[c].from; j <= constraints[c].to; j++)
//        {
//            arrayindex_t e = elements[DOM][j]->id;
//            if (e < Net::Card[PL])
//            {
//                std::cout << Net::Name[PL][e] << " ";
//            }
//            if (e >= Net::Card[PL] && e < Net::Card[PL] + Net::Card[TR])
//            {
//                std::cout << Net::Name[TR][e - Net::Card[PL]] << " ";
//            }
//            if (e >= Net::Card[PL] + Net::Card[TR])
//            {
//                std::cout << e << " ";
//            }
//        }
//        std::cout << std::endl << "-->" << std::endl;
//        for (arrayindex_t j = constraints[c].from; j <= constraints[c].to; j++)
//        {
//            arrayindex_t e = elements[COD][j]->id;
//           if (e < Net::Card[PL])
//            {
//                std::cout << Net::Name[PL][e] << " ";
//            }
//           if (e >= Net::Card[PL] && e < Net::Card[PL] + Net::Card[TR])
//            {
//                std::cout << Net::Name[TR][e - Net::Card[PL]] << " ";
//            }
//            if (e >= Net::Card[PL] + Net::Card[TR])
//            {
//                std::cout << e << " ";
//            }
//        }
//
//    }
//}
//void SymmetryCalculator::printInitialConstraints()
//{
//    for (arrayindex_t c = 0; c < cardConstraints; c++)
//    {
//        std::cout << "==========================================" << std::endl;
//        for (arrayindex_t j = constraints[c].from; j <= constraints[c].to; j++)
//        {
//            arrayindex_t e = elements[DOM][j]->id;
//            int co = elements[DOM][j]->color;
//            if (e < Net::Card[PL])
//            {
//                std::cout << Net::Name[PL][e];
//            }
//            if (e >= Net::Card[PL] && e < Net::Card[PL] + Net::Card[TR])
//            {
//                std::cout << Net::Name[TR][e - Net::Card[PL]];
//            }
//            if (e >= Net::Card[PL] + Net::Card[TR])
//            {
//                std::cout << e;
//           }
//            std::cout << "(" << co << ") ";
//        }
//        std::cout << std::endl;
//
//    }
//}
//
//void SymmetryCalculator::printInitialElements()
//{
//    for (arrayindex_t i = 0; i < Net::Card[PL] + Net::Card[TR] + sizeFormula; i++)
//    {
//        if (i < Net::Card[PL])
//        {
//            std::cout << Net::Name[PL][elements[DOM][i]->id];
//        }
//        if (i >= Net::Card[PL] && i < Net::Card[PL] + Net::Card[TR])
//        {
//            std::cout << Net::Name[TR][elements[DOM][i]->id - Net::Card[PL]];
//        }
//        if (i >= Net::Card[PL] + Net::Card[TR])
//        {
//            std::cout << elements[DOM][i]->id;
//        }
//        std::cout << " " << elements[DOM][i]->property[DOM] << std::endl;
//    }
//}
//void SymmetryCalculator::printElements()
//{
//    for (arrayindex_t i = 0; i < Net::Card[PL] + Net::Card[TR] + sizeFormula; i++)
//    {
//        if (i < Net::Card[PL])
//        {
//            std::cout << Net::Name[PL][elements[DOM][i]->id];
//        }
//        if (i >= Net::Card[PL] && i < Net::Card[PL] + Net::Card[TR])
//        {
//            std::cout << Net::Name[TR][elements[DOM][i]->id - Net::Card[PL]];
//        }
//        if (i >= Net::Card[PL] + Net::Card[TR])
//        {
//            std::cout << elements[DOM][i]->id;
//        }
//        std::cout << " " << elements[DOM][i]->property[DOM] << std::endl;
//    }
//    std::cout << std::endl << "------------" << std::endl;
//    for (arrayindex_t i = 0; i < Net::Card[PL] + Net::Card[TR] + sizeFormula; i++)
//    {
//        if (i < Net::Card[PL])
//        {
//            std::cout << Net::Name[PL][elements[COD][i]->id];
//        }
//        if (i >= Net::Card[PL] && i < Net::Card[PL] + Net::Card[TR])
//        {
//            std::cout << Net::Name[TR][elements[COD][i]->id - Net::Card[PL]];
//        }
//        if (i >= Net::Card[PL] + Net::Card[TR])
//        {
//            std::cout << elements[COD][i]->id;
//        }
//        std::cout << " " << elements[COD][i]->property[COD] << std::endl;
//    }
//}
//
//void SymmetryCalculator::printId(arrayindex_t id)
//{
//  if(id < Net::Card[PL])
//  {
//      std::cout << Net::Name[PL][id]; return;
//  }
//  if(id < Net::Card[PL]+Net::Card[TR])
//  {
//      std::cout << Net::Name[TR][id-Net::Card[PL]]; return;
//  }
//  std::cout << id-(Net::Card[PL]+Net::Card[TR]); return;
//}
//
//void SymmetryCalculator::printGraph()
//{
//  std::cout << std::endl;
//  for(arrayindex_t i = 0; i < Net::Card[PL]+Net::Card[TR]+sizeFormula;i++)
//  {
//      Vertex * vv = elements[DOM][i];
//      // print node
//      printId(vv->id);
//      std::cout << " " << vv->color << std::endl;
//
//      // print succs
//      for(arrayindex_t j = 0; j < vv->cardSucc; j++)
//      {
//          std::cout << "\t-->";
//          printId(vv->succ[j]->id);
//          std::cout << " " << vv->colorSucc[j] << std::endl;
//      }
//      // print pres
//      for(arrayindex_t j = 0; j < vv->cardPre; j++)
//      {
//          std::cout << "\t<--";
//          printId(vv->pre[j]->id);
//          std::cout << " " << vv->colorPre[j] << std::endl;
//      }
//  }
//}

SymmetryCalculator::~SymmetryCalculator()
{
    for (arrayindex_t i = 0; i < Net::Card[PL] + Net::Card[TR] + sizeFormula; i++)
    {
        delete elements[0][DOM][i];
    }
    delete[] elements[0][DOM];
    delete[] elements[0][COD];
    delete[] elements[0];
    delete[] constraints[0];
    delete[] elements;
    delete[] constraints;
    delete[] todo;
    delete[] firstTouched;
}


void *SymmetryCalculator::ExploreSymmetryThread(void *arg)
{
    // now, we need to try all possible images for id_min.
    // The candidates are the nodes in constraint c_min.
    // We process them in ascending order. We need to
    // care about the following things:
    // 1. After every descend, the elements in c_min can
    // be in different order, so the index in the elements
    // array is not a good iterator. Instead, we look,
    // in every re-iteration, for the smallest node index in
    // c_min that is larger than the previously used one.
    // 2. When we add generators, additional generators may
    // be inferred by composing known ones (this is faster than
    // the calculation procedure. Such images need to be skipped.

    ThreadTicket *T = static_cast<ThreadTicket *>(arg);
    G->openLevel(T->depth);  // book space for this level in store
    arrayindex_t previous_image = T ->depth;  // bookkeeping on previously used image
    // initially: identity = T -> depth

    arrayindex_t thr = T -> idx;
    arrayindex_t currentCardConstraints = cardConstraints[thr];
    while (true)  // for all images do...
    {
        // set back to status before recursive descent
        // subsequent levels have duplicated elements structure

        for (arrayindex_t i = 0; i < Net::Card[PL] + Net::Card[TR] + sizeFormula; i++)
        {
            elements[thr][DOM][i]->property[DOM][thr] = 0;
            elements[thr][DOM][i]->property[COD][thr] = 0;
        }
        while (cardConstraints[thr] > currentCardConstraints)
        {
            Constraint *c = constraints[thr] + --cardConstraints[thr];
            Constraint *p = c->parent;
            if (c->from < p->from)
            {
                p->from = c->from;
            }
            if (c->to > p->to)
            {
                p->to = c->to;
            }
            for (arrayindex_t i = c->from; i <= c->to; i++)
            {
                elements[thr][DOM][i]->inConstraint[DOM][thr] = p;
                elements[thr][COD][i]->inConstraint[COD][thr] = p;
            }
        }
        for (arrayindex_t c = 0; c < cardConstraints[thr]; c++)
        {
            constraints[thr][c].unprocessed = false;
            constraints[thr][c].touched = false;
        }
        firstTouched[thr] = NULL;
        constraints[thr][T->c_min].unprocessed = true;
        constraints[thr][cardConstraints[thr] - 1].unprocessed = true;
        constraints[thr][cardConstraints[thr] - 1].next = constraints[thr] + T->c_min;
        constraints[thr][T->c_min].next = NULL;
        todo[thr] = constraints[thr] + (cardConstraints[thr] - 1);

        // select next image to be processed
        Constraint *ourConstraint = constraints[thr] + T->c_min;
        arrayindex_t next_index;
        arrayindex_t next_id = Net::Card[PL];
        for (arrayindex_t i = ourConstraint->from; i <= ourConstraint->to; i++)
        {
            arrayindex_t current_id = elements[thr][COD][i]->id;
            if (current_id > previous_image  //not yet processed
                    && current_id < next_id  // smaller than other
                    // candidates
                    && !G->store[T->depth][current_id - T->depth - 1])  // no generator known
            {
                next_index = i;
                next_id = current_id;
            }
        }
        if (next_id >= Net::Card[PL])  // no image remains
        {
            break;
        }

        // rearrange constraint structure such that current_id
        // becomes image for id_min

        // first is the position where the singleton
        // constraint sits in the elements array

        arrayindex_t first = constraints[thr][cardConstraints[thr] - 1].from;
        Vertex *swap = elements[thr][COD][next_index];
        elements[thr][COD][next_index] = elements[thr][COD][first];
        elements[thr][COD][first] = swap;
        elements[thr][COD][first]->inConstraint[COD][thr] = constraints[thr] + (cardConstraints[thr] - 1);
        elements[thr][COD][next_index]->inConstraint[COD][thr] = constraints[thr] + T->c_min;
        previous_image = next_id;

        // dive down to find a generator
        if (ExploreSymmetry(thr))
        {
            // add symmetry to G
            arrayindex_t *vector = static_cast<arrayindex_t *>(calloc(sizeof(arrayindex_t),
                                   Net::Card[PL] - T->depth));
            for (arrayindex_t i = 0; i < Net::Card[PL]; i++)
            {
                arrayindex_t argument = elements[thr][DOM][i]->id;
                if (argument >= T->depth)
                {
                    vector[argument - T->depth] = elements[thr][COD][i]->id;
                }
            }
            G->add(T->depth, vector);
        }
        else
        {
            pthread_mutex_lock(&(G->mx));
            --(G->possibleGenerators);
            pthread_mutex_unlock(&(G->mx));
        }
        if (stop)  // set externally
        {
            // leave loop
            break;  // LCOV_EXCL_LINE
        }
    }
    // clean up and return control
    G->condense_level(T->depth);


    delete[] elements[thr][DOM];
    delete[] elements[thr][COD];
    delete[] elements[thr];
    delete[] constraints[thr];

    // check out this thread
    pthread_mutex_lock(&rt_mutex);
    T -> next = freeThreads;
    freeThreads = T;
    // wake up main thread that may wait for thread to complete
    pthread_cond_signal(&thread_cond);
    pthread_mutex_unlock(&rt_mutex);
    return NULL;
}
