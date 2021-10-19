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


/* We create a CNF formula that is unsatisiable if all siphons in the net
 * contain a sufficiently marked trap. This condition is sufficient for
 * deadlock freedom.
 * In the theory paper, the formula works on Variables p^i where p is a
 * place and i is an index between 0 and (including) #P. Let P^i be the
 * set of all variables with index i, and let X^i be those variables of P^i
 * that are true in a satisfying assignment.
 * The formula in the paper consists of the following sets of clauses:
 * 1. Siphon: operates on P^0 such that X^i is an arbitrary siphon of the net.
 * 2. for all i (0 <=i<#P), Step^i: operates on P^i and P^(i+1) such that 
 *    X^(i+1) results from X^i by removing all places p that have a post-
 *    transition t such that all post-places of t are not in X^i.
 *    The step formulas mimic the process of computing the max. trap in X^0.
 *    Hence, X^#P is the maximal trap in X^0.
 * 3: Unmarked: operates on P^#P and is satisfied if the trap X^#P is unmarked.
 *
 * The size of this formula is quadratic in the size of the net which may
 * cause memory problems. For this reason, we introduce a parameter "depth"
 * and modify the formula as follows:
 * 1. Siphon, as above
 * 2. Step^i, as above, but only for 0<=i < depth (depth should be at most #P)
 * 3. Eval = Unmarked OR Notrap
 * 3a.Unmarked: as above, but operates on P^depth
 * 3b.Notrap: operates on P^depth and is satisfied if X^depth is not a trap.
 *
 * The SAT check of this formula can have three outcomes:
 * A) unsatisfiable: every siphon contains a marked trap and we can assert
 *    deadlock freedom
 * B) satisfiable and Unmarked = true: There is a siphon where (a superset of)
 *    the max trap is unmarked -> deadlock freedom can not be asserted, 
 *    increasing depth would not help
 * C) satisfiable, Unmarked= false (hence: Notrap = true): There is a siphon
 *    where the computation of max trap needs more then "depth" steps.
 *    Deadlock freedom can not be asserted, but re-run with larger value for 
 *    depth can perhaps lead to case A.
 * Case C is impossible if depth = #P.
 *
 * In the implementation, we use additional variables:
 * - Unmarked and Notrap, for easy diagnosis on the satisfying assignment
 * For avoiding formula explosion:
 * - for all i <= depth and all transitions t: post^i,t that is true if
 *   X^i contains post-places of t.
 * - for all t: pre^depth,t that is true if X^depth contains pre-places of t.
 * - for all t: Notrap^t that is true if t violates the trap condition on
 *   X^depth.
 */

#include <Core/Dimensions.h>
#include <Net/Net.h>
#include <Net/Marking.h>
#include <Core/Runtime.h>
#include <Siphon/lola2minisat.h>

#include <ThirdParty/minisat/ThirdParty/zlib/zlib.h>
#include <ThirdParty/minisat/utils/System.h>
#include <ThirdParty/minisat/utils/ParseUtils.h>
#include <ThirdParty/minisat/utils/Options.h>
#include <ThirdParty/minisat/core/Dimacs.h>
#include <ThirdParty/minisat/simp/SimpSolver.h>


using namespace std;
using namespace Minisat;

/* In Minisat, variables are made of integers starting with 0 
   We start with macros for addressing our variables */

#define VAR_UNMARKED 0
#define VAR_NOTRAP 1
#define VAR_NOTRAP_T(T) (2+(T)) 
#define VAR_PRE(T) (2 + Net::Card[TR] +(T))
#define VAR_POST(T,I) (2 + (I+2)* Net::Card[TR] + (T))
#define VAR_P(P,I) (2 + (SiphonDepth+3) * Net::Card[TR] + (I) * Net::Card[PL] + (P))

// The depth to which the step formulas are unrolled
arrayindex_t SiphonDepth = 0;

// CREATE SIPHON FORMULA
// =====================
// = Nonempty and, for all transitions t, (Post^t,0 -> OR(p in pre(t)) p^0)

void create_siphon(SimpSolver &S)
{
	// nonempty = OR(p in P) p^0

	vec<Lit> nonempty;
	for(arrayindex_t i = 0; i < Net::Card[PL]; i++) 
	{
		nonempty.push(mkLit(VAR_P(i,0)));
	}
	S.addClause_(nonempty); // ship clause

	for(arrayindex_t t = 0; t < Net::Card[TR]; t++) // for all transitions
	{
		// (Post^t,0 -> OR(p in pre(t)) p^0)

		vec<Lit> siphonproperty;
		siphonproperty.push(~mkLit(VAR_POST(t,0)));
		for(arrayindex_t a = 0; a < Net::CardArcs[TR][PRE][t];a++)
		{
			siphonproperty.push(mkLit(VAR_P(Net::Arc[TR][PRE][t][a],0)));
		}
		S.addClause_(siphonproperty); // ship clause
	}
}

// CREATE SINGLE POST FORMULA
// ==========================
// = Post^t,i <-> OR(p in post(t)) p^i

void create_post(SimpSolver &S, arrayindex_t t, arrayindex_t i)
{
	// --> part of <-> is single clause
	vec<Lit> post_implication;
	post_implication.push(~mkLit(VAR_POST(t,i)));
	for(arrayindex_t a = 0; a < Net::CardArcs[TR][POST][t];a++)
	{
		post_implication.push(mkLit(VAR_P(Net::Arc[TR][POST][t][a],i)));
	}
	S.addClause_(post_implication); // ship clause

	// <-- part of <->: for all p in post(t): clause Post^t,i or not p^i
	for(arrayindex_t a = 0; a < Net::CardArcs[TR][POST][t]; a ++)
	{
		vec<Lit> post_replication;
		post_replication.push(mkLit(VAR_POST(t,i)));
		post_replication.push(~mkLit(VAR_P(Net::Arc[TR][POST][t][a],i)));
		S.addClause_(post_replication); // ship clause
	}
}

// CREATE STEP FORMULA
// ===================
// = (for all p in P:) p^i+1 <-> p^i and AND(t in post(p)) Post^t,i 

void create_step(SimpSolver &S, arrayindex_t i)
{
	for(arrayindex_t p = 0; p < Net::Card[PL];p++) // for all places p
	{
		// definition of p^i+1 part "-->" 
   		// amounts to 1 + card(post(p)) clauses

		// clause 1: not p^i+1 or p^i

		vec<Lit> nextp1;
		nextp1.push(~mkLit(VAR_P(p,i+1)));
		nextp1.push(mkLit(VAR_P(p,i)));
		S.addClause_(nextp1);
		
		// clause 2+ (for all t in post(p):) not p^i+1 or Post^t,i
		for(arrayindex_t b = 0; b < Net::CardArcs[PL][POST][p];b++)
		{
			vec<Lit> nextp2;
			nextp2.push(~mkLit(VAR_P(p,i+1)));
			nextp2.push(mkLit(VAR_POST(Net::Arc[PL][POST][p][b],i)));
			S.addClause_(nextp2); 
		}

		// definition of p^i+1 part "<--" 
		// amounts to single clause
   	 	// not p^i or p^i+1 or OR(t in post(p)) not Post^t,i
		vec<Lit> nextp3;
		nextp3.clear();
		nextp3.push(~mkLit(VAR_P(p,i)));	
		nextp3.push(mkLit(VAR_P(p,i+1)));	
		for(arrayindex_t b = 0; b < Net::CardArcs[PL][POST][p]; b++)
		{
			nextp3.push(~mkLit(VAR_POST(Net::Arc[PL][POST][p][b],i)));
		}
		S.addClause_(nextp3);
	}
}

// CREATE PRE FORMULA
// ==================
// (like POST but reverse arc direction)

void create_pre(SimpSolver &S, arrayindex_t t)
{
	// --> part of <-> is single clause
	vec<Lit> pre_implication;
	pre_implication.push(~mkLit(VAR_PRE(t)));
	for(arrayindex_t a = 0; a < Net::CardArcs[TR][PRE][t];a++)
	{
		pre_implication.push(mkLit(VAR_P(Net::Arc[TR][PRE][t][a],SiphonDepth)));
	}
	S.addClause_(pre_implication); // ship clause

	// <-- part of <->: for all p in pre(t): clause Pre^t,depth or not p^depth
	for(arrayindex_t a = 0; a < Net::CardArcs[TR][PRE][t]; a ++)
	{
		vec<Lit> pre_replication;
		pre_replication.push(mkLit(VAR_PRE(t)));
		pre_replication.push(~mkLit(VAR_P(Net::Arc[TR][PRE][t][a],SiphonDepth)));
		S.addClause_(pre_replication); // ship clause
	}
}

// CREATE UNMARKED FORMULA
// =======================
// = unmarked <-> AND(p:m0(p) > 0) not p^depth 

void create_unmarked(SimpSolver &S)
{
	// implication amounts to clauses, for all p (m0(p)>0):
	//  not unmarked or not p^n
	for(arrayindex_t p = 0; p < Net::Card[PL]; p++)
	{
		if(Marking::Initial[p] >0)
		{
			vec<Lit> unmarked_implication;
			unmarked_implication.push(~mkLit(VAR_UNMARKED));
			unmarked_implication.push(~mkLit(VAR_P(p,SiphonDepth)));
			S.addClause_(unmarked_implication); // ship clause
		}
	}
	// replication amounts to single clause
	// unmarked or OR(p:m0(p)>0) p^depth
	vec<Lit> unmarked_replication;
	unmarked_replication.push(mkLit(VAR_UNMARKED));
	for(arrayindex_t p = 0; p < Net::Card[PL];p++)
	{
		if(Marking::Initial[p] > 0)
		{
			unmarked_replication.push(mkLit(VAR_P(p,SiphonDepth)));
		}
	}
	S.addClause_(unmarked_replication); // ship clause
}

// CREATE NOTRAP FORMULA
// =====================
// = notrap <-> exists t in T: notrap_t

void create_notrap(SimpSolver &S)
{
	// implication amounts to single clause
	vec<Lit> notrap_implication;
	notrap_implication.push(~mkLit(VAR_NOTRAP));
	for(arrayindex_t t = 0; t < Net::Card[TR];t++)
	{
		notrap_implication.push(mkLit(VAR_NOTRAP_T(t)));
	}
	S.addClause_(notrap_implication); // ship clause

	// replication amounts to clauses (for all t in T)
	// not notrap_t(t) or notrap
	for(arrayindex_t t = 0; t < Net::Card[TR];t++)
	{
		vec<Lit> notrap_replication;
		notrap_replication.push(~mkLit(VAR_NOTRAP_T(t)));
		notrap_replication.push(mkLit(VAR_NOTRAP));
		S.addClause_(notrap_replication); // ship clause
	}
}

// CREATE FORMULA NOTRAP_T
// =======================
// = notrap^t <--> Pre^t,depth and not Post^t,depth

void create_notrap_t(SimpSolver &S, arrayindex_t t)
{
	// implication amounts to two clauses
	vec<Lit> notrap_t1;
	notrap_t1.push(~mkLit(VAR_NOTRAP_T(t)));
	notrap_t1.push(mkLit(VAR_PRE(t)));
	S.addClause_(notrap_t1); // ship clause

	vec<Lit> notrap_t2;
	notrap_t2.push(~mkLit(VAR_NOTRAP_T(t)));
	notrap_t2.push(~mkLit(VAR_POST(t,SiphonDepth)));
	S.addClause_(notrap_t2); // ship clause

	// replication amounts to single clause
	vec<Lit> notrap_t3;
	notrap_t3.push(mkLit(VAR_NOTRAP_T(t)));
	notrap_t3.push(mkLit(VAR_POST(t,SiphonDepth)));
	notrap_t3.push(~mkLit(VAR_PRE(t)));
	S.addClause_(notrap_t3); // ship clause
}

// CREATE EVAL FORMULA
// ===================
// = (unmarked or notrap) + definitions of unmarked, notrap, and pre

void create_eval(SimpSolver &S)
{
	// actual eval formula
	vec<Lit> eval;
	eval.push(mkLit(VAR_UNMARKED));
	eval.push(mkLit(VAR_NOTRAP));
	S.addClause_(eval);

	create_unmarked(S);
	create_notrap(S);
	for(arrayindex_t t = 0; t < Net::Card[TR];t++)
	{
		create_pre(S,t);
		create_notrap_t(S,t);
	}
}

// CREATE WHOLE FORMULA 
// ====================
// = Siphon and AND(i=0..depth-1) Step^i and Eval

void create_whole(SimpSolver &S)
{

	// Introduce sufficiently many variables to Minisat
	for(unsigned int i = 0; 
	    i < (2 + (SiphonDepth+3) * Net::Card[TR] + (SiphonDepth+1) * Net::Card[PL]);
	    i++)
	{
		S.newVar();
	}
	
	// create main formula

	create_siphon(S);
	for(arrayindex_t i = 0; i < SiphonDepth; i++)
	{
		create_step(S,i);
	}
	create_eval(S);

	// create definitions of Post^t,i variables

	for(arrayindex_t t = 0; t < Net::Card[TR];t++)
	{
		for(arrayindex_t i = 0; i <= SiphonDepth; i++)
		{
			create_post(S,t,i);
		}
	}
}

siphon_result_t run_minisat(int argc,char ** argv);

siphon_result_t lola2minisat()
{
	// check multiplicities: siphon/trap property can only be applied
        // if all [p,t] arcs have multiplicity 1

	for(arrayindex_t t = 0; t < Net::Card[TR]; t++)
	{
		for(arrayindex_t p = 0; p < Net::CardArcs[TR][PRE][t]; p++)
		{
			if(Net::Mult[TR][PRE][t][p] != 1)
			{
				return SIPHON_INHOMOGENIOUS;
			}
		}
	}

	// Get a value for SiphonDepth from command line
	// build the CNF formula

	if(RT::args.siphondepth_given)
	{
		SiphonDepth = RT::args.siphondepth_arg;
		if(SiphonDepth > Net::Card[PL])
		{
			SiphonDepth = Net::Card[PL];
		}
	}
	else
	{
		SiphonDepth = Net::Card[PL];
	}
	if(SiphonDepth < 1) 
	{
		SiphonDepth = 1;
	}	

	// get minisat args and turn them into argc/argv

	char * original;
	if(RT::args.minisatargs_given)
	{
		original = RT::args.minisatargs_arg;
	}
	else
	{
		original = new char[1];
		original[0] = '\0';
	}
	int argcc = 0;
	char * passed_string = new char[strlen(original) +5];
	strcpy(passed_string,"x x "); // dummy file name
	strcpy(passed_string+4,original);
	for(int i = 0; i < strlen(passed_string); i++)
	{
		if(passed_string[i] > ' ' && passed_string[i+1] <= ' ')
		{
			// count ends of visible char sequences
			argcc++;
		}
	}
	char ** argvv = new char * [argcc];
	int argnr = 0;
	int j = strlen(passed_string);
	for(int i = 0; i < j; i++)
	{
		if( i == 0 ||
		   (passed_string[i] > ' ' && passed_string[i-1] == '\0'))
		{
			argvv[argnr++] = passed_string + i;
		}
		if(passed_string[i] <= ' ')
		{
			passed_string[i] = '\0';
		}
	}

	// run minisat
	
	return run_minisat(argcc,argvv);
}



// signal handler: cannot be tested
// LCOV_EXCL_START

static Solver* solver;
void printStats(Solver& solver);
static void SIGINT_interrupt(int signum) { solver->interrupt(); }
static void SIGINT_exit(int signum) {
    printf("\n"); printf("*** INTERRUPTED ***\n");
    if (solver->verbosity > 0){
        printStats(*solver);
        printf("\n"); printf("*** INTERRUPTED ***\n"); }
    _exit(1); }

// LCOV_EXCL_STOP

void printAssignment(SimpSolver & S);

siphon_result_t run_minisat(int argc,char ** argv)
{
    try {
	setUsageHelp("USAGE: %s [options] <input-file> <result-output-file>\n\n  where input may be either in plain or gzipped DIMACS.\n");
#if defined(__linux__)
        fpu_control_t oldcw, newcw;
        _FPU_GETCW(oldcw); newcw = (oldcw & ~_FPU_EXTENDED) | _FPU_DOUBLE; _FPU_SETCW(newcw);
#endif
        // Extra options:
        //
        IntOption    verb   ("MAIN", "verb",   "Verbosity level (0=silent, 1=some, 2=more).", 0, IntRange(0, 2));
        BoolOption   pre    ("MAIN", "pre",    "Completely turn on/off any preprocessing.", true);
        StringOption dimacs ("MAIN", "dimacs", "If given, stop after preprocessing and write the result to this file.");
        IntOption    cpu_lim("MAIN", "cpu-lim","Limit on CPU time allowed in seconds.\n", std::numeric_limits<int32_t>::max(), IntRange(0, std::numeric_limits<int32_t>::max()));
        IntOption    mem_lim("MAIN", "mem-lim","Limit on memory usage in megabytes.\n", std::numeric_limits<int32_t>::max(), IntRange(0, std::numeric_limits<int32_t>::max()));

        parseOptions(argc, argv, true);
        
        SimpSolver  S;
        double      initial_time = cpuTime();

        if (!pre) S.eliminate(true);

        S.verbosity = verb;
        
        solver = &S;
        // Use signal handlers that forcibly quit until the solver will be able to respond to
        // interrupts:
        signal(SIGINT, SIGINT_exit);
        signal(SIGXCPU,SIGINT_exit);

        // Set limit on CPU-time:
        if (cpu_lim != std::numeric_limits<int32_t>::max()){
            rlimit rl;
            getrlimit(RLIMIT_CPU, &rl);
            if (rl.rlim_max == RLIM_INFINITY || (rlim_t)cpu_lim < rl.rlim_max){
                rl.rlim_cur = cpu_lim;
                if (setrlimit(RLIMIT_CPU, &rl) == -1)
                    printf("WARNING! Could not set resource limit: CPU-time.\n");
            } }

        // Set limit on virtual memory:
        if (mem_lim != std::numeric_limits<int32_t>::max()){
            rlim_t new_mem_lim = (rlim_t)mem_lim * 1024*1024;
            rlimit rl;
            getrlimit(RLIMIT_AS, &rl);
            if (rl.rlim_max == RLIM_INFINITY || new_mem_lim < rl.rlim_max){
                rl.rlim_cur = new_mem_lim;
                if (setrlimit(RLIMIT_AS, &rl) == -1)
                    printf("WARNING! Could not set resource limit: Virtual memory.\n");
            } }
        
//        if (argc == 1)
//            printf("Reading from standard input... Use '--help' for help.\n");
//
//        gzFile in = (argc == 1) ? gzdopen(0, "rb") : gzopen(argv[1], "rb");
//        if (in == NULL)
//            printf("ERROR! Could not open file: %s\n", argc == 1 ? "<stdin>" : argv[1]), exit(1);
        
        if (S.verbosity > 0){
            printf("============================[ Problem Statistics ]=============================\n");
            printf("|                                                                             |\n"); }
        
//        parse_DIMACS(in, S);
//        gzclose(in);
	create_whole(S);
	RT::rep->status("STP: formula with %u variables and %u clauses shipped to Minisat",S.nVars(), S.nClauses());

        FILE* res = (argc >= 3) ? fopen(argv[2], "wb") : NULL;

        if (S.verbosity > 0){
            printf("|  Number of variables:  %12d                                         |\n", S.nVars());
            printf("|  Number of clauses:    %12d                                         |\n", S.nClauses()); }
        
        double parsed_time = cpuTime();
        if (S.verbosity > 0)
            printf("|  Parse time:           %12.2f s                                       |\n", parsed_time - initial_time);

        // Change to signal-handlers that will only notify the solver and allow it to terminate
        // voluntarily:
        signal(SIGINT, SIGINT_interrupt);
        signal(SIGXCPU,SIGINT_interrupt);

        S.eliminate(true);
        double simplified_time = cpuTime();
        if (S.verbosity > 0){
            printf("|  Simplification time:  %12.2f s                                       |\n", simplified_time - parsed_time);
            printf("|                                                                             |\n"); }

        if (!S.okay()){
            if (res != NULL) fprintf(res, "UNSAT\n"), fclose(res);
            if (S.verbosity > 0){
                printf("===============================================================================\n");
                printf("Solved by simplification\n");
                printStats(S);
                printf("\n"); }
		return SIPHON_PROPERTY_TRUE;
        }

        if (dimacs){
            if (S.verbosity > 0)
                printf("==============================[ Writing DIMACS ]===============================\n");
            S.toDimacs((const char*)dimacs);
            if (S.verbosity > 0)
                printStats(S);
            return SIPHON_INDETERMINATE;
        }

        vec<Lit> dummy;
        lbool ret = S.solveLimited(dummy);
        
        if (S.verbosity > 0){
            printStats(S);
            printf("\n"); }
//        printf(ret == l_True ? "SATISFIABLE\n" : ret == l_False ? "UNSATISFIABLE\n" : "INDETERMINATE\n");
//if(ret == l_True) printAssignment(S);
        if (res != NULL){
            if (ret == l_True){
                fprintf(res, "SAT\n");
                for (int i = 0; i < S.nVars(); i++)
                    if (S.model[i] != l_Undef)
                        fprintf(res, "%s%s%d", (i==0)?"":" ", (S.model[i]==l_True)?"":"-", i+1);
                fprintf(res, " 0\n");
            }else if (ret == l_False)
                fprintf(res, "UNSAT\n");
            else
                fprintf(res, "INDET\n");
            fclose(res);
        }
if(ret == l_False)
{
	return SIPHON_PROPERTY_TRUE;
}
if(ret != l_True)
{
	return SIPHON_INDETERMINATE;
}
assert(ret == l_True);
if(S.model[0] != l_False)
{
	// variable UNMARKED is true
        // print witness siphon
        if (RT::args.siphonwitness_given)
        {
            RT::rep->status("print witness siphon (%s)",
                            RT::rep->markup(MARKUP_PARAMETER, "--siphonwitness").str());
            Output o("witness siphon", RT::args.siphonwitness_arg);
	    for(arrayindex_t i = 0; i < Net::Card[PL];i++)
	    {
		if(S.model[VAR_P(i,0)] != l_False)
		{
            		fprintf(o, "%s\n", Net::Name[PL][i]);
		}
	    }
	    fprintf(o,"\n");
	    for(arrayindex_t i = 0; i < Net::Card[PL];i++)
	    {
		if(S.model[VAR_P(i,SiphonDepth)] != l_False)
		{
            		fprintf(o, "%s\n", Net::Name[PL][i]);
		}
	    }
        }
	return SIPHON_PROPERTY_FALSE;
}
	
assert(S.model[1] == l_True);
// variable NOTRAP is true
return SIPHON_INCONCLUSIVE;
//#ifdef NDEBUG
//        exit(ret == l_True ? 10 : ret == l_False ? 20 : 0);     // (faster than "return", which will invoke the destructor for 'Solver')
//#else
//        return (ret == l_True ? 10 : ret == l_False ? 20 : 0);
//#endif
    } catch (OutOfMemoryException&){
        printf("===============================================================================\n");
        printf("INDETERMINATE\n");
        exit(0);
    }
}

// below: only debig functions
// LCOV_EXCL_START

void printValue(lbool v)
{
	if(v == l_True)
	{
		printf("T");
		return;
	}
	if(v == l_False)
	{
		printf("F");
		return;
	}
	if(v == l_Undef)
	{
		printf("U");
		return;
	}
	printf("?");
}

void printAssignment(SimpSolver & S)
{
	// unmarked

	printf("%u = Unmarked: ",VAR_UNMARKED);
	printValue(S.model[VAR_UNMARKED]);
	printf("\n");
	
	// notrap

	printf("%u = NoTrap: ",VAR_NOTRAP);
	printValue(S.model[VAR_NOTRAP]);
	printf("\n");

	// notrap^t

	for(arrayindex_t t = 0; t < Net::Card[TR]; t++)
	{
		printf("%u = Notrap[%s]: ",VAR_NOTRAP_T(t),Net::Name[TR][t]);
		printValue(S.model[VAR_NOTRAP_T(t)]);
		printf("\n");
	}

	// pre^t

	for(arrayindex_t t = 0; t < Net::Card[TR]; t++)
	{
		printf("%u = Pre[%s]: ",VAR_PRE(t),Net::Name[TR][t]);
		printValue(S.model[VAR_PRE(t)]);
		printf("\n");
	}

	// post^t,i

	for(arrayindex_t i = 0; i <= SiphonDepth;i++)
	{
		for(arrayindex_t t = 0; t < Net::Card[TR]; t++)
		{
			printf("%u = Post[%s,%u]: ",VAR_POST(t,i),Net::Name[TR][t],i);
			printValue(S.model[VAR_POST(t,i)]);
			printf("\n");
		}
	}

	// p^t,i

	for(arrayindex_t i = 0; i <= SiphonDepth;i++)
	{
		for(arrayindex_t p = 0; p < Net::Card[PL]; p++)
		{
			printf("%u = Place[%s,%u]: ",VAR_P(p,i),Net::Name[PL][p],i);
			printValue(S.model[VAR_P(p,i)]);
			printf("\n");
		}
	}
}

// LCOV_EXCL_STOP
