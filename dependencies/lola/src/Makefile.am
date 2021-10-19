# the main program
bin_PROGRAMS = lola

# the plain sources you need to compile (no generated code)
lola_SOURCES = $(KCSOURCES)                                           \
               cmdline.c cmdline.h                                    \
               Core/Dimensions.h                                      \
               Core/Handlers.cc                                       \
               Core/Handlers.h                                        \
               Core/Runtime.cc                                        \
               Core/Runtime.h                                         \
               Core/main.cc                                           \
               CoverGraph/CoverGraph.cc                               \
               CoverGraph/CoverGraph.h                                \
               CoverGraph/CoverPayload.h                              \
               Exploration/AutomataTree.cc                            \
               Exploration/AutomataTree.h                             \
               Exploration/CTLExploration.cc                          \
               Exploration/CTLExploration.h                           \
               Exploration/ChooseTransition.h                         \
               Exploration/FirelistStubbornComputeBound.h             \
               Exploration/FirelistStubbornComputeBound.cc            \
               Exploration/ComputeBoundExploration.h                  \
               Exploration/ComputeBoundExploration.cc                 \
               Exploration/ChooseTransitionHashDriven.cc              \
               Exploration/ChooseTransitionHashDriven.h               \
               Exploration/ChooseTransitionRandomly.cc                \
               Exploration/ChooseTransitionRandomly.h                 \
               Exploration/DFSExploration.cc                          \
               Exploration/TSCCExplorationAGEF.cc                     \
               Exploration/TSCCExplorationEGAGEF.cc                   \
               Exploration/TSCCExploration.h                          \
               Exploration/DFSExploration.h                           \
               Exploration/DFSStackEntry.cc                           \
               Exploration/DFSStackEntry.h                            \
               Exploration/DeadlockExploration.cc                     \
               Exploration/DeadlockExploration.h                      \
               Exploration/Firelist.cc                                \
               Exploration/Firelist.h                                 \
               Exploration/FirelistStubbornLTL.cc		      \
               Exploration/FirelistStubbornLTL.h	              \
               Exploration/FirelistStubbornTsccAlwaysUpset.h          \
               Exploration/FirelistStubbornTsccAlwaysUpset.cc         \
               Exploration/FirelistStubbornTsccUpset.h                \
               Exploration/FirelistStubbornTsccUpset.cc               \
               Exploration/FirelistStubbornDeadlock.cc                \
               Exploration/FirelistStubbornDeadlock.h                 \
               Exploration/FirelistStubbornDeletion.cc                \
               Exploration/FirelistStubbornDeletion.h                 \
               Exploration/FirelistStubbornStatePredicate.cc          \
               Exploration/FirelistStubbornStatePredicate.h           \
               Exploration/LTLExploration.cc                          \
               Exploration/LTLExploration.h                           \
               Exploration/LTLStack.h                                 \
               Exploration/ParallelExploration.cc                     \
               Exploration/ParallelExploration.h                      \
               Exploration/SearchStack.h                              \
               Exploration/SimpleProperty.h                           \
               Exploration/StatePredicateProperty.cc                  \
               Exploration/StatePredicateProperty.h                   \
               Formula/FormulaInfo.cc                                 \
               Formula/FormulaInfo.h                                  \
               Formula/CTL/AUFormula.cc                               \
               Formula/CTL/AUFormula.h                                \
               Formula/CTL/AXFormula.cc                               \
               Formula/CTL/AXFormula.h                                \
               Formula/CTL/AtomicFormula.cc                           \
               Formula/CTL/AtomicFormula.h                            \
               Formula/CTL/CTLFormula.cc                              \
               Formula/CTL/CTLFormula.h                               \
               Formula/CTL/ConjunctionFormula.cc                      \
               Formula/CTL/ConjunctionFormula.h                       \
               Formula/CTL/DFSFormula.cc                              \
               Formula/CTL/DFSFormula.h                               \
               Formula/CTL/DisjunctionFormula.cc                      \
               Formula/CTL/DisjunctionFormula.h                       \
               Formula/CTL/EUFormula.cc                               \
               Formula/CTL/EUFormula.h                                \
               Formula/CTL/EXFormula.cc                               \
               Formula/CTL/EXFormula.h                                \
               Formula/CTL/NotFormula.cc                              \
               Formula/CTL/NotFormula.h                               \
               Formula/LTL/BuechiAutomata.cc                          \
               Formula/LTL/BuechiAutomata.h                           \
               Formula/LTL/BuechiFromLTL.h                            \
               Formula/LTL/alternating.c                              \
               Formula/LTL/buchi.c                                    \
               Formula/LTL/cache.c                                    \
               Formula/LTL/generalized.c                              \
               Formula/LTL/lex.c                                      \
               Formula/LTL/ltl2ba.h                                   \
               Formula/LTL/ltlmain.c                                  \
               Formula/LTL/mem.c                                      \
               Formula/LTL/parse.c                                    \
               Formula/LTL/rewrt.c                                    \
               Formula/LTL/set.c                                      \
               Formula/LTL/trans.c                                    \
               Formula/StatePredicate/AtomicStatePredicate.cc         \
               Formula/StatePredicate/AtomicStatePredicate.h          \
               Formula/StatePredicate/ConjunctionStatePredicate.cc    \
               Formula/StatePredicate/ConjunctionStatePredicate.h     \
               Formula/StatePredicate/DeadlockPredicate.cc            \
               Formula/StatePredicate/DeadlockPredicate.h             \
               Formula/StatePredicate/DisjunctionStatePredicate.cc    \
               Formula/StatePredicate/DisjunctionStatePredicate.h     \
               Formula/StatePredicate/FalsePredicate.cc               \
               Formula/StatePredicate/FalsePredicate.h                \
               Formula/StatePredicate/StatePredicate.cc               \
               Formula/StatePredicate/StatePredicate.h                \
               Formula/StatePredicate/FireablePredicate.h                \
               Formula/StatePredicate/FireablePredicate.cc                \
               Formula/StatePredicate/TruePredicate.cc                \
               Formula/StatePredicate/TruePredicate.h                 \
               Frontend/Parser/LexicNet.ll                            \
               Frontend/Parser/LexicBuechi.ll                         \
               Frontend/Parser/LexicFormula.ll                        \
               Frontend/Parser/ParserBuechi.yy                        \
               Frontend/Parser/ParserFormula.yy                       \
               Frontend/Parser/ParserPTNet.cc                         \
               Frontend/Parser/ParserPTNet.h                          \
               Frontend/Parser/ParserNet.yy                           \
               Frontend/Parser/error.cc                               \
               Frontend/Parser/error.h                                \
               Frontend/SymbolTable/ArcList.cc                        \
               Frontend/SymbolTable/ArcList.h                         \
               Frontend/SymbolTable/PlaceSymbol.cc                    \
               Frontend/SymbolTable/PlaceSymbol.h                     \
               Frontend/SymbolTable/Symbol.cc                         \
               Frontend/SymbolTable/Symbol.h                          \
               Frontend/SymbolTable/SymbolTable.cc                    \
               Frontend/SymbolTable/SymbolTable.h                     \
               Frontend/SymbolTable/TransitionSymbol.cc               \
               Frontend/SymbolTable/TransitionSymbol.h                \
               InputOutput/CompressedIO.cc                            \
               InputOutput/CompressedIO.h                             \
               InputOutput/InputOutput.cc                             \
               InputOutput/InputOutput.h                              \
               InputOutput/JSON.cc                                    \
               InputOutput/JSON.h                                     \
               InputOutput/Reporter.cc                                \
               InputOutput/Reporter.h                                 \
               InputOutput/SimpleString.cc                            \
               InputOutput/SimpleString.h                             \
               InputOutput/Socket.cc                                  \
               InputOutput/Socket.h                                   \
               InputOutput/vasprintf.c                                \
               InputOutput/vasprintf.h                                \
               Net/LinearAlgebra.cc                                   \
               Net/LinearAlgebra.h                                    \
               Net/Marking.cc                                         \
               Net/Marking.h                                          \
               Net/Net.cc                                             \
               Net/Net.h                                              \
               Net/NetState.cc                                        \
               Net/NetState.h                                         \
               Net/Place.cc                                           \
               Net/Place.h                                            \
               Net/Transition.cc                                      \
               Net/Transition.h                                       \
               Planning/LTLTask.cc                                    \
               Planning/ComputeBoundTask.cc                           \
               Planning/ComputeBoundTask.h                            \
               Planning/CompoundTask.cc                               \
               Planning/CompoundTask.h                                \
               Planning/ConjunctionTask.cc                            \
               Planning/DisjunctionTask.cc                            \
               Planning/LeafTask.cc                                   \
               Planning/LeafTask.h                                    \
               Planning/ConjunctionTask.h                             \
               Planning/DisjunctionTask.h                             \
               Planning/BooleanTask.h                                 \
               Planning/LTLTask.h                                     \
               Planning/DeadlockFindpathTask.cc                       \
               Planning/DeadlockFindpathTask.h                        \
               Planning/DeadlockSearchTask.cc                         \
               Planning/DeadlockSearchTask.h                          \
               Planning/DeadlockTask.h                                \
               Planning/EFAGTask.h                                    \
               Planning/AGEFAGTask.h                                  \
               Planning/EFAGEFTask.h                                  \
               Planning/EFAGEFTask.cc                                 \
               Planning/AGEFTask.h                                    \
               Planning/AGEFTask.cc                                   \
               Planning/EmptyTask.h                                   \
               Planning/InitialTask.cc                                \
               Planning/InitialTask.h                                 \
               Planning/InvariantTask.h                               \
               Planning/NoDeadlockTask.h                              \
               Planning/ParallelTask.cc                               \
               Planning/ParallelTask.h                                \
               Planning/ReachabilityFindpathTask.cc                   \
               Planning/ReachabilityFindpathTask.h                    \
               Planning/ReachabilitySearchTask.cc                     \
               Planning/ReachabilitySearchTask.h                      \
               Planning/ReachabilityTask.h                            \
               Planning/SequentialTask.h                              \
               Planning/SiphonTrapTask.h                              \
	       Planning/StateEquationTask.h			      \
	       Planning/StateEquationTask.cc			      \
               Planning/StoreCreator.h                                \
               Planning/StoreCreator.cc                               \
               Planning/FullTask.cc                                   \
               Planning/FullTask.h                                    \
               Planning/Task.cc                                       \
               Planning/Task.h                                        \
               Planning/CTLTask.cc                                    \
               Planning/CTLTask.h                                     \
               Planning/StoreCreator.h                                \
	       Siphon/lola2minisat.h				      \
               Stores/CompareStore.h                                  \
               Stores/CompareStore.inc                                \
               Stores/CycleStore.h                                    \
               Stores/EmptyStore.h                                    \
               Stores/NetStateEncoder/BitEncoder.cc                   \
               Stores/NetStateEncoder/BitEncoder.h                    \
               Stores/NetStateEncoder/CopyEncoder.cc                  \
               Stores/NetStateEncoder/CopyEncoder.h                   \
               Stores/NetStateEncoder/FullCopyEncoder.cc              \
               Stores/NetStateEncoder/FullCopyEncoder.h               \
               Stores/NetStateEncoder/NetStateEncoder.cc              \
               Stores/NetStateEncoder/NetStateEncoder.h               \
               Stores/NetStateEncoder/SimpleCompressedEncoder.cc      \
               Stores/NetStateEncoder/SimpleCompressedEncoder.h       \
               Stores/PluginStore.h                                   \
               Stores/PluginStore.inc                                 \
               Stores/Store.h                                         \
               Stores/Store.inc                                       \
               Stores/SymmetryStore.h                                 \
               Stores/VectorStores/HashingWrapperStore.h              \
               Stores/VectorStores/HashingWrapperStore.inc            \
               Stores/VectorStores/PrefixTreeStore.cc                 \
               Stores/VectorStores/PrefixTreeStore.h                  \
               Stores/VectorStores/PrefixTreeStore.inc                \
               Stores/VectorStores/VBloomStore.h                      \
               Stores/VectorStores/VBloomStore.inc                    \
               Stores/VectorStores/VSTLStore.cc                       \
               Stores/VectorStores/VSTLStore.h                        \
               Stores/VectorStores/VSTLStore.inc                      \
               Stores/VectorStores/VectorStore.h                      \
               SweepLine/Sweep.h                                      \
               SweepLine/Sweep.inc                                    \
               SweepLine/SweepEmptyStore.cc                           \
               SweepLine/SweepEmptyStore.h                            \
               SweepLine/SweepListStore.h                             \
               SweepLine/SweepListStore.inc                           \
               SweepLine/SweepRingStore.h                             \
               SweepLine/SweepRingStore.inc                           \
               Symmetry/AutomorphismGraph.h                           \
               Symmetry/AutomorphismGraph.cc                          \
               Symmetry/Constraints.cc                                \
               Symmetry/Constraints.h                                 \
               Symmetry/Symmetry.cc                                   \
               Symmetry/Symmetry.h                                    \
               Witness/Condition.cc                                   \
               Witness/Condition.h                                    \
               Witness/Event.cc                                       \
               Witness/Event.h                                        \
               Witness/Path.cc                                        \
               Witness/Path.h					      \
	       Siphon/lola2minisat.cc				      \
	ThirdParty/minisat/core/Dimacs.h \
	ThirdParty/minisat/core/Solver.cc \
	ThirdParty/minisat/core/Solver.h \
	ThirdParty/minisat/core/SolverTypes.h \
	ThirdParty/minisat/mtl/Alg.h \
	ThirdParty/minisat/mtl/Alloc.h \
	ThirdParty/minisat/mtl/IntTypes.h \
	ThirdParty/minisat/mtl/Map.h \
	ThirdParty/minisat/mtl/Queue.h \
	ThirdParty/minisat/mtl/Sort.h \
	ThirdParty/minisat/mtl/Heap.h \
	ThirdParty/minisat/mtl/Vec.h \
	ThirdParty/minisat/mtl/XAlloc.h \
	ThirdParty/minisat/simp/Main.cc \
	ThirdParty/minisat/simp/SimpSolver.cc \
	ThirdParty/minisat/simp/SimpSolver.h \
	ThirdParty/minisat/utils/Options.cc \
	ThirdParty/minisat/utils/Options.h \
	ThirdParty/minisat/utils/ParseUtils.h \
	ThirdParty/minisat/utils/System.h \
	ThirdParty/minisat/utils/System.cc \
	ThirdParty/minisat/ThirdParty/zlib/zconf.h \
	ThirdParty/minisat/ThirdParty/zlib/zlib.h
	

#############################################################################
# EVERYTHING BELOW THIS LINE IS GENERIC - YOU MUST NOT CHANGE ANYTHING BELOW
#############################################################################

include $(top_srcdir)/m4/Makefile.src


##########################################
# everything about the Kimwitu++ sources #
##########################################

# define flags for Kimwitu++:
#  - create a file "ast-system-yystype.h" for Flex and Bison
#  - prefix all generated files with "ast-system-"
#  - suffix all generated files with "cc"
#  - do not create functions for phylum I/O
KCFLAGS = --yystype --file-prefix=ast-system- --suffix=cc --no-csgio

# a dummy file to use Makefile's timestamps
KCDUMMY = Frontend/Parser/.timestamp

BUILT_SOURCES += $(KCDUMMY)


# Files that create C++ files that are not linked to the executable. These
# sources can be deleted before compilation (usually the unparsers).
KCFILES_TEMP =

# files that create C++ that have to be linked to executable
KCFILES_NECESSARY = Frontend/Parser/formula_abstract.k Frontend/Parser/formula_rewrite.k Frontend/Parser/formula_unparse.k

# source files generated by Kimwitu++ to be distributed
KCSOURCES = Frontend/Parser/ast-system-k.cc Frontend/Parser/ast-system-k.h \
	Frontend/Parser/ast-system-rk.cc Frontend/Parser/ast-system-rk.h \
	Frontend/Parser/ast-system-unpk.cc Frontend/Parser/ast-system-unpk.h \
	$(KCFILES_NECESSARY:.k=.cc) \
	$(KCFILES_NECESSARY:.k=.h) \
	Frontend/Parser/ast-system-yystype.h

# Dependencies of the Kimwitu++ files: the timestamp is renewed when a
# Kimwitu++ file is touched. This triggers the creation of all source files.
# The sed-script fixes problems occurring when BPEL2oWFN is compiled on
# x86_64 machines.
$(KCDUMMY): $(KCFILES_NECESSARY) $(KCFILES_TEMP)
	$(AM_V_at)touch $@
	$(AM_V_GEN)$(KC) $(KCFLAGS) $^
	$(AM_V_at)$(SED) -i 's/%x", reinterpret_cast<int>/%lx", reinterpret_cast<unsigned long int>/g' ast-system-k.cc
	$(AM_V_at)$(SED) -i 's/%x_%d", reinterpret_cast<int>/%lx_%d", reinterpret_cast<unsigned long int>/g' ast-system-k.cc
	$(AM_V_at)mv $(KCSOURCES:Frontend/Parser/%=%) Frontend/Parser

MAINTAINERCLEANFILES += $(KCSOURCES) $(KCDUMMY)

EXTRA_DIST += $(KCDUMMY) $(KCFILES_TEMP) $(KCFILES_NECESSARY)
