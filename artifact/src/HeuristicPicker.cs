#define GUROBI

using System;
using System.Collections.Generic;
using Petri;
using System.Linq;
using System.Diagnostics;
using Benchmark;
using HeuristicFrontier;
using Utils;
#if GUROBI
#endif

namespace PetriTool
{
    class HeuristicPicker
    {
        public static HeuristicFrontier<Marking> ChooseHeuristicFrontier(HeuristicOption heuristicOptions, PetriNet net, List<MarkingWithConstraints> targetMarkings)
        {
            List<string> availableHeuristics = new List<string>(new string[]
            {
                "NMarkingEQGurobi", "QMarkingEQGurobi"
            });
            string chosenHeuristic = heuristicOptions.chosenHeuristic.ToLower();

            Stopwatch heuristicInitWatch = Stopwatch.StartNew();

            HeuristicFrontier<Marking> frontier;

            switch (chosenHeuristic)
            {
                case ("nmarkingeqgurobi"):
#if GUROBI
                    frontier = new GurobiMarkingEquationOverNFrontier(net, targetMarkings);
                    return frontier;
#else
                    Console.WriteLine("Gurobi needs to be installed, and the compile flag set, to use this heuristic! See the README for more information.");
                    System.Environment.Exit(5);
                    break;
#endif
                case ("qmarkingeqgurobi"):
#if GUROBI
                    frontier = new GurobiMarkingEquationOverQFrontier(net, targetMarkings);
                    return frontier;
#else
                    Console.WriteLine("Gurobi needs to be installed, and the compile flag set, to use this heuristic! See the README for more information.");
                    System.Environment.Exit(5);
                    break;
#endif
                case ("list-heuristics"):
                    Console.WriteLine("Available heuristics for unity-frontier are:");
                    Console.WriteLine(String.Join(" ", availableHeuristics));
                    Console.WriteLine();
                    System.Environment.Exit(1);
                    break;
                default:
                    Console.WriteLine("Could not resolve heuristic function " + chosenHeuristic);
                    goto case "list-heuristics";
            }

            throw new Exception("Error: could not resolve the heuristic function (or lack thereof)");
        }


        public static Func<Marking, float?> ChooseForwardHeuristic(
            SearchBenchmarkEntry diagnostics,
            HeuristicOption heuristicOptions,
            PetriNet net,
            Marking initialMarking,
            List<MarkingWithConstraints> targetMarkings)
        {
            List<string> availableHeuristics = new List<string>(new string[]
            {
                "zero",
                "markingEQ",
                "qReachability",
                "NMarkingEQGurobi",
                "QMarkingEQGurobi",
                "syntactic",
                "markingEqSyntacticBaseline",
                "qReachabilitySyntacticBaseline",
                "structuralPerPlace-Q",
                "structuralPerPlace-N",
                "euclidean"
            });
            string chosenHeuristic = heuristicOptions.chosenHeuristic.ToLower();

            Func<Marking, float?> heuristicFunction = null;
            Stopwatch heuristicInitWatch = Stopwatch.StartNew();

            switch (chosenHeuristic)
            {
                case ("zero"):
                    heuristicFunction = x => 0;
                    break;
                case ("markingeq"):
                    heuristicFunction = Z3Heuristics.InitializeMarkingEquationHeuristic(net, targetMarkings);
                    break;
                case ("qreachability"):
                    heuristicFunction = Z3Heuristics.InitializeQReachabilityHeuristic(net, targetMarkings);
                    break;
                case ("nmarkingeqgurobi"):
#if GUROBI
                    heuristicFunction = GurobiHeuristics.InitializeMarkingEquationHeuristic(
                        net.Places,
                        net.Transitions,
                        targetMarkings,
                        GurobiConsts.Domains.N);
#else
                    Console.WriteLine("Gurobi needs to be installed, and the compile flag set, to use this heuristic! See the README for more information.");
                    System.Environment.Exit(5);
#endif
                    break;
                case ("qmarkingeqgurobi"):
#if GUROBI
                    heuristicFunction = GurobiHeuristics.InitializeMarkingEquationHeuristic(
                        net.Places, net.Transitions, targetMarkings, GurobiConsts.Domains.Q
                    );
#else
                    Console.WriteLine("Gurobi needs to be installed, and the compile flag set, to use this heuristic! See the README for more information.");
                    System.Environment.Exit(5);
#endif
                    break;
                case ("syntactic"):
                    heuristicFunction = StructuralHeuristics.InitializeSyntacticDistanceHeuristic(net, targetMarkings);
                    break;
                case ("markingeqsyntacticbaseline"):
                    heuristicFunction = Z3Heuristics.InitializeMarkingEquationHeuristicWithSyntacticDistanceBaseline(net, targetMarkings);
                    break;
                case ("qreachabilitysyntacticbaseline"):
                    heuristicFunction = Z3Heuristics.InitializeQReachabilityHeuristicWithSyntacticDistanceBaseline(net, targetMarkings);
                    break;
                case ("structuralperplace-q"):
#if GUROBI
                    heuristicFunction = StructuralHeuristics.InitializeStructuralQReachabilityHeuristicGurobi(net,
                                                                                                              initialMarking,
                                                                                                              targetMarkings,
                                                                                                              GurobiConsts.Domains.Q);
#else
                    Console.WriteLine("Gurobi needs to be installed, and the compile flag set, to use this heuristic! See the README for more information.");
                    System.Environment.Exit(5);
#endif
                    break;
                case ("structuralperplace-n"):
#if GUROBI
                    heuristicFunction = StructuralHeuristics.InitializeStructuralQReachabilityHeuristicGurobi(net,
                                                                                                              initialMarking,
                                                                                                              targetMarkings,
                                                                                                              GurobiConsts.Domains.N);
#else
                    Console.WriteLine("Gurobi needs to be installed, and the compile flag set, to use this heuristic! See the README for more information.");
                    System.Environment.Exit(5);
#endif
                    break;
                case ("euclidean"):
                    heuristicFunction = NumericalHeuristics.InitializeEuclideanDistance(net, targetMarkings);
                    break;
                case ("list-heuristics"):
                    Console.WriteLine("Available heuristics are:");
                    Console.WriteLine(String.Join(" ", availableHeuristics));
                    Console.WriteLine();
                    System.Environment.Exit(1);
                    break;
                default:
                    Console.WriteLine("Could not resolve heuristic function " + chosenHeuristic);
                    goto case "list-heuristics";
            }

            heuristicInitWatch.Stop();

            diagnostics.timeHeuristicInit = heuristicInitWatch.ElapsedMilliseconds;

            return heuristicFunction;
        }

        public static Func<Marking, float?> ChooseHeuristicBackwardsCover(
            ref SearchBenchmarkEntry diagnostics,
            HeuristicOption heuristicOptions,
            PetriNet net,
            Marking initialMarking,
            List<MarkingWithConstraints> targetMarkings)
        {
            List<string> availableHeuristics = new List<string>(new string[]
            {
                "QCoverabilityZ3", "MarkingEQZ3", "QMarkingEQGurobi", "NMarkingEQGurobi"
            });
            string chosenHeuristic = heuristicOptions.chosenHeuristic.ToLower();

            Func<Marking, float?> heuristicFunction = null;
            Stopwatch heuristicInitWatch = Stopwatch.StartNew();

            switch (chosenHeuristic)
            {
                case ("qcoverabilityz3"):
                    heuristicFunction = Z3Heuristics.InitializeQCoverabilityHeuristicBackwardsCover(
                        net,
                        initialMarking);
                    break;
                case ("markingeqz3"):
                    heuristicFunction = Z3Heuristics.InitializeMarkingEquationBackwardsCover(
                        net,
                        initialMarking);
                    break;
#if GUROBI
                case ("qmarkingeqgurobi"):
                    heuristicFunction = GurobiHeuristics.InitializeMarkingEquationHeuristicForBackwardsCoverability(
                        net.Places,
                        net.Transitions,
                        initialMarking);
                    break;
                case ("nmarkingeqgurobi"):
                    heuristicFunction = GurobiHeuristics.InitializeMarkingEquationHeuristicForBackwardsCoverability(
                        net.Places,
                        net.Transitions,
                        initialMarking,
                        domain: GurobiConsts.Domains.N);
                    break;
#else
                case ("qmarkingeqgurobi"):
                case ("nmarkingeqgurobi"):
                    Console.WriteLine("Gurobi needs to be installed, and the compile flag set, to use this heuristic! See the README for more information.");
                    System.Environment.Exit(5);
                    break;
#endif
                case ("list-heuristics"):
                    Console.WriteLine("Available heuristics are:");
                    Console.WriteLine(String.Join(" ", availableHeuristics));
                    Console.WriteLine();
                    System.Environment.Exit(1);
                    break;
                default:
                    Console.WriteLine("Could not resolve heuristic function " + chosenHeuristic);
                    goto case "list-heuristics";
            }
            heuristicInitWatch.Stop();

            diagnostics.timeHeuristicInit = heuristicInitWatch.ElapsedMilliseconds;

            return heuristicFunction;
        }
    }
}