using System;
using System.Collections.Generic;
using Petri;
using System.Diagnostics;
using Benchmark;
using Utils;

namespace PetriTool
{
    class HeuristicPicker
    {
        public static Func<Marking, float?> ChooseForwardHeuristic(
            BenchmarkEntryWithHeuristics diagnostics,
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
                #if GUROBI
                "NMarkingEQGurobi",
                "QMarkingEQGurobi",
                #endif
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