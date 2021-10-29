using System;
using System.Collections.Generic;
using Petri;
using System.Linq;
using System.Diagnostics;
using Benchmark;
using CommandLine;
using SearchAlgorithms;
using System.IO;
using MathNet.Numerics.Statistics;
using CommandLine.Text;
using HeuristicFrontier;
using Utils;
#if GUROBI
using static Petri.GurobiHeuristics;
#endif

namespace PetriTool
{
    class SearchQueryEntryPoints
    {
        public static void SingleQueryWithHeuristic<T>(T options) where T : SingleQueryOptions, HeuristicOption
        {
            {
                Type queryType = options.GetType();
                Stopwatch queryWatch = Stopwatch.StartNew();

                SearchBenchmarkEntry benchmarkEntry = new SearchBenchmarkEntry();

                FullParser parser = ParserPicker.ChooseFullParser(options.netFilePath, options.formulaFilePath);

                Stopwatch parseWatch = Stopwatch.StartNew();
                (PetriNet net, Marking initialMarking) = parser.ReadNet(options.netFilePath);
                parseWatch.Stop();
                benchmarkEntry.timeForNetParsing = parseWatch.ElapsedMilliseconds;
                parseWatch.Restart();
                List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(options.formulaFilePath);
                parseWatch.Stop();
                benchmarkEntry.timeForFormulaParsing = parseWatch.ElapsedMilliseconds;

                // Console.WriteLine(String.Join("\n", net.Places));
                // Console.WriteLine(String.Join("\n--\n", net.Transitions));
                // Console.WriteLine("#####################################");
                benchmarkEntry.numberOfPlaces = net.Places.Count;
                benchmarkEntry.numberOfTransitions = net.Transitions.Count;

                if (options.verbose)
                {
                    Console.WriteLine("Before pruning: ");
                    Console.WriteLine("Net: " + net.ToString());
                    Console.WriteLine("Initial Marking: " + initialMarking);
                    Console.WriteLine("Target Markings: " + String.Join(" ", targetMarkings));
                }

                if (options.prune)
                {
                    Pruning.Prune(benchmarkEntry, ref net, ref initialMarking, ref targetMarkings, true, false);

                    if (net.Places.Count == 0)
                    {
                        if (targetMarkings.Any(targetMarking => targetMarking.SatisfiedByMarking(new Marking())))
                        {
                            string outp = benchmarkEntry.ToJSON();

                            Console.WriteLine(outp);
                            System.Environment.Exit(0);
                        }
                    }
                }

                // Console.WriteLine(String.Join("\n", net.Places));
                // Console.WriteLine(String.Join("\n--\n", net.Transitions));

                if (options.precompute_max)
                {
                    Dictionary<Place, int> maxPerPlace = net.Places.ToDictionary(place => place, place => 0);
                    foreach (Transition transition in net.Transitions)
                    {
                        if (transition.GetPostPlaces().Count > 1)
                        {
                            Console.WriteLine("Error - Transition " + transition.Name + " has more than one output arc. The option for precomputing the maximum tokens for each place is only available for pseudo-communication-free nets!");
                            System.Environment.Exit(76);
                        }

                        foreach ((Place place, int value) in transition.GetGuard())
                        {
                            maxPerPlace[place] = Math.Max(value + 1, maxPerPlace[place]);
                        }
                    }

                    net = new PetriNetWithCapacities(net, maxPerPlace);
                }

                List<Transition> path = null;
                if (typeof(UnityFrontierOptions).IsAssignableFrom(options.GetType()))
                {
                    HeuristicFrontier<Marking> frontier = HeuristicPicker.ChooseHeuristicFrontier(
                                                        options,
                                                        net,
                                                        targetMarkings
                                                );

                    Stopwatch watch = Stopwatch.StartNew();
                    switch (options)
                    {
                        case AStarUnityFrontierOptions queryOptions:
                            path = PetriNetUtils.UnityFrontierSearch(net, initialMarking, targetMarkings, frontier, PetriNetUtils.DistanceSuccessorFunction,
                            diagnostics: benchmarkEntry);
                            break;
                        case BestFirstUnityFrontierOptions queryOptions:
                            path = PetriNetUtils.UnityFrontierSearch(net, initialMarking, targetMarkings, frontier, PetriNetUtils.BestFirstSuccessorFunction,
                            diagnostics: benchmarkEntry);
                            break;
                        default:
                            throw new NotImplementedException("Handling for options " + options + " is not implemented.");
                    }
                    watch.Stop();
                    benchmarkEntry.timeInAlgorithm = watch.ElapsedMilliseconds;

                }
                else
                {
                    Func<Marking, float?> heuristicFunction = null;

                    if (options is AStarQueryOptions || options is BestFirstQueryOptions)
                    {
                        heuristicFunction = HeuristicPicker.ChooseForwardHeuristic(benchmarkEntry, options, net, initialMarking, targetMarkings);
                    }


                    {
                        Stopwatch watch = Stopwatch.StartNew();
                        switch (options)
                        {
                            case AStarQueryOptions queryOptions:
                                path = PetriNetUtils.PetriNetAStar(net, initialMarking, targetMarkings, heuristicFunction,
                                diagnostics: benchmarkEntry);
                                break;
                            case BestFirstQueryOptions queryOptions:
                                path = PetriNetUtils.PetriNetBestFirstSearch(net, initialMarking, targetMarkings, heuristicFunction,
                                    diagnostics: benchmarkEntry);
                                break;
                            default:
                                throw new NotImplementedException("Handling for options " + options + " is not implemented.");
                        }
                        watch.Stop();
                        benchmarkEntry.timeInAlgorithm = watch.ElapsedMilliseconds;
                    }
                }

                benchmarkEntry.path = path != null
                            ? String.Join(", ", path.Select(t => t.Name))
                            : "unreachable";

                queryWatch.Stop();
                benchmarkEntry.timeInQuery = queryWatch.ElapsedMilliseconds;

                string output = benchmarkEntry.ToJSON();

                Console.WriteLine(output);
            }
        }

        public static void SaturationSearch(SaturationSearchOptions options)
        {
            SaturationSearchBenchmarkEntry benchmarkEntry = new SaturationSearchBenchmarkEntry();
            Stopwatch queryWatch = Stopwatch.StartNew();

            FullParser parser = ParserPicker.ChooseFullParser(options.netFilePath, options.formulaFilePath);
            (PetriNet net, Marking initialMarking) = parser.ReadNet(options.netFilePath);
            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(options.formulaFilePath);

            benchmarkEntry.numberOfPlaces = net.Places.Count;
            benchmarkEntry.numberOfTransitions = net.Transitions.Count;


            Func<Marking, Tuple<IEnumerable<Transition>, float?>> restrictionHeuristic =
                InitializeMarkingEquationTransitionSupportComputation(net.Places, net.Transitions, targetMarkings, GurobiConsts.Domains.N);

            Func<IEnumerable<Transition>, Func<Marking, float?>> directionalHeuristicGen =
                (restrictedTransitions) => InitializeMarkingEquationHeuristic(net.Places, restrictedTransitions.ToList(), targetMarkings, GurobiConsts.Domains.Q);

            Func<Marking, bool> targetEvaluationFunction =
                marking => targetMarkings.Any(x => x.SatisfiedByMarking(marking));

            (var markings, var transitions) = TransitionSaturationSearch.FindShortestPath(
                initialMarking,
                targetEvaluationFunction,
                net.Transitions,
                restrictionHeuristic,
                directionalHeuristicGen,
                PetriNetUtils.DistanceSuccessorFunction,
                verbose: options.verbose,
                diagnostics: benchmarkEntry
            );

            benchmarkEntry.path = transitions != null ? String.Join(", ", transitions.Select(t => t.Name)) : "unreachable";

            queryWatch.Stop();
            benchmarkEntry.timeInQuery = queryWatch.ElapsedMilliseconds;
            Console.WriteLine(benchmarkEntry.ToJSON());

        }
    }
}