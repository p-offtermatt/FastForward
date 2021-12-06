using System;
using System.Collections.Generic;
using Priority_Queue;
using Benchmark;
using System.Diagnostics;
using System.Linq;

namespace SearchAlgorithms
{
    public static class AStarAlgorithm
    {
        /// <summary>
        /// 
        /// </summary>
        /// <param name="initialNode"></param>
        /// <param name="Func<T"></param>
        /// <param name="targetEvaluationFunction"></param>
        /// <param name="Func<T"></param>
        /// <param name="successorFunction"></param>
        /// <param name="Func<T"></param>
        /// <param name="heuristicFunction"></param>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        public static Tuple<List<T>, List<A>> FindShortestPath<T, A>(
            T initialNode,
            Func<T, bool> targetEvaluationFunction,
            IEnumerable<A> actions,
            Func<T, A, Tuple<T, float>[]> successorFunction, // takes a node and an action, returns an array of tuples (successor, cost)
            Func<T, float?> heuristicFunction,
            Func<T, bool> reachabilityTestFunction = null,
            SearchBenchmarkEntry diagnostics = null,
            bool verbose = false)
        {
            IEnumerable<T> initialNodes = new HashSet<T>().Append(initialNode);
            return FindShortestPathMultipleInitialNodes<T, A>(initialNodes, targetEvaluationFunction, actions, successorFunction, heuristicFunction, reachabilityTestFunction, diagnostics, verbose);
        }

        public static Tuple<List<T>, List<A>> FindShortestPathMultipleInitialNodes<T, A>(
            IEnumerable<T> initialNodes,
            Func<T, bool> targetEvaluationFunction,
            IEnumerable<A> actions,
            Func<T, A, Tuple<T, float>[]> successorFunction, // takes a node and an action, returns an array of tuples (successor, cost)
            Func<T, float?> heuristicFunction,
            Func<T, bool> reachabilityTestFunction = null,
            SearchBenchmarkEntry diagnostics = null,
            bool verbose = false)
        {
            // stores the nodes that have not been expanded yet
            var openSet = new SimplePriorityQueue<T>();

            // maps nodes to their successor and the action via which they were obtained
            var predecessors = new Dictionary<T, Tuple<A, T>>();

            // maps nodes to the current cost needed to reach them from the start node
            Dictionary<T, float> gScore = new Dictionary<T, float>();


            foreach (T initialNode in initialNodes)
            {
                if (reachabilityTestFunction != null)
                {
                    if (!reachabilityTestFunction(initialNode))
                    {
                        continue;
                    }
                }

#if !COMP
                Stopwatch watch = null;

                if (diagnostics != null)
                {
                    watch = System.Diagnostics.Stopwatch.StartNew();
                }
#endif
                // null indicates the neighbour cannot reach any target
                float? heuristicScoreOrNull = heuristicFunction(initialNode);

#if !COMP
                if (verbose) Console.WriteLine("Finished first heuristic check... Value is " + heuristicScoreOrNull);

                if (diagnostics != null)
                {
                    watch.Stop();
                    diagnostics.timeInHeuristicCalculation += watch.ElapsedMilliseconds;
                    diagnostics.timesHeuristicCalculated += 1;
                }
#endif

                if (!heuristicScoreOrNull.HasValue)
                {
                    continue;
                }
                openSet.Enqueue(initialNode, heuristicFunction(initialNode).Value);
                gScore[initialNode] = 0;
            }

            int nodeCount = 0;

            while (openSet.Count != 0)
            {
                T currentNode = openSet.Dequeue();

#if !COMP
                if (verbose)
                {
                    nodeCount += 1;
                    if (nodeCount % 1 == 0)
                    {
                        Console.WriteLine("Expanded nodes: " + nodeCount.ToString());
                        Console.WriteLine("Current Node: " + currentNode);
                    }
                }
                if (diagnostics != null)
                {
                    diagnostics.ExpandedNodes += 1;
                }
#endif

                if (targetEvaluationFunction(currentNode))
                {
                    Tuple<List<T>, List<A>> reconstructPath(T finalNode)
                    {
                        var actionList = new List<A>();
                        var nodeList = new List<T>();

                        T cur = finalNode;
                        nodeList.Add(cur);
                        while (predecessors.ContainsKey(cur))
                        {
                            Tuple<A, T> predecessorTuple = predecessors[cur];
                            actionList.Add(predecessorTuple.Item1);
                            cur = predecessorTuple.Item2;
                            nodeList.Add(cur);
                        }
                        nodeList.Reverse();
                        actionList.Reverse();
                        return new Tuple<List<T>, List<A>>(nodeList, actionList);
                    }

                    return reconstructPath(currentNode);
                }

                foreach (A action in actions)
                {
                    Tuple<T, float>[] neighbour_tuples = successorFunction(currentNode, action);
                    foreach (Tuple<T, float> neighbour_tuple in neighbour_tuples)
                    {
                        T neighbour = neighbour_tuple.Item1;
                        float dist = neighbour_tuple.Item2;
#if !COMP
                        Stopwatch watch = null;

                        if (diagnostics != null)
                        {
                            watch = System.Diagnostics.Stopwatch.StartNew();
                        }
#endif
                        // null indicates the neighbour cannot reach any target
                        float? heuristicScoreOrNull = heuristicFunction(neighbour);
#if !COMP
                        if (diagnostics != null)
                        {
                            watch.Stop();
                            diagnostics.timeInHeuristicCalculation += watch.ElapsedMilliseconds;
                            diagnostics.timesHeuristicCalculated += 1;
                        }
#endif

                        if (!heuristicScoreOrNull.HasValue)
                        {
                            // skip neighbour if heuristic score is null
                            continue;
                        }

                        float heuristicScore = heuristicScoreOrNull.Value;

#if !COMP
                        if (verbose) Console.WriteLine("Add neighbour with heuristic score " + heuristicScore);
#endif

                        float totalCurrentDistance = gScore[currentNode] + dist;

                        if (totalCurrentDistance < gScore.GetValueOrDefault(neighbour, float.MaxValue))
                        {
                            predecessors[neighbour] = new Tuple<A, T>(action, currentNode);
                            gScore[neighbour] = totalCurrentDistance;
                            float newScore = totalCurrentDistance + heuristicScore;

                            if (openSet.Contains(neighbour))
                            {
                                openSet.UpdatePriority(neighbour, newScore);
                            }
                            else
                            {
                                openSet.Enqueue(neighbour, newScore);
                            }
                        }
                    }
                }
            }

            return new Tuple<List<T>, List<A>>(null, null);
        }
    }
}