using System;
using System.Collections.Generic;
using Priority_Queue;
using Benchmark;
using System.Linq;

namespace SearchAlgorithms
{

    public static class TransitionSaturationSearch
    {

        private static void AddNodeToReverseMap<T>(
            ref Dictionary<float?, List<T>> reverseMap,
            T node,
            float? value)
        {
            List<T> nodes = reverseMap.GetValueOrDefault(value, new List<T>());
            nodes.Add(node);
            reverseMap[value] = nodes;
        }

        private static int LimitToRange(
            this int value, int inclusiveMinimum, int inclusiveMaximum)
        {
            if (value < inclusiveMinimum) { return inclusiveMinimum; }
            if (value > inclusiveMaximum) { return inclusiveMaximum; }
            return value;
        }

        private static List<T> GetRestrictionNodes<T>(Dictionary<float?, List<T>> scoresToNodesMap, int reevaluationNodesNum)
        {
            IOrderedEnumerable<float?> orderedScores = scoresToNodesMap.Keys.OrderBy(v => v);
            List<T> restrictionNodes = new List<T>();

            int remainingNodesToAdd = reevaluationNodesNum;

            foreach (float? score in orderedScores)
            {
                List<T> nodesWithScore = scoresToNodesMap[score];
                int nodesWithScoreNum = nodesWithScore.Count;

                restrictionNodes.AddRange(nodesWithScoreNum <= remainingNodesToAdd ? nodesWithScore : nodesWithScore.GetRange(0, remainingNodesToAdd));
                remainingNodesToAdd -= nodesWithScoreNum;

                if (remainingNodesToAdd <= 0)
                {
                    return restrictionNodes;
                }
            }

            return restrictionNodes;
        }


        // If the target is not found with the restricted actions from the initial node,
        // this gives, as a percentage, the fraction of nodes that should
        // be evaluated for the new restriction function, and which will serve as a new frontier as well.
        private static decimal RestrictionNodeReevaluationRate = 0.1M;

        // if set to a non-zero value, gives an absolute cap on the number of nodes evaluated with the
        // restriction function in each step. If it is 0, then no cap is assumed.
        private static int RestrictionNodeReevaluationMax = 100;


        public static Tuple<List<T>, List<A>> FindShortestPath<T, A>(
           T initialNode,
           Func<T, bool> targetEvaluationFunction,
           IEnumerable<A> actions,
           Func<T, Tuple<IEnumerable<A>, float?>> actionRestrictFunction,
           Func<IEnumerable<A>, Func<T, float?>> heuristicFunctionGen,
           Func<T, A, Tuple<T, float>[]> successorFunction, // takes a node and an action, returns an array of tuples (successor, cost)
           SaturationSearchBenchmarkEntry diagnostics = null,
           bool verbose = false)
        {
            (IEnumerable<A> restrictedActions, float? distance) = actionRestrictFunction(initialNode);
            if (!distance.HasValue)
            {
                return new Tuple<List<T>, List<A>>(null, null);
            }

            // int expandDistance = (int)Math.Ceiling(distance.Value);

            Func<T, float?> heuristicFunction = heuristicFunctionGen(restrictedActions);

            var predecessors = new Dictionary<T, Tuple<A, T>>();

            SimplePriorityQueue<T> frontier = new SimplePriorityQueue<T>();
            frontier.Enqueue(initialNode, 0);

            // maps nodes to the current cost needed to reach them from the start node
            Dictionary<T, float> gScore = new Dictionary<T, float>();
            gScore[initialNode] = 0;

            // dictionary from estimated distances to nodes; used in order to enrich
            // most promising nodes with extra transitions later
            Dictionary<float?, List<T>> heuristicValuesToNodes = new Dictionary<float?, List<T>>();

            // dictionary from nodes to estimated distance; used in order to
            // not recompute heuristic values of nodes
            Dictionary<T, float?> nodesToHeuristicValues = new Dictionary<T, float?>();

            int numIterations = 0;

            while (true)
            {
                if (diagnostics != null)
                {
                    diagnostics.transitionSetSizePerIteration += restrictedActions.Count().ToString() + ", ";
                    diagnostics.numberOfIterations++;
                }
                if (verbose)
                {
                    numIterations++;
                    Console.WriteLine(String.Format("Numer of actions in iteration {0}: {1}", numIterations, restrictedActions.Count()));
                }
                int expandedNodesInIteration = 0;

                while (frontier.Count != 0)
                {
                    T currentNode = frontier.Dequeue();

                    expandedNodesInIteration += 1;
                    diagnostics.ExpandedNodes++;


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
                    foreach (A action in restrictedActions)
                    {
                        Tuple<T, float>[] neighbour_tuples = successorFunction(currentNode, action);
                        foreach (Tuple<T, float> neighbour_tuple in neighbour_tuples)
                        {
                            T neighbour = neighbour_tuple.Item1;
                            float dist = neighbour_tuple.Item2;

                            float? heuristicScoreOrNull;
                            if (!nodesToHeuristicValues.TryGetValue(neighbour, out heuristicScoreOrNull))
                            {
                                diagnostics.timesHeuristicCalculated++;
                                heuristicScoreOrNull = heuristicFunction(neighbour);
                                nodesToHeuristicValues.Add(neighbour, heuristicScoreOrNull);
                            }

                            if (!heuristicScoreOrNull.HasValue)
                            {
                                // skip neighbour if heuristic score is null
                                continue;
                            }

                            float heuristicScore = heuristicScoreOrNull.Value;

                            AddNodeToReverseMap(ref heuristicValuesToNodes, neighbour, heuristicScoreOrNull);


                            float totalCurrentDistance = gScore[currentNode] + dist;

                            if (totalCurrentDistance < gScore.GetValueOrDefault(neighbour, float.MaxValue))
                            {
                                predecessors[neighbour] = new Tuple<A, T>(action, currentNode);
                                gScore[neighbour] = totalCurrentDistance;
                                float newScore = totalCurrentDistance + heuristicScore;

                                if (frontier.Contains(neighbour))
                                {
                                    frontier.UpdatePriority(neighbour, newScore);
                                }
                                else
                                {
                                    frontier.Enqueue(neighbour, newScore);
                                }
                            }
                        }
                    }
                }
                // restricted actions were not sufficient to find the target; expand allowed actions

                int reexpandedNodesNum = ((int)Decimal.Round(expandedNodesInIteration * RestrictionNodeReevaluationRate)).LimitToRange(1, RestrictionNodeReevaluationMax);

                List<T> reexpandedNodes = GetRestrictionNodes(heuristicValuesToNodes, reexpandedNodesNum);

                heuristicValuesToNodes.Clear();
                nodesToHeuristicValues.Clear();


                foreach (T node in reexpandedNodes)
                {
                    (IEnumerable<A> restrictedActionsForNode, float? distanceForNode) = actionRestrictFunction(initialNode);
                    if (distanceForNode.HasValue)
                    {
                        // frontier.Enqueue(node, distanceForNode.Value + gScore[node]);
                        restrictedActions = restrictedActions.Union(restrictedActionsForNode);
                        // AddNodeToReverseMap(ref heuristicValuesToNodes, node, distanceForNode.Value);
                        // nodesToHeuristicValues.Add(node, distanceForNode.Value);
                    }
                }

                frontier.Enqueue(initialNode, distance.Value);
                heuristicFunction = heuristicFunctionGen(restrictedActions);

                // restart A* with expanded set of transitions (think iterative deepening A*)
            }
        }
    }
}
