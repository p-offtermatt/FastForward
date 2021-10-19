using Xunit;
using SearchAlgorithms;
using System;
using System.Collections.Generic;
using Petri;
using System.Linq;
using Xunit.Abstractions;
using Priority_Queue;

namespace Testing
{
    public class AStarTest
    {

        private readonly ITestOutputHelper output;

        public AStarTest(ITestOutputHelper output)
        {
            this.output = output;
        }

        /// <summary>
        /// Tests A* on the toy graph https://i.stack.imgur.com/znIPt.jpg
        /// </summary>
        [Fact]
        public void testOnToyGraph()
        {
            string initialNode = "S";
            Func<string, bool> targetEvaluationFunction = x => x == "E";
            List<string> actions = new List<string>(new string[] { "left", "right" });

            Tuple<string, float>[] successorFunction(string node, string action)
            {
                Tuple<string, float> successor;
                switch (node)
                {
                    case "S":
                        successor = action == "left" ? new Tuple<string, float>("A", 1) : new Tuple<string, float>("B", 2);
                        break;
                    case "A":
                        successor = action == "left" ? new Tuple<string, float>("Y", 7) : new Tuple<string, float>("X", 4);
                        break;
                    case "B":
                        successor = action == "left" ? new Tuple<string, float>("C", 7) : new Tuple<string, float>("D", 1);
                        break;
                    case "Y":
                        successor = new Tuple<string, float>("E", 3);
                        break;
                    case "X":
                        successor = new Tuple<string, float>("E", 2);
                        break;
                    case "C":
                        successor = new Tuple<string, float>("E", 5);
                        break;
                    case "D":
                        successor = new Tuple<string, float>("E", 12);
                        break;
                    default:
                        output.WriteLine(String.Format("Something went wrong, node is {0}, action is {1}", node, action));
                        successor = null;
                        break;
                }
                return new Tuple<string, float>[] { successor };
            }

            Func<string, float?> heuristicFunction = node =>
            {
                switch (node)
                {
                    case "A": return 5;
                    case "B": return 6;
                    case "C": return 4;
                    case "D": return 15;
                    case "X": return 5;
                    case "Y": return 8;
                    default:
                        return 0;
                }
            };

            (_, List<string> path) = AStarAlgorithm.FindShortestPath(initialNode, targetEvaluationFunction, actions, successorFunction, heuristicFunction);

            // correct path is S->A->X->E, which corresponds to actions (left right (left|right))
            Assert.True(path[0] == "left");
            Assert.True(path[1] == "right");
            Assert.True(path[2] == "left" || path[2] == "right");
            Assert.Equal(3, path.Count);
        }




        [Fact]
        public void AStarLeabasicapproach()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/leabasicapproach.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/leabasicapproach.formula"));

            Func<Marking, float?> zeroHeuristic = x => 0;

            List<Transition> shortestPath = PetriNetUtils.PetriNetAStar(net, initialMarking, targetMarkings, zeroHeuristic);
            Assert.NotNull(shortestPath);

            // lola gives the path t6 t7 t0 t1, we test for this here (if this fails, it might mean we found a different path
            // that is also correct)
            List<string> stringPath = shortestPath.Select(x => x.Name).ToList();
            List<string> expectedStringPath = new List<string>(new string[] { "t6", "t7", "t0", "t1" });
            Assert.Equal(expectedStringPath, stringPath);
        }

        [Fact]
        public void AStarModulo()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/modulo.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/modulo.formula"));

            Func<Marking, float?> zeroHeuristic = x => 0;

            List<Transition> shortestPath = PetriNetUtils.PetriNetAStar(net, initialMarking, targetMarkings, zeroHeuristic);
            Assert.NotNull(shortestPath);

            List<string> stringPath = shortestPath.Select(x => x.Name).ToList();
            List<string> expectedStringPath = new List<string>(new string[] { "t1", "t1", "t1", "t1", "t1", "t1", "t1", "t1", "t1" });
            Assert.Equal(expectedStringPath, stringPath);
        }

        [Fact]
        public void AStarSwimmingPool()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/swimming_pool.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/swimming_pool.formula"));

            Func<Marking, float?> zeroHeuristic = x => 0;

            List<Transition> shortestPath = PetriNetUtils.PetriNetAStar(net, initialMarking, targetMarkings, zeroHeuristic);
            Assert.NotNull(shortestPath);

            // reference path is from lola
            List<string> stringPath = shortestPath.Select(x => x.Name).ToList();
            List<string> expectedStringPath = new List<string>(new string[] { "t0", "t1", "t2", "t0" });
            Assert.Equal(expectedStringPath, stringPath);
        }

        [Fact]
        public void AStarManufacture2()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/manufacture2.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/manufacture2.formula"));

            Func<Marking, float?> zeroHeuristic = x => 0;

            List<Transition> shortestPath = PetriNetUtils.PetriNetAStar(net, initialMarking, targetMarkings, zeroHeuristic);
            Assert.NotNull(shortestPath);

            // in this case, the expected path is slightly altered from the lola path. All of the changes are reorderings, lola just resolved
            // ties differently.
            // lolas path originally was t4 t2 t2 t0 t0 t3 t5 t1 t4 t2 t0 
            List<string> stringPath = shortestPath.Select(x => x.Name).ToList();
            List<string> expectedStringPath = new List<string>(new string[] { "t2", "t2", "t4", "t0", "t0", "t3", "t5", "t1", "t2", "t4", "t0" });
            Assert.Equal(expectedStringPath, stringPath);
        }
    }

    public class PriorityQueueTest
    {
        private readonly ITestOutputHelper output;

        public PriorityQueueTest(ITestOutputHelper output)
        {
            this.output = output;
        }

        [Fact]
        public void TestCandidateMinimization()
        {
            SimplePriorityQueue<Marking, float> queue = new SimplePriorityQueue<Marking, float>();
            Place p1 = new Place("m1");
            Place p2 = new Place("m2");
            Place p3 = new Place("m3");

            Marking m1 = new Marking(new Dictionary<Place, int>
            {
                [p1] = 4,
                [p2] = 2,
                [p3] = 4,
            });
            queue.Enqueue(m1, 4);

            Marking m2 = new Marking(new Dictionary<Place, int>
            {
                [p1] = 5,
                [p2] = 2,
                [p3] = 4,
            });
            queue.Enqueue(m2, 2);

            Marking m3 = new Marking(new Dictionary<Place, int>
            {
                [p1] = 1,
                [p2] = 1,
                [p3] = 1,
            });
            queue.Enqueue(m3, 3);

            Marking m4 = new Marking(new Dictionary<Place, int>
            {
                [p1] = 0,
                [p2] = 1,
                [p3] = 10,
            });
            queue.Enqueue(m4, 1);

            BackwardsCoverUtils.MinimizeCandidates(ref queue);

            Assert.Equal(2, queue.Count);
            Assert.Equal(m4, queue.Dequeue());
            Assert.Equal(m3, queue.Dequeue());
        }
    }
}