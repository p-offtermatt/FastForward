using Xunit;
using SearchAlgorithms;
using System;
using System.Collections.Generic;
using Petri;
using System.Linq;
using Xunit.Abstractions;

namespace Testing
{
    public class AStarQReachabilityTest
    {

        private readonly ITestOutputHelper output;

        public AStarQReachabilityTest(ITestOutputHelper output)
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
        public void AStarBasicME()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/basicME.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/basicME.formula"));

            Func<Marking, float?> qReachabilityHeuristic = Z3Heuristics.InitializeQReachabilityHeuristic(net, targetMarkings);

            List<Transition> shortestPath = PetriNetUtils.PetriNetAStar(net, initialMarking, targetMarkings, qReachabilityHeuristic);
            Assert.Null(shortestPath);
        }

        [Fact]
        public void AStarLeabasicapproach()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/leabasicapproach.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/leabasicapproach.formula"));

            Func<Marking, float?> qReachabilityHeuristic = Z3Heuristics.InitializeQReachabilityHeuristic(net, targetMarkings);

            List<Transition> shortestPath = PetriNetUtils.PetriNetAStar(net, initialMarking, targetMarkings, qReachabilityHeuristic);
            Assert.NotNull(shortestPath);

            // lola gives the path t6 t7 t0 t1, we test for this here (if this fails, it might mean we found a different path
            // that is also correct)
            List<string> stringPath = shortestPath.Select(x => x.Name).ToList();
            List<string> expectedStringPath = new List<string>(new string[] { "t6", "t7", "t0", "t1" });
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

            Func<Marking, float?> qReachabilityHeuristic = Z3Heuristics.InitializeQReachabilityHeuristic(net, targetMarkings);

            List<Transition> shortestPath = PetriNetUtils.PetriNetAStar(net, initialMarking, targetMarkings, qReachabilityHeuristic);
            Assert.NotNull(shortestPath);

            // reference path is from lola
            List<string> stringPath = shortestPath.Select(x => x.Name).ToList();
            List<string> expectedStringPath = new List<string>(new string[] { "t0", "t1", "t2", "t0" });
            Assert.Equal(expectedStringPath, stringPath);
        }

        [Fact]
        public void AStarBingham_h50()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/bingham_h50.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/bingham_h50.formula"));

            Func<Marking, float?> qReachabilityHeuristic = Z3Heuristics.InitializeQReachabilityHeuristic(net, targetMarkings);

            List<Transition> shortestPath = PetriNetUtils.PetriNetAStar(net, initialMarking, targetMarkings, qReachabilityHeuristic);
            Assert.Null(shortestPath);
        }

        [Fact]
        public void AStarManufacture2()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/manufacture2.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/manufacture2.formula"));

            Func<Marking, float?> qReachabilityHeuristic = Z3Heuristics.InitializeQReachabilityHeuristic(net, targetMarkings);

            List<Transition> shortestPath = PetriNetUtils.PetriNetAStar(net, initialMarking, targetMarkings, qReachabilityHeuristic);
            Assert.NotNull(shortestPath);

            // in this case, the expected path is slightly altered from the lola path. All of the changes are reorderings, lola just resolved
            // ties differently.
            // lolas path originally was t4 t2 t2 t0 t0 t3 t5 t1 t4 t2 t0 
            List<string> stringPath = shortestPath.Select(x => x.Name).ToList();
            List<string> expectedStringPath = new List<string>(new string[] { "t2", "t2", "t4", "t0", "t0", "t3", "t5", "t1", "t2", "t4", "t0" });
            Assert.Equal(expectedStringPath, stringPath);
        }

        [Fact]
        public void AStarVerySimpleTest()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/very_simple_test.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/very_simple_test.formula"));

            Func<Marking, float?> qReachabilityHeuristic = Z3Heuristics.InitializeQReachabilityHeuristic(net, targetMarkings);

            List<Transition> shortestPath = PetriNetUtils.PetriNetAStar(net, initialMarking, targetMarkings, qReachabilityHeuristic);
            Assert.NotNull(shortestPath);

            List<string> stringPath = shortestPath.Select(x => x.Name).ToList();
            List<string> expectedStringPath = new List<string>(new string[] { "t0" });
            Assert.Equal(expectedStringPath, stringPath);
        }

        [Fact]
        public void AStarManufacturing()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/manufacturing.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/manufacturing.formula"));

            Func<Marking, float?> qReachabilityHeuristic = Z3Heuristics.InitializeQReachabilityHeuristic(net, targetMarkings);

            List<Transition> shortestPath = PetriNetUtils.PetriNetAStar(net, initialMarking, targetMarkings, qReachabilityHeuristic);

            // initial marking is empty but all transitions have nonempty presets, so there should be no path
            Assert.Null(shortestPath);
        }



        // marking equation heuristic does not handle unreachable cases well, it is way more likely to end up in an infinite loop
        // than the qreach heuristic

        // [Fact]
        // public void AStarBasicMEMarkingEQ()
        // {
            LolaParser parser = new LolaParser();
        //     Tuple<PetriNet, Marking> input = Parser.ReadNet(Utils.GetPathForTestfile("lola/basicME.lola"));
        //     PetriNet net = input.Item1;
        //     Marking initialMarking = input.Item2;

        //     List<MarkingWithConstraints> targetMarkings = Parser.ReadFormula(Utils.GetPathForTestfile("lola/basicME.formula"));

        //     Func<Marking, float?> markingEqHeuristic = Z3Heuristics.InitializMarkingEquationHeuristic(net, targetMarkings);

        //     List<AbstractTransition> shortestPath = PetriNetUtils.PetriNetAStar(net, initialMarking, targetMarkings, markingEqHeuristic);

        //     // initial marking is empty but all transitions have nonempty presets, so there should be no path
        //     Assert.Null(shortestPath);
        // }
    };
}