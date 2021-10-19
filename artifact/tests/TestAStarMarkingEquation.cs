using Xunit;
using System;
using System.Collections.Generic;
using Petri;
using System.Linq;
using Xunit.Abstractions;

namespace Testing
{
    public class AStarMarkingEquationTest
    {

        private readonly ITestOutputHelper output;

        public AStarMarkingEquationTest(ITestOutputHelper output)
        {
            this.output = output;
        }

        // [Fact]
        // public void AStarBasicME()
        // {
        //     Tuple<PetriNet, Marking> input = Parser.ReadNet("../../../../nets/basicME.lola");
        //     PetriNet net = input.Item1;
        //     Marking initialMarking = input.Item2;

        //     List<MarkingWithConstraints> targetMarkings = Parser.ReadFormula("../../../../nets/basicME.formula");

        //     Func<Marking, float?> markingEquationHeuristic = Z3Heuristics.InitializeMarkingEquationHeuristic(net, targetMarkings);

        //     List<UpdateTransition> shortestPath = PetriNetUtils.PetriNetAStar(net, initialMarking, targetMarkings, markingEquationHeuristic);
        //     Assert.Null(shortestPath);
        // }

        [Fact]
        public void AStarLeabasicapproach()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/leabasicapproach.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/leabasicapproach.formula"));

            Func<Marking, float?> markingEquationHeuristic = Z3Heuristics.InitializeMarkingEquationHeuristic(net, targetMarkings);

            List<Transition> shortestPath = PetriNetUtils.PetriNetAStar(net, initialMarking, targetMarkings, markingEquationHeuristic);
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

            Func<Marking, float?> markingEquationHeuristic = Z3Heuristics.InitializeMarkingEquationHeuristic(net, targetMarkings);

            List<Transition> shortestPath = PetriNetUtils.PetriNetAStar(net, initialMarking, targetMarkings, markingEquationHeuristic);
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

            Func<Marking, float?> markingEquationHeuristic = Z3Heuristics.InitializeMarkingEquationHeuristic(net, targetMarkings);

            List<Transition> shortestPath = PetriNetUtils.PetriNetAStar(net, initialMarking, targetMarkings, markingEquationHeuristic);
            Assert.NotNull(shortestPath);

            // in this case, the expected path is slightly altered from the lola path. All of the changes are reorderings, lola just resolved
            // ties differently.
            // lola's path originally was t4 t2 t2 t0 t0 t3 t5 t1 t4 t2 t0 
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

            Func<Marking, float?> markingEquationHeuristic = Z3Heuristics.InitializeMarkingEquationHeuristic(net, targetMarkings);

            List<Transition> shortestPath = PetriNetUtils.PetriNetAStar(net, initialMarking, targetMarkings, markingEquationHeuristic);
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

            Func<Marking, float?> markingEquationHeuristic = Z3Heuristics.InitializeMarkingEquationHeuristic(net, targetMarkings);

            List<Transition> shortestPath = PetriNetUtils.PetriNetAStar(net, initialMarking, targetMarkings, markingEquationHeuristic);

            // initial marking is empty but all transitions have nonempty presets, so there should be no path
            Assert.Null(shortestPath);
        }

        [Fact]
        public void AStarPncSASemiLiv()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/pncsasemiliv.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/pncsasemiliv.formula"));

            Func<Marking, float?> markingEquationHeuristic = Z3Heuristics.InitializeMarkingEquationHeuristic(net, targetMarkings);

            List<Transition> shortestPath = PetriNetUtils.PetriNetAStar(net, initialMarking, targetMarkings, markingEquationHeuristic);
            Assert.NotNull(shortestPath);
        }

        [Fact]
        public void TestSypetXML23()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/xml23.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/xml23.formula"));

            Func<Marking, float?> heuristic = StructuralHeuristics.InitializeSyntacticDistanceHeuristic(net, targetMarkings);
            List<Transition> shortestPath = PetriNetUtils.PetriNetAStar(net, initialMarking, targetMarkings, heuristic);
            Assert.NotNull(shortestPath);
        }
    }
}