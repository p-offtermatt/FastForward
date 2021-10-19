using Xunit;
using System;
using System.Collections.Generic;
using Petri;
using Xunit.Abstractions;

namespace Testing
{
    public class TestCycleHeuristic
    {
        private readonly ITestOutputHelper output;

        public TestCycleHeuristic(ITestOutputHelper output)
        {
            this.output = output;
        }

        [Fact]
        public void TestCycleHeuristic_BasicME()
        {
            LolaParser parser = new LolaParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("lola/basicME_simplified.lola"));

            Func<Marking, float?> cycleHeuristic = Z3Heuristics.InitializeHandleHeuristic(net, net);
            Assert.True(cycleHeuristic(initialMarking).HasValue, "Heuristic has no value!");
            Assert.True(
                cycleHeuristic(initialMarking).Value == 0,
                "Expected heuristic distance was " + 0 + ", but actual distance is " + cycleHeuristic(initialMarking).Value.ToString());
        }

        [Fact]
        public void TestCycleHeuristic_Leabasicapproach()
        {
            LolaParser parser = new LolaParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("lola/leabasicapproach.lola"));

            Func<Marking, float?> cycleHeuristic = Z3Heuristics.InitializeHandleHeuristic(net, net);
            Assert.True(cycleHeuristic(initialMarking).HasValue, "Heuristic has no value!");
            Assert.True(
                cycleHeuristic(initialMarking).Value == 0,
                "Expected heuristic distance was " + 0 + ", but actual distance is " + cycleHeuristic(initialMarking).Value.ToString());
        }

        [Fact]
        public void TestCycleHeuristic_Manufacture2()
        {
            LolaParser parser = new LolaParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("lola/manufacture2.lola"));

            Func<Marking, float?> cycleHeuristic = Z3Heuristics.InitializeHandleHeuristic(net, net);
            Assert.True(cycleHeuristic(initialMarking).HasValue);
            Assert.True(
                cycleHeuristic(initialMarking).Value == 0.5,
                "Expected heuristic distance was " + 0.5 + ", but actual distance is " + cycleHeuristic(initialMarking).Value.ToString());
        }

        [Fact]
        public void TestCycleHeuristic_Manufacturing()
        {
            LolaParser parser = new LolaParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("lola/manufacturing.lola"));

            Func<Marking, float?> cycleHeuristic = Z3Heuristics.InitializeHandleHeuristic(net, net);
            Assert.False(cycleHeuristic(initialMarking).HasValue);
        }

        [Fact]
        public void TestCycleHeuristic_swimming_pool()
        {
            LolaParser parser = new LolaParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("lola/swimming_pool.lola"));

            Func<Marking, float?> cycleHeuristic = Z3Heuristics.InitializeHandleHeuristic(net, net);
            Assert.True(cycleHeuristic(initialMarking).HasValue, "Heuristic has no value!");
            Assert.True(
                cycleHeuristic(initialMarking).Value == 0,
                "Expected heuristic distance was " + 0 + ", but actual distance is " + cycleHeuristic(initialMarking).Value.ToString());
        }

        [Fact]
        public void TestCycleHeuristic_pingpong()
        {
            LolaParser parser = new LolaParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("lola/pingpong.lola"));

            Func<Marking, float?> cycleHeuristic = Z3Heuristics.InitializeHandleHeuristic(net, net);
            Assert.True(cycleHeuristic(initialMarking).HasValue, "Heuristic has no value!");
            Assert.True(
                cycleHeuristic(initialMarking).Value == 2,
                "Expected heuristic distance was " + 2 + ", but actual distance is " + cycleHeuristic(initialMarking).Value.ToString());
        }
    }

    public class TestLTLModelChecking
    {
        [Fact]
        public void TestCycleFinding_pingpong()
        {
            LolaParser parser = new LolaParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("lola/pingpong.lola"));

            Func<Marking, float?> handleHeuristic = Z3Heuristics.InitializeHandleHeuristic(net, net);
            Func<IEnumerable<MarkingWithConstraints>, Func<Marking, float?>> loopHeuristicFactory = Z3Heuristics.InitializeCycleHeuristicFactory(net);
            (var handle, var cycle) = PetriNetUtils.FindLoopsWithAStar(net, net, initialMarking, handleHeuristic, loopHeuristicFactory);
        }

        [Fact]
        public void TestCycleFindingWithConditions_leader_election_n2()
        {
            string filepath = Utils.GetPathForTestfile("pnet/leader_election_cr79_n2.pnet");
            PnetParser parser = new PnetParser();
            (PetriNet handleNet, Marking initialMarking) = parser.ReadNet(filepath);

            (var atleastOneTimeSets, var zeroTimesSet) = parser.ReadCycleConditions(handleNet, filepath);

            (var cycleNet, var indicatorPlaces) = PetriNetUtils.CreateCycleNet(handleNet, atleastOneTimeSets, zeroTimesSet);

            Func<Marking, float?> handleHeuristic = Z3Heuristics.InitializeHandleHeuristic(handleNet, cycleNet, indicatorPlaces);
            Func<IEnumerable<MarkingWithConstraints>, Func<Marking, float?>> loopHeuristicFactory = Z3Heuristics.InitializeCycleHeuristicFactory(cycleNet, indicatorPlaces);
            (var handle, var cycle) = PetriNetUtils.FindLoopsWithAStar(handleNet, cycleNet, initialMarking, handleHeuristic, loopHeuristicFactory, indicatorPlaces);
            Assert.Null(cycle);
        }

        [Fact]
        public void TestCycleFindingWithConditions_lamport_n2()
        {
            string filepath = Utils.GetPathForTestfile("pnet/lamport_n2.pnet");

            PnetParser parser = new PnetParser();

            (PetriNet handleNet, Marking initialMarking) = parser.ReadNet(filepath);

            (var atleastOneTimeSets, var zeroTimesSet) = parser.ReadCycleConditions(handleNet, filepath);

            (var cycleNet, var indicatorPlaces) = PetriNetUtils.CreateCycleNet(handleNet, atleastOneTimeSets, zeroTimesSet);

            Func<Marking, float?> handleHeuristic = Z3Heuristics.InitializeHandleHeuristic(handleNet, cycleNet, indicatorPlaces);
            Func<IEnumerable<MarkingWithConstraints>, Func<Marking, float?>> loopHeuristicFactory = Z3Heuristics.InitializeCycleHeuristicFactory(cycleNet, indicatorPlaces);
            (var handle, var cycle) = PetriNetUtils.FindLoopsWithAStar(handleNet, cycleNet, initialMarking, handleHeuristic, loopHeuristicFactory, indicatorPlaces);
            Assert.Null(cycle);
        }

        [Fact]
        public void TestCycleFindingWithConditions_peterson_n2()
        {
            string filepath = Utils.GetPathForTestfile("pnet/peterson_n2.pnet");

            PnetParser parser = new PnetParser();
            (PetriNet handleNet, Marking initialMarking) = parser.ReadNet(filepath);

            (var atleastOneTimeSets, var zeroTimesSet) = parser.ReadCycleConditions(handleNet, filepath);

            (var cycleNet, var indicatorPlaces) = PetriNetUtils.CreateCycleNet(handleNet, atleastOneTimeSets, zeroTimesSet);

            Func<Marking, float?> handleHeuristic = Z3Heuristics.InitializeHandleHeuristic(handleNet, cycleNet, indicatorPlaces);
            Func<IEnumerable<MarkingWithConstraints>, Func<Marking, float?>> loopHeuristicFactory = Z3Heuristics.InitializeCycleHeuristicFactory(cycleNet, indicatorPlaces);
            (var handle, var cycle) = PetriNetUtils.FindLoopsWithAStar(handleNet, cycleNet, initialMarking, handleHeuristic, loopHeuristicFactory, indicatorPlaces);
            Assert.Null(cycle);
        }
    }
}
