#if GUROBI
using Xunit;
using System;
using System.Collections.Generic;
using Petri;
using Xunit.Abstractions;
using Utils;

namespace Testing
{
    public class StructuralHeuristicsTest
    {

        private readonly ITestOutputHelper output;

        public StructuralHeuristicsTest(ITestOutputHelper output)
        {
            this.output = output;
        }

        [Fact]
        public void Leabasicapproach_HeuristicOverN()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/leabasicapproach.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/leabasicapproach.formula"));

            Func<Marking, float?> structuralQReachabilityHeuristic = StructuralHeuristics.InitializeStructuralQReachabilityHeuristicGurobi(net, initialMarking, targetMarkings, GurobiConsts.Domains.N);

            foreach (Place place in net.Places)
            {
                Marking singlePlaceMarking = new Marking();
                singlePlaceMarking[place] = 1;

                float? structuralQReachDistance = structuralQReachabilityHeuristic(singlePlaceMarking);

                Assert.NotNull(structuralQReachDistance);
            }
        }

        [Fact]
        public void Leabasicapproach_HeuristicOverQ()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/leabasicapproach.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/leabasicapproach.formula"));

            Func<Marking, float?> structuralQReachabilityHeuristic = StructuralHeuristics.InitializeStructuralQReachabilityHeuristicGurobi(net, initialMarking, targetMarkings, GurobiConsts.Domains.Q);

            foreach (Place place in net.Places)
            {
                Marking singlePlaceMarking = new Marking();
                singlePlaceMarking[place] = 1;

                float? structuralQReachDistance = structuralQReachabilityHeuristic(singlePlaceMarking);

                Assert.NotNull(structuralQReachDistance);
            }
        }

        [Fact]
        public void manufacturing_HeuristicOverN()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/manufacturing.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/manufacturing.formula"));

            Func<Marking, float?> structuralQReachabilityHeuristic = StructuralHeuristics.InitializeStructuralQReachabilityHeuristicGurobi(net, initialMarking, targetMarkings, GurobiConsts.Domains.N);

            HashSet<float?> scores = new HashSet<float?>();

            foreach (Place place in net.Places)
            {
                Marking singlePlaceMarking = new Marking();
                singlePlaceMarking[place] = 1;

                float? structuralQReachDistance = structuralQReachabilityHeuristic(singlePlaceMarking);

                scores.Add(structuralQReachDistance);
            }

            Assert.Contains(null, scores);
            Assert.Contains(0, scores);
        }

        [Fact]
        public void manufacturing_HeuristicOverQ()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/manufacturing.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/manufacturing.formula"));

            Func<Marking, float?> structuralQReachabilityHeuristic = StructuralHeuristics.InitializeStructuralQReachabilityHeuristicGurobi(net, initialMarking, targetMarkings, GurobiConsts.Domains.Q);

            HashSet<float?> scores = new HashSet<float?>();

            foreach (Place place in net.Places)
            {
                Marking singlePlaceMarking = new Marking();
                singlePlaceMarking[place] = 1;

                float? structuralQReachDistance = structuralQReachabilityHeuristic(singlePlaceMarking);

                scores.Add(structuralQReachDistance);
            }

            Assert.Contains(null, scores);
            Assert.Contains(0, scores);
        }
    }
}
#endif