using Xunit;
using SearchAlgorithms;
using System;
using System.Collections.Generic;
using Petri;
using System.Linq;
using Xunit.Abstractions;

namespace Testing
{
    public class BackwardsCoverabilityTest
    {

        private readonly ITestOutputHelper output;

        public BackwardsCoverabilityTest(ITestOutputHelper output)
        {
            this.output = output;
        }

        [Fact]
        public void testBaseExpansion()
        {
            Marking m = new Marking(new Dictionary<Place, int>
            {
                [new Place("p1")] = 4,
                [new Place("p2")] = 1,
            });

            string transitionName = "t1";
            Dictionary<Place, int> transitionPre = new Dictionary<Place, int>
            {
                [new Place("p1")] = 3,
                [new Place("p2")] = 6,
            };

            Dictionary<Place, int> transitionPost = new Dictionary<Place, int>
            {
                [new Place("p1")] = 4,
                [new Place("p2")] = 3,
            };

            UpdateTransition transition = new UpdateTransition(transitionName, transitionPre, transitionPost);

            Marking resultMarking = BackwardsCoverUtils.ExpandBase(m, transition);

            Marking referenceMarking = new Marking(new Dictionary<Place, int>
            {
                [new Place("p1")] = 3,
                [new Place("p2")] = 6,
            });

            Assert.Equal(referenceMarking, resultMarking);
        }

        [Fact]
        public void BackwardsCoverBasicME()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/basicME.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/basicME.formula"));

            bool coverable = BackwardsCoverability.DetermineCoverability(net, initialMarking, targetMarkings);
            Assert.False(coverable);
        }

        [Fact]
        public void BackwardsCoverLeabasicapproach()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/leabasicapproach.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/leabasicapproach.formula"));

            bool coverable = BackwardsCoverability.DetermineCoverability(net, initialMarking, targetMarkings);
            Assert.True(coverable);
        }

        [Fact]
        public void BackwardsCoverSwimmingPool()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/swimming_pool.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/swimming_pool.formula"));

            bool coverable = BackwardsCoverability.DetermineCoverability(net, initialMarking, targetMarkings);
            Assert.True(coverable);
        }

        [Fact]
        public void BackwardsCoverManufacture2()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/manufacture2.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/manufacture2.formula"));

            Func<Marking, float?> zeroHeuristic = x => 0;

            bool coverable = BackwardsCoverability.DetermineCoverability(net, initialMarking, targetMarkings);
            Assert.True(coverable);
        }

        // uncoverable instances take too long in the basic approach, so this test is too time consuming

        // [Fact]
        // public void BackwardsCoverBingham_h50()
        // {
        //     Tuple<PetriNet, Marking> input = Parser.ReadNet(Utils.GetPathForTestfile("lola/bingham_h50.lola"));
        //     PetriNet net = input.Item1;
        //     Marking initialMarking = input.Item2;

        //     List<MarkingWithConstraints> targetMarkings = Parser.ReadFormula(Utils.GetPathForTestfile("lola/bingham_h50.formula"));

        //     Func<Marking, float?> zeroHeuristic = x => 0;

        //     bool coverable = BackwardsCoverability.DetermineCoverability(net, initialMarking, targetMarkings);
        //     Assert.False(coverable);
        // }

        [Fact]
        public void BackwardsCoverWithQueueBasicME()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/basicME.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/basicME.formula"));

            Func<Marking, float?> zeroHeuristic = x => 0;

            var path = BackwardsCoverability.DetermineCoverabilityWithQueue(net, initialMarking, targetMarkings, zeroHeuristic);
            Assert.Null(path);
        }

        [Fact]
        public void BackwardsCoverWithQueueManufacture2()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/manufacture2.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/manufacture2.formula"));

            Func<Marking, float?> zeroHeuristic = x => 0;

            var path = BackwardsCoverability.DetermineCoverabilityWithQueue(net, initialMarking, targetMarkings, zeroHeuristic);
            Assert.NotNull(path);
        }

        [Fact]
        public void BackwardsCoverWithQueueLeabasicapproach()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/leabasicapproach.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/leabasicapproach.formula"));

            Func<Marking, float?> zeroHeuristic = x => 0;

            var path = BackwardsCoverability.DetermineCoverabilityWithQueue(net, initialMarking, targetMarkings, zeroHeuristic);
            Assert.NotNull(path);

            List<string> referencePath = new List<string>(new string[] {
                "t0", "t1", "t6", "t7"
            });

            Assert.Equal(referencePath, path.Select(x => x.Name));
        }

        [Fact]
        public void BackwardsCoverWithQueueAndHeuristicLeabasicapproach()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/leabasicapproach.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/leabasicapproach.formula"));

            Func<Marking, float?> heuristic =
                Z3Heuristics.InitializeQCoverabilityHeuristicBackwardsCover(net, initialMarking);

            var path = BackwardsCoverability.DetermineCoverabilityWithQueue(net, initialMarking, targetMarkings, heuristic);
            Assert.NotNull(path);

            List<string> referencePath = new List<string>(new string[] {
                "t0", "t1", "t6", "t7"
            });

            Assert.Equal(referencePath, path.Select(x => x.Name));
        }

        [Fact]
        public void TestBackwardsCoverHeuristicOnExpandedMarkings()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/leabasicapproach.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/leabasicapproach.formula"));
            Marking target = targetMarkings.First().Marking;

            Marking m1 = new Marking(new Dictionary<Place, int>
            {
                [new Place("Cbefore")] = 1,
                [new Place("Sbad")] = 1,
                [new Place("unlockC")] = 1,
            });

            Marking m2 = new Marking(new Dictionary<Place, int>
            {
                [new Place("Cbefore")] = 1,
                [new Place("Sbad")] = 1,
                [new Place("unlockC")] = 1,
            });

            Func<Marking, float?> heuristic =
                Z3Heuristics.InitializeQCoverabilityHeuristicBackwardsCover(net, initialMarking);

            Assert.Equal(4, heuristic(target).Value);
            Assert.Equal(3, heuristic(m1).Value);
            Assert.Equal(3, heuristic(m2).Value);
        }


        [Fact]
        public void BackwardsCoverWithQueueAndHeuristicManufacture2()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/manufacture2.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/manufacture2.formula"));

            Func<Marking, float?> heuristic =
                Z3Heuristics.InitializeQCoverabilityHeuristicBackwardsCover(net, initialMarking);

            var path = BackwardsCoverability.DetermineCoverabilityWithQueue(net, initialMarking, targetMarkings, heuristic);
            Assert.NotNull(path);

            List<string> referencePath = new List<string>(new string[]{"t0", "t0", "t2", "t3", "t4", "t5", "t1", "t0",
            "t4", "t2", "t2"});


            output.WriteLine(String.Join("\n", path));

            Assert.Equal(referencePath, path.Select(x => x.Name));
        }

        [Fact]
        public void BackwardsCoverWithQueueSwimming_pool()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/swimming_pool.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/swimming_pool.formula"));

            Func<Marking, float?> zeroHeuristic = x => 0;

            var path = BackwardsCoverability.DetermineCoverabilityWithQueue(net, initialMarking, targetMarkings, zeroHeuristic);
            Assert.NotNull(path);

            // target is actually a reachability query, which is not satisfied by the empty firing sequence.
            // however, since the backward algorithm uses only coverability,
            // we implicitly treat every "=" constraint as a ">=" constraint, which means
            // that a target is satisfied at the initial marking
            Assert.Empty(path);
        }

        [Fact]
        public void BackwardsCoverWithQueueWithHeuristicMultipool()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/multipool.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/multipool.formula"));

            Func<Marking, float?> heuristic =
                Z3Heuristics.InitializeQCoverabilityHeuristicBackwardsCover(net, initialMarking);


            var path = BackwardsCoverability.DetermineCoverabilityWithQueue(net, initialMarking, targetMarkings, heuristic);
            Assert.Null(path);
        }

        // uncoverable instances take too long in the basic approach, so this test is too time consuming


        [Fact]
        public void BackwardsCoverWithQueueBingham_h50()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/bingham_h50.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/bingham_h50.formula"));

            Func<Marking, float?> heuristic =
                Z3Heuristics.InitializeQCoverabilityHeuristicBackwardsCover(net, initialMarking);

            var path = BackwardsCoverability.DetermineCoverabilityWithQueue(net, initialMarking, targetMarkings, heuristic);
            Assert.Null(path);
        }
    }
}