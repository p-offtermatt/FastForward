using Xunit;
using System;
using System.Collections.Generic;
using Petri;
using System.Linq;
using Xunit.Abstractions;
using System.Diagnostics;
using System.Collections;
using Utils;
#if GUROBI
using static Petri.GurobiHeuristics;

namespace Testing
{
    public class TestGurobi
    {
        private readonly ITestOutputHelper output;

        public TestGurobi(ITestOutputHelper output)
        {
            this.output = output;
        }

        [Fact]
        public void testSimpleGurobiExample()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/leabasicapproach.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/leabasicapproach.formula"));

            Func<Marking, float?> heuristicFunction = GurobiHeuristics.InitializeMarkingEquationHeuristic(net.Places, net.Transitions, targetMarkings);

            float expectedScore = 4;

            Assert.Equal(expectedScore, heuristicFunction(initialMarking));
        }

        [Fact]
        public void testMultiTargetGurobiExample()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/swimming_pool.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/swimming_pool.formula"));

            Func<Marking, float?> heuristicFunction = GurobiHeuristics.InitializeMarkingEquationHeuristic(net.Places, net.Transitions, targetMarkings);

            float expectedScore = 4;

            Assert.Equal(expectedScore, heuristicFunction(initialMarking));
        }

        [Fact]
        public void testBackwardsGurobi()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/leabasicapproach.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/leabasicapproach.formula"));

            Marking targetMarking = targetMarkings.First().Marking;

            Func<Marking, float?> heuristicFunction = GurobiHeuristics.InitializeMarkingEquationHeuristicForBackwardsCoverability(net.Places, net.Transitions, initialMarking);

            float expectedScore = 4;

            Assert.Equal(expectedScore, heuristicFunction(targetMarking));
        }

        [Fact]
        public void testContinuousGurobiExample()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/swimming_pool.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/swimming_pool.formula"));

            Func<Marking, float?> heuristicFunction = GurobiHeuristics.InitializeMarkingEquationHeuristic(
                net.Places, net.Transitions, targetMarkings, GurobiConsts.Domains.Q);

            float expectedScore = 4;
            Assert.Equal(expectedScore, heuristicFunction(initialMarking));
        }

        [Fact]
        public void testWarmStartGurobiExample()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/pncsacover.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/pncsacover.formula"));

            Func<Marking, float?> heuristicFunction = GurobiHeuristics.InitializeMarkingEquationHeuristic(
                net.Places, net.Transitions, targetMarkings, GurobiConsts.Domains.Q);

            Stopwatch watch = Stopwatch.StartNew();
            heuristicFunction(initialMarking);
            watch.Stop();

            Marking nextInitialMarking = new Marking(new Dictionary<Place, int>
            {
                [new Place("x13")] = 1,
                [new Place("x3")] = 1,
                [new Place("x21")] = 1,
            });
            Stopwatch secondAttemptWatch = Stopwatch.StartNew();
            heuristicFunction(nextInitialMarking);
            secondAttemptWatch.Stop();
        }


        public class TestPlaceBounds
        {
            [Fact]
            public void Test_BasicME()
            {
                LolaParser parser = new LolaParser();
                Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/basicME.lola"));
                PetriNet net = input.Item1;
                Marking initialMarking = input.Item2;

                double bound = 0;

                bound = GetPlaceBoundsViaMarkingEquation(net.Places, net.Transitions, initialMarking, 0, GurobiConsts.Domains.Q).First().Value;
                Assert.Equal(1.0, bound);

                for (int i = 2; i <= 30; i++)
                {
                    bound = GetPlaceBoundsViaMarkingEquation(net.Places, net.Transitions, initialMarking, i - 1, GurobiConsts.Domains.Q).First().Value;
                    Assert.Equal((double)i, bound);
                }
            }
        }
    }

    public class TestGurobiWithTransfers
    {
        private readonly ITestOutputHelper output;

        public TestGurobiWithTransfers(ITestOutputHelper output)
        {
            this.output = output;
        }

        class TransferMarkingEquationTestData : IEnumerable<object[]>
        {
            private Place p1;
            private Place p2;
            private Place p3;
            private Place p4;
            private Place p5;

            private MultitransferTransition multi1;
            private MultitransferTransition multi2;

            private SetTransferTransition transfer1;


            private UpdateTransition update1;
            private UpdateTransition update2;
            private UpdateTransition update3;
            private UpdateTransition update4;



            private Marking initialMarking1;
            private Marking initialMarking2;


            private PetriNet net1;
            private PetriNet net2;
            private PetriNet net3;



            public TransferMarkingEquationTestData()
            {
                this.p1 = new Place("p1");
                this.p2 = new Place("p2");
                this.p3 = new Place("p3");
                this.p4 = new Place("p4");
                this.p5 = new Place("p5");

                multi1 = new MultitransferTransition(
                    "multi1",
                    new Dictionary<Place, HashSet<Place>> { [p1] = new HashSet<Place> { p4 } },
                    new Dictionary<Place, int> { [p2] = 1 },
                    new Dictionary<Place, int> { [p3] = 1 }
                );

                update1 = new UpdateTransition(
                    "update1",
                    new Dictionary<Place, int> { [p1] = 1 },
                    new Dictionary<Place, int> { [p1] = 2 }
                );

                update2 = new UpdateTransition(
                    "update1",
                    new Dictionary<Place, int> { [p1] = 1 },
                    new Dictionary<Place, int> { [p2] = 1 }
                );

                initialMarking1 = new Marking { [p1] = 1 };

                net1 = new PetriNet(new HashSet<Place> { p1, p2, p3, p4, p5 }, new HashSet<Transition> { multi1, update1, update2 });

                update3 = new UpdateTransition(
                    "update3",
                    new Dictionary<Place, int> { [p3] = 2 },
                    new Dictionary<Place, int> { [p4] = 1 }
                );

                transfer1 = new SetTransferTransition("transfer1", new HashSet<Place> { p1 }, new HashSet<Place> { p3 });

                net2 = new PetriNet(new HashSet<Place> { p1, p2, p3, p4 }, new HashSet<Transition> { update3, transfer1, update2 });

                initialMarking2 = new Marking { [p1] = 3 };

                update4 = new UpdateTransition(
                    "update4",
                    new Dictionary<Place, int> { [p1] = 3 },
                    new Dictionary<Place, int> { [p2] = 3 }
                );

                net3 = new PetriNet(new HashSet<Place> { p1, p2, p3, p4 }, new HashSet<Transition> { multi1, update4 });
            }

            public IEnumerator<object[]> GetEnumerator()
            {

                List<object[]> configs = new List<object[]>{
                    new object[]{net1, initialMarking1, 1, 1, 1, 1, true},
                    new object[]{net1, initialMarking1, 1, 1, 0, 5, false},
                    new object[]{net1, initialMarking1, 4, 3, 0, 0, true},
                    new object[]{net1, initialMarking1, 4, 3, 2, 0, true},
                    new object[]{net1, initialMarking1, 4, 3, 2, 4, true},
                    new object[]{net1, initialMarking1, 0, 3, 2, 0, true},
                    new object[]{net1, initialMarking1, 0, 0, 0, 2, false},
                    new object[]{net1, initialMarking1, 0, 0, 2, 2, true},
                    new object[]{net2, initialMarking2, 1, 1, 1, 0, true},
                    new object[]{net2, initialMarking2, 2, 1, 1, 0, false},
                    new object[]{net2, initialMarking2, 0, 1, 1, 1, false},
                    new object[]{net2, initialMarking2, 0, 1, 0, 1, true},
                    new object[]{net2, initialMarking2, 0, 0, 2, 1, false},
                    new object[]{net2, initialMarking2, 0, 0, 1, 1, true},
                    new object[]{net2, initialMarking2, 0, 1, 2, 1, false},
                    new object[]{net2, initialMarking2, 0, 0, 2, 0, false},
                    new object[]{net2, initialMarking2, 2, 0, 0, 0, false},
                    new object[]{net2, initialMarking2, 3, 0, 0, 0, true},
                    new object[]{net3, initialMarking2, 3, 0, 0, 0, true},
                    new object[]{net3, initialMarking2, 0, 0, 2, 1, true},
                    new object[]{net3, initialMarking2, 0, 0, 0, 3, false},
                    new object[]{net3, initialMarking2, 1, 1, 1, 0, true},
                    new object[]{net3, initialMarking2, 1, 1, 0, 1, false},
                    new object[]{net3, initialMarking2, 1, 0, 1, 1, true},
                };

                foreach (object[] config in configs)
                {
                    Marking marking = new Marking();
                    marking[new Place("p1")] = (int)config[2];
                    marking[new Place("p2")] = (int)config[3];
                    marking[new Place("p3")] = (int)config[4];
                    marking[new Place("p4")] = (int)config[5];

                    Constraints constraints = new Constraints(marking.Keys.ToDictionary(place => place, place => ConstraintOperators.Equal));

                    bool reachable = (bool)config[6];

                    MarkingWithConstraints targetMarking = new MarkingWithConstraints(marking, constraints);

                    Marking initialMarking = (Marking)config[1];
                    PetriNet net = (PetriNet)config[0];

                    yield return new object[] { net, initialMarking, targetMarking, reachable };
                }
            }

            IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
        }
        [Theory]
        [ClassData(typeof(TransferMarkingEquationTestData))]
        public void SimpleTransferMarkingEquationTest(PetriNet net, Marking initialMarking, MarkingWithConstraints targetMarking, bool reachableInApproximation)
        {
            var heuristic = InitializeMarkingEquationHeuristic(net.Places, net.Transitions, new List<MarkingWithConstraints> { targetMarking });
            float? score = heuristic(initialMarking);

            Assert.Equal(reachableInApproximation, score.HasValue);
        }

        [Theory]
        [InlineData("delegatebuffer", true)]
        [InlineData("FreeBSD__rdma_addr.c_vs", false)]
        public void TransferHeuristicTest(string filepath, bool reachableInApproximation)
        {
            TTSParser parser = new BinaryTTSParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("tts/" + filepath + ".tts"));
            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("tts/" + filepath + ".prop"));

            var heuristic = InitializeMarkingEquationHeuristic(net.Places, net.Transitions, targetMarkings);
            float? score = heuristic(initialMarking);

            Assert.Equal(reachableInApproximation, score.HasValue);
        }
    }
}
#endif
