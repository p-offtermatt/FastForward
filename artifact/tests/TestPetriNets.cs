using Xunit;
using System;
using System.Collections.Generic;
using Petri;
using System.Linq;
using Xunit.Abstractions;
using PetriTool;
using System.Text.RegularExpressions;

namespace Testing
{


    public class PetriNetsTests
    {

        private readonly ITestOutputHelper output;

        public PetriNetsTests(ITestOutputHelper output)
        {
            this.output = output;
        }

        [Fact]
        public void TestMarkingSubtraction()
        {
            Place p1 = new Place("p1");
            Place p2 = new Place("p2");
            Place p3 = new Place("p3");
            Place p4 = new Place("p4");

            Marking m1 = new Marking()
                {
                    {p1, 1},
                    {p2, 4},
                    {p3, 5},
                    {p4, 0}
                }
            ;

            Marking m2 = new Marking()
                {
                    {p1, 1},
                    {p2, -1},
                    {p3, 0},
                    {p4, 2}
                }
            ;

            Marking expected = new Marking()
                {
                    {p1, 0},
                    {p2, 5},
                    {p3, 5},
                    {p4, -2}
                }
            ;

            Assert.Equal(expected, m1 - m2);
        }

        [Theory]
        [InlineData("remove-uncoverable-transitions/1.lola", "r1 r2 r3")]
        [InlineData("remove-uncoverable-transitions/2.lola", null)]
        [InlineData("remove-uncoverable-transitions/3.lola", null)]
        [InlineData("remove-uncoverable-transitions/4.lola", "r1")]
        public void TestRemoveUncoverableTransitions(string net_filepath, string space_separated_expected_removed_transitionnames)
        {
            NetParser parser = new LolaParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile(net_filepath));
            PetriNet modifiedNet = UtilityEntrypoints.RemoveUncoverableTransitions(net, initialMarking);

            string[] actualRemovedTransitionNames =
                net.Transitions.Where(transition => !modifiedNet.Transitions.Contains(transition)).Select(transition => transition.Name).ToArray();

            if (space_separated_expected_removed_transitionnames == null)
            {
                Assert.Equal(0, actualRemovedTransitionNames.Count());
                return;
            }

            string[] expectedRemovedTransitionNames = space_separated_expected_removed_transitionnames.Split(" ");
            Array.Sort(actualRemovedTransitionNames);
            Array.Sort(expectedRemovedTransitionNames);

            Assert.Equal(expectedRemovedTransitionNames, actualRemovedTransitionNames);
        }

        [Theory]
        [InlineData(1, 2, 3, 4, 5, 6, 7, 8, true, false)]
        [InlineData(1, 2, 3, 4, 5, 6, 7, 8, false, false)]
        [InlineData(2, 3, 4, 5, 1, 2, 3, 4, true, true)]
        [InlineData(2, 2, 3, 4, 1, 2, 3, 4, true, true)]
        [InlineData(1, 2, 3, 4, 1, 2, 3, 4, true, false)]
        [InlineData(1, 2, 3, 4, 1, 2, 3, 4, false, true)]
        [InlineData(0, 2, 3, 4, 1, 2, 3, 4, true, false)]
        [InlineData(0, 2, 3, 4, 1, 2, 3, 4, false, false)]
        public void TestM1GreaterM2(int m1p1, int m1p2, int m1p3, int m1p4, int m2p1, int m2p2, int m2p3, int m2p4, bool strict, bool expected)
        {
            Place p1 = new Place("p1");
            Place p2 = new Place("p2");
            Place p3 = new Place("p3");
            Place p4 = new Place("p4");

            Marking m1 = new Marking();
            m1[p1] = m1p1;
            m1[p2] = m1p2;
            m1[p3] = m1p3;
            m1[p4] = m1p4;

            Marking m2 = new Marking();
            m2[p1] = m2p1;
            m2[p2] = m2p2;
            m2[p3] = m2p3;
            m2[p4] = m2p4;

            if (strict)
            {
                Assert.Equal(expected, m1 > m2);
                Assert.Equal(!expected, m1 <= m2);
            }
            else
            {
                Assert.Equal(expected, m1 >= m2);
                Assert.Equal(!expected, m1 < m2);
            }
        }

        [Fact]
        public void TestMarkedPlaces()
        {
            List<Place> expectedPlaces = new List<Place>(new Place[] {new Place("x0"),
                                                                      new Place("x1"),
                                                                      new Place("x2"),
                                                                      new Place("x3"),
                                                                      new Place("x4")});

            Marking marking = new Marking
            {
                [new Place("y0")] = 0,
                [new Place("x0")] = 2,
                [new Place("x1")] = 1,
                [new Place("x2")] = 5,
                [new Place("x3")] = 1,
                [new Place("x4")] = 17,
                [new Place("y1")] = 0,
            };

            Assert.Equal(expectedPlaces.OrderBy(p => p.ToString()), marking.GetMarkedPlaces().ToList().OrderBy(p => p.ToString()));
        }

        [Fact]
        public void TestBackwardsPruning()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/backwards_pruning_test.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targets = parser.ReadFormula(Utils.GetPathForTestfile("lola/backwards_pruning_test.formula"));

            IEnumerable<Place> nonzeroPlaces = Pruning.ComputeBackwardsNonZeroPlacesForTarget(net, targets[0]);

            HashSet<Place> expectedNonzeroPlaces = new HashSet<Place>{
                new Place("x0"),
                new Place("x2"),
            };

            Assert.True(expectedNonzeroPlaces.SetEquals(nonzeroPlaces));
        }

        [Fact]
        public void TestBackwardsPruningMath5()
        {
            PetriNet net = new PetriNet(new List<Place>{
                new Place("x0"),
                new Place("x1"),
                new Place("x2"),
                new Place("x3"),
                new Place("x4"),
            },
            new List<UpdateTransition>());

            MarkingWithConstraints notReachabilityQuery = new MarkingWithConstraints(new Marking
            {
                [new Place("y0")] = 0,
                [new Place("x0")] = 2,
                [new Place("x1")] = 1,
                [new Place("x2")] = 5,
                [new Place("x3")] = 1,
                [new Place("x4")] = 17,
                [new Place("y1")] = 0,
            }, new Constraints
            {
                [new Place("y0")] = ConstraintOperators.Equal,
                [new Place("x0")] = ConstraintOperators.GreaterEqual,
                [new Place("x1")] = ConstraintOperators.GreaterEqual,
                [new Place("x2")] = ConstraintOperators.Equal,
                [new Place("x3")] = ConstraintOperators.Equal,
                [new Place("x4")] = ConstraintOperators.Equal,
                [new Place("y1")] = ConstraintOperators.GreaterEqual,
            });
            Assert.False(MarkingWithConstraints.IsReachabilityQueryForNet(net, notReachabilityQuery));

            MarkingWithConstraints reachabilityQuery = new MarkingWithConstraints(new Marking
            {
                [new Place("x0")] = 0,
                [new Place("x1")] = 1,
                [new Place("x2")] = 5,
                [new Place("x3")] = 1,
                [new Place("x4")] = 17,
            }, new Constraints
            {
                [new Place("x0")] = ConstraintOperators.Equal,
                [new Place("x1")] = ConstraintOperators.Equal,
                [new Place("x2")] = ConstraintOperators.Equal,
                [new Place("x3")] = ConstraintOperators.Equal,
                [new Place("x4")] = ConstraintOperators.Equal,
            });
            Assert.True(MarkingWithConstraints.IsReachabilityQueryForNet(net, reachabilityQuery));

            MarkingWithConstraints notReachabilityQuery2 = new MarkingWithConstraints(new Marking
            {
                [new Place("x0")] = 0,
                [new Place("x1")] = 1,
                [new Place("x2")] = 5,
                [new Place("x3")] = 1,
            }, new Constraints
            {
                [new Place("x0")] = ConstraintOperators.Equal,
                [new Place("x1")] = ConstraintOperators.Equal,
                [new Place("x2")] = ConstraintOperators.Equal,
                [new Place("x3")] = ConstraintOperators.Equal,
            });
            Assert.False(MarkingWithConstraints.IsReachabilityQueryForNet(net, notReachabilityQuery2));

            //IEnumerable<Place> nonzeroPlaces = Pruning.ComputeBackwardsNonZeroPlacesForTarget(net, targets[0]);
        }

        [Fact]
        public void TestIsReachabilityQuery()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/backwards_pruning_test.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targets = parser.ReadFormula(Utils.GetPathForTestfile("lola/backwards_pruning_test.formula"));
            MarkingWithConstraints targetMarking = targets[0];

            IEnumerable<Place> nonzeroFinalPlaces = new HashSet<Place> {
                new Place("x2")
            };

            Assert.Equal(net.Places.Except(targetMarking.GetZeroPlaces()).OrderBy(t => t.ToString()),
                nonzeroFinalPlaces.OrderBy(t => t.ToString()));

            //IEnumerable<Place> nonzeroPlaces = Pruning.ComputeBackwardsNonZeroPlacesForTarget(net, targets[0]);
        }

        [Fact]
        public void TestMarkingWithConstraintsZeroPlaces()
        {
            List<Place> expectedZeroPlaces = new List<Place>(new Place[] { new Place("y0") });

            Marking marking = new Marking
            {
                [new Place("y0")] = 0,
                [new Place("x0")] = 2,
                [new Place("x1")] = 1,
                [new Place("x2")] = 5,
                [new Place("x3")] = 1,
                [new Place("x4")] = 17,
                [new Place("y1")] = 0,
            };

            Constraints constraints = new Constraints
            {
                [new Place("y0")] = ConstraintOperators.Equal,
                [new Place("x0")] = ConstraintOperators.GreaterEqual,
                [new Place("x1")] = ConstraintOperators.GreaterEqual,
                [new Place("x2")] = ConstraintOperators.Equal,
                [new Place("x3")] = ConstraintOperators.Equal,
                [new Place("x4")] = ConstraintOperators.Equal,
                [new Place("y1")] = ConstraintOperators.GreaterEqual,
            };

            MarkingWithConstraints markingWithConstraints = new MarkingWithConstraints(marking, constraints);

            Assert.Equal(expectedZeroPlaces.OrderBy(p => p.ToString()), markingWithConstraints.GetZeroPlaces().ToList().OrderBy(p => p.ToString()));
        }

        [Fact]
        public void TestPreAndPostPlaces()
        {
            List<Place> expectedPlaces = new List<Place>(new Place[] {new Place("x0"),
                                                                      new Place("x1"),
                                                                      new Place("x2"),
                                                                      new Place("x3"),
                                                                      new Place("x4")});
            UpdateTransition t = new UpdateTransition("t1",
            new Dictionary<Place, int>
            {
                [new Place("y0")] = 0,
                [new Place("x0")] = 2,
                [new Place("x1")] = 1,
                [new Place("x2")] = 5,
                [new Place("x3")] = 1,
                [new Place("x4")] = 17,
                [new Place("y1")] = 0,
            },
            new Dictionary<Place, int>
            {
                [new Place("y0")] = 0,
                [new Place("x0")] = 2,
                [new Place("x1")] = 1,
                [new Place("x2")] = 5,
                [new Place("x3")] = 1,
                [new Place("x4")] = 17,
                [new Place("y1")] = 0,
            });

            Assert.Equal(expectedPlaces.OrderBy(p => p.ToString()), t.GetPrePlaces().ToList().OrderBy(p => p.ToString()));
            Assert.Equal(expectedPlaces.OrderBy(p => p.ToString()), t.GetPostPlaces().ToList().OrderBy(p => p.ToString()));

        }

        /// <summary>
        /// Test parsing the basicME petri net, 
        /// </summary>
        [Fact]
        public void testBasicMENet()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/basicME.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<Place> expectedPlaces = new List<Place>(new Place[] {new Place("x0"),
                                                                      new Place("x1"),
                                                                      new Place("x2"),
                                                                      new Place("x3"),
                                                                      new Place("x4")});

            // order before comparing because we don't care about order, just equal elements            
            Assert.Equal(expectedPlaces.OrderBy(t => t.ToString()), net.Places.OrderBy(t => t.ToString()));

            Marking expectedMarking = new Marking(new Dictionary<Place, int>
            {
                [new Place("x0")] = 1,
                [new Place("x1")] = 1,
                [new Place("x2")] = 1,
            });

            Assert.Equal(expectedMarking.OrderBy(t => t.ToString()), initialMarking.OrderBy(t => t.ToString()));

            List<UpdateTransition> expectedTransitions = new List<UpdateTransition>(new UpdateTransition[] {
                new UpdateTransition("t1",
                new Dictionary<Place, int>{
                    [new Place("x0")] = 1,
                    [new Place("x1")] = 1,
                    [new Place("x2")] = 1,
                },
                new Dictionary<Place, int>{
                    [new Place("x2")] = 1,
                    [new Place("x4")] = 1,
                }),
                new UpdateTransition("t2",
                new Dictionary<Place, int>{
                    [new Place("x3")] = 1,
                },
                new Dictionary<Place, int>{
                    [new Place("x0")] = 1,
                    [new Place("x2")] = 1,
                }),
                new UpdateTransition("t0",
                new Dictionary<Place, int>{
                    [new Place("x0")] = 1,
                    [new Place("x1")] = 1,
                    [new Place("x2")] = 1,
                },
                new Dictionary<Place, int>{
                    [new Place("x1")] = 1,
                    [new Place("x3")] = 1,
                }),
                new UpdateTransition("t3",
                new Dictionary<Place, int>{
                    [new Place("x4")] = 1,
                },
                new Dictionary<Place, int>{
                    [new Place("x0")] = 1,
                    [new Place("x1")] = 1,
                }),
                new UpdateTransition("t_x0",
                new Dictionary<Place, int>(),
                new Dictionary<Place, int>{
                    [new Place("x0")] = 1,
                }),
            });

            Assert.Equal(expectedTransitions.OrderBy(t => t.ToString()), net.Transitions.OrderBy(t => t.ToString()));
        }

        /// <summary>
        /// Test parsing a model in .pnml format
        /// </summary>
        [Fact]
        public void TestPNMLParsing()
        {
            PNMLParser parser = new PNMLParser();

            Tuple<PetriNet, Marking> res = parser.ReadNet(Utils.GetPathForTestfile("mcc/CloudOpsManagement-PT-00002by00001.pnml"));
            PetriNet net = res.Item1;
            Marking initialMarking = res.Item2;

            // reference information is from https://mcc.lip6.fr/2019/pdf/CloudOpsManagement-form.pdf
            Assert.Equal(27, net.Places.Count);
            Assert.Equal(29, net.Transitions.Count);

            // spot test transition for correctness of pre/post
            UpdateTransition tlink0 = ((UpdateTransition)net.Transitions.Where(t => t.Name == "tlink0").First());
            Marking pre = new Marking(new Dictionary<Place, int>
            {
                [new Place("Container")] = 1,
            });
            Marking post = new Marking(new Dictionary<Place, int>
            {
                [new Place("OsContainer")] = 1,
            });

            Assert.Equal(pre, tlink0.Pre);
            Assert.Equal(post, tlink0.Post);
        }

        /// <summary>
        /// Test parsing a model in .pnml format
        /// </summary>
        [Fact]
        public void TestSimplePNML()
        {
            PNMLParser parser = new PNMLParser();

            Tuple<PetriNet, Marking> res = parser.ReadNet(Utils.GetPathForTestfile("pnml/b3.pnml"));
            PetriNet net = res.Item1;
            Marking initialMarking = res.Item2;

            // reference information is from https://mcc.lip6.fr/2019/pdf/CloudOpsManagement-form.pdf
            Assert.Equal(12, net.Places.Count);
            Assert.Equal(8, net.Transitions.Count);
        }


        [Fact]
        public void testHashCode()
        {
            Marking m1 = new Marking(new Dictionary<Place, int>
            {
                [new Place("x1")] = 1
            });

            IDictionary<Marking, float> dict = new Dictionary<Marking, float>
            {
                [m1] = 0
            };

            Assert.True(dict.ContainsKey(m1));

            foreach (Marking m in dict.Keys)
            {
                Assert.True(dict.ContainsKey(m));
            }

            // ensure successor function does not modify m1
            PetriNetUtils.DistanceSuccessorFunction(m1, new UpdateTransition("t1",
                    new Dictionary<Place, int>
                    {
                        [new Place("x1")] = 0,
                    },
                    new Dictionary<Place, int>
                    {
                        [new Place("x1")] = 1,
                    }));
            Assert.True(dict.ContainsKey(m1));
        }

        [Fact]
        public void testPruningBasicME()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/basicME.lola"));
            HashSet<Place> nonZeroPlaces = Pruning.ComputeNonZeroPlaces(input.Item1, input.Item2);

            Assert.Equal(input.Item1.Places.ToHashSet(), nonZeroPlaces);
        }

        [Fact]
        public void testPruningManufacturing()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/manufacturing.lola"));
            HashSet<Place> nonZeroPlaces = Pruning.ComputeNonZeroPlaces(input.Item1, input.Item2);

            Assert.Empty(nonZeroPlaces);
        }

        [Fact]
        public void testPruningDekkerVsSatabs2()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/dekker_vs_satabs.2.lola"));
            HashSet<Place> nonZeroPlaces = Pruning.ComputeNonZeroPlaces(input.Item1, input.Item2);

            Assert.True(input.Item1.Places.Count > nonZeroPlaces.Count);
        }

        [Fact]
        public void testCompletePruningManufacturing()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/manufacturing.lola"));
            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/manufacturing.lola"));
            Tuple<PetriNet, Marking, List<MarkingWithConstraints>> restrictedSystem =
                        Pruning.PruneWithZeroPlaceHeuristic(input.Item1, input.Item2, targetMarkings);

            PetriNet restrictedNet = restrictedSystem.Item1;
            Marking restrictedInitialMarking = restrictedSystem.Item2;
            List<MarkingWithConstraints> restrictedTargets = restrictedSystem.Item3;

            Assert.Empty(restrictedNet.Places);
            Assert.Empty(restrictedNet.Transitions);

            Assert.Empty(restrictedInitialMarking);
            Assert.Empty(restrictedTargets);
        }

        [Fact]
        public void TestCompletePruningBasicME()
        {
            LolaParser parser = new LolaParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("lola/basicME.lola"));
            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/basicME.formula"));
            HashSet<Place> nonZeroPlaces = Pruning.ComputeNonZeroPlaces(net, initialMarking);
            Assert.Equal(net.Places.ToHashSet(), nonZeroPlaces);
            Tuple<PetriNet, Marking, List<MarkingWithConstraints>> restrictedSystem =
                        Pruning.PruneWithZeroPlaceHeuristic(net, initialMarking, targetMarkings);

            PetriNet restrictedNet = restrictedSystem.Item1;
            Marking restrictedInitialMarking = restrictedSystem.Item2;
            List<MarkingWithConstraints> restrictedTargets = restrictedSystem.Item3;

            // use .ToHashSet to ignore order of elements
            Assert.Equal(net.Places.ToHashSet(), restrictedNet.Places.ToHashSet());
            Assert.Equal(net.Transitions.ToHashSet(), restrictedNet.Transitions.ToHashSet());

            Assert.Equal(initialMarking, restrictedInitialMarking);
            Assert.Equal(targetMarkings.First(), restrictedTargets.First());
        }

        [Fact]
        public void TestCompletePruningBoop()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input =
                parser.ReadNet(Utils.GetPathForTestfile("lola/Boop_simple_vf_satabs.1.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings =
                parser.ReadFormula(Utils.GetPathForTestfile("lola/Boop_simple_vf_satabs.1.formula"));

            Tuple<PetriNet, Marking, List<MarkingWithConstraints>> restrictedSystem =
                    Pruning.PruneWithZeroPlaceHeuristic(net, initialMarking, targetMarkings);

            PetriNet restrictedNet = restrictedSystem.Item1;
            Marking restrictedInitialMarking = restrictedSystem.Item2;
            List<MarkingWithConstraints> restrictedTargets = restrictedSystem.Item3;

            Func<Marking, float?> qReachabilityHeuristic = Z3Heuristics.InitializeQReachabilityHeuristic(net, targetMarkings);

            List<Transition> shortestPathUnrestricted = PetriNetUtils.PetriNetAStar(
                net, initialMarking,
                targetMarkings, qReachabilityHeuristic
            );

            Func<Marking, float?> restrictedQReachabilityHeuristic =
                Z3Heuristics.InitializeQReachabilityHeuristic(net, targetMarkings);

            List<Transition> shortestPathRestricted = PetriNetUtils.PetriNetAStar(
                restrictedNet, restrictedInitialMarking,
                restrictedTargets, restrictedQReachabilityHeuristic
            );


            Assert.Equal(shortestPathUnrestricted, shortestPathRestricted);
        }

        [Fact]
        public void TestBackwardPruningInitialRemoved()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input =
                parser.ReadNet(Utils.GetPathForTestfile("lola/pruning_removeAll.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings =
                parser.ReadFormula(Utils.GetPathForTestfile("lola/pruning_removeAll.formula"));

            Tuple<PetriNet, Marking, List<MarkingWithConstraints>> restrictedSystem =
                    Pruning.PruneWithBackwardsZeroPlaceHeuristic(net, initialMarking, targetMarkings);

            PetriNet restrictedNet = restrictedSystem.Item1;
            Marking restrictedInitialMarking = restrictedSystem.Item2;
            List<MarkingWithConstraints> restrictedTargets = restrictedSystem.Item3;

            Assert.Empty(restrictedNet.Places);
        }

        [Fact]
        public void TestBackwardsPruningBoop()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input =
                parser.ReadNet(Utils.GetPathForTestfile("lola/Boop_simple_vf_satabs.1.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings =
                parser.ReadFormula(Utils.GetPathForTestfile("lola/Boop_simple_vf_satabs.1.formula"));

            Tuple<PetriNet, Marking, List<MarkingWithConstraints>> restrictedSystem =
                    Pruning.PruneWithBackwardsZeroPlaceHeuristic(net, initialMarking, targetMarkings);

            PetriNet restrictedNet = restrictedSystem.Item1;
            Marking restrictedInitialMarking = restrictedSystem.Item2;
            List<MarkingWithConstraints> restrictedTargets = restrictedSystem.Item3;

            Func<Marking, float?> qReachabilityHeuristic = Z3Heuristics.InitializeQReachabilityHeuristic(net, targetMarkings);

            List<Transition> shortestPathUnrestricted = PetriNetUtils.PetriNetAStar(
                net, initialMarking,
                targetMarkings, qReachabilityHeuristic
            );

            Func<Marking, float?> restrictedQReachabilityHeuristic =
                Z3Heuristics.InitializeQReachabilityHeuristic(net, targetMarkings);

            List<Transition> shortestPathRestricted = PetriNetUtils.PetriNetAStar(
                restrictedNet, restrictedInitialMarking,
                restrictedTargets, restrictedQReachabilityHeuristic
            );

            Assert.Equal(shortestPathUnrestricted, shortestPathRestricted);
        }

        [Fact]
        public void TestCompletePruningSimpleBoop()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input =
                parser.ReadNet(Utils.GetPathForTestfile("lola/boop_pruning.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings =
                parser.ReadFormula(Utils.GetPathForTestfile("lola/boop_pruning.formula"));

            Tuple<PetriNet, Marking, List<MarkingWithConstraints>> restrictedSystem =
                    Pruning.PruneWithZeroPlaceHeuristic(net, initialMarking, targetMarkings);

            PetriNet restrictedNet = restrictedSystem.Item1;
            Marking restrictedInitialMarking = restrictedSystem.Item2;
            List<MarkingWithConstraints> restrictedTargets = restrictedSystem.Item3;

            Func<Marking, float?> qReachabilityHeuristic = Z3Heuristics.InitializeQReachabilityHeuristic(net, targetMarkings);

            List<Transition> shortestPathUnrestricted = PetriNetUtils.PetriNetAStar(
                net, initialMarking,
                targetMarkings, qReachabilityHeuristic
            );

            Func<Marking, float?> restrictedQReachabilityHeuristic =
                Z3Heuristics.InitializeQReachabilityHeuristic(net, targetMarkings);

            List<Transition> shortestPathRestricted = PetriNetUtils.PetriNetAStar(
                restrictedNet, restrictedInitialMarking,
                restrictedTargets, restrictedQReachabilityHeuristic
            );


            Assert.Equal(shortestPathUnrestricted, shortestPathRestricted);
        }

        [Fact]
        public void testPruningRemoveTargetMarking()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input =
                parser.ReadNet(Utils.GetPathForTestfile("lola/pruned_target.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings =
                parser.ReadFormula(Utils.GetPathForTestfile("lola/pruned_target.formula"));

            Tuple<PetriNet, Marking, List<MarkingWithConstraints>> restrictedSystem =
                    Pruning.PruneWithZeroPlaceHeuristic(net, initialMarking, targetMarkings);

            PetriNet restrictedNet = restrictedSystem.Item1;
            Marking restrictedInitialMarking = restrictedSystem.Item2;
            List<MarkingWithConstraints> restrictedTargets = restrictedSystem.Item3;

            Assert.Empty(restrictedTargets);
        }

        [Fact]
        public void TestMarkingCover()
        {
            Marking m1 = new Marking(new Dictionary<Place, int>
            {
                [new Place("p1")] = 3,
                [new Place("p2")] = 2,
                [new Place("p3")] = 1,
                [new Place("p4")] = 4,
                [new Place("p5")] = 2,
            });

            Marking m2 = new Marking(new Dictionary<Place, int>
            {
                [new Place("p1")] = 3,
                [new Place("p2")] = 2,
                [new Place("p3")] = 1,
                [new Place("p4")] = 4,
                [new Place("p5")] = 2,
            });

            Marking m3 = new Marking(new Dictionary<Place, int>
            {
                [new Place("p1")] = 4,
                [new Place("p2")] = 3,
                [new Place("p3")] = 1,
                [new Place("p4")] = 4,
                [new Place("p5")] = 2,
            });

            Marking m4 = new Marking(new Dictionary<Place, int>
            {
                [new Place("p1")] = 4,
                [new Place("p2")] = 3,
            });

            Assert.False(m1.CoveredBy(m4));
            Assert.True(m1.CoveredBy(m2));
            Assert.True(m2.CoveredBy(m1));
            Assert.True(m3.Covers(m1));
            Assert.False(m3.CoveredBy(m1));
            Assert.False(m1.CoveredBy(m4));
            Assert.False(m4.CoveredBy(m1));
            Assert.True(m4.CoveredBy(m3));

            Assert.False(m1 <= m4);
            Assert.True(m1 <= m2);
            Assert.True(m2 <= m1);
            Assert.True(m3 >= m1);
            Assert.False(m3 <= m1);
            Assert.False(m1 <= m4);
            Assert.False(m4 <= m1);
            Assert.True(m4 <= m3);
        }

        [Fact]
        public void TestMarkingEquality()
        {
            Place p1 = new Place("p1");
            Place p2 = new Place("p2");
            Place p3 = new Place("p3");
            Place p4 = new Place("p4");
            Place p5 = new Place("p5");

            Marking m1 = new Marking();
            m1[p1] = 1;
            m1[p2] = 1;
            m1[p3] = 1;

            Marking m2 = new Marking();
            m2[p1] = 1;
            m2[p2] = 1;
            m2[p3] = 1;
            m2[p4] = 0;
            m2[p5] = 0;

            Assert.Equal(m1.GetHashCode(), m2.GetHashCode());

            Assert.True(m1.Equals(m2));
            Assert.True(m2.Equals(m1));
        }
    }

    public class TestLolaOutput
    {
        [Theory]
        [InlineData("xmltpn/Model.1bt1__0_____u__.xml.tpn")]
        [InlineData("xmltpn/Model.1c2v__0_____u__.xml.tpn")]
        [InlineData("xmltpn/Model.1gm8__0_____u__.xml.tpn")]
        [InlineData("xmltpn/Model.1j5k__0_____u__.xml.tpn")]
        [InlineData("xmltpn/Model.1kwn__0_____u__.xml.tpn")]
        [InlineData("xmltpn/Model.1mso__0_____u__.xml.tpn")]
        [InlineData("xmltpn/Model.1n9b__0_____u__.xml.tpn")]
        [InlineData("xmltpn/Model.1nf9__0_____u__.xml.tpn")]
        public void TestBadNames(string lolafile)
        {
            XMLTPNParser parser = new XMLTPNParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile(lolafile));
            string lolaOutput = net.ToLola(initialMarking);

            LolaParser lolaParser = new LolaParser();

            (PetriNet resultNet, Marking resultInitialMarking) = lolaParser.ReadNetFromString(lolaOutput.Trim());


            Assert.Equal(net.Places.Count, resultNet.Places.Count);
            Assert.Equal(net.Transitions.Count, resultNet.Transitions.Count);
            Assert.Equal(initialMarking.Count, resultInitialMarking.Count);
        }

        [Fact]
        public void TestBasicME()
        {
            LolaParser parser = new LolaParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("lola/basicME.lola"));
            string lolaOutput = net.ToLola(initialMarking);

            (PetriNet resultNet, Marking resultInitialMarking) = parser.ReadNetFromString(lolaOutput);

            Assert.Equal(net, resultNet);
        }

        [Fact]
        public void TestManufacture2()
        {
            LolaParser parser = new LolaParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("lola/manufacture2.lola"));
            string lolaOutput = net.ToLola(initialMarking);

            (PetriNet resultNet, Marking resultInitialMarking) = parser.ReadNetFromString(lolaOutput);

            Assert.Equal(net, resultNet);
        }

        [Fact]
        public void TestKanban()
        {
            LolaParser parser = new LolaParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("lola/kanban.lola"));
            string lolaOutput = net.ToLola(initialMarking);

            (PetriNet resultNet, Marking resultInitialMarking) = parser.ReadNetFromString(lolaOutput);

            Assert.Equal(net, resultNet);
        }
    }

    public class TestPnetParsing
    {
        [Fact]
        public void TestLeaderElection2()
        {
            PnetParser parser = new PnetParser();

            (PetriNet net, Marking initialMarking) =
                parser.ReadNet(Utils.GetPathForTestfile("pnet/leader_election_cr79_n2.pnet"));

            PetriNet referenceNet = new PetriNet(new Place[]{
                new Place("s1n1"),
                new Place("s1n2"),
                new Place("s1m1"),
                new Place("s1m2"),
                new Place("s2n1"),
                new Place("s2n2"),
                new Place("s2m1"),
                new Place("s2m2"),
                new Place("lead"),
            }, new UpdateTransition[]{
                new UpdateTransition("s1send1",
                new Dictionary<Place, int>{
                    [new Place("s1n1")] = 1,
                },
                new Dictionary<Place, int>{
                    [new Place("s1m1")] = 1,
                }),

                new UpdateTransition("s1send2",
                new Dictionary<Place, int>{
                    [new Place("s1n2")] = 1,
                },
                new Dictionary<Place, int>{
                    [new Place("s1m2")] = 1,
                }),

                new UpdateTransition("s1acpt1",
                new Dictionary<Place, int>{
                    [new Place("s2m1")] = 1,
                },
                new Dictionary<Place, int>{
                    [new Place("lead")] = 1,
                }),

                new UpdateTransition("s1pass2",
                new Dictionary<Place, int>{
                    [new Place("s2m2")] = 1,
                },
                new Dictionary<Place, int>{
                    [new Place("s1m2")] = 1,
                }),

                new UpdateTransition("s2send1",
                new Dictionary<Place, int>{
                    [new Place("s2n1")] = 1,
                },
                new Dictionary<Place, int>{
                    [new Place("s2m1")] = 1,
                }),

                new UpdateTransition("s2send2",
                new Dictionary<Place, int>{
                    [new Place("s2n2")] = 1,
                },
                new Dictionary<Place, int>{
                    [new Place("s2m2")] = 1,
                }),

                new UpdateTransition("s2disc1",
                new Dictionary<Place, int>{
                    [new Place("s1m1")] = 1,
                },
                new Dictionary<Place, int>{
                }),

                new UpdateTransition("s2acpt2",
                new Dictionary<Place, int>{
                    [new Place("s1m2")] = 1,
                },
                new Dictionary<Place, int>{
                    [new Place("lead")] = 1,
                }),

                new UpdateTransition("newleader",
                new Dictionary<Place, int>{
                    [new Place("lead")] = 1,
                },
                new Dictionary<Place, int>{
                    [new Place("s1n1")] = 1,
                    [new Place("s2n2")] = 1,
                }),
            });

            Assert.Equal(referenceNet, net);
        }
    }

    public class TestLoopPlaceIdentification
    {
        [Fact]
        public void Test_concdb__single_client_writes__depth_0()
        {
            LolaParser parser = new LolaParser();
            string filepath = Utils.GetPathForTestfile("lola/concdb__single_client_writes__depth_0.lola");
            (PetriNet net, _) =
                 parser.ReadNet(filepath);

            HashSet<UpdateTransition> aritficialTransitions = PetriNetUtils.IdentifyArtificalLoopTransitions(net);


            Assert.Single(aritficialTransitions);
            Assert.True(aritficialTransitions.First().Name == "t_l0");

        }
    }

    public class TestPNMLOutput
    {
        private readonly ITestOutputHelper output;

        public TestPNMLOutput(ITestOutputHelper output)
        {
            this.output = output;
        }

        [Theory]
        [InlineData("lola/basicME.lola")]
        [InlineData("lola/A.s00000021__s00000698.lola")]
        [InlineData("lola/pingpong.lola")]
        [InlineData("lola/manufacturing.lola")]
        [InlineData("lola/xml23.lola")]
        public void TestPNMLOutputAndReparse(string filename)
        {
            LolaParser parser = new LolaParser();
            string filepath = Utils.GetPathForTestfile(filename);
            (PetriNet net, Marking initialMarking) = parser.ReadNet(filepath);

            Regex rgx = new Regex("[^a-zA-Z0-9 -]");
            filename = rgx.Replace(filename, "");

            string outputFilepath = "net.pnml";
            UtilityEntrypoints.WritePNMLToFile(outputFilepath, net, initialMarking);

            PNMLParser pnml_parser = new PNMLParser();
            (PetriNet readNet, Marking readInitialMarking) = pnml_parser.ReadNet(outputFilepath);

            Assert.True(net.Equals(readNet));

            Assert.True(initialMarking.Equals(readInitialMarking));
        }
    }


    public class TestCycleFinding
    {

        private readonly ITestOutputHelper output;

        public TestCycleFinding(ITestOutputHelper output)
        {
            this.output = output;
        }

        [Fact]
        public void TestCycleNetCreationLamport_n2()
        {
            string filepath = Utils.GetPathForTestfile("pnet/lamport_n2.pnet");

            PnetParser parser = new PnetParser();

            (PetriNet handleNet, Marking initialMarking) =
                parser.ReadNet(filepath);

            (var atleastOneTimeSets, var zeroTimesSet) = parser.ReadCycleConditions(handleNet, filepath);

            (var cycleNet, var indicatorPlaces) = PetriNetUtils.CreateCycleNet(handleNet, atleastOneTimeSets, zeroTimesSet);
            Assert.NotEqual(handleNet, cycleNet);
            Assert.NotEmpty(indicatorPlaces);
            Assert.Null(cycleNet.GetTransitionByName("t1s4"));

            string[] modifiedTransitionsP1 = new string[] { "t1s1", "t1s2", "t1s3", "t1s3j2b2t", "t1s3j2b2f", "t1s5" };
            string[] modifiedTransitionsP2 = new string[] { "t2s1", "t2s2", "t2s2j1s1b1t", "t2s2j1s1b1f", "t2s2j1s2", "t2s2j1s3b1t", "t2s2j1s3b1f", "t2s2j1s4", "t2s3", "t2s4", "t2s5" };

            foreach (string modifiedTransition in modifiedTransitionsP1)
            {
                Assert.True(((UpdateTransition)cycleNet.GetTransitionByName(modifiedTransition)).Post[new Place("atleastOneTimeSetPlace_1")] == 1);
                Assert.False(((UpdateTransition)handleNet.GetTransitionByName(modifiedTransition)).Post.ContainsKey(new Place("atleastOneTimeSetPlace_1")));
            }

            foreach (string modifiedTransition in modifiedTransitionsP2)
            {
                Assert.True(((UpdateTransition)cycleNet.GetTransitionByName(modifiedTransition)).Post[new Place("atleastOneTimeSetPlace_2")] == 1);
                Assert.False(((UpdateTransition)handleNet.GetTransitionByName(modifiedTransition)).Post.ContainsKey(new Place("atleastOneTimeSetPlace_2")));
            }
        }

        [Fact]
        public void TestCycleNetCreationLeaderElection_n2()
        {
            string filepath = Utils.GetPathForTestfile("pnet/leader_election_cr79_n2.pnet");

            PnetParser parser = new PnetParser();

            (PetriNet handleNet, Marking initialMarking) =
                parser.ReadNet(filepath);

            (var atleastOneTimeSets, var zeroTimesSet) = parser.ReadCycleConditions(handleNet, filepath);

            (var cycleNet, var indicatorPlaces) = PetriNetUtils.CreateCycleNet(handleNet, atleastOneTimeSets, zeroTimesSet);
            Assert.NotEqual(handleNet, cycleNet);
            Assert.Empty(indicatorPlaces);
            Assert.Null(cycleNet.GetTransitionByName("newleader"));
        }

        [Fact]
        public void TestCycleNetCreationPeterson_n2()
        {
            string filepath = Utils.GetPathForTestfile("pnet/peterson_n2.pnet");

            PnetParser parser = new PnetParser();

            (PetriNet handleNet, Marking initialMarking) =
                parser.ReadNet(filepath);

            (var atleastOneTimeSets, var zeroTimesSet) = parser.ReadCycleConditions(handleNet, filepath);

            (var cycleNet, var indicatorPlaces) = PetriNetUtils.CreateCycleNet(handleNet, atleastOneTimeSets, zeroTimesSet);
            Assert.NotEqual(handleNet, cycleNet);
            Assert.NotEmpty(indicatorPlaces);
            Assert.Null(cycleNet.GetTransitionByName("t1s11"));

            string[] modifiedTransitionsP1 = new string[] { "t1s1", "t1s2j1", "t1s3j1turn1v0", "t1s3j1turn1v1", "t1s3j1turn1v2", "t1s4j1", "t1s5j1k2q2v0", "t1s5j1k2q2v1", "t1s5j1k2q2v2", "t1s6j1k2turn1v0", "t1s6j1k2turn1v1", "t1s6j1k2turn1v2", "t1s7j1k2", "t1s8j1k2", "t1s9j1", "t1s10", "t1s12", "t1s13" };
            string[] modifiedTransitionsP2 = new string[] { "t2s1", "t2s2j1", "t2s3j1turn1v0", "t2s3j1turn1v1", "t2s3j1turn1v2", "t2s4j1", "t2s5j1k1q1v0", "t2s5j1k1q1v1", "t2s5j1k1q1v2", "t2s6j1k1turn1v0", "t2s6j1k1turn1v1", "t2s6j1k1turn1v2", "t2s7j1k1", "t2s8j1k1", "t2s9j1", "t2s10", "t2s11", "t2s12", "t2s13" };

            foreach (string modifiedTransition in modifiedTransitionsP1)
            {
                Assert.True(((UpdateTransition)cycleNet.GetTransitionByName(modifiedTransition)).Post[new Place("atleastOneTimeSetPlace_1")] == 1);
                Assert.False(((UpdateTransition)handleNet.GetTransitionByName(modifiedTransition)).Post.ContainsKey(new Place("atleastOneTimeSetPlace_1")));
            }

            foreach (string modifiedTransition in modifiedTransitionsP2)
            {
                Assert.True(((UpdateTransition)cycleNet.GetTransitionByName(modifiedTransition)).Post[new Place("atleastOneTimeSetPlace_2")] == 1);
                Assert.False(((UpdateTransition)handleNet.GetTransitionByName(modifiedTransition)).Post.ContainsKey(new Place("atleastOneTimeSetPlace_2")));
            }
        }

        [Fact]
        public void ToDotSpecTest()
        {
            Place p1 = new Place("p1");
            Place p2 = new Place("p2");
            Place p3 = new Place("p3");
            Place p4 = new Place("p4");
            List<Place> places = new List<Place>{
                p1, p2, p3, p4
            };

            List<Transition> transitions = new List<Transition>
            {
                new UpdateTransition("t1", new Dictionary<Place, int>{
                    [p1] = 1,
                    [p2] = 1
                },
                new Dictionary<Place, int>{
                    [p3] = 1
                })
            };

            Marking initialMarking = new Marking
            {
                [p1] = 1
            };

            MarkingWithConstraints targetMarking1 = MarkingWithConstraints.AsCoverability(new Marking
            {
                [p2] = 3
            });

            MarkingWithConstraints targetMarking2 = MarkingWithConstraints.AsCoverability(new Marking
            {
                [p4] = 2
            });

            List<MarkingWithConstraints> targetMarkings = new List<MarkingWithConstraints> { targetMarking1, targetMarking2 };

            PetriNet net = new PetriNet(places, transitions);

            string result = net.ToDotspec(initialMarking, targetMarkings);
            output.WriteLine(result);

        }
    }
}