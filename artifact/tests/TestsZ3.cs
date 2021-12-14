using Xunit;
using System;
using System.Collections.Generic;
using Petri;
using System.Linq;
using Xunit.Abstractions;
using Microsoft.Z3;
using Utils;
using System.Diagnostics;

namespace Testing
{
    public class TestZ3
    {
        private readonly ITestOutputHelper output;

        public TestZ3(ITestOutputHelper output)
        {
            this.output = output;
        }
        [Fact]
        public void simpleExample()
        {
            using (Context ctx = new Context())
            {
                BoolExpr x = ctx.MkBoolConst("x");
                BoolExpr y = ctx.MkBoolConst("y");
                BoolExpr x_xor_y = ctx.MkXor(x, y);

                IntExpr number = ctx.MkIntConst("k");
                IntExpr constant = ctx.MkInt(8);
                BoolExpr geq = ctx.MkGt(number, constant);

                BoolExpr geq_and_x_xor_y = ctx.MkAnd(geq, x_xor_y);

                Model model = Z3Utils.Check(ctx, geq_and_x_xor_y, Status.SATISFIABLE);
                int kValue = ((IntNum)model.Evaluate(number)).Int;
                bool xValue = ((BoolExpr)model.Evaluate(x)).BoolValue == Z3_lbool.Z3_L_TRUE;
                bool yValue = ((BoolExpr)model.Evaluate(y)).BoolValue == Z3_lbool.Z3_L_TRUE;

                Assert.True(xValue ^ yValue);
                Assert.True(kValue > 8);
            }
        }

        [Fact]
        public void vectorExample()
        {
            using (Context ctx = new Context())
            {
                Sort vectorType = ctx.MkBitVecSort(32);
                BitVecExpr x = (BitVecExpr)ctx.MkConst("x", vectorType);
                BitVecNum zeroes = (BitVecNum)ctx.MkNumeral(0, vectorType);
                BitVecNum ten = (BitVecNum)ctx.MkNumeral(10, vectorType);

                BitVecExpr x_minus_ten = ctx.MkBVSub(x, ten);
                BoolExpr x_minus_ten_geq_0 = ctx.MkBVSGE(x_minus_ten, zeroes);

                BoolExpr x_geq_ten = ctx.MkBVSGE(x, ten);

                BoolExpr implies = ctx.MkImplies(x_minus_ten_geq_0, x_geq_ten);

                BoolExpr x_lt_ten = ctx.MkBVSLT(x, ten);

                BoolExpr and = ctx.MkAnd(x_lt_ten, implies);
                Model model = Z3Utils.Check(ctx, and, Status.SATISFIABLE);

                int x_value = ((BitVecNum)model.Evaluate(x)).Int;

                Assert.True(x_value < 10);
            }
        }

        [Fact]
        public void testMarkingEquationSwimmingPool()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/swimming_pool.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/swimming_pool.formula"));

            Dictionary<string, float> expectedParikhImage = new Dictionary<string, float>
            {
                ["t0"] = 2,
                ["t1"] = 1,
                ["t2"] = 1,
            };

            Dictionary<UpdateTransition, float> parikhImage = Z3Heuristics.CalculateParikhImageViaMarkingEquation(net, initialMarking, targetMarkings);

            var actualImage = parikhImage.Where(kvPair => kvPair.Value > 0).Select(kvPair => new KeyValuePair<string, float>(kvPair.Key.Name, kvPair.Value)).OrderBy(t => t.ToString()).ToList();

            Assert.Equal(expectedParikhImage, actualImage);
        }

        [Fact]
        public void testMarkingEquationPncsacover()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/pncsacover.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/pncsacover.formula"));

            Stopwatch watch = Stopwatch.StartNew();
            Dictionary<UpdateTransition, float> parikhImage = Z3Heuristics.CalculateParikhImageViaMarkingEquation(net, initialMarking, targetMarkings);
            watch.Stop();
            Console.WriteLine("Elapsed: " + watch.ElapsedMilliseconds.ToString());
            Console.WriteLine(String.Join("\n", parikhImage));
        }

        [Fact]
        public void testQReachabilitySwimmingPool()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/swimming_pool.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/swimming_pool.formula"));

            Dictionary<string, float> expectedParikhImage = new Dictionary<string, float>
            {
                ["t0"] = 2,
                ["t1"] = 1,
                ["t2"] = 1,
            };

            Dictionary<UpdateTransition, float> parikhImage = Z3Heuristics.CalculateParikhImageViaQReachability(net, initialMarking, targetMarkings);

            var actualImage = parikhImage.Where(kvPair => kvPair.Value > 0).Select(kvPair => new KeyValuePair<string, float>(kvPair.Key.Name, kvPair.Value)).OrderBy(t => t.ToString()).ToList();

            Assert.Equal(expectedParikhImage, actualImage);
        }

        [Fact]
        public void testQReachabilityLeabasicapproach()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/leabasicapproach.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/leabasicapproach.formula"));

            List<KeyValuePair<string, float>> expectedParikhImage = new Dictionary<string, float>
            {
                ["t6"] = 1,
                ["t7"] = 1,
                ["t0"] = 1,
                ["t1"] = 1
            }.OrderBy(t => t.ToString()).ToList();

            Dictionary<UpdateTransition, float> parikhImage = Z3Heuristics.CalculateParikhImageViaQReachability(net, initialMarking, targetMarkings);

            var actualImage = parikhImage.Where(kvPair => kvPair.Value > 0).Select(kvPair => new KeyValuePair<string, float>(kvPair.Key.Name, kvPair.Value)).OrderBy(t => t.ToString()).ToList();

            Assert.Equal(expectedParikhImage, actualImage);
        }

        [Fact]
        public void TestHeuristicLeabasicapproachNonInitialMarking()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/leabasicapproach.lola"));
            PetriNet net = input.Item1;

            Marking initialMarking = new Marking(new Dictionary<Place, int>
            {
                [new Place("unlockS")] = 1,
                [new Place("unlockC")] = 1,
                [new Place("Swhile")] = 1,
                [new Place("Cbefore")] = 1
            });

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/leabasicapproach.formula"));

            float expectedScore = new Dictionary<string, float>
            {
                ["t7"] = 1,
                ["t0"] = 1,
                ["t1"] = 1
            }.OrderBy(t => t.ToString()).ToList().Count;

            Func<Marking, float?> heuristicScore = Z3Heuristics.InitializeQReachabilityHeuristic(net, targetMarkings);

            float? actualScore = heuristicScore(initialMarking);

            Assert.NotNull(actualScore);

            Assert.Equal(expectedScore, actualScore);
        }

        [Fact]
        public void TestHeuristicLeabasicapproachInitialMarking()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/leabasicapproach.lola"));
            PetriNet net = input.Item1;

            Marking initialMarking = new Marking(new Dictionary<Place, int>
            {
                [new Place("unlockS")] = 1,
                [new Place("unlockC")] = 1,
                [new Place("Swhile")] = 1,
                [new Place("Cwhile")] = 1
            });

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/leabasicapproach.formula"));

            float expectedScore = new Dictionary<string, float>
            {
                ["t6"] = 1,
                ["t7"] = 1,
                ["t0"] = 1,
                ["t1"] = 1
            }.OrderBy(t => t.ToString()).ToList().Count;

            Func<Marking, float?> heuristicScore = Z3Heuristics.InitializeQReachabilityHeuristic(net, targetMarkings);

            float? actualScore = heuristicScore(initialMarking);

            Assert.NotNull(actualScore);

            Assert.Equal(expectedScore, actualScore);
        }

        [Fact]
        public void TestMultipleHeuristicCalls()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/leabasicapproach.lola"));
            PetriNet net = input.Item1;

            Marking initialMarking1 = new Marking(new Dictionary<Place, int>
            {
                [new Place("unlockS")] = 1,
                [new Place("unlockC")] = 1,
                [new Place("Swhile")] = 1,
                [new Place("Cwhile")] = 1
            });

            Marking initialMarking2 = new Marking(new Dictionary<Place, int>
            {
                [new Place("unlockS")] = 1,
                [new Place("unlockC")] = 1,
                [new Place("Swhile")] = 1,
                [new Place("Cbefore")] = 1
            });

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/leabasicapproach.formula"));

            Func<Marking, float?> heuristicFunction = Z3Heuristics.InitializeQReachabilityHeuristic(net, targetMarkings);

            float? actualScore1 = heuristicFunction(initialMarking1);
            float? actualScore2 = heuristicFunction(initialMarking2);
            float? actualScore3 = heuristicFunction(initialMarking1);

            float expectedScore1 = 4;
            float expectedScore2 = 3;
            float expectedScore3 = 4;

            Assert.Equal(expectedScore1, actualScore1);
            Assert.Equal(expectedScore2, actualScore2);
            Assert.Equal(expectedScore3, actualScore3);

        }

        [Fact]
        public void testQReachabilitySimplifiedBasicME()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/basicME_simplified.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/basicME_simplified.formula"));

            List<KeyValuePair<string, float>> expectedParikhImage = new Dictionary<string, float>
            {
                ["t0"] = 1,
            }.OrderBy(t => t.ToString()).ToList();

            Dictionary<UpdateTransition, float> parikhImage = Z3Heuristics.CalculateParikhImageViaQReachability(net, initialMarking, targetMarkings);

            var actualImage = parikhImage.Where(kvPair => kvPair.Value > 0f).Select(kvPair => new KeyValuePair<string, float>(kvPair.Key.Name, kvPair.Value)).OrderBy(t => t.ToString()).ToList();
            Assert.Equal(expectedParikhImage, actualImage);
        }

        [Fact]
        public void testQReachabilityVerySimpleTest()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/very_simple_test.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/very_simple_test.formula"));

            List<KeyValuePair<string, float>> expectedParikhImage = new Dictionary<string, float>
            {
                ["t0"] = 1,
            }.OrderBy(t => t.ToString()).ToList();

            Dictionary<UpdateTransition, float> parikhImage = Z3Heuristics.CalculateParikhImageViaQReachability(net, initialMarking, targetMarkings);

            var actualImage = parikhImage.Where(kvPair => kvPair.Value > 0f).Select(kvPair => new KeyValuePair<string, float>(kvPair.Key.Name, kvPair.Value)).OrderBy(t => t.ToString()).ToList();
            Assert.Equal(expectedParikhImage, actualImage);
        }

        [Fact]
        public void testQReachabilityBasicME()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/basicME.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/basicME.formula"));
            var parikhImage = Z3Heuristics.CalculateParikhImageViaQReachability(net, initialMarking, targetMarkings);

            // assert unreachability
            Assert.Null(parikhImage);
        }

        [Fact]
        public void testQReachabilityPncsasemiliv()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/pncsasemiliv.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/pncsasemiliv.formula"));
            var parikhImage = Z3Heuristics.CalculateParikhImageViaQReachability(net, initialMarking, targetMarkings);
            //output.WriteLine(String.Join("\n", parikhImage.Where(kvPair => kvPair.Value > 0).Select(kvPair => new KeyValuePair<string, float>(kvPair.Key.Name, kvPair.Value))));
        }

        [Fact]
        public void testHeuristicPncsasemilivNonInitialMarkings()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/pncsasemiliv.lola"));
            PetriNet net = input.Item1;

            Marking marking1 = new Marking(new Dictionary<Place, int>
            {
                [new Place("x2")] = 1,
                [new Place("x13")] = 1
            });

            Marking marking2 = new Marking(new Dictionary<Place, int>
            {
                [new Place("x13")] = 1,
                [new Place("x3")] = 1,
                [new Place("x21")] = 1,
                [new Place("x2")] = 0
            });

            Marking marking3 = new Marking(new Dictionary<Place, int>
            {
                [new Place("x3")] = 1,
                [new Place("x14")] = 1,
                [new Place("x22")] = 1
            });

            Marking marking4 = new Marking(new Dictionary<Place, int>
            {
                [new Place("x13")] = 1,
                [new Place("x21")] = 1,
                [new Place("x10")] = 1
            });

            Marking marking5 = new Marking(new Dictionary<Place, int>
            {
                [new Place("x14")] = 1,
                [new Place("x4")] = 1,
                [new Place("x23")] = 1
            });

            Marking marking6 = new Marking(new Dictionary<Place, int>
            {
                [new Place("x14")] = 1,
                [new Place("x22")] = 1,
                [new Place("x0")] = 1,
                [new Place("x10")] = 1
            });


            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/pncsasemiliv.formula"));

            Func<Marking, float?> heuristicFunction = Z3Heuristics.InitializeQReachabilityHeuristic(net, targetMarkings);

            float? score1 = heuristicFunction(marking1);
            float? score2 = heuristicFunction(marking2);
            float? score3 = heuristicFunction(marking3);
            float? score4 = heuristicFunction(marking4);
            float? score5 = heuristicFunction(marking5);
            float? score6 = heuristicFunction(marking6);

            Assert.Equal(10, score6);
        }

        [Fact]
        public void testMarkingEquationLeabasicapproach()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/leabasicapproach.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/leabasicapproach.formula"));

            List<KeyValuePair<string, float>> expectedParikhImage = new Dictionary<string, float>
            {
                ["t6"] = 1,
                ["t7"] = 1,
                ["t0"] = 1,
                ["t1"] = 1
            }.OrderBy(t => t.ToString()).ToList();

            Dictionary<UpdateTransition, float> parikhImage = Z3Heuristics.CalculateParikhImageViaMarkingEquation(net, initialMarking, targetMarkings);

            var actualImage = parikhImage.Where(kvPair => kvPair.Value > 0).Select(kvPair => new KeyValuePair<string, float>(kvPair.Key.Name, kvPair.Value)).OrderBy(t => t.ToString()).ToList();

            Assert.Equal(expectedParikhImage, actualImage);
        }

        [Fact]
        public void testMarkingEquationManufacture2()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("lola/manufacture2.lola"));
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("lola/manufacture2.formula"));

            List<KeyValuePair<string, float>> expectedParikhImage = new Dictionary<string, float>
            {
                ["t0"] = 1.5f,
                ["t2"] = 2,
                ["t4"] = 1,
            }.OrderBy(t => t.ToString()).ToList();

            Dictionary<UpdateTransition, float> parikhImage = Z3Heuristics.CalculateParikhImageViaMarkingEquation(net, initialMarking, targetMarkings);

            var actualImage = parikhImage.Where(kvPair => kvPair.Value > 0).Select(kvPair => new KeyValuePair<string, float>(kvPair.Key.Name, kvPair.Value)).OrderBy(t => t.ToString()).ToList();

            Assert.Equal(expectedParikhImage, actualImage);
        }
    }

    public class TestTransitionSupportPruning
    {
        private readonly ITestOutputHelper output;

        public TestTransitionSupportPruning(ITestOutputHelper output)
        {
            this.output = output;
        }


        [Fact]
        public static void ICoverPruningVsTransitionSupportPruning()
        {
            string filename = Utils.GetPathForTestfile("lola/pruning_sanity_check");

            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(filename + ".lola");
            PetriNet net = input.Item1;
            Marking initialMarking = input.Item2;

            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(filename + ".formula");

            (PetriNet net_zeroplacePruned, _, _) = Pruning.PruneWithZeroPlaceHeuristic(net, initialMarking, targetMarkings);

            // cast to int because xunit warns otherwise, even as there is no canonical way to test for collection size
            Assert.Equal(2, (int)net_zeroplacePruned.Transitions.Count);
        }
    }
}