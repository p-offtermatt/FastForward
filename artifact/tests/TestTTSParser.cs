using Xunit;
using System;
using Petri;
using Xunit.Abstractions;
using System.Collections.Generic;
using System.Linq;
using System.Collections;

namespace Testing
{
    public class TestTTSParser
    {

        private readonly ITestOutputHelper output;

        TTSParser parser = new BinaryTTSParser();

        public TestTTSParser(ITestOutputHelper output)
        {
            this.output = output;

        }

        [Fact]
        public void SimpleBinaryParserTest()
        {
            TTSParser parser = new BinaryTTSParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("tts/vass.tts"));

            Assert.Equal(6, net.Transitions.Count);

            HashSet<Place> expectedPlaces = new HashSet<Place>{
                new Place("statebit_0_true"),
                new Place("statebit_0_false"),
                new Place("statebit_1_true"),
                new Place("statebit_1_false"),
                new Place("statebit_2_true"),
                new Place("statebit_2_false"),
                new Place("counter0"),
                new Place("counter1"),
                new Place("counter2"),
                new Place("counter3")
            };

            output.WriteLine(String.Join(" ", net.Places.ToHashSet()));
            Assert.Equal(expectedPlaces, net.Places.ToHashSet());

            IEnumerable<UpdateTransition> updates = net.Transitions.Where(transition => transition is UpdateTransition).Cast<UpdateTransition>();
            IEnumerable<SetTransferTransition> setTransfers = net.Transitions.Where(transition => transition is SetTransferTransition).Cast<SetTransferTransition>();
            IEnumerable<MultitransferTransition> multitransfers = net.Transitions.Where(transition => transition is MultitransferTransition).Cast<MultitransferTransition>();



            Assert.Equal(1, setTransfers.Count());
            Assert.Equal(1, multitransfers.Count());
            Assert.Equal(4, updates.Count());
        }

        [Theory]
        [InlineData("1.prop", 1, 0)]
        [InlineData("2.prop", 7, 1)]
        [InlineData("3.prop", 5, 1)]
        public void BinaryParserFinalMarkingTest(string propfile, int targetState, int targetCounter)
        {
            TTSParser parser = new BinaryTTSParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("tts/vass.tts"));

            List<MarkingWithConstraints> targets = parser.ReadFormula(Utils.GetPathForTestfile("tts/targets/" + propfile));

            Assert.Single(targets);

            MarkingWithConstraints target = targets.First();

            foreach (Place place in net.Places)
            {
                Assert.Equal(ConstraintOperators.GreaterEqual, target.Constraints.GetValueOrDefault(place));
            }

            Place targetCounterPlace = parser.GetCounterPlace(targetCounter);
            Dictionary<Place, int> targetCounterPre = new Dictionary<Place, int> { [targetCounterPlace] = 1 };
            Dictionary<Place, int> targetStatePre = parser.GetPreForState(targetState);

            Marking expectedMarking = new Marking(targetCounterPre.Union(targetStatePre).ToDictionary(kvPair => kvPair.Key, kvPair => kvPair.Value));

            Assert.Equal(expectedMarking, target.Marking);
        }

#if GUROBI
        // [Fact]
        // public void SimpleTransferTest()
        // {
        //     TTSParser parser = new BinaryTTSParser();
        //     (PetriNet netWithTransfer, Marking initialMarkingWithTransfer) = parser.ReadNet(Utils.GetPathForTestfile("tts/simple_transfer/simple_transfer_coverable.tts"));
        //     (PetriNet netWithUpdate, Marking initialMarkingWithUpdate) = parser.ReadNet(Utils.GetPathForTestfile("tts/simple_transfer/simple_transfer_uncoverable.tts"));

        //     List<MarkingWithConstraints> targets = parser.ReadFormula(Utils.GetPathForTestfile("tts/simple_transfer/simple_transfer.prop"));

        //     List<Transition> pathWithTransfers = PetriNetUtils.PetriNetAStar(netWithTransfer, initialMarkingWithTransfer, targets, GurobiHeuristics.InitializeMarkingEquationHeuristic(netWithTransfer, targets));
        //     List<Transition> pathWithUpdate = PetriNetUtils.PetriNetAStar(netWithUpdate, initialMarkingWithUpdate, targets, GurobiHeuristics.InitializeMarkingEquationHeuristic(netWithUpdate, targets));

        //     Assert.Null(pathWithUpdate);
        //     Assert.NotNull(pathWithTransfers);
        //     Assert.Equal(2, pathWithTransfers.Count);
        // }

        // [Fact]
        // public void ParameterizedInitialStateTest()
        // {
        //     TTSParser nonParameterizedParser = new BinaryTTSParser();
        //     TTSParser.ParameterizedInitial = false;
        //     (PetriNet nonparameterizedNet, Marking nonparameterizedInitialMarking) = nonParameterizedParser.ReadNet(Utils.GetPathForTestfile("tts/multitransfer_pruning/simple_multitransfer_pruning.tts"));

        //     TTSParser parameterizedParser = new BinaryTTSParser();
        //     TTSParser.ParameterizedInitial = true;
        //     (PetriNet parameterizedNet, Marking parameterizedInitialMarking) = parameterizedParser.ReadNet(Utils.GetPathForTestfile("tts/multitransfer_pruning/simple_multitransfer_pruning.tts"));

        //     List<MarkingWithConstraints> targets = nonParameterizedParser.ReadFormula(Utils.GetPathForTestfile("tts/multitransfer_pruning/simple_multitransfer_pruning.prop"));

        //     List<Transition> nonparameterizedPath = PetriNetUtils.PetriNetAStar(nonparameterizedNet, nonparameterizedInitialMarking, targets, GurobiHeuristics.InitializeMarkingEquationHeuristic(nonparameterizedNet, targets));
        //     List<Transition> parameterizedPath = PetriNetUtils.PetriNetAStar(parameterizedNet, parameterizedInitialMarking, targets, GurobiHeuristics.InitializeMarkingEquationHeuristic(parameterizedNet, targets));

        //     Assert.NotNull(parameterizedPath);
        //     Assert.Null(nonparameterizedPath);
        // }
        
        // [Fact]
        // public void SimpleSpawnTest()
        // {
        //     TTSParser parser = new BinaryTTSParser();
        //     (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("tts/spawn_transition/spawn_transition.tts"));

        //     List<MarkingWithConstraints> targets = parser.ReadFormula(Utils.GetPathForTestfile("tts/spawn_transition/spawn_transition.prop"));

        //     UpdateTransition spawn1 = (UpdateTransition)net.GetTransitionByName("update_1");
        //     output.WriteLine(spawn1.ToString());
        //     Assert.Equal(2, spawn1.Post.GetValueOrDefault(new Place("counter0")));
        //     Assert.Equal(1, spawn1.Pre.GetValueOrDefault(new Place("counter0"), 0));

        //     UpdateTransition spawn2 = (UpdateTransition)net.GetTransitionByName("update_2");
        //     output.WriteLine(spawn2.ToString());
        //     Assert.Equal(1, spawn2.Post.GetValueOrDefault(new Place("counter0")));
        //     Assert.Equal(1, spawn2.Post.GetValueOrDefault(new Place("counter1")));
        //     Assert.Equal(1, spawn2.Pre.GetValueOrDefault(new Place("counter1")));

        //     List<Transition> path = PetriNetUtils.PetriNetAStar(net,
        //                                                         initialMarking,
        //                                                         targets,
        //                                                         GurobiHeuristics.InitializeMarkingEquationHeuristic(
        //                                                             net,
        //                                                             targets));
        //     Assert.NotNull(path);
        // }
#endif

        [Fact]
        public void SimpleLocalMultitransferTest()
        {
            TTSParser parser = new BinaryTTSParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("tts/local_multitransfer/multitransfer.tts"));

            List<MarkingWithConstraints> targets = parser.ReadFormula(Utils.GetPathForTestfile("tts/local_multitransfer/multitransfer.prop"));

            MultitransferTransition multitransfer = (MultitransferTransition)net.GetTransitionByName("multitransfer_2");
            Assert.True(multitransfer.Transfers.ContainsKey(new Place("counter1")));
            Assert.True(multitransfer.Transfers.ContainsKey(new Place("counter2")));

            Assert.Equal(new HashSet<Place> { new Place("counter3") }, multitransfer.Transfers[new Place("counter2")]);
            Assert.Equal(new HashSet<Place> { new Place("counter3") }, multitransfer.Transfers[new Place("counter1")]);
        }

        [Theory]
        // [InlineData("_all_counters.prop", false)]
        [InlineData(".prop", true)]
        [InlineData("_03.prop", true)]
        [InlineData("_12.prop", true)]
        public void SimpleLocalMultitransferReachabilityTest(string propfile, bool reachable)
        {
            TransferReachabilityTest("tts/local_multitransfer/multitransfer.tts", "tts/local_multitransfer/multitransfer" + propfile, reachable);
        }

        [Theory]
        // [InlineData("tts/vass.tts", "tts/vass.prop", false)]
        [InlineData("tts/simple_vass.tts", "tts/simple_vass.prop", true)]
        public void TransferReachabilityTest(string netfile, string propfile, bool reachable)
        {
            TTSParser parser = new BinaryTTSParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile(netfile));

            List<MarkingWithConstraints> targets = parser.ReadFormula(Utils.GetPathForTestfile(propfile));

            List<Transition> path = PetriNetUtils.PetriNetAStar(net,
                                                                initialMarking,
                                                                targets,
                                                                x => 0);
            if (reachable)
            {
                Assert.NotNull(path);
            }
            else
            {
                Assert.Null(path);
            }
        }

        [Fact]
        public void BinaryParserInitialMarkingTest()
        {
            BinaryTTSParser parser = new BinaryTTSParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("tts/states/states.tts"));

            Dictionary<Place, int> zeroStatePre = parser.GetPreForState(0);
            Place zeroCounterPlace = parser.GetCounterPlace(0);

            Dictionary<Place, int> counterPre = new Dictionary<Place, int> { [zeroCounterPlace] = 1 };

            Marking expectedInitialMarking = new Marking(zeroStatePre.Union(counterPre).ToDictionary(kvPair => kvPair.Key, kvPair => kvPair.Value));

            Assert.Equal(expectedInitialMarking, initialMarking);
        }

        public class BinaryParserStateRepresentationTestData : IEnumerable<object[]>
        {
            public IEnumerator<object[]> GetEnumerator()
            {
                for (int i = 0; i <= 10; i++)
                {
                    for (int j = 0; j <= 10; j++)
                    {
                        if (i == j)
                        {
                            continue;
                        }

                        yield return new object[] { i, j, false, 0 };
                    }
                }
            }

            IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
        }

        [Theory]
        [InlineData(0, 0, true, 9)]
        [InlineData(1, 1, true, 10)]
        [InlineData(2, 2, true, 1)]
        [InlineData(3, 3, true, 3)]
        [InlineData(4, 4, true, 5)]
        [InlineData(5, 5, true, 2)]
        [InlineData(6, 6, true, 1)]
        [InlineData(7, 7, true, 2)]
        [InlineData(8, 8, true, 1)]
        [InlineData(9, 9, true, 8)]
        [InlineData(10, 10, true, 7)]
        [ClassData(typeof(BinaryParserStateRepresentationTestData))]
        public void BinaryParserStateRepresentationTest(int markingStateNum, int transitionNum, bool fireable, int expectedStateNumAfterFiring)
        {
            BinaryTTSParser parser = new BinaryTTSParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("tts/states/states.tts"));

            Transition transition = net.GetTransitionByName("update_" + transitionNum);

            Marking marking = parser.GetMarking(markingStateNum, 0);

            Assert.Equal(fireable, marking.Enables(transition));

            if (fireable)
            {
                Marking resultMarking = transition.FireOn(marking).First();

                Marking expectedStateMarking = parser.GetMarkingForState(expectedStateNumAfterFiring);

                Assert.True(expectedStateMarking.CoveredBy(resultMarking),
                    "Marking does not cover marking for place after firing transition " + transition.ToString() + ". Result Marking: " + resultMarking.ToString() + ", Expected Marking: " + expectedStateMarking.ToString());
            }
        }

        [Fact]
        public void StatebitPlacesTest()
        {
            BinaryTTSParser parser = new BinaryTTSParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("tts/multitransfer_pruning/24_pthread.2.tts"));
            List<MarkingWithConstraints> targetMarkings = parser.ReadFormula(Utils.GetPathForTestfile("tts/multitransfer_pruning/24_pthread.2.prop"));

            int expectedNumberOfBits = 10;

            foreach (Transition transition in net.Transitions)
            {
                foreach (Place place in transition.GetSupportPlaces())
                {
                    string name = place.Name;
                    if (name.StartsWith("statebit_"))
                    {
                        int num = Int32.Parse(name.Split("_")[1]);
                        Assert.True(num < expectedNumberOfBits, "There should only be " + expectedNumberOfBits.ToString() + " bits, but this place is called " + name);
                    }
                }
            }
        }

        public class CounterAndStatesTestData : IEnumerable<object[]>
        {
            public IEnumerator<object[]> GetEnumerator()
            {
                for (int stateNumber = 0; stateNumber < 30; stateNumber++)
                {
                    List<List<int>> counterValues = new List<List<int>>{
                    new List<int>{10, 20, 0, 0, 0, 1, 1},
                    new List<int>{0, 0, 0, 0, 0, 0, 0},
                    new List<int>{1, 0, 2, 3, 4, 10, 20, 0},
                    new List<int>{29,29,29,29,29,29,29,29,29,29}
                    };
                    foreach (List<int> counters in counterValues)
                    {
                        List<int> result = new List<int>(counters);

                        yield return result.Prepend(stateNumber).Cast<object>().ToArray();
                    }
                }
            }

            IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
        }

        [Theory]
        [ClassData(typeof(CounterAndStatesTestData))]
        public void TestGettingCountersAndStates(int state, params int[] counters)
        {
            int numCounters = 30;
            BinaryTTSParser parser = new BinaryTTSParser();

            // prime the parser with the right state and counter numbers
            parser.ReadNet(Utils.GetPathForTestfile("tts/get_states_and_counters.tts"));

            Dictionary<int, int> counterValues = counters.ToHashSet().ToDictionary(num => num, num => counters.Where(x => x == num).Count());

            Marking marking = new Marking();
            foreach ((int counterNum, int value) in counterValues)
            {
                marking[parser.GetCounterPlace(counterNum)] = value;
            }

            Dictionary<Place, int> stateMarking = parser.GetMarkingForState(state);

            foreach ((Place place, int value) in stateMarking)
            {
                marking[place] = value;
            }

            Assert.Equal(state, parser.GetStateNumberFromMarking(marking));

            for (int counterNum = 0; counterNum < numCounters; counterNum++)
            {
                Assert.Equal(counterValues.GetValueOrDefault(counterNum, 0), parser.GetCounterValuesFromMarking(marking).GetValueOrDefault(counterNum, 0));
            }

        }

    }
}