using Xunit;
using Petri;
using System.Collections.Generic;
using System.Collections;
using System.Linq;
using Xunit.Abstractions;
using System;

namespace Testing
{

    public class TestSetTransfers
    {

        private readonly ITestOutputHelper output;

        public TestSetTransfers(ITestOutputHelper output)
        {
            this.output = output;
        }

        static Place p1 = new Place("p1");
        static Place p2 = new Place("p2");
        static Place p3 = new Place("p3");
        static SetTransferTransition transition = new SetTransferTransition("t1", new HashSet<Place> { p1 }, new HashSet<Place> { p2 });
        static Marking m1 = new Marking(new Dictionary<Place, int>
        {
            [p1] = 5,
            [p2] = 0
        });



        public class SplitTestData : IEnumerable<object[]>
        {
            public IEnumerator<object[]> GetEnumerator()
            {
                // returns an array {number, pieces, split expected to be present}

                // test all combinations for small example
                yield return new object[] { 3, 2, new int[] { 1, 2 } };
                yield return new object[] { 3, 2, new int[] { 2, 1 } };
                yield return new object[] { 3, 2, new int[] { 3, 0 } };
                yield return new object[] { 3, 2, new int[] { 0, 3 } };

                // test recursion anchor
                yield return new object[] { 4, 1, new int[] { 4 } };
                yield return new object[] { 2, 1, new int[] { 2 } };

                // test some cases for large examples
                yield return new object[] { 4, 5, new int[] { 0, 1, 1, 1, 1 } };
                yield return new object[] { 4, 5, new int[] { 0, 3, 1, 0, 0 } };
                yield return new object[] { 4, 5, new int[] { 0, 1, 1, 0, 2 } };
                yield return new object[] { 4, 5, new int[] { 2, 0, 1, 0, 1 } };

                yield return new object[] { 4, 5, new int[] { 2, 1, 0, 0, 1 } };

            }

            IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
        }

        [Theory]
        [InlineData(2, 3)]
        [InlineData(4, 1)]
        [InlineData(3, 3)]
        [InlineData(2, 2)]
        [InlineData(1, 6)]
        public void TestSplittingNumberRightPieceNumberAndSum(int number, int pieces)
        {
            foreach (List<int> split in PetriNetUtils.SplitNumberIntoPieces(number, pieces))
            {
                // assert both here so the splits are only computed once
                Assert.Equal(pieces, (int)split.Count);
                Assert.Equal(number, split.Sum());
            }
        }

        [Theory]
        [ClassData(typeof(SplitTestData))]
        public void TestSplittingNumberExampleMembership(int number, int pieces, int[] expectedMember)
        {
            bool memberPresent = false;
            foreach (List<int> split in PetriNetUtils.SplitNumberIntoPieces(number, pieces))
            {
                if (split.ToArray().SequenceEqual(expectedMember))
                {
                    memberPresent = true;
                    break;
                }
            }

            Assert.True(memberPresent);
        }




        [Theory]
        [InlineData(2, 6)]
        [InlineData(0, 10)]
        [InlineData(10, 0)]
        [InlineData(0, 0)]
        [InlineData(5, 11)]
        [InlineData(90, 2)]
        public void FireSetTransferTransitionForwardTheory(int inputPlaceValue, int outputPlaceValue)
        {
            Marking m = new Marking(new Dictionary<Place, int>
            {
                [p1] = inputPlaceValue,
                [p2] = outputPlaceValue
            });

            Marking result = m.Fire(transition).First();

            Assert.Equal(0, result.GetValueOrDefault(p1, 0));
            Assert.Equal(inputPlaceValue + outputPlaceValue, result.GetValueOrDefault(p2, 0));
        }

        [Theory]
        [InlineData(2, 6)]
        [InlineData(0, 10)]
        [InlineData(10, 0)]
        [InlineData(0, 0)]
        [InlineData(5, 11)]
        [InlineData(90, 2)]
        public void FireSetTransferTransitionBackwardsTheory(int inputPlaceValue, int outputPlaceValue)
        {
            Marking m = new Marking(new Dictionary<Place, int>
            {
                [p1] = inputPlaceValue,
                [p2] = outputPlaceValue
            });

            Marking result = m.FireBackwards(transition).First();

            Assert.Equal(0, result.GetValueOrDefault(p2, 0));
            Assert.Equal(inputPlaceValue + outputPlaceValue, result.GetValueOrDefault(p1));
        }

        public class TestTransferSuccessorData : IEnumerable<object[]>
        {
            public IEnumerator<object[]> GetEnumerator()
            {
                yield return new object[] { 0, 0, 1 };
                yield return new object[] { 1, 1, 1 };
                yield return new object[] { 4, 5, 3 };
                yield return new object[] { 1, 0, 2 };
            }

            IEnumerator IEnumerable.GetEnumerator() => this.GetEnumerator();
        }

        [Theory]
        [ClassData(typeof(TestTransferSuccessorData))]
        public void TransferSuccessorTest(int place1Tokens, int place2Tokens, int place3Tokens)
        {
            SetTransferTransition transfer = new SetTransferTransition("t1", new HashSet<Place> { p1 }, new HashSet<Place> { p2 }, new Dictionary<Place, int> { [p3] = 1 }, new Dictionary<Place, int> { [p1] = 1 });
            Marking marking = new Marking { [p1] = place1Tokens, [p2] = place2Tokens, [p3] = place3Tokens };
            HashSet<Marking> results = marking.Fire(transfer);

            foreach (Marking resultMarking in results)
            {
                Assert.Equal(1, resultMarking[p1]);
                Assert.Equal(place1Tokens + place2Tokens, resultMarking.GetValueOrDefault(p2));
                Assert.Equal(place3Tokens - 1, resultMarking.GetValueOrDefault(p3));
            }
        }


        [Fact]
        public void InheritanceTest()
        {
            Assert.IsAssignableFrom<Transition>(transition);
        }

        [Fact]
        public void BioTest()
        {
            Assert.False(transition.IsBio());
        }

        [Fact]
        public void IsEnabledTest()
        {
            Assert.True(m1.Enables(transition));
        }

        [Fact]
        public void IsBackwardsEnabledTest()
        {
            Assert.True(m1.BackwardsEnables(transition));
        }

        [Fact]
        public void FiringTest()
        {
            BinaryTTSParser parser = new BinaryTTSParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("tts/short_witness.tts"));
            List<MarkingWithConstraints> targets = parser.ReadFormula(Utils.GetPathForTestfile("tts/short_witness.prop"));

            Transition transition = net.GetTransitionByName("transfer_0");


            // marking for state 0, 1 token in counter 0
            Marking marking = parser.GetMarking(0, 0);

            Assert.True(transition.IsEnabledIn(marking), "Transition is not enabled in marking!");

            HashSet<Marking> successors = transition.FireOn(marking);

            Assert.True(successors.Count > 0, "The successor set is empty");

            // marking for state 0, 1 token in counter 2
            Marking expectedMarking = parser.GetMarking(0, 2);

            Marking successor = successors.First();

            Assert.Equal(expectedMarking.GetHashCode(), successor.GetHashCode());
            Assert.True(expectedMarking.Equals(successor), "Objects are not equal: " + expectedMarking.ToString() + ", \n" + successor.ToString());
        }
    }
}