using Xunit;
using System;
using Petri;
using Xunit.Abstractions;
using System.Collections.Generic;
using System.Linq;
using System.Collections;

namespace Testing
{
    public class TestMultitransfers
    {

        private readonly ITestOutputHelper output;

        static Place p1 = new Place("p1");
        static Place p2 = new Place("p2");
        static Place p3 = new Place("p3");
        static Place p4 = new Place("p4");

        UpdateTransition update1;


        MultitransferTransition multitransfer;

        public TestMultitransfers(ITestOutputHelper output)
        {
            this.output = output;
            this.update1 = new UpdateTransition(Name: "up1",
            Pre: new Dictionary<Place, int>
            {
                [p1] = 1
            },
            Post: new Dictionary<Place, int>
            {
                [p3] = 1
            });

            this.multitransfer = new MultitransferTransition(name: "t1",
            transfers: new Dictionary<Place, HashSet<Place>>
            {
                [p1] = new HashSet<Place> { p1, p2 }
            },
            updatePre: update1.Pre,
            updatePost: update1.Post
        );
        }

        [Fact]
        public void CopyTest()
        {
            MultitransferTransition copy = (MultitransferTransition)multitransfer.CopyThis();
            copy.Transfers[p2] = new HashSet<Place> { p1 };
            copy.Transfers[p1].Remove(p1);
            copy.UpdateBehaviour.Post.Remove(p3);

            Assert.Equal(new Dictionary<Place, HashSet<Place>>
            {
                [p1] = new HashSet<Place> { p1, p2 }
            },
            multitransfer.Transfers
            );

            Assert.Equal(new Dictionary<Place, HashSet<Place>>
            {
                [p1] = new HashSet<Place> { p2 },
                [p2] = new HashSet<Place> { p1 }
            },
            copy.Transfers
            );

            Assert.Equal(new Dictionary<Place, int> { [p3] = 1 }, multitransfer.UpdateBehaviour.Post);
            Assert.Equal(new Dictionary<Place, int>(), copy.UpdateBehaviour.Post);
        }

        [Fact]
        public void PrePostTest()
        {
            Assert.Equal(new HashSet<Place> { p1, p2, p3 }, multitransfer.GetSupportPlaces());
            Assert.Equal(new HashSet<Place> { p1, p2, p3 }, multitransfer.GetPostPlaces());
            Assert.Equal(new HashSet<Place> { p1 }, multitransfer.GetPrePlaces());
        }

        [Fact]
        public void RemovePreTest()
        {
            MultitransferTransition copy = (MultitransferTransition)multitransfer.CopyThis();
            Assert.False(multitransfer.RemovePlaceFromPre(p2));
            Assert.Equal(copy, multitransfer);

            Assert.True(multitransfer.RemovePlaceFromPre(p1));
            copy.Transfers = new Dictionary<Place, HashSet<Place>>();
            copy.UpdateBehaviour.Pre = new Dictionary<Place, int>();
            Assert.Equal(copy, multitransfer);
        }

        [Fact]
        public void RemovePostTest()
        {
            MultitransferTransition copy = (MultitransferTransition)multitransfer.CopyThis();
            Assert.False(multitransfer.RemovePlaceFromPost(p4));
            Assert.Equal(copy, multitransfer);

            Assert.True(multitransfer.RemovePlaceFromPost(p1));
            copy.Transfers[p1] = new HashSet<Place> { p2 };
            Assert.Equal(copy, multitransfer);

            Assert.True(multitransfer.RemovePlaceFromPost(p3));
            copy.UpdateBehaviour.Post = new Dictionary<Place, int>();
            Assert.Equal(copy, multitransfer);
        }

        [Fact]
        public void EqualityTest()
        {
            MultitransferTransition copy = (MultitransferTransition)multitransfer.CopyThis();
            Assert.Equal(copy, multitransfer);
            Assert.Equal(multitransfer, copy);
            Assert.Equal(multitransfer, multitransfer);
            Assert.Equal(copy, copy);

            copy.Transfers.Remove(p1);

            Assert.NotEqual(copy, multitransfer);
        }

        class FireMultiTransferTestData : IEnumerable<object[]>
        {
            public IEnumerator<object[]> GetEnumerator()
            {
                yield return new object[] { 1, 2, 3, 4 };
                yield return new object[] { 0, 0, 0, 0 };

                for (int i = 0; i < 4; i++)
                {
                    object[] result = new object[4];
                    for (int j = 0; j < 4; j++)
                    {
                        result[j] = i == j ? 1 : 0;
                    }
                    yield return result;

                    for (int j = 0; j < 4; j++)
                    {
                        result[j] = i == j ? 0 : 1;
                    }
                    yield return result;
                }


            }

            IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
        }

        [Theory]
        [ClassData(typeof(FireMultiTransferTestData))]
        public void TestFiring(int p1Tokens, int p2Tokens, int p3Tokens, int p4Tokens)
        {
            Marking initialMarking = new Marking
            {
                [p1] = p1Tokens,
                [p2] = p2Tokens,
                [p3] = p3Tokens,
                [p4] = p4Tokens,
            };

            HashSet<Marking> resultMarkings = multitransfer.FireOn(initialMarking);

            if (p1Tokens < 1)
            {
                Assert.Empty(resultMarkings);
                return;
            }

            foreach (Marking marking in resultMarkings)
            {
                output.WriteLine(marking.ToString());
                Assert.Equal(initialMarking.GetValueOrDefault(p4, 0), marking.GetValueOrDefault(p4, 0));

                if (p4Tokens == 0)
                {
                    Assert.Subset(new HashSet<Place> { p1, p2, p3 }, marking.GetMarkedPlaces());
                }

                Assert.Equal(p1Tokens + p2Tokens - 1, marking.GetValueOrDefault(p1, 0) + marking.GetValueOrDefault(p2, 0));

                Assert.Equal(p3Tokens + 1, marking.GetValueOrDefault(p3, 0));

                Assert.Equal(initialMarking.Values.Sum(), marking.Values.Sum());
            }

            Assert.Contains(resultMarkings, marking => marking.GetValueOrDefault(p2, 0) == p2Tokens);
            Assert.Contains(resultMarkings, marking => marking.GetValueOrDefault(p2, 0) == p1Tokens + p2Tokens - 1);

            Assert.Contains(resultMarkings, marking => marking.GetValueOrDefault(p1, 0) == p1Tokens - 1);
            Assert.Contains(resultMarkings, marking => marking.GetValueOrDefault(p1, 0) == 0);
        }

        class FireComplexMultiTransferTestData : IEnumerable<object[]>
        {
            public IEnumerator<object[]> GetEnumerator()
            {
                List<List<Place>> transferInputs = new List<List<Place>>{
                    new List<Place>{p1},
                    new List<Place>{p1, p2},
                    new List<Place>{p1, p3},
                    new List<Place>{p3, p1},
                    new List<Place>{p1, p2, p3, p4},
                    new List<Place>{p4, p3, p2, p1}
                };

                List<HashSet<Place>> transferOutputs = new List<HashSet<Place>>
                {
                    new HashSet<Place>{p1, p2},
                    new HashSet<Place>{p3},
                    new HashSet<Place>{p1, p2, p3, p4},
                    new HashSet<Place>{p2, p4}
                };

                List<Dictionary<Place, int>> Pres = new List<Dictionary<Place, int>>
                {
                    new Dictionary<Place, int>{
                        [p1] = 2, [p3] = 4
                    },
                    new Dictionary<Place, int>{
                        [p1] = 1, [p2] = 2
                    },
                    new Dictionary<Place, int>{
                        [p4] = 1
                    },
                    new Dictionary<Place, int>{
                        [p4] = 1, [p3] = 2, [p2] = 3, [p1] = 4
                    },
                    new Dictionary<Place, int>{
                    }
                };

                List<Dictionary<Place, int>> Posts = new List<Dictionary<Place, int>>
                {
                    new Dictionary<Place, int>{
                        [p1] = 2, [p3] = 4
                    },
                    new Dictionary<Place, int>{
                        [p1] = 1, [p2] = 2
                    },
                    new Dictionary<Place, int>{
                        [p4] = 1
                    },
                    new Dictionary<Place, int>{
                        [p4] = 4, [p3] = 3, [p2] = 2, [p1] = 1
                    },
                    new Dictionary<Place, int>{
                    },
                    new Dictionary<Place, int>{
                        [p4] = 1, [p3] = 2, [p2] = 3, [p1] = 4
                    },
                };

                List<int[]> tokenNumbers = new List<int[]>
                {
                    new int[]{4, 2, 0, 3},
                    new int[]{0, 1, 0, 5},
                    new int[]{7, 0, 0, 3},
                    new int[]{7, 2, 2, 3},
                    new int[]{4, 3, 2, 1},
                    new int[]{1, 2, 3, 4},
                    new int[]{0, 0, 0, 1},
                    new int[]{0, 0, 1, 0},
                    new int[]{0, 1, 0, 0},
                    new int[]{1, 0, 0, 0},
                };

                List<Marking> markings = tokenNumbers.Select(
                    array => new Marking
                    {
                        [p1] = array[0],
                        [p2] = array[1],
                        [p3] = array[2],
                        [p4] = array[3]
                    }
                ).ToList();

                foreach (List<Place> transferInput in transferInputs)
                {
                    for (int i = 0; i < transferOutputs.Count; i++)
                    {
                        Dictionary<Place, HashSet<Place>> transferInputsToTransferOutputs = new Dictionary<Place, HashSet<Place>>();
                        for (int j = 0; j < transferInput.Count; j++)
                        {
                            Place transferInputPlace = transferInput[j];
                            HashSet<Place> transferOutputPlaces = transferOutputs[(i + j) % transferOutputs.Count];
                            transferInputsToTransferOutputs[transferInputPlace] = transferOutputPlaces;
                        }
                        foreach (Dictionary<Place, int> pre in Pres)
                        {
                            foreach (Dictionary<Place, int> post in Posts)
                            {
                                foreach (Marking marking in markings)
                                {
                                    yield return new object[] {
                                        marking,
                                        new MultitransferTransition(
                                                "transition",
                                                transferInputsToTransferOutputs,
                                                pre,
                                                post
                                            )
                                        };
                                }
                            }
                        }
                    }
                }
            }

            IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
        }

        [Theory]
        [ClassData(typeof(FireComplexMultiTransferTestData))]
        public void ComplexMultitransferTest(Marking inputMarking, MultitransferTransition transition)
        {
            HashSet<Marking> resultMarkings = transition.FireOn(inputMarking);

            Assert.Equal(transition.UpdateBehaviour.Pre.All(kvPair => inputMarking.GetValueOrDefault(kvPair.Key) >= kvPair.Value), transition.IsEnabledIn(inputMarking));
            Assert.Equal(transition.IsEnabledIn(inputMarking), resultMarkings.Count != 0);

            int prePostDifference = transition.UpdateBehaviour.GetPrePostDifference().Sum(kvPair => kvPair.Value);

            if (!transition.IsEnabledIn(inputMarking))
            {
                return;
            }

            int initialTokens = inputMarking.Sum(kvPair => kvPair.Value);

            foreach (Marking curMarking in resultMarkings)
            {
                output.WriteLine(transition.ToString());
                output.WriteLine(inputMarking.ToString());
                output.WriteLine(curMarking.ToString());


                int curTokens = curMarking.Sum(kvPair => kvPair.Value);

                Assert.Equal(initialTokens + prePostDifference, curTokens);

                foreach (Place place in new HashSet<Place> { p1, p2, p3, p4 })
                {
                    int initialMarking = inputMarking.GetValueOrDefault(place);
                    int finalMarking = curMarking.GetValueOrDefault(place);
                    int prePostChange = transition.UpdateBehaviour.GetPrePostDifference().GetValueOrDefault(place, 0);
                    if (!transition.Transfers.ContainsKey(place))
                    {
                        if (!transition.Transfers.Any(kvPair => kvPair.Value.Contains(place)))
                        {
                            Assert.True(initialMarking + prePostChange == finalMarking);
                        }
                        else
                        {
                            Assert.True(initialMarking + prePostChange <= finalMarking);
                        }
                    }
                    else
                    {
                        if (!transition.Transfers.Any(kvPair => kvPair.Value.Contains(place)))
                        {
                            Assert.True(initialMarking + prePostChange >= finalMarking);
                        }
                    }

                }
            }
        }
    }
}
