using Xunit;
using System;
using System.Collections.Generic;
using Petri;
using System.Linq;
using Xunit.Abstractions;

namespace Testing
{
    public class TestParallelMinTime
    {

        private readonly ITestOutputHelper output;

        public TestParallelMinTime(ITestOutputHelper output)
        {
            this.output = output;
        }

        [Theory]
        [InlineData("min-time/1.lola", 1, 4, false)]
        [InlineData("min-time/1.lola", 1, 3, false)]
        [InlineData("min-time/1.lola", 1, 2, false)]
        [InlineData("min-time/1.lola", 4, 4, true)]
        [InlineData("min-time/1.lola", 4, 5, true)]
        [InlineData("min-time/1.lola", 4, 100, true)]
        [InlineData("min-time/1.lola", 4, 3, true)]
        [InlineData("min-time/1.lola", 4, 2, false)]
        [InlineData("min-time/2.lola", 1, 0, false)]
        [InlineData("min-time/2.lola", 1, 1, false)]
        [InlineData("min-time/2.lola", 1, 2, true)]
        [InlineData("min-time/2.lola", 2, 0, false)]
        [InlineData("min-time/2.lola", 2, 1, false)]
        [InlineData("min-time/2.lola", 2, 2, false)]
        [InlineData("min-time/2.lola", 2, 3, true)]
        [InlineData("min-time/2.lola", 3, 0, false)]
        [InlineData("min-time/2.lola", 3, 1, false)]
        [InlineData("min-time/2.lola", 3, 2, false)]
        [InlineData("min-time/2.lola", 3, 3, false)]
        [InlineData("min-time/2.lola", 3, 4, true)]
        [InlineData("min-time/2.lola", 3, 100, true)]

        public void TestBoundingMinTime(string filepath, int initialBudget, int bound, bool expected)
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile(filepath));
            PetriNet net = input.Item1;

            var (isWF, sources, sinks) = net.IsWorkflowNet();
            Place initialPlace = sources.First();
            Place finalPlace = sinks.First();

            Assert.True(isWF);

            var result = GurobiHeuristics.Unroll_MinTimeAtMostBound(net, initialPlace, finalPlace, initialBudget, bound);

            Assert.Equal(expected, result);
        }

        [Theory]
        [InlineData("min-time/1.lola", 0, 0, 0)]
        [InlineData("min-time/1.lola", 10, 0, -1)]
        [InlineData("min-time/1.lola", 3, 0, -1)]
        [InlineData("min-time/1.lola", 3, 50, -1)]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "min-time/1.lola", 2, 2 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "min-time/1.lola", 4, 3 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "min-time/1.lola", 6, 4 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "min-time/1.lola", 8, 5 })]
        [InlineData("min-time/2.lola", 0, 0, 0)]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "min-time/2.lola", 1, 2 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "min-time/2.lola", 2, 3 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "min-time/2.lola", 3, 4 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "min-time/2.lola", 4, 5 })]
        public void TestComputingMinTime(string filepath, int initialBudget, int bound, int expectedMinTime)
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile(filepath));
            PetriNet net = input.Item1;

            var (isWF, sources, sinks) = net.IsWorkflowNet();
            Place initialPlace = sources.First();
            Place finalPlace = sinks.First();

            Assert.True(isWF);

            var result = GurobiHeuristics.Unroll_ComputeMinTimeWithBound(net, initialPlace, finalPlace, initialBudget, bound);

            Assert.Equal(expectedMinTime, result);
        }

        public static IEnumerable<object[]> GetParamsForKnownBounds(string filepath, int initialBudget, int expectedMinTime)
        {
            List<object[]> result = new List<object[]>();
            result.Add(new object[] { filepath, initialBudget, 0, 0 < expectedMinTime ? -1 : expectedMinTime });

            for (int i = Math.Max(1, expectedMinTime - 5); i <= expectedMinTime + 5; i++)
            {
                result.Add(new object[] { filepath, initialBudget, i, i < expectedMinTime ? -1 : expectedMinTime });
            }
            result.Add(new object[] { filepath, initialBudget, expectedMinTime + 50, expectedMinTime });
            return result;
        }
    }
}
