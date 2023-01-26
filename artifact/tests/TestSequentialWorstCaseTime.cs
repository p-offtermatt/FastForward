using Xunit;
using System;
using System.Collections.Generic;
using Petri;
using System.Linq;
using Xunit.Abstractions;

namespace Testing
{
    public class TestSequentialWorstCaseTime
    {

        private readonly ITestOutputHelper output;

        public TestSequentialWorstCaseTime(ITestOutputHelper output)
        {
            this.output = output;
        }

        [Theory]
        [InlineData("min-time/1.lola", 0, 0, 0)]
        [InlineData("min-time/1.lola", 0, 50, 0)]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "min-time/1.lola", 2, 3 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "min-time/1.lola", 3, 4 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "min-time/1.lola", 4, 6 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "min-time/1.lola", 5, 7 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "min-time/1.lola", 6, 9 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "min-time/2.lola", 1, 2 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "min-time/2.lola", 2, 4 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "min-time/2.lola", 3, 6 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "min-time/2.lola", 4, 8 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "min-time/2.lola", 5, 10 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "workflow_tests/unsound_2quasi-sound.lola", 1, 0 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "workflow_tests/unsound_2quasi-sound.lola", 2, 3 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "workflow_tests/unsound_2quasi-sound.lola", 3, 3 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "workflow_tests/unsound_2quasi-sound.lola", 4, 6 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "workflow_tests/unsound_2quasi-sound.lola", 5, 6 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "workflow_tests/unsound_2quasi-sound.lola", 6, 9 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "workflow_tests/sound_line.lola", 1, 4 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "workflow_tests/sound_line.lola", 2, 8 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "workflow_tests/sound_line.lola", 3, 12 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "workflow_tests/sound_line.lola", 4, 16 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "workflow_tests/2sound_line.lola", 1, 0 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "workflow_tests/2sound_line.lola", 2, 4 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "workflow_tests/2sound_line.lola", 3, 4 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "workflow_tests/2sound_line.lola", 4, 8 })]
        [MemberData(nameof(GetParamsForKnownBounds), parameters: new object[] { "workflow_tests/2sound_line.lola", 5, 8 })]
        public void TestComputingMaxTime(string filepath, int initialBudget, int bound, int expectedMinTime)
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile(filepath));
            PetriNet net = input.Item1;

            var (isWF, sources, sinks) = net.IsWorkflowNet();
            Place initialPlace = sources.First();
            Place finalPlace = sinks.First();

            Assert.True(isWF);

            var result = GurobiHeuristics.Unroll_ComputeL(net, initialPlace, finalPlace, initialBudget, bound);

            Assert.Equal(expectedMinTime, result);
        }

        public static IEnumerable<object[]> GetParamsForKnownBounds(string filepath, int initialBudget, int expectedMaxTime)
        {
            List<object[]> result = new List<object[]>();
            result.Add(new object[] { filepath, initialBudget, 0, 0 });

            for (int i = Math.Max(1, expectedMaxTime - 5); i <= expectedMaxTime + 5; i++)
            {
                result.Add(new object[] { filepath, initialBudget, i, i < expectedMaxTime ? i : expectedMaxTime });
            }
            result.Add(new object[] { filepath, initialBudget, expectedMaxTime + 50, expectedMaxTime });
            return result;
        }
    }
}
