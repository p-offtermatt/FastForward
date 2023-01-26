using Xunit;
using System;
using System.Collections.Generic;
using Petri;
using System.Linq;
using Xunit.Abstractions;

namespace Testing
{
    public class TestSoundnessWithBound
    {

        private readonly ITestOutputHelper output;

        public TestSoundnessWithBound(ITestOutputHelper output)
        {
            this.output = output;
        }

        [Theory]
        [InlineData("min-time/1.lola", 1, 20, false)]
        [InlineData("min-time/1.lola", 2, 20, false)]
        [InlineData("min-time/1.lola", 3, 20, false)]
        [InlineData("min-time/1.lola", 4, 20, false)]
        [InlineData("min-time/1.lola", 5, 20, false)]
        [InlineData("min-time/1.lola", 6, 20, false)]
        [InlineData("soundness/1-sound.lola", 1, 30, true)]
        [InlineData("soundness/1-unsound_2-sound.lola", 2, 30, true)]
        [InlineData("soundness/classically-sound_2-unsound.lola", 1, 30, true)]
        [InlineData("soundness/classically-sound_2-unsound.lola", 2, 30, false)]
        [InlineData("workflow_tests/unsound_2quasi-sound.lola", 1, 30, false)]
        [InlineData("workflow_tests/unsound_2quasi-sound.lola", 2, 30, false)]
        [InlineData("workflow_tests/unsound_2quasi-sound.lola", 3, 30, false)]
        [InlineData("workflow_tests/unsound_2quasi-sound.lola", 4, 30, false)]
        [InlineData("workflow_tests/unsound_2quasi-sound.lola", 5, 30, false)]
        [InlineData("workflow_tests/sound_line.lola", 1, 30, true)]
        [InlineData("workflow_tests/sound_line.lola", 2, 30, true)]
        [InlineData("workflow_tests/sound_line.lola", 3, 30, true)]
        [InlineData("workflow_tests/sound_line.lola", 4, 30, true)]
        [InlineData("workflow_tests/sound_line.lola", 5, 30, true)]
        [InlineData("workflow_tests/sound_line.lola", 6, 30, true)]
        [InlineData("workflow_tests/sound_line.lola", 7, 30, true)]
        [InlineData("workflow_tests/2sound_line.lola", 1, 30, false)]
        [InlineData("workflow_tests/2sound_line.lola", 2, 30, true)]
        [InlineData("workflow_tests/2sound_line.lola", 3, 30, false)]
        [InlineData("workflow_tests/2sound_line.lola", 4, 30, true)]
        [InlineData("workflow_tests/2sound_line.lola", 5, 30, false)]
        [InlineData("workflow_tests/2sound_line.lola", 6, 30, true)]
        [InlineData("workflow_tests/2sound_line.lola", 7, 30, false)]

        public void TestSoundnessGurobi(string filepath, int initialBudget, int bound, bool expected)
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile(filepath));
            PetriNet net = input.Item1;

            var (isWF, sources, sinks) = net.IsWorkflowNet();
            Place initialPlace = sources.First();
            Place finalPlace = sinks.First();

            Assert.True(isWF);

            var result = GurobiHeuristics.Unroll_CheckSoundness(net, initialPlace, finalPlace, initialBudget, bound);

            Assert.Equal(expected, result);
        }
    }
}
