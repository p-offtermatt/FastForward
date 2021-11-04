using Xunit;
using System;
using System.Collections.Generic;
using Petri;
using Xunit.Abstractions;
using PetriTool;
using Soundness;
using System.Linq;

namespace Testing
{


    public class TestSoundness
    {

        private readonly ITestOutputHelper output;

        public TestSoundness(ITestOutputHelper output)
        {
            this.output = output;
        }

        [Theory]
        [InlineData("soundness/1-sound.lola", 1, 1, true)]
        [InlineData("soundness/1-unsound_2-sound.lola", 1, 1, false)]
        [InlineData("soundness/1-unsound_2-sound.lola", 2, 2, true)]
        [InlineData("soundness/unbounded.lola", 1, 1, false)]
        [InlineData("soundness/unbounded.lola", 2, 2, false)]
        [InlineData("soundness/classically-sound_2-unsound.lola", 1, 1, true)]
        [InlineData("soundness/classically-sound_2-unsound.lola", 2, 2, false)]
        public void TestIsWorkflowNet(string filepath, int startIndex, int stopIndex, bool expected)
        {
            NetParser parser = ParserPicker.ChooseNetParser(filepath);
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile(filepath));
            PetriNet net = input.Item1;

            (bool isWF, IEnumerable<Place> source, IEnumerable<Place> sink) = net.IsWorkflowNet();

            Assert.True(isWF);

            var result = SoundnessChecker.CheckSoundness(net, source.First(), sink.First(), startIndex, stopIndex);
            Assert.True(expected == result);
        }
    }
}