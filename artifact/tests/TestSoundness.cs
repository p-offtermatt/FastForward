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
        // [InlineData("soundness/1-unsound_2-sound.lola", 1, 1, false)] // cannot be handled with current semi-procedure
        [InlineData("soundness/1-unsound_2-sound.lola", 2, 2, true)]
        [InlineData("soundness/unbounded.lola", 1, 1, false)]
        [InlineData("soundness/unbounded.lola", 2, 2, false)]
        [InlineData("soundness/classically-sound_2-unsound.lola", 1, 1, true)]
        [InlineData("soundness/classically-sound_2-unsound.lola", 2, 2, false)]
        public void TestIsSound(string filepath, int startIndex, int stopIndex, bool expected)
        {
            NetParser parser = ParserPicker.ChooseNetParser(filepath);
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile(filepath));
            PetriNet net = input.Item1;

            (bool isWF, IEnumerable<Place> source, IEnumerable<Place> sink) = net.IsWorkflowNet();

            Assert.True(isWF);

            (bool isSound, int soundNumber) = SoundnessChecker.CheckAnyInRangeSound(net, source.First(), sink.First(), startIndex, stopIndex);
            Assert.Equal(expected, isSound);
        }

        [Theory]
        [InlineData("transition-expression/1.lola", 1, false, "t1")]
        [InlineData("transition-expression/2.lola", 1, false, "t1")]
        [InlineData("soundness/trivial_sound.lola", 1, true, null)]
        [InlineData("soundness/trivial_sound2.lola", 1, true, null)]
        [InlineData("soundness/1-sound.lola", 1, true, null)]
        [InlineData("soundness/classically-sound_2-unsound.lola", 1, true, null)]
        public void TestSoundnessViaTransition(string filepath, int index, bool expected, string faultyTransition)
        {
            NetParser parser = ParserPicker.ChooseNetParser(filepath);
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile(filepath));
            PetriNet net = input.Item1;

            (bool isWF, IEnumerable<Place> source, IEnumerable<Place> sink) = net.IsWorkflowNet();

            Assert.True(isWF);

            Place initial = source.First();
            Marking initialMarking = new Marking();
            initialMarking[initial] = index;

            Place final = sink.First();

            UtilityEntrypoints.RemoveUncoverableTransitions(net, initialMarking);
            net = net.ShortCircuit(initial, final);


            (bool transitionsExpressible, Transition counterexample) = GurobiHeuristics.CheckTransitionExpressibility(net);

            Assert.Equal(expected, transitionsExpressible);

            if (!expected)
            {
                Assert.Equal(faultyTransition, counterexample.Name);
            }
        }
    }
}