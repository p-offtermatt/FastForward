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

#if GUROBI
    public class TestMarkingEquationUnsoundness
    {

        private readonly ITestOutputHelper output;

        public TestMarkingEquationUnsoundness(ITestOutputHelper output)
        {
            this.output = output;
        }

        [Theory]
        [InlineData("soundness/1-unsound_2-sound.lola", true)]
        [InlineData("soundness/trivial_sound.lola", true)]
        [InlineData("soundness/trivial_unsound.lola", false)]
        [InlineData("soundness/trivial_sound2.lola", true)]
        public void TestSoundness(string netfilepath, bool sound)
        {
            NetParser parser = ParserPicker.ChooseNetParser(netfilepath);

            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile(netfilepath));

            (bool isWF, IEnumerable<Place> source, IEnumerable<Place> sink) = net.IsWorkflowNet();

            Assert.True(isWF);

            Place initial = source.First();
            Place final = sink.First();

            Marking counterexample = GurobiHeuristics.CheckMarkingEquationUnsoundness(net, initial, final);

            output.WriteLine(counterexample == null ? "null" : counterexample.ToString());
            Assert.Equal(sound, counterexample == null);
        }
    }
#endif
}