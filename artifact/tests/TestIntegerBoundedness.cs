using Xunit;
using System;
using System.Collections.Generic;
using Petri;
using System.Linq;
using Xunit.Abstractions;

namespace Testing
{
    public class TestIntegerBoundedness
    {

        private readonly ITestOutputHelper output;

        public TestIntegerBoundedness(ITestOutputHelper output)
        {
            this.output = output;
        }

        [Theory]
        [InlineData("workflow_tests/A.lola", true)]
        [InlineData("workflow_tests/B.lola", false)]
        [InlineData("workflow_tests/C.lola", true)]
        [InlineData("workflow_tests/D.lola", true)]
        [InlineData("workflow_tests/E.lola", true)]
        [InlineData("workflow_tests/A.s00000104__s00002819.lola", true)]
        [InlineData("workflow_tests/B3.s00000169__s00002052.lola", true)]
        [InlineData("workflow_tests/B1.s00000125__s00001157.lola", true)]
        [InlineData("integer-boundedness/1.lola", false)]
        [InlineData("integer-boundedness/2.lola", true)]
        [InlineData("integer-boundedness/3.lola", false)]
        public void TestBoundedness(string filepath, bool expected)
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile(filepath));
            PetriNet net = input.Item1;

            var result = GurobiHeuristics.CheckIntegerUnboundedness(net).counterexample == null;

            Assert.True(expected == result, filepath + ", " + expected.ToString());
        }

        [Theory]
        [InlineData("integer-boundedness/bounded_not-wf-bounded.lola", true, false)]
        public void TestWFBoundedness(string filepath, bool boundedNet, bool boundedWFNet)
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile(filepath));
            PetriNet net = input.Item1;

            var result = GurobiHeuristics.CheckIntegerUnboundedness(net).counterexample == null;

            Assert.True(boundedNet == result, filepath + ", " + boundedNet.ToString());

            var (isWF, sources, sinks) = net.IsWorkflowNet();

            Assert.True(isWF);

            var resultWF = GurobiHeuristics.CheckIntegerUnboundedness(net.ShortCircuit(sources.First(), sinks.First())).counterexample == null;
            Assert.True(boundedWFNet == resultWF, filepath + ", " + boundedWFNet.ToString());

        }
    }
}
