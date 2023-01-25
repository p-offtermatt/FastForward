using Xunit;
using System;
using System.Collections.Generic;
using Petri;
using System.Linq;
using Xunit.Abstractions;

namespace Testing
{
    public class TestComputingAn
    {

        private readonly ITestOutputHelper output;

        public TestComputingAn(ITestOutputHelper output)
        {
            this.output = output;
        }

        [Theory]
        [InlineData("compute_a_n/1.lola", 3.0)]
        [InlineData("compute_a_n/2.lola", double.PositiveInfinity)]
        [InlineData("compute_a_n/3.lola", 2.0)]
        [InlineData("compute_a_n/4.lola", 1.5)]
        public void TestGurobiAn(string filepath, double expected)
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile(filepath));
            PetriNet net = input.Item1;

            var (isWF, sources, sinks) = net.IsWorkflowNet();
            Place initialPlace = sources.First();
            Place finalPlace = sinks.First();

            Assert.True(isWF);

            var result = GurobiHeuristics.Compute_A_n(net, initialPlace, finalPlace);

            Assert.Equal(expected, result);
        }
    }
}
