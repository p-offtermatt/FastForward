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


    public class TestIntegerDeadlock
    {

        private readonly ITestOutputHelper output;

        public TestIntegerDeadlock(ITestOutputHelper output)
        {
            this.output = output;
        }

        [Theory]
        [InlineData("integer-deadlocks/no1.lola", false)]
        [InlineData("integer-deadlocks/no2.lola", false)]
        [InlineData("integer-deadlocks/yes1.lola", true)]
        [InlineData("integer-deadlocks/yes2.lola", true)]
        [InlineData("integer-deadlocks/yes3.lola", true)]
        public void TestIsIntegerDeadlock(string filepath, bool hasIntegerDeadlock)
        {
            NetParser parser = ParserPicker.ChooseNetParser(filepath);
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile(filepath));
            PetriNet net = input.Item1;

            (bool isWF, IEnumerable<Place> source, IEnumerable<Place> sink) = net.IsWorkflowNet();

            Assert.True(isWF);

            var result = GurobiHeuristics.CheckIntegerDeadlock(net, source.First(), sink.First(), true);

            if (hasIntegerDeadlock)
            {
                Assert.NotNull(result);
            }
            else
            {
                Assert.Null(result);
            }
        }
    }
}