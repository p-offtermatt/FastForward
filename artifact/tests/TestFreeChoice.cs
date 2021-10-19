using Xunit;
using System;
using System.Collections.Generic;
using Petri;
using System.Linq;
using Xunit.Abstractions;

namespace Testing
{


    public class FreeChoiceTests
    {

        private readonly ITestOutputHelper output;

        public FreeChoiceTests(ITestOutputHelper output)
        {
            this.output = output;
        }

        [Theory]
        [InlineData("free_choice/fc1.lola", true)]
        [InlineData("free_choice/nfc1.lola", false)]
        [InlineData("free_choice/fc2.lola", true)]
        [InlineData("free_choice/nfc2.lola", false)]
        public void TestIsFreeChoice(string filepath, bool expected)
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile(filepath));
            PetriNet net = input.Item1;

            Assert.True(expected == net.IsFreeChoice(), filepath + ", " + expected.ToString());
        }
    }
}
