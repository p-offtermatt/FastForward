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


    public class TestMarkingEquationUnreachability
    {

        private readonly ITestOutputHelper output;

        public TestMarkingEquationUnreachability(ITestOutputHelper output)
        {
            this.output = output;
        }

        [Theory]
        [InlineData("unreachability/n1.lola", "unreachability/n1.formula", false)]
        [InlineData("unreachability/n1.lola", "unreachability/y1.formula", true)]

        public void TestUnreachability(string netfilepath, string formulafilepath, bool reachableExpected)
        {
            FullParser parser = ParserPicker.ChooseFullParser(netfilepath, formulafilepath);

            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile(netfilepath));
            Marking target = parser.ReadFormula(Utils.GetPathForTestfile(formulafilepath)).First().Marking;

            Dictionary<Place, double> result = GurobiHeuristics.CheckUnreachability(net.Places, net.Transitions, initialMarking, target);

            output.WriteLine(result == null ? "null" : String.Join("; ", result));

            Assert.Equal(result == null, reachableExpected);
        }
    }
}