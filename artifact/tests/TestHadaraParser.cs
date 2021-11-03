using Xunit;
using System;
using Petri;
using Xunit.Abstractions;
using System.Collections.Generic;
using System.Linq;
using System.Collections;

namespace Testing
{
    public class TestHadaraParser
    {

        private readonly ITestOutputHelper output;

        TTSParser parser = new BinaryTTSParser();

        public TestHadaraParser(ITestOutputHelper output)
        {
            this.output = output;

        }

        [Theory]
        [InlineData("hadara/A.xml", 6, 5, 14)]
        [InlineData("hadara/B.xml", 4, 3, 8)]
        [InlineData("hadara/C.xml", 4, 3, 8)]
        [InlineData("hadara/D.xml", 8, 7, 26)]
        [InlineData("hadara/E.xml", 8, 6, 19)]
        [InlineData("hadara/F.xml", 7, 6, 18)]
        [InlineData("hadara/DataSet1-1.xml", 206, 165, 554)]
        public void TestNetStats(string filepath, int numPlaces, int numTransitions, int sumArcWeights)
        {
            HadaraParser parser = new HadaraParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile(filepath));

            Assert.Equal(net.Places.Count, numPlaces);
            Assert.Equal(net.Transitions.Count, numTransitions);

            int actualSumArcWeights = net.Transitions.Sum(transition => ((UpdateTransition)transition).Pre.Sum(pair => pair.Value) + ((UpdateTransition)transition).Post.Sum(pair => pair.Value));

            Assert.Equal(sumArcWeights, actualSumArcWeights);
        }

    }
}