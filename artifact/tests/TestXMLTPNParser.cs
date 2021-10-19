using Xunit;
using System;
using Petri;
using Xunit.Abstractions;
using System.Collections.Generic;
using System.Linq;
using System.Collections;

namespace Testing
{
    public class TestXMLTPNParser
    {

        private readonly ITestOutputHelper output;

        XMLTPNParser parser = new XMLTPNParser();

        public TestXMLTPNParser(ITestOutputHelper output)
        {
            this.output = output;
        }

        [Fact]
        public void SimpleTest()
        {
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("xmltpn/small_test.xml.tpn"));

            Assert.Equal(2, net.Transitions.Count);
            Assert.Equal(3, net.Places.Count);


            HashSet<Place> expectedPlaces = new HashSet<Place>{
                new Place("i"),
                new Place("o"),
                new Place("m"),
            };

            HashSet<Transition> expectedTransitions = new HashSet<Transition>{
                new UpdateTransition("\"t1\"",
                new Dictionary<Place, int>{
                    [new Place("i")] = 1,
                },
                new Dictionary<Place, int>{
                    [new Place("o")] = 1,
                    [new Place("m")] = 1,
                }),
                new UpdateTransition("\"t2\"",
                new Dictionary<Place, int>{
                    [new Place("m")] = 1,
                    [new Place("i")] = 1,
                },
                new Dictionary<Place, int>{
                    [new Place("o")] = 1,
                    [new Place("m")] = 1,
                }),
            };

            Assert.Equal(expectedPlaces, net.Places.ToHashSet());
            Assert.Equal(expectedTransitions, net.Transitions.ToHashSet());
        }
    }
}