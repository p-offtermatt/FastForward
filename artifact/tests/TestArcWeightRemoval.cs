using Xunit;
using System;
using System.Collections.Generic;
using Petri;
using System.Linq;
using Xunit.Abstractions;
using PetriTool;
using System.Text.RegularExpressions;
using System.IO;

namespace Testing
{


    public class TestArcWeightRemoval
    {

        private readonly ITestOutputHelper output;

        public TestArcWeightRemoval(ITestOutputHelper output)
        {
            this.output = output;
        }

        [Fact]
        public void TestRemoveConsume()
        {
            NetParser parser = new LolaParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("arc-weight-removal/1.lola"));
            PetriNet originalNet = new PetriNet(net);
            PetriNet newNet = net.ReplaceArcWeights();

            output.WriteLine(net.ToString());
            Assert.Equal(originalNet, net);

            Assert.True(newNet.Places.Count > net.Places.Count);

            using (StreamWriter file = new StreamWriter(Utils.GetPathForTestfile("arc-weight-removal/_tmp.lola"), append: false))
            {
                file.Write(newNet.ToLola(initialMarking));
            }
        }

        [Fact]
        public void TestRemoveProduce()
        {
            NetParser parser = new LolaParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("arc-weight-removal/2.lola"));
            PetriNet originalNet = new PetriNet(net);
            PetriNet newNet = net.ReplaceArcWeights();

            output.WriteLine(net.ToString());
            Assert.Equal(originalNet, net);

            Assert.True(newNet.Places.Count > net.Places.Count);

            using (StreamWriter file = new StreamWriter(Utils.GetPathForTestfile("arc-weight-removal/_tmp.lola"), append: false))
            {
                file.Write(newNet.ToLola(initialMarking));
            }
        }

        [Fact]
        public void TestProduceConsume()
        {
            NetParser parser = new LolaParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("arc-weight-removal/3.lola"));
            PetriNet originalNet = new PetriNet(net);
            PetriNet newNet = net.ReplaceArcWeights();

            output.WriteLine(net.ToString());
            Assert.Equal(originalNet, net);

            Assert.True(newNet.Places.Count > net.Places.Count);

            using (StreamWriter file = new StreamWriter(Utils.GetPathForTestfile("arc-weight-removal/_tmp.lola"), append: false))
            {
                file.Write(newNet.ToLola(initialMarking));
            }
        }

        [Fact]
        public void TestMulticonsume()
        {
            NetParser parser = new LolaParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("arc-weight-removal/4.lola"));
            PetriNet originalNet = new PetriNet(net);
            PetriNet newNet = net.ReplaceArcWeights();

            output.WriteLine(net.ToString());
            Assert.Equal(originalNet, net);

            Assert.True(newNet.Places.Count > net.Places.Count);

            using (StreamWriter file = new StreamWriter(Utils.GetPathForTestfile("arc-weight-removal/_tmp.lola"), append: false))
            {
                file.Write(newNet.ToLola(initialMarking));
            }
        }

        [Fact]
        public void TestLargeExample()
        {
            NetParser parser = new LolaParser();
            (PetriNet net, Marking initialMarking) = parser.ReadNet(Utils.GetPathForTestfile("arc-weight-removal/6.lola"));
            PetriNet originalNet = new PetriNet(net);
            PetriNet newNet = net.ReplaceArcWeights();

            output.WriteLine(net.ToString());
            Assert.Equal(originalNet, net);

            Assert.True(newNet.Places.Count > net.Places.Count);

            using (StreamWriter file = new StreamWriter(Utils.GetPathForTestfile("arc-weight-removal/_tmp.lola"), append: false))
            {
                file.Write(newNet.ToLola(initialMarking));
            }
        }
    }
}