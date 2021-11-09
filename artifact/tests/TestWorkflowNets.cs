using Xunit;
using System;
using System.Collections.Generic;
using Petri;
using Xunit.Abstractions;
using PetriTool;

namespace Testing
{


    public class WorkflowNetsTest
    {

        private readonly ITestOutputHelper output;

        public WorkflowNetsTest(ITestOutputHelper output)
        {
            this.output = output;
        }

        [Fact]
        public void TestGraphReachability()
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile("workflow_tests/A.lola"));
            PetriNet net = input.Item1;

            Place a = new Place("a");
            Place b = new Place("b");
            Place d = new Place("d");


            Assert.True(WorkflowUtils.IsFullyGraphReachable(net, a, true));
            Assert.False(WorkflowUtils.IsFullyGraphReachable(net, b, true));
            Assert.False(WorkflowUtils.IsFullyGraphReachable(net, b, false));
            Assert.False(WorkflowUtils.IsFullyGraphReachable(net, a, false));
            Assert.True(WorkflowUtils.IsFullyGraphReachable(net, d, false));
        }

        [Theory]
        [InlineData("workflow_tests/A.lola", true)]
        [InlineData("workflow_tests/B.lola", false)]
        [InlineData("workflow_tests/C.lola", false)]
        [InlineData("workflow_tests/D.lola", false)]
        [InlineData("workflow_tests/E.lola", false)]
        [InlineData("workflow_tests/A.s00000104__s00002819.lola", false)]
        [InlineData("workflow_tests/B3.s00000169__s00002052.lola", false)]
        [InlineData("workflow_tests/B1.s00000125__s00001157.lola", false)]
        [InlineData("hadara/A.xml", true)]
        [InlineData("hadara/B.xml", true)]
        [InlineData("hadara/C.xml", true)]
        [InlineData("hadara/D.xml", true)]
        [InlineData("hadara/E.xml", true)]
        [InlineData("hadara/F.xml", true)]
        [InlineData("hadara/G.xml", false)]
        [InlineData("hadara/H.xml", false)]
        [InlineData("hadara/I.xml", false)]
        [InlineData("hadara/J.xml", false)]
        [InlineData("hadara/L.xml", false)]
        [InlineData("hadara/M.xml", false)]
        [InlineData("workflow_tests/Model.lcn5--0-----u--.xml.tpn.xml", false)]
        public void TestIsWorkflowNet(string filepath, bool expected)
        {
            NetParser parser = ParserPicker.ChooseNetParser(filepath);
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile(filepath));
            PetriNet net = input.Item1;

            (bool, IEnumerable<Place>, IEnumerable<Place>) result = net.IsWorkflowNet();
            Assert.True(expected == result.Item1, filepath + ", " + expected.ToString());
        }

        [Theory]
        [InlineData("free_choice/fc1.lola")]
        [InlineData("free_choice/fc2.lola")]
        [InlineData("free_choice/A.s00000021__s00000698.lola")]
        [InlineData("free_choice/A.s00000023__s00000777.lola")]
        public void TestWorkflownetTransformationSuccess(string filepath)
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile(filepath));
            PetriNet net = input.Item1;

            WorkflowUtils.TransformToWorkflowNet(net);

            (bool, IEnumerable<Place>, IEnumerable<Place>) result1 = net.IsWorkflowNet();
            Assert.True(result1.Item1, ("On file: " + filepath + ", the net is not a WF net after transformation. \n Net: " + net.ToString() + "\nSources: " + (result1.Item2 is null ? "null" : String.Join(",", result1.Item2)) + "\nSinks: " + (result1.Item3 is null ? " null" : String.Join(",", result1.Item3))
            ));

            // refresh the net and test transformation with initial marking
            var input2 = parser.ReadNet(Utils.GetPathForTestfile(filepath));
            var net2 = input2.Item1;
            Marking initialMarking = input2.Item2;
            WorkflowUtils.TransformToWorkflowNet(net2, initialMarking);

            (bool, IEnumerable<Place>, IEnumerable<Place>) result2 = net.IsWorkflowNet();
            Assert.True(result2.Item1, "On file: " + filepath + ", the net is not a WF net after transformation. \n Net: " + net.ToString());

            Assert.Equal(net, net2);
        }

        [Theory]
        [InlineData("workflow_tests/C.s00000042__s00001018.lola")]
        public void TestWorkflownetTransformationSuccessButNotWF(string filepath)
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile(filepath));
            PetriNet net = input.Item1;

            WorkflowUtils.TransformToWorkflowNet(net);

            (bool, IEnumerable<Place>, IEnumerable<Place>) result1 = net.IsWorkflowNet();
            Assert.False(result1.Item1);
        }

        [Theory]
        [InlineData("free_choice/nfc1.lola")]
        [InlineData("free_choice/nfc2.lola")]
        public void TestWorkflownetTransformationUnSuccess(string filepath)
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input = parser.ReadNet(Utils.GetPathForTestfile(filepath));
            PetriNet net = input.Item1;

            Assert.Throws<WorkflowException>(() => WorkflowUtils.TransformToWorkflowNet(net));

            // Assert that failed transformations do not change the net
            Tuple<PetriNet, Marking> input2 = parser.ReadNet(Utils.GetPathForTestfile(filepath));
            PetriNet net2 = input2.Item1;

            Assert.Equal(net, net2);
        }

        [Theory]
        [InlineData("workflow_tests/1_original.lola", "workflow_tests/1_wf.lola")]
        public void TestWorkflownetTransformationEquality(string filepath_input, string filepath_expected)
        {
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input1 = parser.ReadNet(Utils.GetPathForTestfile(filepath_input));
            PetriNet net1 = input1.Item1;

            Tuple<PetriNet, Marking> input2 = parser.ReadNet(Utils.GetPathForTestfile(filepath_expected));
            PetriNet net2 = input2.Item1;

            WorkflowUtils.TransformToWorkflowNet(net1);

            Assert.Equal(net2, net1);

            Assert.True(net1.IsWorkflowNet().Item1);
            Assert.True(net2.IsWorkflowNet().Item1);
        }

        [Theory]
        [InlineData("workflow_tests/continuous_soundness/false", false)]
        [InlineData("workflow_tests/continuous_soundness/true", true)]
        public void TestContinuousSoundnessForDirectory(string directory_path, bool expected)
        {
            var filepaths = System.IO.Directory.GetFiles(Utils.GetPathForTestfile(directory_path));
            foreach (string filepath in filepaths)
            {
                TestContinuousSoundness(filepath, expected);
            }
        }

        [Theory]
        [InlineData("workflow_tests/B1.s00000125__s00001157_transformed.lola", false)]
        public void TestContinuousSoundness(string filepath_input, bool expected)
        {
            if (!System.IO.File.Exists(filepath_input))
            {
                filepath_input = Utils.GetPathForTestfile(filepath_input);
            }
            output.WriteLine("On net " + filepath_input);
            LolaParser parser = new LolaParser();
            Tuple<PetriNet, Marking> input1 = parser.ReadNet(filepath_input);
            PetriNet net = input1.Item1;
            Marking initialMarking = input1.Item2;

            (bool, Dictionary<Place, double>) result = Z3Heuristics.
                            IsContinuousSound_ViaContinuousReach(net, initialMarking);
            bool actual = result.Item1;

            Assert.True(actual == expected,
                "On net " + filepath_input +
                " expected " + expected + " got " + actual);
        }
    }
}