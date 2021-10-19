using Xunit;
using System;
using System.Collections.Generic;
using Petri;
using Xunit.Abstractions;
using BoolForms;

namespace Testing
{
    public class TestParsingMCCFormulas
    {

        private readonly ITestOutputHelper output;

        public TestParsingMCCFormulas(ITestOutputHelper output)
        {
            this.output = output;
        }

        [Fact]
        public void TestCloudOpsManagementPT00002by00001ReachabilityCardinality()
        {
            MCCFormulaParser formulaParser = new MCCFormulaParser(
                Utils.GetPathForTestfile("mcc/CloudOpsManagement-PT-00002by00001-ReachabilityCardinality.xml"),
                "http://mcc.lip6.fr/");

            List<Tuple<BooleanExpression, bool>> expressions = formulaParser.GetExpressions();

            foreach ((BooleanExpression expression, _) in expressions)
            {
                BooleanExpression dnfExpression = expression.ToDNF();
                output.WriteLine("----------");
                output.WriteLine("Original expression:");
                output.WriteLine(expression.ToString());
                output.WriteLine("Expression in DNF:");
                output.WriteLine(dnfExpression.ToString());
                output.WriteLine("Length: " + expression.Length());
                output.WriteLine("DNF Length: " + dnfExpression.Length());
                if (dnfExpression is Disjunction disj)
                {
                    output.WriteLine("Clauses: " + disj.GetOperands().Count);
                }
                else
                {
                    output.WriteLine("Clauses: 1");
                }

                // Ensure formulas are simplified, flattened and dnf'd, i.e. the operations don't change the formula anymore
                Assert.Equal(dnfExpression, dnfExpression.Flatten());
                Assert.Equal(dnfExpression, dnfExpression.Simplify());
                Assert.Equal(dnfExpression, dnfExpression.ToDNF());

                Assert.True(dnfExpression.Is_NNF());
                Assert.True(dnfExpression.Is_DNF());
            }
        }

        [Fact]
        public void TestCloudOpsManagementPT00002by00001ReachabilityFireability()
        {
            MCCFormulaParser formulaParser = new MCCFormulaParser(
                Utils.GetPathForTestfile("mcc/CloudOpsManagement-PT-00002by00001-ReachabilityFireability.xml"),
                "http://mcc.lip6.fr/");

            PNMLParser parser = new PNMLParser();


            (PetriNet net, Marking initialMarking) = parser.ReadNet(
                Utils.GetPathForTestfile("mcc/CloudOpsManagement-PT-00002by00001.pnml"));

            List<Tuple<BooleanExpression, bool>> expressions = formulaParser.GetExpressions();

            foreach ((BooleanExpression expression, _) in expressions)
            {
                BooleanExpression dnfExpression = expression.ToDNF();
                output.WriteLine("----------");
                output.WriteLine("Original expression:");
                output.WriteLine(expression.ToString());
                output.WriteLine("Expression in DNF:");
                output.WriteLine(dnfExpression.ToString());

                // Ensure formulas are simplified, flattened and dnf'd, i.e. the operations don't change the formula anymore
                Assert.Equal(dnfExpression, dnfExpression.Flatten());
                Assert.Equal(dnfExpression, dnfExpression.Simplify());
                Assert.Equal(dnfExpression, dnfExpression.ToDNF());

                Assert.True(dnfExpression.Is_NNF());
                Assert.True(dnfExpression.Is_DNF());
            }
        }

        [Fact]
        public void TestCloudOpsManagementPT00002by00001ReachabilityFireability_Unfolded()
        {
            MCCFormulaParser formulaParser = new MCCFormulaParser(
                Utils.GetPathForTestfile("mcc/CloudOpsManagement-PT-00002by00001-ReachabilityFireability.xml"),
                "http://mcc.lip6.fr/");

            PNMLParser parser = new PNMLParser();

            (PetriNet net, Marking initialMarking) = parser.ReadNet(
                Utils.GetPathForTestfile("mcc/CloudOpsManagement-PT-00002by00001.pnml"));

            List<Tuple<BooleanExpression, bool>> expressions = formulaParser.GetExpressions();

            foreach ((BooleanExpression expression, _) in expressions)
            {
                BooleanExpression unfoldedExpression = MCCFormulaUtils.UnfoldFireability(expression, net);
                output.WriteLine("Unfolded expression:");
                output.WriteLine(unfoldedExpression.ToString());
                output.WriteLine("Length: " + unfoldedExpression.Length());

                BooleanExpression unfoldedExpressionInDNF = unfoldedExpression.ToDNF();

                output.WriteLine("DNF Length: " + unfoldedExpressionInDNF.Length());

                if (unfoldedExpressionInDNF is Disjunction disj)
                {
                    output.WriteLine("Clauses: " + disj.GetOperands().Count);
                }
                else
                {
                    output.WriteLine("Clauses: 1");
                }

                Assert.True(unfoldedExpressionInDNF.Is_NNF());
                Assert.True(unfoldedExpressionInDNF.Is_DNF());

                Assert.Equal(unfoldedExpressionInDNF, unfoldedExpressionInDNF.Flatten());
                Assert.Equal(unfoldedExpressionInDNF, unfoldedExpressionInDNF.Simplify());
                Assert.Equal(unfoldedExpressionInDNF, unfoldedExpressionInDNF.ToDNF());
            }
        }
    }
}