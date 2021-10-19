using Xunit;
using System.Collections.Generic;
using Petri;
using BoolForms;

namespace Testing
{
    public class TestXmlFormulaParser
    {
        [Fact]
        public void TestSimpleFormula()
        {
            MCCFormulaParser formulaParser = new MCCFormulaParser(Utils.GetPathForTestfile("mcc/SimpleFormula.xml"),
                "http://mcc.lip6.fr/");
            BooleanExpression actual =
                MCCFormulaParser.ConvertQuantifierFreeFormulaToBooleanExpressions(formulaParser.document.FirstChild);

            Literal UndeployWsr3 = new Literal("is-fireable:UndeployWsr3");
            Literal StopSc = new Literal("is-fireable:StopSc");
            Literal StartContainer = new Literal("is-fireable:StartContainer");
            Literal StopContainer = new Literal("is-fireable:StopContainer");
            BooleanExpression expected = (!UndeployWsr3 & StopSc) | (StartContainer & StopContainer);

            Assert.Equal(expected, actual);
        }

        [Fact]
        public void TestCardinalitySimpleFormula()
        {
            MCCFormulaParser formulaParser = new MCCFormulaParser(Utils.GetPathForTestfile("mcc/CardinalitySimpleFormula.xml"),
                "http://mcc.lip6.fr/");
            BooleanExpression actual =
                MCCFormulaParser.ConvertQuantifierFreeFormulaToBooleanExpressions(formulaParser.document.FirstChild);

            Literal threeLEQp5 = new Literal("3 <= p5");
            Literal p1LEQp1 = new Literal("p1 <= p1");
            BooleanExpression expected = threeLEQp5 | !(!(p1LEQp1));
            Assert.Equal(expected, actual);
        }

        [Fact]
        public void TestFireabilityUnfolding()
        {
            List<UpdateTransition> transitions = new List<UpdateTransition>(new UpdateTransition[] {
                new UpdateTransition("t1",
                new Dictionary<Place, int>{
                    [new Place("x0")] = 1,
                    [new Place("x1")] = 2,
                },
                new Dictionary<Place, int>{
                }),
                new UpdateTransition("t2",
                new Dictionary<Place, int>{
                    [new Place("x0")] = 1,
                },
                new Dictionary<Place, int>{
                })});

            List<Place> places = new List<Place>(new Place[]{
                new Place("x0"),
                new Place("x1")
            });
            PetriNet net = new PetriNet(places, transitions);

            Literal OneLEQx0 = new Literal("1 <= x0");
            Literal TwoLEQx1 = new Literal("2 <= x1");
            BooleanExpression expected = (OneLEQx0 & TwoLEQx1) | !(OneLEQx0);

            MCCFormulaParser formulaParser = new MCCFormulaParser(Utils.GetPathForTestfile("mcc/Unfolding-test.xml"),
                "http://mcc.lip6.fr/");

            BooleanExpression expression =
                MCCFormulaParser.ConvertQuantifierFreeFormulaToBooleanExpressions(formulaParser.document.FirstChild);

            Literal IsFireableT1 = new Literal("is-fireable:t1");
            Literal IsFireableT2 = new Literal("is-fireable:t2");

            BooleanExpression expectedExpression = IsFireableT1 | !(IsFireableT2);

            Assert.Equal(expectedExpression, expression);

            BooleanExpression actual = MCCFormulaUtils.UnfoldFireability(expression, net);

            Assert.Equal(expected, actual);
        }

        [Fact]
        public void TestElimination()
        {
            MCCFormulaParser formulaParser = new MCCFormulaParser(Utils.GetPathForTestfile("mcc/LiteralEliminationTest.xml"),
                "http://mcc.lip6.fr/");

            BooleanExpression expression =
                MCCFormulaParser.ConvertQuantifierFreeFormulaToBooleanExpressions(formulaParser.document.FirstChild);

            BooleanExpression expected = BooleanExpression.TRUE;

            BooleanExpression actual = MCCFormulaUtils.SimplifyComparisons(expression);

            Assert.Equal(expected, actual);
        }

        [Fact]
        public void TestEqualComparison()
        {
            MCCFormulaParser formulaParser = new MCCFormulaParser(Utils.GetPathForTestfile("mcc/EqualComparisonTest.xml"),
                "http://mcc.lip6.fr/");

            BooleanExpression expression =
                MCCFormulaParser.ConvertQuantifierFreeFormulaToBooleanExpressions(formulaParser.document.FirstChild);

            BooleanExpression expected = BooleanExpression.TRUE;

            BooleanExpression actual = MCCFormulaUtils.SimplifyComparisons(expression);

            Assert.Equal(expected, actual);
        }

        [Fact]
        public void TestFalseConstantComparison()
        {
            MCCFormulaParser formulaParser = new MCCFormulaParser(Utils.GetPathForTestfile("mcc/FalseConstantComparisonTest.xml"),
                "http://mcc.lip6.fr/");

            BooleanExpression expression =
                MCCFormulaParser.ConvertQuantifierFreeFormulaToBooleanExpressions(formulaParser.document.FirstChild);

            BooleanExpression expected = BooleanExpression.FALSE;

            BooleanExpression actual = MCCFormulaUtils.SimplifyComparisons(expression);

            Assert.Equal(expected, actual);
        }

        [Fact]
        public void TestTrueConstantComparison()
        {
            MCCFormulaParser formulaParser = new MCCFormulaParser(Utils.GetPathForTestfile("mcc/TrueConstantComparisonTest.xml"),
                "http://mcc.lip6.fr/");

            BooleanExpression expression =
                MCCFormulaParser.ConvertQuantifierFreeFormulaToBooleanExpressions(formulaParser.document.FirstChild);

            BooleanExpression expected = BooleanExpression.TRUE;

            BooleanExpression actual = MCCFormulaUtils.SimplifyComparisons(expression);

            Assert.Equal(expected, actual);
        }
    }
}