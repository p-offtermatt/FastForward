using System;
using System.Linq;
using System.Collections.Generic;
using System.Xml;
using Petri;
using BoolForms;

namespace Petri
{

    public class MCCFormulaUtils
    {
        public static bool SatisfiedByMarking(BooleanExpression expression, Marking marking)
        {
            // switch (expression)
            // {
            //     case Literal lit
            // }
            throw new NotImplementedException();
        }

        public static bool IsLEQ(Literal lit)
        {
            return lit.Name.Contains(" <= ");
        }

        public static bool IsFireableConstraint(Literal lit)
        {
            return lit.Name.StartsWith("is-fireable:");
        }

        /// <summary>
        ///  Gets the transition name if the given literal is an 'is-fireable' literal.
        /// Throws an ArgumentException otherwise.
        /// </summary>
        /// <param name="lit"></param>
        /// <returns></returns>
        public static string GetTransitionName(Literal lit)
        {
            if (!IsFireableConstraint(lit))
            {
                throw new ArgumentException("Literal is not an is-fireable literal: " + lit.Name);
            }
            else
            {
                return lit.Name.Substring("is-fireable".Length);
            }
        }

        /// <summary>
        /// If the literal is an 'integer-le' literal, returns strings representing the two operands.
        /// Throws an ArgumentException otherwise.
        /// </summary>
        /// <param name="lit"></param>
        public static Tuple<String, String> GetLEQOperands(Literal lit)
        {
            if (!IsLEQ(lit))
            {
                throw new ArgumentException("Literal is not an 'integer-le' constraint: " + lit.Name);
            }

            var operands = lit.Name.Split(" <= ", StringSplitOptions.RemoveEmptyEntries);
            return new Tuple<String, String>(operands.First(), operands.Last());
        }

        /// <summary>
        ///  Unfolds all fireability atoms and transforms them into bounds on token counts of places.
        /// is-fireable(t) for a transition t becomes a conjunction of integer-le(t.pre[p], token-count(p))) for all p
        /// </summary>
        /// <returns>The expression where all fireability-literals are unfolded into token-count literals</returns>
        public static BooleanExpression UnfoldFireability(BooleanExpression expr, PetriNet net)
        {
            if (expr is NAryOperator nary)
            {
                var newOperands = nary.GetOperands().Select(op => UnfoldFireability(op, net));
                if (expr is Disjunction)
                {
                    return new Disjunction(newOperands);
                }
                else if (expr is Conjunction)
                {
                    return new Conjunction(newOperands);
                }
                throw new ArgumentException("Can not handle subclass of NAry operator: " + expr.GetType());
            }
            else if (expr is Literal lit)
            {
                if (lit.Name.StartsWith("is-fireable:"))
                {
                    // build a conjunction specifying the concrete bounds for each place
                    var transitionName = lit.Name.Substring("is-fireable:".Length);
                    var transition = net.Transitions.Where(t => t.Name == transitionName).FirstOrDefault();
                    var conjunctionOperands = new List<BooleanExpression>();
                    if (transition == null)
                    {
                        throw new ArgumentException("There is no transition with name: " + transitionName +
                            ", \n but the given expression is " + lit.ToString());
                    }
                    foreach (var (place, amount) in transition.GetGuard())
                    {
                        if (amount > 0)
                        {
                            conjunctionOperands.Add(new Literal(amount + " <= " + place.Name));
                        }
                    }
                    if (conjunctionOperands.Count > 1)
                    {
                        return new Conjunction(conjunctionOperands);
                    }
                    else
                    {
                        // first must exist, since a conjunction can't have 0 operands
                        return conjunctionOperands.First();
                    }
                }
            }
            else if (expr is Negation neg)
            {
                return new Negation(UnfoldFireability(neg.InnerExpression, net));
            }
            else if (expr is Constant)
            {
                return expr;
            }
            throw new ArgumentException("Expression of type '" + expr.GetType() + "' is not supported!");
        }

        /// <summary>
        /// Simplifies the literals of the given formula that uses literals of the form described in
        /// https://mcc.lip6.fr/pdf/MCC2020-formula_manual.pdf, ReachabilityCategories.
        /// The following simplifications are performed:
        ///  - Constant comparison: if c1, c2 constant, then integer-le(c1, c2) = constant true if c1 <= c2, else constant false
        ///  - Equal comparison: integer-le(p1, p1) = constant true
        ///  - Elimination: (A <= B | B <= A)  = true
        /// </summary>
        /// <returns>The simplified expression.</return>
        public static BooleanExpression SimplifyComparisons(BooleanExpression expr)
        {
            if (expr is NAryOperator nary)
            {
                var newOperands = nary.GetOperands().Select(op => SimplifyComparisons(op));
                if (expr is Disjunction)
                {
                    // elimination: check for integer-le literals that have the same operands in reverse order
                    IEnumerable<Literal> lits = newOperands.Where(op => op is Literal).Cast<Literal>();
                    foreach (Literal lit in lits)
                    {
                        if (!MCCFormulaUtils.IsLEQ(lit))
                        {
                            continue;
                        }
                        else
                        {
                            var literalOperands = MCCFormulaUtils.GetLEQOperands(lit);
                            foreach (Literal otherLit in lits)
                            {
                                if (!MCCFormulaUtils.IsLEQ(otherLit))
                                {
                                    continue;
                                }
                                var otherLiteralOperands = MCCFormulaUtils.GetLEQOperands(otherLit);

                                if (literalOperands.Item1.Equals(otherLiteralOperands.Item2) &&
                                    literalOperands.Item2.Equals(otherLiteralOperands.Item1))
                                {
                                    return BooleanExpression.TRUE;
                                }
                            }
                        }
                    }

                    return new Disjunction(newOperands);
                }
                else if (expr is Conjunction)
                {
                    return new Conjunction(newOperands);
                }
                throw new ArgumentException("Can not handle subclass of NAry operator: " + expr.GetType());
            }
            else if (expr is Literal lit)
            {
                if (MCCFormulaUtils.IsLEQ(lit))
                {
                    // literal is of the form x <= y
                    var (operand1Name, operand2Name) = MCCFormulaUtils.GetLEQOperands(lit);

                    // constant comparison
                    if (operand1Name == operand2Name)
                    {
                        return BooleanExpression.TRUE;
                    }

                    int operand1AsInt;
                    int operand2AsInt;

                    bool operand1IsInt = Int32.TryParse(operand1Name, out operand1AsInt);
                    bool operand2IsInt = Int32.TryParse(operand2Name, out operand2AsInt);

                    if (operand1IsInt && operand2IsInt)
                    {
                        return operand1AsInt <= operand2AsInt ? BooleanExpression.TRUE : BooleanExpression.FALSE;
                    }

                    // can't simplify
                    return lit;
                }
            }
            else if (expr is Negation neg)
            {
                return new Negation(SimplifyComparisons(neg.InnerExpression));
            }
            else if (expr is Constant)
            {
                return expr;
            }
            throw new ArgumentException("Expression of type '" + expr.GetType() + "' is not supported!");
        }
    }

    public class MCCFormulaParser : FormulaParser
    {

        public XmlDocument document;
        private XmlNamespaceManager namespaceManager;

        private String namespaceName;

        public MCCFormulaParser(String xmlFilePath, String namespaceName)
        {
            document = new XmlDocument();
            document.Load(xmlFilePath);
            this.namespaceName = namespaceName;

            // create ns manager
            namespaceManager = new XmlNamespaceManager(document.NameTable);
            namespaceManager.AddNamespace("mcc", namespaceName);
        }

        /// <summary>
        /// Transforms the given reachability formula into
        /// the format E F formula.
        /// Additionally, returns a boolean that indicates whether the formula has to be negated or not.
        /// </summary>
        /// <returns>A tuple containing a boolean that is true when the result of the formula has to be negated,
        /// and the root node of the quantifier free formula.</returns>
        public Tuple<bool, XmlNode> ConvertFormulaToQuantifierFree(XmlNode formulaNode)
        {
            XmlNode quantifierNode =
                    formulaNode.SelectSingleNode(
                        "./mcc:exists-path/mcc:finally|./mcc:all-paths/mcc:globally",
                        namespaceManager);

            bool negatedQuery;
            if (quantifierNode.Name == "finally")
            {
                negatedQuery = false;
            }
            else // quantifierFreeFormulaNode.Name == "globally"
            {
                negatedQuery = true;
                // exploit the fact that A G pi = not E F not pi
                XmlNode negation = document.CreateElement("negation", namespaceName);

                XmlNode formulaChildNode = quantifierNode.FirstChild;

                quantifierNode.ReplaceChild(negation, formulaChildNode);
                negation.AppendChild(formulaChildNode);
            }
            XmlNode quantifierFreeFormulaNode = quantifierNode.FirstChild;
            return new Tuple<bool, XmlNode>(negatedQuery, quantifierFreeFormulaNode);
        }

        public static List<Tuple<BooleanExpression, bool>> ReadXMLTargets(String xmlFilePath)
        {
            // TODO change this later, or make it an argument to be passed via CLI
            string namespaceName = "http://mcc.lip6.fr/";


            MCCFormulaParser parser = new MCCFormulaParser(xmlFilePath, namespaceName);
            List<Tuple<BooleanExpression, bool>> expressions = parser.GetExpressions();

            expressions = expressions.Select(pair => new Tuple<BooleanExpression, bool>(pair.Item1.ToDNF(), pair.Item2)).ToList();


            return expressions;
        }

        public XmlNodeList ReadProperties()
        {
            return document.SelectNodes("//mcc:property", namespaceManager);
        }

        public XmlNode GetFormulaFromProperty(XmlNode property)
        {
            return property.SelectSingleNode("./mcc:formula", namespaceManager);
        }

        public List<Tuple<BooleanExpression, bool>> GetExpressions()
        {
            var properties = this.ReadProperties();

            List<Tuple<BooleanExpression, bool>> result = new List<Tuple<BooleanExpression, bool>>();

            foreach (XmlNode property in properties)
            {
                XmlNode formula = GetFormulaFromProperty(property);
                (bool negatedQuery, XmlNode quantifierFreeFormula) = ConvertFormulaToQuantifierFree(formula);
                BooleanExpression expression = ConvertQuantifierFreeFormulaToBooleanExpressions(quantifierFreeFormula);
                result.Add(new Tuple<BooleanExpression, bool>(expression, negatedQuery));
            }
            return result;
        }

        public static BooleanExpression ConvertQuantifierFreeFormulaToBooleanExpressions(XmlNode formula)
        {
            if (formula.Name == "conjunction" || formula.Name == "disjunction")
            {

                var operands = formula.ChildNodes;
                var booleanExpressionChilds =
                    operands.Cast<XmlNode>().Select(child => ConvertQuantifierFreeFormulaToBooleanExpressions(child));

                if (formula.Name == "conjunction")
                {
                    return new Conjunction(booleanExpressionChilds);
                }
                else // formula.Name == "disjunction"
                {
                    return new Disjunction(booleanExpressionChilds);
                }
            }
            else if (formula.Name == "is-fireable")
            {
                var transitionInQuestion = formula.FirstChild.InnerText;
                return new Literal("is-fireable:" + transitionInQuestion);
            }
            else if (formula.Name == "negation")
            {
                return new Negation(ConvertQuantifierFreeFormulaToBooleanExpressions(formula.FirstChild));
            }
            else if (formula.Name == "integer-le")
            {
                var leftChild = formula.FirstChild.InnerText;
                var rightChild = formula.LastChild.InnerText;
                return new Literal(leftChild + " <= " + rightChild);
            }
            else
            {
                throw new ArgumentException(
                        "Do not know how to handle the following formula with name " + formula.Name + ": " + formula.OuterXml);
            }
        }

        public List<MarkingWithConstraints> ReadFormula(string filepath)
        {
            throw new NotImplementedException();
        }
    }
}
