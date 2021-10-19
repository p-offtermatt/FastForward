#define GUROBI

#if GUROBI
using Gurobi;
using Petri;
using System.Collections.Generic;
using System.Linq;
using Utils;
using System;

namespace HeuristicFrontier
{
    public abstract class GurobiMarkingEquationFrontier : HeuristicFrontier<Marking>
    {

        private GRBModel model;

        /// <summary>
        /// Concrete implementations can set the domain to any of Gurobis domains, such as GRB.INTEGER or GRB.CONTINUOUS
        /// </summary>
        /// <returns>A char representing the chosen domain. See the Gurobi .NET API.</returns>
        public abstract char getDomain();

        private int iteration = 0;

        PetriNet net;

        private Dictionary<GRBVar, Marking> indicatorVarsToMarkings;
        private Dictionary<Marking, GRBVar> markingsToIndicatorVars;

        private GRBLinExpr[] leftHandSides;
        private GRBLinExpr[] rightHandSides;

        // store the combined lhs+rhs and remember their names/senses
        private GRBConstr[] equations;

        private String[] constrNames;
        private char[] senses;
        //


        private GRBLinExpr sumOfIndicators;
        private GRBConstr sumOfIndicatorsConstr;

        private GRBLinExpr optimizationObjective;


        public GurobiMarkingEquationFrontier(PetriNet net, List<MarkingWithConstraints> targetMarkings)
        {
            this.model = GurobiHeuristics.InitializeModel();
            this.net = net;

            // TODO: Handle multiple targets
            MarkingWithConstraints targetMarking = targetMarkings.First();

            // create transition vars
            GRBVar[] transitionTimesFiredVars = GurobiHeuristics.CreateTransitionTimesFiredVars(net.Transitions, getDomain(), model);

            leftHandSides = GurobiHeuristics.GeneratePlaceExprs(net, transitionTimesFiredVars);
            rightHandSides = new GRBLinExpr[net.Places.Count];

            equations = new GRBConstr[net.Places.Count];
            constrNames = new String[net.Places.Count];
            senses = new char[net.Places.Count];


            for (int i = 0; i < net.Places.Count; i++)
            {
                Place place = net.Places[i];
                GRBLinExpr rightHandSide = new GRBLinExpr(targetMarking.Marking.GetValueOrDefault(place, 0));
                rightHandSides[i] = rightHandSide;

                GRBLinExpr leftHandSide = leftHandSides[i];

                char sense = targetMarking.Constraints.GetValueOrDefault(place, ConstraintOperators.GreaterEqual) == ConstraintOperators.GreaterEqual ?
                     GRB.GREATER_EQUAL : GRB.EQUAL;

                String constrName = "markingEquationConstraint_" + place.Name.Truncate(200);

                equations[i] = model.AddConstr(leftHandSide, sense, rightHandSide, constrName);
                constrNames[i] = constrName;
                senses[i] = sense;

            }

            indicatorVarsToMarkings = new Dictionary<GRBVar, Marking>();
            markingsToIndicatorVars = new Dictionary<Marking, GRBVar>();

            sumOfIndicators = new GRBLinExpr();

            sumOfIndicatorsConstr = model.AddConstr(sumOfIndicators, '=', 1, "sumOfIndicatorsConstraint");

            optimizationObjective = GurobiHeuristics.CreateVariableSumExpression(transitionTimesFiredVars);
            model.SetObjective(optimizationObjective, GRB.MINIMIZE);
        }

        public override void Add(Marking node, float distanceFromStart)
        {
            GRBVar indicatorVar = model.AddVar(0, 1, 0, GRB.BINARY, "markingIndicatorVar_" + node.ToString().Truncate(200));
            indicatorVarsToMarkings.Add(indicatorVar, node);
            markingsToIndicatorVars.Add(node, indicatorVar);

            sumOfIndicators.AddTerm(1, indicatorVar);
            optimizationObjective.AddTerm(distanceFromStart, indicatorVar);

            model.SetObjective(optimizationObjective);

            model.Remove(sumOfIndicatorsConstr);

            sumOfIndicatorsConstr = model.AddConstr(sumOfIndicators, '=', 1, "sumOfIndicatorsConstraint");


            foreach (Place place in node.GetMarkedPlaces())
            {
                int index = net.Places.IndexOf(place);
                int tokenCount = node[place];

                GRBConstr constraint = equations[index];

                model.Remove(equations[index]);

                rightHandSides[index].AddTerm(-tokenCount, indicatorVar);

                equations[index] = model.AddConstr(leftHandSides[index], senses[index], rightHandSides[index], constrNames[index]);
            }
        }


        public override (double, Marking) PopMinNode()
        {
            model.Optimize();
            // iteration++;
            // model.Write("out" + iteration + ".lp");
            // model.Write("out" + iteration + ".mps");
            // model.Write("out" + iteration + ".sol");
            // model.Write("out" + iteration + ".json");



            if (model.Status != GRB.Status.OPTIMAL && model.Status != GRB.Status.SUBOPTIMAL)
            {
                return (0, null);
            }

            foreach ((GRBVar var, Marking marking) in indicatorVarsToMarkings)
            {
                double solutionValue = var.X;

                if (solutionValue == 1)
                {
                    model.Remove(var);
                    for (int i = 0; i < net.Places.Count; i++)
                    {
                        rightHandSides[i].Remove(var);
                    }

                    optimizationObjective.Remove(var);
                    sumOfIndicators.Remove(var);
                    indicatorVarsToMarkings.Remove(var);
                    markingsToIndicatorVars.Remove(marking);
                    return (model.ObjVal, marking);

                }
            }

            throw new System.Exception("No indicator variable was set to 1 - this behaviour is unexpected!");
        }

        public override void UpdateNodeScore(Marking node, float newScore)
        {
            GRBVar var = markingsToIndicatorVars[node];

            optimizationObjective.Remove(var);
            optimizationObjective.AddTerm(newScore, var);
        }

        public override bool Contains(Marking node)
        {
            return markingsToIndicatorVars.ContainsKey(node);
        }
    }

    public class GurobiMarkingEquationOverNFrontier : GurobiMarkingEquationFrontier
    {
        public GurobiMarkingEquationOverNFrontier(PetriNet net, List<MarkingWithConstraints> targetMarkings) : base(net, targetMarkings)
        { }

        public override char getDomain()
        {
            return GRB.INTEGER;
        }
    }

    public class GurobiMarkingEquationOverQFrontier : GurobiMarkingEquationFrontier
    {
        public GurobiMarkingEquationOverQFrontier(PetriNet net, List<MarkingWithConstraints> targetMarkings) : base(net, targetMarkings)
        { }

        public override char getDomain()
        {
            return GRB.CONTINUOUS;
        }
    }
}
#endif