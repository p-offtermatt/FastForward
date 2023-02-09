#define GUROBI

using System;
using System.Linq;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using Utils;
using Benchmark;
using Gurobi;
using static Utils.GurobiConsts;

namespace Petri
{
    public static class GurobiHeuristics
    {

        public static Marking CheckMarkingEquationUnsoundness(PetriNet net, Place initialPlace, Place finalPlace)
        {
            GRBModel model = InitializeModel();

            // Allow solving nonconvex problems
            model.Parameters.NonConvex = 2;
            model.Parameters.FeasibilityTol = 0.000000001;

            GRBVar[] initialMarkingVars = GeneratePlaceMarkingVars(net.Places, GRB.CONTINUOUS, model, "initialMarking_");
            GRBVar[] intermediateMarkingVars = GeneratePlaceMarkingVars(net.Places, GRB.INTEGER, model, "intermediateMarking_");
            GRBVar[] finalMarkingVars = GeneratePlaceMarkingVars(net.Places, GRB.CONTINUOUS, model, "finalMarking_");
            GRBVar[] farkasVars = GeneratePlaceMarkingVars(net.Places, GRB.CONTINUOUS, model, "farkasVar_");

            Marking initialMarking = new Marking() { { initialPlace, 1 } };
            Marking finalMarking = new Marking() { { finalPlace, 1 } };

            InitializeMarkingConstraints(net.Places, model, initialMarkingVars, initialMarking, "initialMarkingConstr_");
            InitializeMarkingConstraints(net.Places, model, finalMarkingVars, finalMarking, "finalMarkingConstr_");

            InitializeMarkingConstraints(net.Places, model, intermediateMarkingVars, MarkingWithConstraints.AsCoverability(new Marking()), "intermediateMarking_nonnegative_");

            GRBVar[] effectVars = GeneratePlaceMarkingVars(net.Places, GRB.CONTINUOUS, model, "effect_");
            for (int i = 0; i < net.Places.Count; i++)
            {
                model.AddConstr(intermediateMarkingVars[i] + effectVars[i] - finalMarkingVars[i], '=', 0, "effect_matches");
            }

            GRBVar[] transitionVars = CreateTransitionTimesFiredVars(net.Transitions, GRB.CONTINUOUS, model);



            GenerateMarkingEquationConstraints(net.Places, net.Transitions, model, transitionVars, initialMarkingVars, intermediateMarkingVars, "reachMarkingEquation_");

            GenerateMarkingEquationUnreachabilityConstraint(net.Places, net.Transitions, effectVars, farkasVars, model);

            model.Optimize();

            if (model.Status == GRB.Status.INFEASIBLE)
            {
                return null;
            }
            else
            {
                return ExtractMarking(net.Places, model, intermediateMarkingVars);
            }
        }

        public static Dictionary<Transition, double> CheckIntegerDeadlock(PetriNet net, Place initialPlace, Place finalPlace, bool parikhImageOverIntegers,
        string netName = "")
        {
            GRBModel model = InitializeModel();

            GRBVar[] transitionVars = new GRBVar[net.Transitions.Count];

            {
                int i = 0;
                foreach (Transition t in net.Transitions)
                {
                    transitionVars[i] = model.AddVar(0, Utils.GurobiConsts.UpperBound, 0, GRB.INTEGER, "transitionVariable_" + t.Name.Truncate(100));
                    i += 1;
                }
            }
            {
                GRBLinExpr[] placeEffects = new GRBLinExpr[net.Places.Count];
                int i = 0;
                foreach (Place p in net.Places)
                {
                    GRBLinExpr placeEffect = new GRBLinExpr();
                    int j = 0;
                    foreach (UpdateTransition t in net.Transitions)
                    {
                        placeEffect.AddTerm(t.GetPrePostDifference().GetValueOrDefault(p), transitionVars[j]);
                        j += 1;
                    }
                    placeEffects[i] = placeEffect;
                    i++;
                    if (p.Equals(initialPlace))
                    {
                        // initial place can have negative effect
                        continue;
                    }
                    model.AddConstr(placeEffect, '>', 0.0, "Effect nonnegative " + p.Name.Truncate(200));
                }

                // ensure -initialPlace != finalPlace by checking \abs{initialPlace + finalPlace} \geq 1
                int initialPlaceIndex = net.Places.FindIndex(p => p.Equals(initialPlace));
                int finalPlaceIndex = net.Places.FindIndex(p => p.Equals(finalPlace));

                GRBVar initFinalDiffPositiveIndicator = model.AddVar(
                    0,
                    Utils.GurobiConsts.UpperBound,
                    0,
                    GRB.BINARY,
                    "initial_final_difference_positive_indicator");

                GRBVar initFinalDiffNegativeIndicator = model.AddVar(
                    0,
                    Utils.GurobiConsts.UpperBound,
                    0,
                    GRB.BINARY,
                    "initial_final_difference_positive_indicator");

                model.AddGenConstrIndicator(initFinalDiffPositiveIndicator,
                                            1,
                                            placeEffects[initialPlaceIndex] + placeEffects[finalPlaceIndex],
                                            GRB.GREATER_EQUAL,
                                            1,
                                            "init_final_diff_positive");

                model.AddGenConstrIndicator(initFinalDiffNegativeIndicator,
                                            1,
                                            placeEffects[initialPlaceIndex] + placeEffects[finalPlaceIndex],
                                            GRB.LESS_EQUAL,
                                            -1,
                                            "init_final_diff_negative");

                model.AddConstr(
                    initFinalDiffNegativeIndicator + initFinalDiffPositiveIndicator,
                    GRB.GREATER_EQUAL,
                    1,
                    "initial_final_difference_positive_or_negative"); // initial place will have negative effect, so add it to check 


                // ensure the marking is a deadlock over the naturals
                // for each transition we check that
                // at least one of its input places is smaller than the pre of the transition in that place
                ImposeDeadlock(net, model, placeEffects);
            }




            // model.ModelSense = GRB.MINIMIZE;
            // // model.SetObjective(objective, GRB.MINIMIZE);

            // model.Write("gurobi" + netName + ".mps");
            // model.Write("../../../gurobi.lp");


            model.Optimize();


            if (model.Status != GRB.Status.OPTIMAL && model.Status != GRB.Status.SUBOPTIMAL)
            {
                return null;
            }
            else
            {
                // model.Write("gurobi" + netName + ".sol");
                double[] transitionMults = model.Get(GRB.DoubleAttr.X, transitionVars);
                Dictionary<Transition, double> result = new Dictionary<Transition, double>();
                for (int i = 0; i < net.Transitions.Count; i++)
                {
                    double transitionMult = transitionMults[i];
                    if (transitionMult == 0.0)
                    {
                        continue;
                    }
                    result.Add(net.Transitions[i], transitionMult);
                }
                return result;
            }
        }

        private static void ImposeDeadlock(PetriNet net, GRBModel model, GRBLinExpr[] placeEffects)
        {
            int i;
            for (i = 0; i < net.Transitions.Count; i++)
            {
                UpdateTransition transition = (UpdateTransition)net.Transitions[i];

                GRBLinExpr placeLockSum = new GRBLinExpr();

                for (int j = 0; j < net.Places.Count; j++)
                {
                    Place place = net.Places[j];
                    int guard = transition.GetGuard().GetValueOrDefault(place, 0);
                    if (guard != 0)
                    {
                        GRBVar placeLockIndicator =
                            model.AddVar(0, Utils.GurobiConsts.UpperBound, 0, GRB.BINARY, "place_disables_transition_" + place.Name.Truncate(100) + "_____________" + transition.Name.Truncate(100));
                        model.AddGenConstrIndicator(placeLockIndicator, 1, placeEffects[j], GRB.LESS_EQUAL, guard - 1, "deadlock_transition_" + transition.Name.Truncate(100) + "_place_" + place.Name.Truncate(100));
                        placeLockSum.AddTerm(1, placeLockIndicator);
                    }
                }
                model.AddConstr(placeLockSum, GRB.GREATER_EQUAL, 1, "at_least_one_place_locks_" + transition.Name.Truncate(200));
            }
        }

        /// <summary>
        /// Returns a function that checks whether for a given transition t of the input net
        /// the effect of -t can be expressed by the other transitions of the net.
        /// </summary>
        /// <param name="net"></param>
        /// <returns>Returns a tuple containing the boolean analysis result and the first transition that was found not to be reverse-expressible
        /// (or null  if no such transition exists).</returns>
        public static Func<UpdateTransition, bool> InitializeTransitionExpressibilityChecker(PetriNet net)
        {
            GRBModel model = InitializeModel();

            GRBVar[] transitionVars = CreateTransitionTimesFiredVars(net.Transitions, GRB.INTEGER, model);
            GRBConstr[] targetEffectConstraints = new GRBConstr[net.Places.Count];

            for (int i = 0; i < net.Places.Count; i++)
            {
                Place place = net.Places[i];
                GRBLinExpr expr = new GRBLinExpr();

                for (int j = 0; j < net.Transitions.Count; j++)
                {
                    GRBVar transitionVar = transitionVars[j];
                    UpdateTransition transition = (UpdateTransition)net.Transitions[j];
                    int transitionEffect = transition.GetPrePostDifference().GetValueOrDefault(place);
                    expr.AddTerm(transitionEffect, transitionVar);
                }

                // left hand side will be modified later
                targetEffectConstraints[i] = model.AddConstr(expr, '=', 0, place.Name.Truncate(200) + "_effect");
            }

            bool CheckTransitionExpressibility(UpdateTransition transition)
            {
                for (int i = 0; i < net.Places.Count; i++)
                {
                    Place place = net.Places[i];
                    targetEffectConstraints[i].Set(GRB.DoubleAttr.RHS, -transition.GetPrePostDifference().GetValueOrDefault(place, 0));
                }

                model.Optimize();
                // model.Write("../../../out.lp");
                // model.Write("../../../out.sol");

                if (model.Status != GRB.Status.OPTIMAL && model.Status != GRB.Status.SUBOPTIMAL)
                {
                    return false;
                }
                else
                {
                    return true;
                }
            }

            return CheckTransitionExpressibility;
        }

        // Gurobi does not allow <X constraints;
        // instead, test for <=X-ZEROEPS
        private const double ZEROEPS = 1;

        public static Dictionary<Place, double> CheckUnreachability(List<Place> places, List<Transition> transitions, Marking initialMarking, Marking finalMarking)
        {
            GRBModel model = InitializeModel();

            GRBVar[] effectVars = GeneratePlaceMarkingVars(places, GRB.CONTINUOUS, model, "effect_");
            InitializeMarkingConstraints(places, model, effectVars, finalMarking - initialMarking, "effect_");

            GRBVar[] farkasVars = GeneratePlaceMarkingVars(places, GRB.CONTINUOUS, model, "farkas_");

            GenerateMarkingEquationUnreachabilityConstraint(places, transitions, effectVars, farkasVars, model);

            model.Optimize();


            if (model.Status != GRB.Status.OPTIMAL && model.Status != GRB.Status.SUBOPTIMAL)
            {
                return null;
            }
            else
            {

                Dictionary<Place, double> result = new Dictionary<Place, double>();
                for (int i = 0; i < places.Count; i++)
                {
                    result[places[i]] = (int)farkasVars[i].X;
                }
                return result;
            }
        }

        // Uses Farkas Lemma to generate constraints that are satisfied if and only if initialMarking cannot reach targetMarking.
        public static void GenerateMarkingEquationUnreachabilityConstraint(List<Place> places, List<Transition> transitions,
        GRBVar[] placeEffectVars, GRBVar[] placeFarkasVars, GRBModel model)
        {
            foreach (UpdateTransition transition in transitions)
            {
                Dictionary<Place, int> effects = transition.GetPrePostDifference();
                GRBLinExpr effectSum = new GRBLinExpr();

                for (int i = 0; i < places.Count; i++)
                {
                    Place place = places[i];
                    int effect = effects.GetValueOrDefault(place, 0);
                    if (effect == 0)
                    { continue; }

                    GRBVar placeFarkasVar = placeFarkasVars[i];

                    effectSum.AddTerm(effect, placeFarkasVar);
                }

                model.AddConstr(effectSum, '>', 0, "farkas_first_constraint_" + transition.Name.Truncate(200));
            }

            GRBQuadExpr placeSum = new GRBQuadExpr();

            for (int i = 0; i < places.Count; i++)
            {
                Place place = places[i];
                GRBVar placeFarkasVar = placeFarkasVars[i];
                GRBVar placeEffectVar = placeEffectVars[i];

                placeSum.AddTerm(1, placeFarkasVar, placeEffectVar);
            }

            model.AddQConstr(placeSum, '<', 0 - ZEROEPS, "farkas_second_constraint");
        }

        public static (bool isBounded, Dictionary<Transition, double> counterexample) CheckIntegerUnboundedness(PetriNet net)
        {
            //Check whether from zero marking, the zero marking can be covered
            GRBModel model = InitializeModel();

            GRBVar[] transitionVars = new GRBVar[net.Transitions.Count];

            {
                int i = 0;
                foreach (Transition t in net.Transitions)
                {
                    transitionVars[i] = model.AddVar(0, Utils.GurobiConsts.UpperBound, 0, GRB.CONTINUOUS, "transitionVariable_" + t.Name.Truncate(100));
                    i += 1;
                }
            }

            GRBVar[] placeCoveredIndicators = new GRBVar[net.Places.Count];

            {
                int i = 0;
                foreach (Place p in net.Places)
                {
                    GRBLinExpr placeEffect = new GRBLinExpr();
                    int j = 0;
                    foreach (UpdateTransition t in net.Transitions)
                    {
                        int effect = t.Post.GetValueOrDefault(p, 0) - t.Pre.GetValueOrDefault(p, 0);
                        if (effect != 0)
                        {
                            placeEffect.AddTerm(effect, transitionVars[j]);
                        };
                        j += 1;
                    }

                    model.AddConstr(placeEffect, GRB.GREATER_EQUAL, 0, "placeEffectNonnegative_" + p.Name.Truncate(200));

                    GRBVar indicatorVar = model.AddVar(0, 1, 0, GRB.BINARY, "placeGreaterZeroVar_" + p.Name.Truncate(200));
                    placeCoveredIndicators[i] = indicatorVar;
                    model.AddGenConstrIndicator(indicatorVar, 1, placeEffect, GRB.GREATER_EQUAL, 1, "placeEffectPositive_" + p.Name.Truncate(200));
                    i += 1;
                }
            }

            GRBVar anyPlaceCovered = model.AddVar(0, 1, 0, GRB.BINARY, "AnyPlaceCovered");

            model.AddGenConstrOr(anyPlaceCovered, placeCoveredIndicators, "AnyPlaceCoveredConstraint");
            model.AddConstr(anyPlaceCovered, GRB.EQUAL, 1, "AnyPlaceCovered_Activated");

            // GRBVar[] zeroMarkingVars = GeneratePlaceMarkingVars(net.Places, 'N', model, namePrefix: "zeroMarking_");
            // GRBConstr[] zeroMarkingConstraint = InitializeMarkingConstraints(net.Places,
            //                                                                    model,
            //                                                                    zeroMarkingVars,
            //                                                                    "zeroMarkingConstraint_");

            // GRBVar[] finalMarkingVars = GeneratePlaceMarkingVars(net.Places, 'N', model, namePrefix: "finalMarking_");
            // GenerateMarkingEquationConstraints(net.Places, net.Transitions, model, transitionVars, zeroMarkingVars, finalMarkingVars);

            // AddStrictlyGreaterMarkingConstraint(net.Places, model, finalMarkingVars, new Marking(), "final_marking_stricly_greater_than_zero");

            // GRBLinExpr objective = CreateVariableSumExpression(transitionVars);

            model.ModelSense = GRB.MINIMIZE;
            // // model.SetObjective(objective, GRB.MINIMIZE);

            model.Optimize();
            if (model.Status != GRB.Status.OPTIMAL && model.Status != GRB.Status.SUBOPTIMAL)
            {
                return (true, null);
            }
            else
            {
                // model.Set(GRB.IntParam.SolutionNumber, 0);
                return (false, ExtractTransitionMultiset(net.Transitions, model, transitionVars));
            }
        }

        /// <summary>
        /// Similar to <see cref="Unroll_MinTimeAtMostBound"/>,
        /// but instead of computing whether there exists a parallel execution within the bound,
        /// finds the minimal parallel execution up to the given bound.
        /// </summary>
        /// <param name="net">The workflow net</param>
        /// <param name="initialPlace">Its initial place, i.e. place without inputs</param>
        /// <param name="finalPlace">Its final place, i.e. place without outputs</param>
        /// <param name="initialBudget">The token count in the initial place</param>
        /// <param name="bound">The bound on the length of runs</param>
        /// <returns>The computed value for MinTime among executions up to the given bound.
        /// -1 if no execution exists.</returns>
        /// <exception cref="Exception"></exception>
        /// <seealso cref="Unroll_MinTimeAtMostBound(PetriNet, Place, Place, int, int)"/>
        public static int Unroll_ComputeMinTimeWithBound(PetriNet net,
                                        Place initialPlace, Place finalPlace, int initialBudget, int bound)
        {
            GRBModel model = InitializeModel();

            // execution should be at most bound long
            int runLength = bound;
            GRBVar[,] transitionVars = Unroll_CreateTransitionVars(net, model, runLength, GRB.INTEGER);
            GRBLinExpr[,] placeEffects = Unroll_ImposePlacesNonnegative(net, initialPlace, initialBudget, model, runLength, transitionVars);
            Unroll_ImposeBlockConstraints(net, model, runLength, transitionVars, placeEffects);
            Unroll_ImposeFinalMarkingConstraint(net, finalPlace, initialBudget, model, runLength, placeEffects);

            // objective is to minimize the number of blocks with non-zero transition var
            // create indicator vars that indicate for each step whether it is non-zero
            GRBLinExpr indicatorSum = Unroll_CreateIndicatorSumVar(net, model, runLength, transitionVars, usedForMinimizing: true);
            model.SetObjective(indicatorSum, GRB.MINIMIZE);


            // model.Write("../../../Gurobi.lp");
            model.Optimize();
            if (model.Status == GRB.Status.INFEASIBLE)
            {
                return -1;
            }
            else if (model.Status == GRB.Status.OPTIMAL || model.Status == GRB.Status.SUBOPTIMAL)
            {
                return (int)model.ObjVal;
            }
            else
            {
                throw new Exception("Unexpected status code from Gurobi: " + model.Status);
            }
        }

        public static bool Unroll_CheckSoundness(PetriNet net,
                                        Place initialPlace, Place finalPlace, int initialBudget, int bound)
        {
            GRBModel model = InitializeModel();

            // execution should be at most bound long
            int runLength = bound;
            GRBVar[,] transitionVars = Unroll_CreateTransitionVars(net, model, runLength, GRB.INTEGER);
            GRBLinExpr[,] placeEffects = Unroll_ImposePlacesNonnegative(net, initialPlace, initialBudget, model, runLength, transitionVars);
            Unroll_ImposeRunConstraints(net, model, runLength, transitionVars, placeEffects);

            // ensure we end at a deadlock
            GRBLinExpr[] lastStepPlaceEffects =
                Enumerable.Range(0, net.Places.Count)
                          .Select(placeIndex => placeEffects[placeIndex, runLength])
                          .ToArray();
            ImposeDeadlock(net, model, lastStepPlaceEffects);

            // ensure that we didn't reach the correct final marking
            ImposeNotFinalMarkingConstraint(net, finalPlace, initialBudget, model, lastStepPlaceEffects);


            // model.Write("../../../Gurobi.lp");
            model.Optimize();
            if (model.Status == GRB.Status.INFEASIBLE)
            // a witness means we found a deadlock that's not the correct final marking, so net is unsound
            // so infeasibility means the net is sound
            {
                return true;
            }
            else if (model.Status == GRB.Status.OPTIMAL || model.Status == GRB.Status.SUBOPTIMAL)
            {
                return false;
            }
            else
            {
                throw new Exception("Unexpected status code from Gurobi: " + model.Status);
            }
        }

        private static GRBLinExpr Unroll_CreateIndicatorSumVar(PetriNet net, GRBModel model, int runLength, GRBVar[,] transitionVars, bool usedForMinimizing)
        {
            GRBVar[] stepNonzeroIndicatorVars = new GRBVar[runLength];
            for (int step = 0; step < runLength; step++)
            {
                GRBLinExpr transitionCountForThisStep = new GRBLinExpr();
                for (int i = 0; i < net.Transitions.Count; i++)
                {
                    GRBVar transitionVar = transitionVars[i, step];
                    transitionCountForThisStep.AddTerm(1, transitionVar);
                }
                GRBVar indicator = model.AddVar(0, 1, 0, GRB.BINARY, "step_" + step.ToString() + "_indicator");
                stepNonzeroIndicatorVars[step] = indicator;

                // variable indicates whether any transition var for the step is non-zero.
                // since all are positive, just check the sum
                // implication direction is important: 
                // (var = 0) implies (transition sum = 0) works when minimizing, but not when maximizing
                // (var = 1) implies (transition sum \geq 1) works when maximizing, but not when minimizing
                string constrName = "step_" + step.ToString() + "_indicator_constraint";
                if (usedForMinimizing)
                {
                    model.AddGenConstrIndicator(indicator, 0, transitionCountForThisStep, GRB.EQUAL, 0, constrName);
                }
                else
                {
                    model.AddGenConstrIndicator(indicator, 1, transitionCountForThisStep, GRB.GREATER_EQUAL, 1, constrName);
                }
            }

            GRBLinExpr indicatorSum = CreateVariableSumExpression(stepNonzeroIndicatorVars);
            return indicatorSum;
        }

        public static int Unroll_ComputeL(PetriNet net,
                                        Place initialPlace, Place finalPlace, int initialBudget, int bound)
        {
            GRBModel model = InitializeModel();

            // execution should be at most bound long
            int runLength = bound;
            GRBVar[,] transitionVars = Unroll_CreateTransitionVars(net, model, runLength, GRB.INTEGER);
            GRBLinExpr[,] placeEffects = Unroll_ImposePlacesNonnegative(net, initialPlace, initialBudget, model, runLength, transitionVars);
            Unroll_ImposeRunConstraints(net, model, runLength, transitionVars, placeEffects);

            // objective is to find the longest run
            // so maximize the number of steps with non-zero transition var (i.e. steps where a transition is fired)
            GRBLinExpr indicatorSum = Unroll_CreateIndicatorSumVar(net, model, runLength, transitionVars, usedForMinimizing: false);

            model.SetObjective(indicatorSum, GRB.MAXIMIZE);

            // model.Write("../../../Gurobi.lp");
            model.Optimize();
            if (model.Status == GRB.Status.OPTIMAL || model.Status == GRB.Status.SUBOPTIMAL)
            {
                return (int)model.ObjVal;
            }
            else
            {
                // since this just finds any run, without a target marking, this should never be infeasible
                // so complain if it is
                throw new Exception("Expected LP to be feasible, but got unexpected status code from Gurobi: " + model.Status);
            }
        }

        /// <summary>
        /// Checks whether there exists a parallel execution that goes from
        /// {initialPlace: initialBudget} to {finalPlace: initialBudget}
        /// which uses at most bound blocks.
        /// parallel executions are runs where transitions are grouped into blocks such that:
        /// * each block can only include transition at most once
        /// * the pre of each block must be satisfied before the next step
        /// </summary>
        /// <param name="net">The workflow net</param>
        /// <param name="initialPlace">Its initial place, i.e. place without inputs</param>
        /// <param name="finalPlace">Its final place, i.e. place without outputs</param>
        /// <param name="initialBudget">The token count in the initial place</param>
        /// <param name="bound">The bound on the length of runs</param>
        /// <returns>True if there exists a parallel execution with at most bound blocks, false if not.</returns>
        /// <exception cref="Exception"></exception>
        public static bool Unroll_MinTimeAtMostBound(PetriNet net,
                                        Place initialPlace, Place finalPlace, int initialBudget, int bound)
        {
            GRBModel model = InitializeModel();

            // execution should be at most bound long
            int runLength = bound;
            GRBVar[,] transitionVars = Unroll_CreateTransitionVars(net, model, runLength, GRB.INTEGER);
            GRBLinExpr[,] placeEffects = Unroll_ImposePlacesNonnegative(net, initialPlace, initialBudget, model, runLength, transitionVars);
            Unroll_ImposeBlockConstraints(net, model, runLength, transitionVars, placeEffects);
            Unroll_ImposeFinalMarkingConstraint(net, finalPlace, initialBudget, model, runLength, placeEffects);

            // model.Write("../../../Gurobi.lp");
            model.Optimize();
            if (model.Status == GRB.Status.INFEASIBLE)
            {
                return false;
            }
            else if (model.Status == GRB.Status.OPTIMAL || model.Status == GRB.Status.SUBOPTIMAL)
            {
                return true;
            }
            else
            {
                throw new Exception("Unexpected status code from Gurobi: " + model.Status);
            }
        }

        private static void Unroll_ImposeFinalMarkingConstraint(PetriNet net, Place finalPlace, int initialBudget, GRBModel model, int runLength, GRBLinExpr[,] placeEffects)
        {
            // check that final effect is correct: all tokens are moved to final place
            // (recall that place effect on initial also includes the initial marking, so can ignore here)
            for (int i = 0; i < net.Places.Count; i++)
            {
                Place place = net.Places[i];

                model.AddConstr(
                    placeEffects[i, runLength],
                    GRB.EQUAL,
                    place.Equals(finalPlace) ? initialBudget : 0,
                    "correctFinalMarking_" + place.Name.Truncate(100)
                );
            }
        }

        // either 1) there is a token left outside the final place
        // 2) there are too many tokens in the final place
        // 3) there are too few tokens in the final place
        private static void ImposeNotFinalMarkingConstraint(PetriNet net, Place finalPlace, int initialBudget,
        GRBModel model, GRBLinExpr[] placeEffects)
        {
            int finalPlaceIndex = net.Places.FindIndex(place => place.Equals(finalPlace));

            // check whether the effect outside the final place is positive
            GRBLinExpr endEffectOutsideFinalPlace = placeEffects
                            .Where((_, index) => !net.Places[index].Equals(finalPlace))
                            .Aggregate(new GRBLinExpr(), (acc, expr) => acc + expr);
            GRBVar placeEffectOutsideFinalIsNonzeroIndicator = model.AddVar(
                0,
                1,
                0,
                GRB.BINARY,
                "placeEffectOutsideFinalIsNonzeroIndicator");
            model.AddGenConstrIndicator(placeEffectOutsideFinalIsNonzeroIndicator,
                                        1,
                                        endEffectOutsideFinalPlace,
                                        GRB.GREATER_EQUAL,
                                        1,
                                        "placeEffectOutSideFinalIsNonzeroConstraint");

            // check whether the final place has less than initialBudget tokens 
            // since token counts are integers so x < y is expressed as x <= y-1)
            GRBVar tooFewTokensIndicator = model.AddVar(0, 1, 0, GRB.BINARY, "TooFewFinalTokensIndicator");
            model.AddGenConstrIndicator(
                tooFewTokensIndicator,
                1,
                placeEffects[finalPlaceIndex],
                GRB.LESS_EQUAL,
                initialBudget - 1,
                "TooFewFinalTokens");

            // symmetrically, check whether the final place has more than initialBudget tokens
            GRBVar tooManyTokensIndicator = model.AddVar(0, 1, 0, GRB.BINARY, "TooManyFinalTokensIndicator");
            model.AddGenConstrIndicator(
                tooManyTokensIndicator,
                1,
                placeEffects[finalPlaceIndex],
                GRB.GREATER_EQUAL,
                initialBudget + 1,
                "TooManyFinalTokens");

            // make sure at least one of the violations holds, i.e. their sum is at least 1
            model.AddConstr(
                endEffectOutsideFinalPlace + tooFewTokensIndicator + tooManyTokensIndicator,
                GRB.GREATER_EQUAL,
                1,
                "FinalMarkingWrong_IndicatorSum"
            );
        }

        // imposes run constraints: each step contains at most single transition, fired by factor 1
        // since transitionVars are integers, this just means the sum of transition vars in each step has to be at most 1
        private static void Unroll_ImposeRunConstraints(
            PetriNet net,
            GRBModel model,
            int runLength,
            GRBVar[,] transitionVars,
            GRBLinExpr[,] placeEffects)
        {
            for (int step = 0; step < runLength; step++)
            {
                GRBLinExpr stepTransitionCount = new GRBLinExpr();
                for (int i = 0; i < net.Transitions.Count; i++)
                {
                    stepTransitionCount.AddTerm(1, transitionVars[i, step]);
                }
                model.AddConstr(stepTransitionCount, GRB.LESS_EQUAL, 1, "step_" + step.ToString() + "_contains_one_transition");
            }
        }

        // imposes that the transition vars encode a parallel execution.
        // transitions are split into blocks such that:
        // * each block can only include transition at most once
        //  * the pre of each block must be satisfied before the next step
        private static void Unroll_ImposeBlockConstraints(PetriNet net, GRBModel model, int runLength, GRBVar[,] transitionVars, GRBLinExpr[,] placeEffects)
        {
            for (int step = 0; step < runLength; step++)
            {
                for (int i = 0; i < net.Transitions.Count; i++)
                {
                    model.AddConstr(
                        transitionVars[i, step],
                        GRB.LESS_EQUAL,
                        1,
                        "transitionOccursAtMostOnce_" + step.ToString() + "_name_" + net.Transitions[i].Name.Truncate(100));
                }

                for (int i = 0; i < net.Places.Count; i++)
                {
                    Place place = net.Places[i];
                    GRBLinExpr stepPre = new GRBLinExpr();
                    for (int j = 1; j < net.Transitions.Count; j++)
                    {
                        UpdateTransition transition = (UpdateTransition)net.Transitions[j];
                        stepPre.AddTerm(transition.Pre.GetValueOrDefault(place, 0), transitionVars[j, step]);
                    }

                    model.AddConstr(stepPre,
                                    GRB.LESS_EQUAL,
                                    placeEffects[i, step],
                                    "blockCanBeFired_step_" + step.ToString() + "_name_" + place.Name.Truncate(100));
                }
            }
        }

        private static GRBLinExpr[,] Unroll_ImposePlacesNonnegative(PetriNet net, Place initialPlace, int initialBudget, GRBModel model, int runLength, GRBVar[,] transitionVars)
        {
            // i, j is the place effect on place i before step j is fired
            GRBLinExpr[,] placeEffectsBeforeStep = new GRBLinExpr[net.Places.Count, runLength + 1];
            for (int i = 0; i < net.Places.Count; i++)
            {
                Place place = net.Places[i];
                placeEffectsBeforeStep[i, 0] = new GRBLinExpr(place.Equals(initialPlace) ? initialBudget : 0);
                for (int step = 1; step < runLength + 1; step++)
                {
                    GRBLinExpr placeEffectBeforePrevStep = placeEffectsBeforeStep[i, step - 1];
                    GRBLinExpr placeEffectBeforeStep = new GRBLinExpr(placeEffectBeforePrevStep);


                    for (int j = 0; j < net.Transitions.Count; j++)
                    {
                        UpdateTransition transition = (UpdateTransition)net.Transitions[j];
                        int effect = transition.Post.GetValueOrDefault(place, 0) - transition.Pre.GetValueOrDefault(place, 0);
                        if (effect != 0)
                        {
                            placeEffectBeforeStep.AddTerm(effect, transitionVars[j, step - 1]);
                        };
                    }

                    placeEffectsBeforeStep[i, step] = placeEffectBeforeStep;
                    model.AddConstr(placeEffectBeforeStep,
                                    GRB.GREATER_EQUAL,
                                    0,
                                    "placeNonnegative_Step_" + step.ToString() + "_name_" + place.Name.Truncate(150));

                }
            }
            return placeEffectsBeforeStep;
        }

        private static GRBVar[,] Unroll_CreateTransitionVars(PetriNet net, GRBModel model, int runLength, char variableDomain)
        {
            GRBVar[,] transitionVars = new GRBVar[net.Transitions.Count, runLength];

            {
                for (int step = 0; step < runLength; step++)
                {
                    int i = 0;
                    foreach (Transition t in net.Transitions)
                    {
                        transitionVars[i, step] = model.AddVar(0,
                                                         Utils.GurobiConsts.UpperBound,
                                                         0,
                                                         variableDomain,
                                                         "transitionVariable_" + step.ToString() + "_" + t.Name.Truncate(100));
                        i += 1;
                    }
                }
            }

            return transitionVars;
        }

        public static double Compute_A_n(PetriNet net,
                                        Place initialPlace, Place finalPlace)
        {
            // execution should be at most bound*numTransitions long
            //Check whether from zero marking, the zero marking can be covered
            GRBModel model = InitializeModel();

            GRBVar[] transitionVars = new GRBVar[net.Transitions.Count];
            GRBLinExpr transitionCount = new GRBLinExpr();

            {
                int i = 0;

                foreach (Transition t in net.Transitions)
                {
                    transitionVars[i] = model.AddVar(0,
                                                     Utils.GurobiConsts.UpperBound,
                                                     0,
                                                     GRB.CONTINUOUS,
                                                     "transitionVariable_" + t.Name.Truncate(100));
                    transitionCount.AddTerm(1, transitionVars[i]);
                    i += 1;
                }
            }

            GRBConstr[] effectNonnegative = new GRBConstr[net.Places.Count];
            {
                for (int i = 0; i < net.Places.Count; i++)
                {
                    Place place = net.Places[i];
                    GRBLinExpr placeEffect = new GRBLinExpr();

                    for (int j = 0; j < net.Transitions.Count; j++)
                    {
                        UpdateTransition transition = (UpdateTransition)net.Transitions[j];
                        int effect = transition.Post.GetValueOrDefault(place, 0) - transition.Pre.GetValueOrDefault(place, 0);
                        if (effect != 0)
                        {
                            placeEffect.AddTerm(effect, transitionVars[j]);
                        };
                    }

                    model.AddConstr(placeEffect,
                                    GRB.GREATER_EQUAL,
                                    place.Equals(initialPlace) ? -1 : 0, // initial place starts with 1 token
                                    "placeEffectNonnegative_" + place.Name.Truncate(200));
                }
            }

            model.SetObjective(transitionCount, GRB.MAXIMIZE);

            // model.Write("../../../Gurobi.lp");
            model.Optimize();
            if (model.Status == GRB.Status.UNBOUNDED || model.Status == GRB.Status.INF_OR_UNBD)
            // never infeasible unless no transition consumes only from initial => unbounded should be guaranteed
            {
                return -1.0;
            }
            else if (model.Status == GRB.Status.INFEASIBLE)
            {
                throw new Exception("Searching for A_n, but LP is infeasible: model has no transition consuming only from the initial place");
            }
            else
            {
                return model.Get(GRB.DoubleAttr.ObjVal);
            }
        }

        public static (bool isBounded, Dictionary<Transition, double> counterexample) CheckForNonnegativeCycle(PetriNet net)
        {
            //Check whether from zero marking, the zero marking can be covered
            GRBModel model = InitializeModel();

            GRBVar[] transitionVars = new GRBVar[net.Transitions.Count];

            {
                int i = 0;
                foreach (Transition t in net.Transitions)
                {
                    transitionVars[i] = model.AddVar(0, Utils.GurobiConsts.UpperBound, 0, GRB.CONTINUOUS, "transitionVariable_" + t.Name.Truncate(100));
                    i += 1;
                }
            }
            GRBLinExpr transitionSum = new GRBLinExpr();
            transitionSum.AddTerms(transitionVars.Select(_ => 1.0).ToArray(), transitionVars);

            model.AddConstr(transitionSum, '>', 1, "Cycle nonempty");
            {
                foreach (Place p in net.Places)
                {
                    GRBLinExpr placeEffect = new GRBLinExpr();
                    int j = 0;
                    foreach (UpdateTransition t in net.Transitions)
                    {
                        placeEffect.AddTerm(t.GetPrePostDifference().GetValueOrDefault(p), transitionVars[j]);
                        j += 1;
                    }
                    model.AddConstr(placeEffect, '>', 0.0, "Effect nonnegative " + p.Name.Truncate(200));
                }
            }


            // model.ModelSense = GRB.MINIMIZE;
            // // model.SetObjective(objective, GRB.MINIMIZE);

            // model.Write("gurobi.lp");

            model.Optimize();
            if (model.Status != GRB.Status.OPTIMAL && model.Status != GRB.Status.SUBOPTIMAL)
            {
                return (true, null);
            }
            else
            {
                double[] transitionMults = model.Get(GRB.DoubleAttr.X, transitionVars);
                Dictionary<Transition, double> result = new Dictionary<Transition, double>();
                for (int i = 0; i < net.Transitions.Count; i++)
                {
                    double transitionMult = transitionMults[i];
                    if (transitionMult == 0.0)
                    {
                        continue;
                    }
                    result.Add(net.Transitions[i], transitionMult);
                }
                return (false, result);
            }
        }

        // computes, for each transition, the min number of times the transition is used among continuous pseudo-runs where that transition is the most used one
        // in the result, having an empty map for a transition the lp for that transition returned "unreachable"
        public static Dictionary<Transition, Dictionary<Transition, double>> ComputeTransitionBottlenecks(PetriNet net, Place initialPlace, Place finalPlace)
        {
            Dictionary<Transition, Dictionary<Transition, double>> result = new Dictionary<Transition, Dictionary<Transition, double>>();

            //Check whether from zero marking, the zero marking can be covered
            GRBModel model = InitializeModel();

            GRBVar[] transitionVars = new GRBVar[net.Transitions.Count];

            {
                int i = 0;
                foreach (Transition t in net.Transitions)
                {
                    transitionVars[i] = model.AddVar(0, Utils.GurobiConsts.UpperBound, 0, GRB.CONTINUOUS, "transitionVariable_" + t.Name.Truncate(100));
                    i += 1;
                }
            }

            // ensure effect is correct: effect for initial is -1, for final is +1, and for others is 0
            {
                foreach (Place p in net.Places)
                {
                    GRBLinExpr placeEffect = new GRBLinExpr();
                    int j = 0;
                    foreach (UpdateTransition t in net.Transitions)
                    {
                        placeEffect.AddTerm(t.GetPrePostDifference().GetValueOrDefault(p), transitionVars[j]);
                        j += 1;
                    }
                    string constraintName;
                    double targetValue;
                    if (p.Equals(initialPlace))
                    {
                        constraintName = "initial_effect_correct";
                        targetValue = -1.0;
                    }
                    else if (p.Equals(finalPlace))
                    {
                        constraintName = "final_effect_correct";
                        targetValue = 1.0;
                    }
                    else
                    {
                        constraintName = p.Name.Truncate(200) + "_effect_correct";
                        targetValue = 0.0;
                    }
                    model.AddConstr(placeEffect, '=', targetValue, constraintName);
                }
            }

            // ensure one transition is used at least as much as all others (that one transition will be picked from all transitions later, so that each transition gets a turn)
            GRBConstr[] transitionMaximalConstraints = new GRBConstr[net.Transitions.Count];
            GRBVar placeHolderTransitionVar = transitionVars[0];
            for (int i = 0; i < net.Transitions.Count; i++)
            {
                transitionMaximalConstraints[i] = model.AddConstr(placeHolderTransitionVar, GRB.GREATER_EQUAL, transitionVars[i], "transition_greater_" + net.Transitions[i].Name.Truncate(200));

            }

            // for each transition, compute the bottleneck
            for (int curTransitionNum = 0; curTransitionNum < net.Transitions.Count; curTransitionNum++)
            {
                // adjust the maximal constraints to have this transition on left-hand-side
                for (int i = 0; i < net.Transitions.Count; i++)
                {
                    GRBConstr constraint = transitionMaximalConstraints[i];
                    model.Remove(constraint);
                    // essentially copy the constraint, but change the lhs to match the curTransition
                    transitionMaximalConstraints[i] = model.AddConstr(transitionVars[curTransitionNum], GRB.GREATER_EQUAL, transitionVars[i], "transition_greater_" + net.Transitions[i].Name.Truncate(200));
                }
                model.SetObjective(
                    new GRBLinExpr(transitionVars[curTransitionNum], 1.0),
                    GRB.MINIMIZE
                );
                // model.Write("gurobi-transitionbottlenecks.lp");
                model.Optimize();
                if (model.Status != GRB.Status.OPTIMAL && model.Status != GRB.Status.SUBOPTIMAL)
                {
                    result.Add(net.Transitions[curTransitionNum], new Dictionary<Transition, double>());
                }
                else
                {
                    double[] transitionMults = model.Get(GRB.DoubleAttr.X, transitionVars);
                    Dictionary<Transition, double> multsDict = new Dictionary<Transition, double>();
                    for (int j = 0; j < net.Transitions.Count; j++)
                    {
                        double transitionMult = transitionMults[j];
                        if (transitionMult > 0)
                        {
                            multsDict.Add(net.Transitions[j], transitionMult);
                        }
                    }
                    result.Add(net.Transitions[curTransitionNum], multsDict);
                }
            }

            return result;
        }

        public static (bool transitionMultExceeded, Dictionary<Transition, double> counterexample) CheckTransitionMults(PetriNet net, Place initialPlace, int count)
        {
            //Check whether from zero marking, the zero marking can be covered
            GRBModel model = InitializeModel();

            GRBVar[] transitionVars = new GRBVar[net.Transitions.Count];

            {
                int i = 0;
                foreach (Transition t in net.Transitions)
                {
                    transitionVars[i] = model.AddVar(0, Utils.GurobiConsts.UpperBound, 0, GRB.INTEGER, "transitionVariable_" + t.Name.Truncate(100));
                    i += 1;
                }
            }
            GRBLinExpr transitionSum = new GRBLinExpr();
            transitionSum.AddTerms(transitionVars.Select(_ => 1.0).ToArray(), transitionVars);
            model.AddConstr(transitionSum, GRB.GREATER_EQUAL, 1, "Run_nonempty");

            GRBLinExpr[] placeEffects = new GRBLinExpr[net.Places.Count];


            {
                int i = 0;
                foreach (Place p in net.Places)
                {
                    GRBLinExpr placeEffect = new GRBLinExpr();
                    int j = 0;
                    foreach (UpdateTransition t in net.Transitions)
                    {
                        placeEffect.AddTerm(t.GetPrePostDifference().GetValueOrDefault(p), transitionVars[j]);
                        j += 1;
                    }
                    placeEffects[i] = placeEffect;
                    i += 1;
                    // initial place can have negative efefct
                    if (!p.Equals(initialPlace))
                    {
                        model.AddConstr(placeEffect, '>', 0.0, "Effect nonnegative " + p.Name.Truncate(200));
                    }
                }
            }

            GRBVar maxMult = model.AddVar(0, GRB.INFINITY, 0, GRB.INTEGER, "max_transition_mult_var");
            model.AddGenConstrMax(maxMult, transitionVars, 0, "max_transition_mult_assignment");
            model.AddConstr((count + 1) * -placeEffects[net.Places.IndexOf(initialPlace)],
                            GRB.LESS_EQUAL,
                            maxMult,
                            "is_any_transition_used_more_than_count_times_k?");

            // model.Write("gurobi_transitionmult" + count.ToString() + ".lp");

            model.Optimize();
            if (model.Status != GRB.Status.OPTIMAL && model.Status != GRB.Status.SUBOPTIMAL)
            {
                return (false, null);
            }
            else
            {
                double[] transitionMults = model.Get(GRB.DoubleAttr.X, transitionVars);
                Dictionary<Transition, double> result = new Dictionary<Transition, double>();
                for (int i = 0; i < net.Transitions.Count; i++)
                {
                    double transitionMult = transitionMults[i];
                    if (transitionMult == 0.0)
                    {
                        continue;
                    }
                    result.Add(net.Transitions[i], transitionMult);
                }
                return (true, result);
            }
        }


        public static Func<Place, float?> InitializePlaceToStructuralQReachabilityDistance(
            List<Place> places,
            List<Transition> transitions,
            Marking initialMarking,
            List<MarkingWithConstraints> targetMarkings,
            Domains domain
        )
        {
            char gurobiDomain = EvaluateDomain(domain);

            GRBModel model = InitializeModel();

            GRBVar[] initialPlaceVars = GeneratePlaceMarkingVars(places, gurobiDomain, model, namePrefix: "initialMarking_");
            GRBVar[] finalPlaceVars = GeneratePlaceMarkingVars(places, gurobiDomain, model, namePrefix: "finalMarking_");
            GRBVar[] intermediatePlaceVars = GeneratePlaceMarkingVars(places, gurobiDomain, model, namePrefix: "intermediateMarking_");

            GRBVar[] prefixTransitionVars = CreateTransitionTimesFiredVars(transitions, gurobiDomain, model, "prefixFired_");
            GRBVar[] suffixTransitionVars = CreateTransitionTimesFiredVars(transitions, gurobiDomain, model, "suffixFired_");


            InitializeMarkingConstraints(places, model, initialPlaceVars, initialMarking, "initialMarkingConstraint_");
            GRBConstr[] finalMarkingConstraints = InitializeMarkingConstraints(places,
                                                                               model,
                                                                               finalPlaceVars,
                                                                               "finalMarkingConstraint_");
            GRBConstr[] intermediateMarkingConstraints = InitializeMarkingConstraints(places,
                                                                                      model,
                                                                                      intermediatePlaceVars,
                                                                                      "intermediateMarkingConstraint_");

            GenerateMarkingEquationConstraints(places, transitions, model, prefixTransitionVars, initialPlaceVars, intermediatePlaceVars, "prefixConstraint_");
            GenerateMarkingEquationConstraints(places, transitions, model, suffixTransitionVars, intermediatePlaceVars, finalPlaceVars, "suffixConstraint_");

            GRBLinExpr optimizationObjective = CreateVariableSumExpression(suffixTransitionVars);

            model.SetObjective(optimizationObjective, GRB.MINIMIZE);

            float? calculateDistanceToTargetMarkingViaPlace(Place place, MarkingWithConstraints targetMarking)
            {
                SetMarkingConstraintsRightHandSides(places, finalMarkingConstraints, targetMarking);

                Marking intermediateMarking = new Marking(places.ToDictionary(p => p, p => 0));
                intermediateMarking[place] = 1;

                Constraints constraints = new Constraints(places.ToDictionary(p => p, p => ConstraintOperators.GreaterEqual));
                MarkingWithConstraints intermediateConstrainedMarking = new MarkingWithConstraints(intermediateMarking, constraints);
                SetMarkingConstraintsRightHandSides(places, intermediateMarkingConstraints, intermediateConstrainedMarking);

                model.Optimize();


                if (model.Status != GRB.Status.OPTIMAL && model.Status != GRB.Status.SUBOPTIMAL)
                {
                    return null;
                }
                else
                {
                    return (float)model.ObjVal;
                }

            }

            float? placeDistance(Place place)
            {
                return targetMarkings.Min(marking => calculateDistanceToTargetMarkingViaPlace(place, marking));
            }

            return placeDistance;
        }

        private static void SetMarkingConstraintsRightHandSides(List<Place> places, GRBConstr[] markingConstraints, MarkingWithConstraints targetMarking)
        {
            for (int i = 0; i < places.Count; i++)
            {
                Place place = places[i];
                GRBConstr constraint = markingConstraints[i];
                int tokenAmount = targetMarking.Marking[place];
                char sense = targetMarking.Constraints[place] == ConstraintOperators.Equal ? GRB.EQUAL : GRB.GREATER_EQUAL;

                constraint.Set(GRB.DoubleAttr.RHS, (double)tokenAmount);
                constraint.Set(GRB.CharAttr.Sense, sense);
            }
        }

        public static Func<Marking, float?> InitializeMarkingEquationHeuristic(
            List<Place> places,
            List<Transition> transitions,
            List<MarkingWithConstraints> targetMarkings,
            Domains domain = Domains.Q,
            bool optimizeSize = true)
        {
            (GRBModel[] targetsToModels, GRBConstr[][] targetsToInitialConstraints, GRBVar[][] targetsToTransitionVars) = GenerateModelsForTargets(places, transitions, targetMarkings, domain, optimizeSize);

            Marking prevMarking = new Marking();

            // model is prepared, now we are prepared to return the actual heuristic function
            float? MarkingEquationHeuristic(Marking marking)
            {
                float?[] scores = new float?[targetMarkings.Count];
                for (int i = 0; i < targetMarkings.Count; i++)
                {
                    MarkingWithConstraints targetMarking = targetMarkings[i];
                    GRBModel model = targetsToModels[i];

                    for (int i1 = 0; i1 < places.Count; i1++)
                    {
                        Place p = places[i1];
                        if (prevMarking.GetValueOrDefault(p) == marking.GetValueOrDefault(p))
                        {
                            continue;
                        }
                        int value = marking.GetValueOrDefault(p, 0);
                        targetsToInitialConstraints[i][i1].Set(GRB.DoubleAttr.RHS, (double)value);
                    }

                    prevMarking = marking;

                    model.Optimize();

                    if (model.Status != GRB.Status.OPTIMAL && model.Status != GRB.Status.SUBOPTIMAL)
                    {
                        scores[i] = null;
                        i++;
                        continue;
                    }
                    else
                    {
                        scores[i] = (float)model.ObjVal;
                        i++;
                        continue;
                    }
                }
                return scores.Min();
            }

            return MarkingEquationHeuristic;
        }

        public static Func<Marking, Tuple<IEnumerable<Transition>, float?>> InitializeMarkingEquationTransitionSupportComputation(
            List<Place> places, List<Transition> transitions, List<MarkingWithConstraints> targetMarkings, Domains domain = Domains.Q
            )
        {
            (GRBModel[] targetsToModels, GRBConstr[][] targetsToInitialConstraints, GRBVar[][] targetsToTransitionVars) = GenerateModelsForTargets(places, transitions, targetMarkings, domain);

            foreach (GRBModel model in targetsToModels)
            {
                // get the best solutions in a systematic way
                model.Parameters.PoolSearchMode = 2;

                // find solutions up to this*100% longer, i.e. 0.2 means if optimal solution has length 100, we discard solutions with length 121 or more
                model.Parameters.PoolGap = 0.01;

                model.Parameters.PoolSolutions = 20;
            }

            Tuple<IEnumerable<Transition>, float?> NMarkingEquationHeuristic(Marking marking)
            {
                HashSet<Transition>[] usedTransitions = new HashSet<Transition>[targetMarkings.Count];
                float?[] distanceEstimations = new float?[targetMarkings.Count];

                for (int i = 0; i < targetMarkings.Count; i++)
                {
                    MarkingWithConstraints targetMarking = targetMarkings[i];
                    GRBModel model = targetsToModels[i];
                    GRBVar[] transitionVars = targetsToTransitionVars[i];

                    for (int i1 = 0; i1 < places.Count; i1++)
                    {
                        Place p = places[i1];
                        targetsToInitialConstraints[i][i1].Set(GRB.DoubleAttr.RHS, (double)marking.GetValueOrDefault(p, 0));
                    }

                    model.Optimize();
                    if (model.Status != GRB.Status.OPTIMAL && model.Status != GRB.Status.SUBOPTIMAL)
                    {
                        usedTransitions[i] = null;
                        distanceEstimations[i] = null;
                    }
                    else
                    {
                        // Console.WriteLine(model.SolCount);
                        usedTransitions[i] = new HashSet<Transition>();
                        distanceEstimations[i] = (float)model.ObjVal;
                        for (int solutionNumber = 0; solutionNumber < model.SolCount; solutionNumber++)
                        {
                            // Console.WriteLine("Solution Number: " + solutionNumber + " has obj val: " + model.PoolObjVal);
                            model.Parameters.SolutionNumber = solutionNumber;
                            Dictionary<Transition, double> transitionMultiset = ExtractTransitionMultiset(transitions, model, transitionVars);
                            // Console.WriteLine(String.Join("|", transitionMultiset.Select(tuple => tuple.Item1.Name + ": " + tuple.Item2)));

                            GRBVar[] finalMarkingVars = new GRBVar[places.Count];
                            for (int placeIndex = 0; placeIndex < places.Count; placeIndex++)
                            {
                                Place place = places[placeIndex];
                                String varname = "finalMarkingAfterTransfers_" + place.Name.Truncate(100);
                                GRBVar placeVar = model.GetVarByName(varname);
                                finalMarkingVars[placeIndex] = placeVar;
                            }
                            Marking finalMarking = ExtractMarking(places, model, finalMarkingVars);
                            // Console.WriteLine("Final Marking: " + finalMarking.ToString());
                            // Console.WriteLine("------------------------------------");
                            usedTransitions[i].UnionWith(transitionMultiset.Keys.ToHashSet());
                        }
                    }
                }
                return new Tuple<IEnumerable<Transition>, float?>(usedTransitions.Aggregate(new HashSet<Transition>(), (acc, set) =>
                {
                    if (set != null)
                    {
                        acc.UnionWith(set);
                    }
                    return acc;
                }), distanceEstimations.Min());
            }

            return NMarkingEquationHeuristic;
        }

        public static HashSet<Transition> ComputeTransitionSupportForMarkingWithOneTarget(
            List<Place> places, List<Transition> transitions, MarkingWithConstraints targetMarking, Marking initialMarking, float solutionLengthBound
            )
        {
            char gurobiDomain = GRB.INTEGER;

            GRBModel model = InitializeModel();

            // Constraints to ensure solution satisfies initial + effect = target
            GRBVar[] initialPlaceVars = GeneratePlaceMarkingVars(places, gurobiDomain, model, namePrefix: "initialMarking_");
            GRBVar[] finalPlaceVars = GeneratePlaceMarkingVars(places, gurobiDomain, model, namePrefix: "finalMarking_");
            GRBVar[] transitionVars = CreateTransitionTimesFiredVars(transitions, gurobiDomain, model, "transitionFired_");

            InitializeMarkingConstraints(places, model, initialPlaceVars, initialMarking, "initialMarkingConstraint_");
            GRBConstr[] finalMarkingConstraints = InitializeMarkingConstraints(places,
                                                                               model,
                                                                               finalPlaceVars,
                                                                                targetMarking,
                                                                               "finalMarkingConstraint_");

            GenerateMarkingEquationConstraints(places, transitions, model, transitionVars, initialPlaceVars, finalPlaceVars, "markingEquationConstraint_");

            // Additional indicator variables for support
            GRBVar[] transitionSupportVars = new GRBVar[transitions.Count];
            for (int i = 0; i < transitions.Count; i++)
            {
                Transition transition = transitions[i];
                GRBVar transitionSupportVariable = model.AddVar(0, 1, GurobiConsts.ObjectiveValue, Gurobi.GRB.BINARY, "supportIndicator_" + transition.Name);
                model.AddGenConstrIndicator(transitionSupportVariable, 1, transitionVars[i], GRB.GREATER_EQUAL, 1,
                    "indicatorTrue->TransitionUsed_" + transition.Name);
                model.AddGenConstrIndicator(transitionSupportVariable, 0, transitionVars[i], GRB.EQUAL, 0,
                    "indicatorFalse->TransitionUnused_" + transition.Name);
                transitionSupportVars[i] = transitionSupportVariable;
            }

            GRBLinExpr supportSize = CreateVariableSumExpression(transitionSupportVars);
            model.SetObjective(supportSize, GRB.MAXIMIZE);

            GRBLinExpr solutionLength = CreateVariableSumExpression(transitionVars);

            // Constrain length of obtained solutions
            model.AddConstr(solutionLength, GRB.LESS_EQUAL, solutionLengthBound, "solution length respects the upper bound");

            model.Optimize();


            if (model.Status != GRB.Status.OPTIMAL && model.Status != GRB.Status.SUBOPTIMAL)
            {
                return null;
            }
            else
            {
                double[] support = model.Get(GRB.DoubleAttr.X, transitionSupportVars);
                HashSet<Transition> transitionsInSupport = transitions.Where((transition, index) =>
                {
                    return support[index] > 0.5;
                }).ToHashSet();
                return transitionsInSupport;
            }
        }

        private static Tuple<GRBModel[], GRBConstr[][], GRBVar[][]> GenerateModelsForTargets(List<Place> places, List<Transition> transitions, List<MarkingWithConstraints> targetMarkings, Domains domain, bool optimizeLength = true)
        {
            GRBModel[] targetsToModels = new GRBModel[targetMarkings.Count];
            GRBConstr[][] targetsToInitialConstraints = new GRBConstr[targetMarkings.Count][];
            GRBVar[][] targetsToTransitionVars = new GRBVar[targetMarkings.Count][];

            for (int i = 0; i < targetMarkings.Count; i++)
            {
                MarkingWithConstraints targetMarking = targetMarkings[i];
                (GRBModel model, GRBConstr[] initialConstraints, GRBVar[] transitionVars) = GenerateMarkingEquationModel(places, transitions, targetMarking, domain);
                targetsToModels[i] = model;
                targetsToInitialConstraints[i] = initialConstraints;
                targetsToTransitionVars[i] = transitionVars;
            }

            return new Tuple<GRBModel[], GRBConstr[][], GRBVar[][]>(targetsToModels, targetsToInitialConstraints, targetsToTransitionVars);
        }

        private static Tuple<GRBModel, GRBConstr[], GRBVar[]> GenerateMarkingEquationModel(List<Place> places, List<Transition> transitions, MarkingWithConstraints targetMarking, Domains domain, bool optimizeLength = true)
        {
            char gurobiDomain = EvaluateDomain(domain);
            GRBModel model = InitializeModel();

            // final marking vars should not change
            // Dictionary<Place, GRBVar> finalMarkingVars = new Dictionary<Place, GRBVar>();

            // create update transition vars
            GRBVar[] transitionTimesFiredVars = CreateTransitionTimesFiredVars(transitions, gurobiDomain, model);

            List<TransferTransition> transferTransitions = transitions.Where(t => t is TransferTransition).Cast<TransferTransition>().ToList();

            if (optimizeLength)
            {
                // create objective: minimize t1+t2+t3+...
                GRBLinExpr optimizationObjective = CreateVariableSumExpression(transitionTimesFiredVars);

                model.SetObjective(optimizationObjective, GRB.MINIMIZE);
            }

            // create place constraints and vars:
            GRBVar[] initialMarkingVars = GeneratePlaceMarkingVars(places, gurobiDomain, model, namePrefix: "initialMarking_");

            GRBVar[] finalMarkingVarsAfterTransfers = GeneratePlaceMarkingVars(places, gurobiDomain, model, namePrefix: "finalMarkingAfterTransfers_");

            if (transferTransitions.Count > 0)
            {
                GRBVar[] markingVarsAfterNormalTransitions = GeneratePlaceMarkingVars(places, gurobiDomain, model, namePrefix: "finalMarking_");
                GenerateMarkingEquationConstraints(places, transitions, model, transitionTimesFiredVars, initialMarkingVars, markingVarsAfterNormalTransitions);
                // create transfer transition vars
                GRBVar[] transferTransitionIndicatorVars = null;
                List<GRBVar> transferTransitionTimesFiredVars = transitionTimesFiredVars.Where((var, index) => transitions[index] is TransferTransition).ToList();
                transferTransitionIndicatorVars = GurobiTransfers.CreateTransferTransitionsIndicatorVars(transferTransitions, transferTransitionTimesFiredVars, model);
                GurobiTransfers.GenerateTransferConstraints(places, transferTransitions, domain, model, transferTransitionIndicatorVars, markingVarsAfterNormalTransitions, finalMarkingVarsAfterTransfers);
            }
            else
            {
                GenerateMarkingEquationConstraints(places, transitions, model, transitionTimesFiredVars, initialMarkingVars, finalMarkingVarsAfterTransfers);
            }

            InitializeMarkingConstraints(places, model, finalMarkingVarsAfterTransfers, targetMarking, "finalMarkingConstraint_");

            // pX_init = a       <-- initial marking constraint, starts uninitialized (i.e. a=0) and is modified in later calls
            GRBConstr[] initialMarkingConstraints =
                GenerateMarkingConstraints(
                    places,
                    model,
                    initialMarkingVars,
                    initial: true);

            return new Tuple<GRBModel, GRBConstr[], GRBVar[]>(model, initialMarkingConstraints, transitionTimesFiredVars);
        }

        /// <summary>
        /// Uses the marking equation over the domain specified by <paramref name="domain"/>  to estimate the maximal number of tokens
        /// obtainable in any place via a path of length at most <paramref name="pathLengthBound"/>.
        /// </summary>
        /// <param name="net">A Petri net.</param>
        /// <param name="initialMarking">The initial marking at which paths start.</param>
        /// <param name="pathLengthBound">An upper bound on the length of paths to consider.</param>
        /// <param name="domain">Either <c>domains.N</c> or <c>domains.Q</c>. Determines the domain of the transition variables in the marking equation
        /// utilized for this.</param>
        /// <returns>A dictionary mapping each place to the maximal number of tokens in any place after at most <paramref name="pathLengthBound"/> steps.</returns>
        public static Dictionary<Place, double> GetPlaceBoundsViaMarkingEquation(
            List<Place> places,
            List<Transition> transitions,
            Marking initialMarking,
            double pathLengthBound,
            Domains domain = Domains.Q
            )
        {
            char gurobiDomain = EvaluateDomain(domain);
            GRBModel model = InitializeModel();

            // final marking vars should not change
            // Dictionary<Place, GRBVar> finalMarkingVars = new Dictionary<Place, GRBVar>();

            // create transition vars
            GRBVar[] transitionTimesFiredVars = CreateTransitionTimesFiredVars(transitions, gurobiDomain, model);

            // create place constraints and vars:
            GRBVar[] initialMarkingVars = GeneratePlaceMarkingVars(places, gurobiDomain, model, namePrefix: "initialMarking_");
            GRBVar[] finalMarkingVars = GeneratePlaceMarkingVars(places, gurobiDomain, model, namePrefix: "finalMarking_");

            GRBVar largestPlaceVar = model.AddVar(LowerBound, UpperBound, ObjectiveValue, GRB.CONTINUOUS, "largestPlaceMarking");

            // largestPlaceMarking is the maximum among marked places in the path
            GRBGenConstr largestPlaceMaxConstraint = model.AddGenConstrMax(largestPlaceVar, finalMarkingVars, 0.0, "largestPlaceMarking is maximum of place markings");

            GRBLinExpr transitionSum = CreateVariableSumExpression(transitionTimesFiredVars);
            GRBConstr pathShorterThanBoundConstraint = model.AddConstr(transitionSum, GRB.LESS_EQUAL, new GRBLinExpr(pathLengthBound), "Constraint: sum of transition multiplicities must be less or equal to the path bound");

            // create objective: maximize largest #tokens in all places
            GRBLinExpr optimizationObjective = new GRBLinExpr(largestPlaceVar);
            model.SetObjective(optimizationObjective, GRB.MAXIMIZE);


            GenerateMarkingEquationConstraints(places, transitions, model, transitionTimesFiredVars, initialMarkingVars, finalMarkingVars);

            InitializeMarkingConstraints(places, model, initialMarkingVars, initialMarking, "Initial Marking Constraint_");

            model.Optimize();
            double bound = 0.0;

            if (model.Status != GRB.Status.OPTIMAL && model.Status != GRB.Status.SUBOPTIMAL)
            {
                throw new Exception("Gurobi could not find a solution, have a look at the model to find out why.");
            }
            else
            {
                bound = model.ObjVal;
            }
            return places.ToDictionary(place => place, place => bound);
        }

        public static GRBModel InitializeModel()
        {
            GRBEnv env = new GRBEnv(true);
            // env.LogFile = "gurobi_env.log";
            env.LogToConsole = 0;
            env.Start();
            GRBModel model = new GRBModel(env);
            model.Parameters.LogToConsole = 0;
            // model.Parameters.LogFile = "gurobi.log";
            return model;
        }

        private static Dictionary<Transition, double> ExtractTransitionMultiset(List<Transition> transitions, GRBModel model, GRBVar[] transitionVars)
        {
            double[] transitionMults = model.Get(GRB.DoubleAttr.Xn, transitionVars);
            Dictionary<Transition, double> result = new Dictionary<Transition, double>();
            for (int i = 0; i < transitions.Count; i++)
            {
                double transitionMult = transitionMults[i];
                if (transitionMult == 0.0)
                {
                    continue;
                }
                result.Add(transitions[i], transitionMult);
            }
            return result;
        }

        private static Marking ExtractMarking(List<Place> places, GRBModel model, GRBVar[] placeVars)
        {
            Marking result = new Marking();
            for (int i = 0; i < places.Count; i++)
            {
                var placeVar = placeVars[i];
                result[places[i]] = (int)placeVars[i].Xn;
            }
            return result;
        }

        private static List<Transition> ExtractTransitionSequence(List<Transition> transitions, GRBModel model, GRBVar[][] transitionVarsPerTimeStep)
        {
            List<Transition> result = new List<Transition>();
            for (int step = 0; step < transitionVarsPerTimeStep.Count(); step++)
            {
                double[] transitionMults = model.Get(GRB.DoubleAttr.Xn, transitionVarsPerTimeStep[step]);
                int firedTransitionIndex = Array.IndexOf(transitionMults, 1.0);
                result.Add(transitions[firedTransitionIndex]);
            }
            return result;
        }


        public static Func<Marking, float?> InitializeMarkingEquationHeuristicForBackwardsCoverability(
            List<Place> places,
            List<Transition> transitions,
            Marking initialMarking,
            Domains domain = Domains.Q)
        {
            Stopwatch completeWatch = Stopwatch.StartNew();
            Stopwatch partWatch = new Stopwatch();

            char gurobiDomain = EvaluateDomain(domain);
            GRBModel model = InitializeModel();


            GRBVar[] transitionTimesFiredVars = CreateTransitionTimesFiredVars(transitions, gurobiDomain, model);

            GRBLinExpr optimizationObjective = CreateVariableSumExpression(transitionTimesFiredVars);

            model.SetObjective(optimizationObjective, GRB.MINIMIZE);

            // create place constraints and vars:
            var finalMarkingVars = GeneratePlaceMarkingVars(places, gurobiDomain, model, namePrefix: "finalMarking_");

            partWatch.Start();
            GenerateMarkingEquationConstraintsBackwardsCoverability(
                places,
                transitions,
                initialMarking,
                model,
                transitionTimesFiredVars,
                finalMarkingVars);
            partWatch.Stop();

            // final marking constraints are left uninitialized for now
            GRBConstr[] finalMarkingConstraints = GenerateMarkingConstraints(
                places,
                model,
                finalMarkingVars,
                initial: false);

            float? MarkingEquationBackwardsHeuristic(Marking marking)
            {
                for (int i = 0; i < places.Count; i++)
                {
                    Place p = places[i];
                    finalMarkingConstraints[i].Set(GRB.DoubleAttr.RHS, (double)marking.GetValueOrDefault(p, 0));
                }

                model.Optimize();

                if (model.Status != GRB.Status.OPTIMAL && model.Status != GRB.Status.SUBOPTIMAL)
                {
                    return null;
                }
                else
                {
                    return (float)model.ObjVal;
                }
            }
            completeWatch.Stop();

            return MarkingEquationBackwardsHeuristic;

        }

        private static GRBConstr[] GenerateMarkingConstraints(List<Place> places,
                                                              GRBModel model,
                                                              GRBVar[] placeMarkingVars,
                                                              bool initial)
        {
            GRBConstr[] initialMarkingConstraints = new GRBConstr[places.Count];
            for (int i = 0; i < places.Count; i++)
            {
                Place place = places[i];
                GRBConstr initialMarkingConstraint = model.AddConstr(
                    placeMarkingVars[i],
                    '=',
                    0,
                    (initial ? "_initialMarkingConstraint" : "_finalMarkingConstraint") + place.Name.Truncate(100));
                initialMarkingConstraints[i] = initialMarkingConstraint;
            }
            return initialMarkingConstraints;
        }

        public static GRBLinExpr CreateVariableSumExpression(GRBVar[] variables)
        {
            GRBLinExpr expression = new GRBLinExpr();

            double[] coefficients = new double[variables.Count()];
            System.Array.Fill(coefficients, 1.0);

            expression.AddTerms(coefficients, variables);
            return expression;
        }

        public static GRBVar[] CreateTransitionTimesFiredVars(List<Transition> transitions, char gurobiDomain, GRBModel model, string namePrefix = "transitionTimesFired_")
        {
            GRBVar[] transitionTimesFiredVars = new GRBVar[transitions.Count];
            for (int i = 0; i < transitions.Count; i++)
            {
                Transition t = transitions[i];
                GRBVar transitionTimesFired = model.AddVar(LowerBound, UpperBound, ObjectiveValue,
                    gurobiDomain, namePrefix + t.Name.Truncate(180));
                transitionTimesFiredVars[i] = transitionTimesFired;
            }
            return transitionTimesFiredVars;
        }

        private static GRBConstr[] InitializeMarkingConstraints(List<Place> places,
                GRBModel model,
                GRBVar[] placeVars,
                MarkingWithConstraints marking,
                string namePrefix)
        {
            GRBConstr[] constraints = new GRBConstr[places.Count];
            for (int i = 0; i < places.Count; i++)
            {
                Place place = places[i];
                GRBVar placeVar = placeVars[i];
                int tokenAmount = marking.Marking.GetValueOrDefault(place, 0);
                char sense = marking.Constraints.GetValueOrDefault(place, ConstraintOperators.GreaterEqual) == ConstraintOperators.Equal ? GRB.EQUAL : GRB.GREATER_EQUAL;
                GRBLinExpr leftHandSide = new GRBLinExpr(placeVar, 1d);
                GRBConstr constr = model.AddConstr(leftHandSide, sense, tokenAmount, namePrefix + place.Name.Truncate(100));
                constraints[i] = constr;
            }
            return constraints;
        }

        private static GRBConstr[] InitializeMarkingConstraints(
            List<Place> places,
            GRBModel model,
            GRBVar[] markingVars,
            Marking marking,
            string namePrefix)
        {
            Constraints constraints = new Constraints(places.ToDictionary(
                place => place,
                _ => ConstraintOperators.Equal
            ));
            MarkingWithConstraints constrainedMarking = new MarkingWithConstraints(marking, constraints);
            return InitializeMarkingConstraints(places, model, markingVars, constrainedMarking, namePrefix);
        }

        private static GRBConstr[] InitializeMarkingConstraints(
            List<Place> places,
            GRBModel model,
            GRBVar[] markingVars,
            string namePrefix)
        {
            Constraints constraints = new Constraints(places.ToDictionary(
                place => place,
                _ => ConstraintOperators.Equal
            ));
            Marking marking = new Marking(places.ToDictionary(place => place, _ => 0));
            MarkingWithConstraints constrainedMarking = new MarkingWithConstraints(marking, constraints);
            return InitializeMarkingConstraints(places, model, markingVars, constrainedMarking, namePrefix);
        }

        private static void AddStrictlyGreaterMarkingConstraint(
            List<Place> places,
            GRBModel model,
            GRBVar[] markingVars,
            Marking marking,
            string namePrefix)
        {
            GRBVar[] indicatorVariables = new GRBVar[places.Count];
            for (int i = 0; i < places.Count; i++)
            {
                Place place = places[i];

                GRBVar strictlyGreaterVar =
                    model.AddVar(0, 1, ObjectiveValue, 'N', "strictly_greater_indicator_" + namePrefix + place.Name.Truncate(100));
                indicatorVariables[i] = strictlyGreaterVar;

                List<GRBVar> placeGreaterEqualVars = new List<GRBVar>(places.Count - 1);
                for (int j = 0; j < places.Count; j++)
                {
                    Place checkedPlace = places[j];
                    GRBVar greaterEqualVar =
                        model.AddVar(0, 1, ObjectiveValue, 'N', place.Name.Truncate(100) + checkedPlace.Name.Truncate(100) + "_greaterequal");
                    model.AddGenConstrIndicator(greaterEqualVar, 1, markingVars[j], '>', marking.GetValueOrDefault(checkedPlace, 0) + (i == j ? 1 : 0), checkedPlace.Name.Truncate(100) + "_grequal_marking_in_" + place.Name.Truncate(100) + "_greater");

                    placeGreaterEqualVars.Add(greaterEqualVar);
                }

                model.AddGenConstrAnd(strictlyGreaterVar,
                                      placeGreaterEqualVars.ToArray(),
                                      namePrefix + "_marking_greater_" + place.Name.Truncate(100));

            }
            GRBVar orVariable = model.AddVar(0, 1, ObjectiveValue, 'N', ("any_stricly_greater_indicator_" + namePrefix).Truncate(400));

            model.AddGenConstrOr(orVariable, indicatorVariables, namePrefix + "_any_place_strictly_covers?");
            model.AddConstr(orVariable, '=', 1, namePrefix + "_or_is_true");
        }

        private static void GenerateMarkingEquationConstraints(List<Place> places,
                                                               List<Transition> transitions,
                                                               GRBModel model,
                                                               GRBVar[] transitionTimesFiredVars,
                                                               GRBVar[] initialMarkingVars,
                                                               GRBVar[] finalMarkingVars,
                                                               string namePrefix = "markingEquationConstraint_")
        {
            Stopwatch termAddingTime = new Stopwatch();

            // pX_init + t1*(post(t1)[pX]-pre(t1)[pX]) + ... =|>= targetMarking[pX]    <-- marking equation constraint
            for (int i = 0; i < places.Count; i++)
            {
                Place place = places[i];


                double[] coefficients = new double[transitions.Count];
                for (int j = 0; j < transitions.Count; j++)
                {
                    Transition transition = transitions[j];
                    double coefficient;
                    switch (transition)
                    {
                        case UpdateTransition update:
                            coefficient = (double)update.GetPrePostDifference().GetValueOrDefault(place, 0);
                            break;
                        case TransferTransition transfer:
                            coefficient = (double)transfer.UpdateBehaviour.GetPrePostDifference().GetValueOrDefault(place, 0);
                            break;
                        default:
                            throw new NotImplementedException("Marking Equation constraint behaviour not defined for transition of type " + transition.GetType());
                    }
                    coefficients[j] = coefficient;
                }

                GRBVar[] terms = transitionTimesFiredVars;

                GRBLinExpr placeEquationLHS = new GRBLinExpr();

                placeEquationLHS.AddTerm(1.0, initialMarkingVars[i]);
                placeEquationLHS.AddTerms(coefficients, terms);

                GRBLinExpr rhs = new GRBLinExpr();
                rhs.AddTerm(1.0, finalMarkingVars[i]);

                GRBConstr markingEqConstraint = model.AddConstr(placeEquationLHS, '=', rhs, namePrefix + place.Name.Truncate(100));

            }
        }

        private static void GenerateMarkingEquationConstraintsBackwardsCoverability(List<Place> places,
                                                            List<Transition> transitions,
                                                               Marking initialMarking,
                                                               GRBModel model,
                                                               GRBVar[] transitionTimesFiredVars,
                                                               GRBVar[] finalMarkingVars)
        {
            // pX_init + t1*(post(t1)[pX]-pre(t1)[pX]) + ... =|>= targetMarking[pX]    <-- marking equation constraint
            for (int i = 0; i < places.Count; i++)
            {
                Place place = places[i];

                // 60% seems like a fair estimate of the upper range of percentage of transitions
                // that any place participates in, so initialize the list with this estimate
                ArrayList coefficients = new ArrayList((int)(transitions.Count * 0.6));
                ArrayList terms = new ArrayList((int)(transitions.Count * 0.6));
                for (int j = 0; j < transitions.Count; j++)
                {
                    UpdateTransition t = (UpdateTransition)transitions[j];
                    int effect = t.Post.GetValueOrDefault(place, 0) - t.Pre.GetValueOrDefault(place, 0);
                    if (effect != 0)
                    {
                        coefficients.Add((double)effect);
                        terms.Add(transitionTimesFiredVars[j]);
                    }
                }


                GRBLinExpr placeEquationLHS = new GRBLinExpr();


                placeEquationLHS.AddConstant(initialMarking.GetValueOrDefault(place, 0));
                placeEquationLHS.AddTerms(
                    (double[])coefficients.ToArray(typeof(double)),
                    (GRBVar[])terms.ToArray(typeof(GRBVar)));

                GRBLinExpr rhs = finalMarkingVars[i];

                char sense = '>';

                GRBConstr markingEqConstraint = model.AddConstr(
                    placeEquationLHS,
                    sense,
                    rhs,
                    "markingEquationConstraint_" + place.Name.Truncate(100));
            }
        }

        public static GRBVar[] GeneratePlaceMarkingVars(List<Place> places, char gurobiDomain, GRBModel model, string namePrefix, Dictionary<Place, double> upperBounds = null)
        {
            GRBVar[] initialMarkingVars = new GRBVar[places.Count];
            for (int i = 0; i < places.Count; i++)
            {
                Place place = places[i];
                double placeUpperBound = UpperBound;
                if (upperBounds != null)
                {
                    placeUpperBound = upperBounds.GetValueOrDefault(place, UpperBound);
                }

                GRBVar placeInitialMarkingVar = model.AddVar(-GRB.INFINITY,
                                                             placeUpperBound,
                                                             ObjectiveValue,
                                                             gurobiDomain,
                                                             namePrefix + place.Name.Truncate(100));
                initialMarkingVars[i] = placeInitialMarkingVar;
            }
            return initialMarkingVars;
        }

        public static GRBLinExpr[] GeneratePlaceExprs(PetriNet net, GRBVar[] transitionTimesFiredVars)
        {
            GRBLinExpr[] result = new GRBLinExpr[net.Places.Count];
            for (int i = 0; i < net.Places.Count; i++)
            {
                Place place = net.Places[i];
                Dictionary<Transition, int> incidence = net.GetIncidence(place);
                GRBLinExpr expr = new GRBLinExpr();

                foreach ((Transition transition, int value) in incidence)
                {
                    int transitionIndex = net.Transitions.IndexOf(transition);
                    // If this is a bottleneck, consider AddTerms 
                    expr.AddTerm(value, transitionTimesFiredVars[transitionIndex]);
                }
                result[i] = expr;
            }
            return result;
        }
    }
}