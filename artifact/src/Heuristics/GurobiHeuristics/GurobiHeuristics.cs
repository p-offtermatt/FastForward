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

        public static Dictionary<Transition, double> CheckIntegerUnboundedness(PetriNet net)
        {
            //Check whether from zero marking, the zero marking can be covered
            GRBModel model = InitializeModel();

            GRBVar[] transitionVars = CreateTransitionTimesFiredVars(net.Transitions, 'N', model,
            "transitionTimesFired_");

            GRBVar[] zeroMarkingVars = GeneratePlaceMarkingVars(net.Places, 'N', model, namePrefix: "zeroMarking_");
            GRBConstr[] zeroMarkingConstraint = InitializeMarkingConstraints(net.Places,
                                                                               model,
                                                                               zeroMarkingVars,
                                                                               "zeroMarkingConstraint_");

            GRBVar[] finalMarkingVars = GeneratePlaceMarkingVars(net.Places, 'N', model, namePrefix: "finalMarking_");
            GenerateMarkingEquationConstraints(net.Places, net.Transitions, model, transitionVars, zeroMarkingVars, finalMarkingVars);

            AddStrictlyGreaterMarkingConstraint(net.Places, model, finalMarkingVars, new Marking(), "final_marking_stricly_greater_than_zero");

            model.Optimize();
            if (model.Status != GRB.Status.OPTIMAL && model.Status != GRB.Status.SUBOPTIMAL)
            {
                return null;
            }
            else
            {
                return ExtractTransitionMultiset(net.Transitions, model, transitionVars);
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
            List<Place> places, List<Transition> transitions, List<MarkingWithConstraints> targetMarkings, Domains domain = Domains.Q
            )
        {
            (GRBModel[] targetsToModels, GRBConstr[][] targetsToInitialConstraints, GRBVar[][] targetsToTransitionVars) = GenerateModelsForTargets(places, transitions, targetMarkings, domain);

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
                                String varname = "finalMarkingAfterTransfers_" + place.Name;
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
                Console.WriteLine(model.Status);
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

        private static Tuple<GRBModel[], GRBConstr[][], GRBVar[][]> GenerateModelsForTargets(List<Place> places, List<Transition> transitions, List<MarkingWithConstraints> targetMarkings, Domains domain)
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

        private static Tuple<GRBModel, GRBConstr[], GRBVar[]> GenerateMarkingEquationModel(List<Place> places, List<Transition> transitions, MarkingWithConstraints targetMarking, Domains domain)
        {
            char gurobiDomain = EvaluateDomain(domain);
            GRBModel model = InitializeModel();

            // final marking vars should not change
            // Dictionary<Place, GRBVar> finalMarkingVars = new Dictionary<Place, GRBVar>();

            // create update transition vars
            GRBVar[] transitionTimesFiredVars = CreateTransitionTimesFiredVars(transitions, gurobiDomain, model);

            List<TransferTransition> transferTransitions = transitions.Where(t => t is TransferTransition).Cast<TransferTransition>().ToList();

            // create objective: minimize t1+t2+t3+...
            GRBLinExpr optimizationObjective = CreateVariableSumExpression(transitionTimesFiredVars);

            model.SetObjective(optimizationObjective, GRB.MINIMIZE);

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
            env.LogFile = "gurobi_env.log";
            env.LogToConsole = 0;
            env.Start();
            GRBModel model = new GRBModel(env);
            model.Parameters.LogToConsole = 0;
            model.Parameters.LogFile = "gurobi.log";
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
                    (initial ? "_initialMarkingConstraint" : "_finalMarkingConstraint") + place.Name.Truncate(200));
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
                GRBLinExpr rightHandSide = new GRBLinExpr(tokenAmount);
                GRBConstr constr = model.AddConstr(leftHandSide, sense, rightHandSide, namePrefix + place.Name);
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
                    model.AddVar(0, 1, ObjectiveValue, 'N', "strictly_greater_indicator_" + namePrefix + place.Name.Truncate(200));
                indicatorVariables[i] = strictlyGreaterVar;

                List<GRBVar> placeGreaterEqualVars = new List<GRBVar>(places.Count - 1);
                for (int j = 0; j < places.Count; j++)
                {
                    Place checkedPlace = places[j];
                    GRBVar greaterEqualVar =
                        model.AddVar(0, 1, ObjectiveValue, 'N', place.Name.Truncate(200) + checkedPlace.Name.Truncate(200) + "_greaterequal");
                    model.AddGenConstrIndicator(greaterEqualVar, '1', markingVars[j], '>', marking.GetValueOrDefault(checkedPlace, 0) + (i == j ? 1 : 0), checkedPlace.Name.Truncate(200) + "_grequal_marking_in_" + place.Name.Truncate(200) + "_greater");

                    placeGreaterEqualVars.Add(greaterEqualVar);
                }

                model.AddGenConstrAnd(strictlyGreaterVar,
                                      placeGreaterEqualVars.ToArray(),
                                      namePrefix + "_marking_greater_" + place.Name.Truncate(200));

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

                GRBConstr markingEqConstraint = model.AddConstr(placeEquationLHS, '=', rhs, namePrefix + place.Name.Truncate(200));

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
                    "markingEquationConstraint_" + place.Name.Truncate(200));
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
                                                             namePrefix + place.Name.Truncate(200));
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