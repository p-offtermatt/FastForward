using System;
using System.Linq;
using Microsoft.Z3;
using Utils;
using System.Collections.Generic;

namespace Petri
{
    public static class Z3Heuristics
    {
        public const string defaultInitialMarkingVariableName = "initialMarking_";
        public const string defaultFinalMarkingVariableName = "finalMarking_";
        public const string defaultTransitionTimesFiredVariableName = "times_{#1}_fired";
        public const string defaultFiringOrderConstraintVariableName = "firing_order";
        public static int z3TimeOut = 2000 * 1000;

        public static Func<Place, float> GenerateStructuralQReachabilityHeuristicForPlace(PetriNet net, Marking initialMarking, List<MarkingWithConstraints> targetMarkings)
        {
            using (Context ctx = new Context(new Dictionary<string, string>
            {
                ["timeout"] = Z3Heuristics.z3TimeOut.ToString(),
            }))
            {
                BoolExpr initialMarkingConstraint =
                    Z3Heuristics.GenerateMarkingConstraint(ctx,
                                                                  net,
                                                                  initialMarking,
                                                                  markingVariableName: "initialMarking_");

                BoolExpr finalMarkingConstraint =
                    Z3Heuristics.GenerateFinalMarkingConstraint(ctx,
                                                                targetMarkings,
                                                                finalMarkingVariableName: "finalMarking_");

                BoolExpr prefixReachabilityConstraint = GenerateQReachabilityConstraint(ctx,
                                                                net,
                                                                initialMarkingVariableName: "initialMarking_",
                                                                finalMarkingVariableName: "intermediateMarking_",
                                                                transitionTimesFiredVariableName: "prefix_{#1}_fired",
                                                                firingOrderConstraintVariableName: "prefix_firing_order");

                BoolExpr suffixReachabilityConstraint = GenerateQReachabilityConstraint(
                                                                ctx,
                                                                net,
                                                                initialMarkingVariableName: "intermediateMarking_",
                                                                finalMarkingVariableName: "finalMarking_",
                                                                transitionTimesFiredVariableName: "suffix_{#1}_fired",
                                                                firingOrderConstraintVariableName: "suffix_firing_order");
                BoolExpr intermediateMarkingGreaterZero = GenerateMarkingGreaterZeroConstraint(net, ctx, "intermediateMarking_");

                BoolExpr prefixMarkingEqConstraint = GenerateMarkingEquationConstraint(
                    ctx,
                    net,
                    initialMarkingVariableName: "initialMarking_",
                    finalMarkingVariableName: "intermediateMarking_",
                    transitionTimesFiredVariableName: "prefix_{#1}_fired");

                BoolExpr suffixMarkingEqConstraint = GenerateMarkingEquationConstraint(
                    ctx,
                    net,
                    initialMarkingVariableName: "intermediateMarking_",
                    finalMarkingVariableName: "finalMarking_",
                    transitionTimesFiredVariableName: "suffix_{#1}_fired");

                ArithExpr suffixParikhImageSumTerm = GenerateParikhImageSumTerm(ctx, net, transitionTimesFiredVariableName: "suffix_{#1}_fired");

                Optimize minimizer = ctx.MkOptimize();

                minimizer.Add(
                    initialMarkingConstraint,
                    finalMarkingConstraint,
                    //prefixReachabilityConstraint,
                    //suffixReachabilityConstraint,
                    prefixMarkingEqConstraint,
                    suffixMarkingEqConstraint,
                    intermediateMarkingGreaterZero);

                minimizer.MkMinimize(suffixParikhImageSumTerm);

                float ComputeDistanceForPlace(Place place)
                {
                    BoolExpr intermediatePlaceGreaterEqualOne = ctx.MkGe(ctx.MkIntConst("intermediateMarking_" + place.Name), ctx.MkInt(1));
                    minimizer.Push();
                    minimizer.Add(intermediatePlaceGreaterEqualOne);

                    //Console.WriteLine(minimizer.ToString());

                    Status status = minimizer.Check();
                    Model model = minimizer.Model;

                    minimizer.Pop();

                    if (status == Status.SATISFIABLE)
                    {
                        Dictionary<UpdateTransition, float> parikhImage = GetParikhImageFromModel(ctx, net, model, transitionTimesFiredVariableName: "suffix_{#1}_fired");
                        return parikhImage.Aggregate(0f, (accumulator, elem) => accumulator + elem.Value);
                    }
                    else
                    {
                        return float.PositiveInfinity;
                    }
                }

                return ComputeDistanceForPlace;
            }
        }

        private static BoolExpr GenerateMarkingGreaterZeroConstraint(PetriNet net, Context ctx, string markingVariableName)
        {
            return ctx.MkAnd(net.Places.Select(place =>
                ctx.MkGe(ctx.MkIntConst(markingVariableName + place.ToString()), ctx.MkInt(0))
            ));
        }



        /// <summary>
        /// Initializes an instance of the heuristic function that overapproximates PetriNet reachability via continuous reachability.
        /// Note: make sure to initialize a new heuristic function when you use a different net.
        /// This function makes use of caching to reduce the continuous reachability queries.
        /// </summary>
        /// <param name="net"></param>
        /// <param name="targetMarkings"></param>
        /// <returns>Two functions that can be applied to a marking: the first returns the sum of the parikh image
        /// of the shortest firing sequence that reaches a target marking, the second just returns whether the target marking can be reached at all from the given marking.</returns>
        public static Func<Marking, float?> InitializeQReachabilityHeuristic(PetriNet net,
            List<MarkingWithConstraints> targetMarkings)
        {
            using (Context ctx = new Context(new Dictionary<string, string>
            {
                ["timeout"] = Z3Heuristics.z3TimeOut.ToString(),
            }))
            {
                BoolExpr finalMarkingConstraint =
                    Z3Heuristics.GenerateFinalMarkingConstraint(ctx, targetMarkings);
                BoolExpr qReachableConstraint = Z3Heuristics.GenerateQReachabilityConstraint(ctx, net);

                Optimize minimizer = ctx.MkOptimize();

                minimizer.Add(ctx.MkAnd(qReachableConstraint, finalMarkingConstraint));
                minimizer.MkMinimize(GenerateParikhImageSumTerm(ctx, net));


                return InitializeHeuristicFromOptimizer(net, ctx, minimizer);
            }
        }

        public static (bool, Dictionary<Place, Double>) IsContinuousSound_ViaContinuousReach(PetriNet net, Marking initialMarking)
        {
            // initialMarking may not be the initial place (but it may help distinguish ambiguity)
            // so the real initial marking, i.e. 1 token in the initial place, is built separately
            Place initialPlace = WorkflowUtils.GetInitialPlace(net, initialMarking);
            Marking initialPlaceMarking = new Marking();
            initialPlaceMarking.Add(initialPlace, 1);

            Place finalPlace = WorkflowUtils.GetFinalPlace(net);
            Marking finalMarking = new Marking();
            finalMarking.Add(finalPlace, 1);


            using (Context ctx = new Context(new Dictionary<string, string>
            {
                ["timeout"] = Z3Heuristics.z3TimeOut.ToString(),
            }))
            {
                BoolExpr finalMarkingConstraint =
                    Z3Heuristics.GenerateMarkingConstraint(ctx, net, finalMarking, markingVariableName: "final_");

                BoolExpr initialMarkingConstraint =
                    Z3Heuristics.GenerateMarkingConstraint(ctx, net, initialPlaceMarking, markingVariableName: "initial_");

                Marking marking = new Marking();
                Constraints constraints = new Constraints();
                foreach (Place place in net.Places)
                {
                    constraints.Add(place, ConstraintOperators.GreaterEqual);
                    marking.Add(place, 0);
                }

                BoolExpr middleMarkingPositiveConstraint =
                    Z3Heuristics.
                    GenerateUpwardMarkingConstraint(
                        ctx,
                        new MarkingWithConstraints(marking, constraints),
                        markingVariableName: "middle_",
                        markingVarIsInt: false
                    );

                BoolExpr frontQReachableConstraint =
                    Z3Heuristics.GenerateQReachabilityConstraint(ctx, net, initialMarkingVariableName: "initial_", finalMarkingVariableName: "middle_",
                    transitionTimesFiredVariableName: "times_{#1}_fired_front",
                    firingOrderConstraintVariableName: "firing_order_front_",
                    initialMarkingVarIsInt: true,
                    finalMarkingVarIsInt: false
                    );

                BoolExpr endQReachableConstraint =
                    Z3Heuristics.GenerateQReachabilityConstraint(ctx, net, initialMarkingVariableName: "middle_", finalMarkingVariableName: "final_",
                    transitionTimesFiredVariableName: "times_{#1}_fired_end",
                    firingOrderConstraintVariableName: "firing_order_end_",
                    initialMarkingVarIsInt: false,
                    finalMarkingVarIsInt: true
                    );
                BoolExpr endUnreachableConstraint = ctx.MkNot(endQReachableConstraint);

                List<Expr> endVars = new List<Expr>();

                foreach (Transition transition in net.Transitions)
                {
                    endVars.Add(
                        ctx.MkRealConst(
                            "times_{#1}_fired_end".Replace("{#1}", transition.Name))
                    );

                    endVars.Add(
                        ctx.MkIntConst(
                            "firing_order_end_{#1}".Replace("{#1}", transition.Name))
                    );

                    endVars.Add(
                        ctx.MkIntConst(
                            "reverse_firing_order_end_{#1}".Replace("{#1}", transition.Name))
                    );
                }

                foreach (Place place in net.Places)
                {
                    endVars.Add(
                        ctx.MkIntConst(
                            "firing_order_end_{#1}".Replace(
                                "{#1}", place.Name
                            )
                        )
                    );
                    endVars.Add(
                        ctx.MkIntConst(
                            "reverse_firing_order_end_{#1}".Replace(
                                "{#1}", place.Name
                            )
                        )
                    );
                }

                Solver solver = ctx.MkSolver();

                solver.Add(
                    ctx.MkAnd(
                        initialMarkingConstraint,
                        finalMarkingConstraint,
                        middleMarkingPositiveConstraint,
                        frontQReachableConstraint,
                        ctx.MkNot(
                            ctx.MkExists(
                                endVars.ToArray(),
                                endQReachableConstraint
                            )
                        )
                    )
                );

                Status status = solver.Check();

                // Console.WriteLine("------");
                // Console.WriteLine(solver);
                // Console.WriteLine("------");

                if (status == Status.UNSATISFIABLE)
                {
                    return (true, null);
                }
                else if (status == Status.SATISFIABLE)
                {
                    Model model = solver.Model;
                    Dictionary<Place, double> counterexampleMarking =
                        new Dictionary<Place, double>();

                    foreach (Place place in net.Places)
                    {
                        RealExpr evaluation = (RealExpr)model.Evaluate(ctx.MkRealConst("middle_" + place.Name));

                        if (evaluation.IsRatNum)
                        {
                            double tokens =
                                ((double)((RatNum)evaluation).Numerator.Int64) /
                                ((double)((RatNum)evaluation).Denominator.Int64);
                            counterexampleMarking.Add(place, tokens);
                        }
                        else
                        {
                            continue;
                        }
                    }

                    return (false, counterexampleMarking);
                }
                else // status == Status.UNKNOWN
                {
                    throw new Z3Exception("Query to Z3 returned unknown.");
                }
            }
        }

        /// <summary>
        /// Returns a function that is evaluated on markings. It runs the supplied minimizer, constraining the initial
        /// marking variables supplied to this method and minimizing the transition variables supplied to this method.
        /// It returns the minimized parikh image.
        /// </summary>
        /// <param name="net">A Petri net.</param>
        /// <param name="ctx">The context the minimizer was generated from.</param>
        /// <param name="optimizer">An optimizer, i.e. either a minimizer or maximizer.</param>
        /// <param name="initialMarkingVariableName">The variable names of the variables used to constrain the initial marking in Z3.</param>
        /// <param name="transitionTimesFiredVariableName">The variable names used to describe the transition multiplicities in Z3.</param>
        /// <returns></returns>
        private static Func<Marking, float?> InitializeHeuristicFromOptimizer(PetriNet net,
            Context ctx,
            Optimize optimizer,
            string initialMarkingVariableName = defaultInitialMarkingVariableName,
            string transitionTimesFiredVariableName = defaultTransitionTimesFiredVariableName)
        {
            Dictionary<Marking, float?> cachedMarkings = new Dictionary<Marking, float?>();
            float? QReachabilityHeuristic(Marking marking)
            {
                if (cachedMarkings.ContainsKey(marking))
                {
                    return cachedMarkings[marking];
                }
                else
                {
                    BoolExpr initialMarkingConstraint =
                        Z3Heuristics.GenerateMarkingConstraint(ctx, net, marking, initialMarkingVariableName);


                    optimizer.Push();
                    optimizer.Add(initialMarkingConstraint);

                    Status status = optimizer.Check();

                    Model model = optimizer.Model;

                    // Console.WriteLine("------");
                    // Console.WriteLine(optimizer);
                    // Console.WriteLine("------");

                    optimizer.Pop();
                    if (status == Status.SATISFIABLE)
                    {
                        Dictionary<UpdateTransition, float> parikhImage = new Dictionary<UpdateTransition, float>();
                        foreach (UpdateTransition transition in net.Transitions)
                        {
                            RealExpr evaluation =
                                (RealExpr)model.Evaluate(ctx.MkRealConst(transitionTimesFiredVariableName.Replace("{#1}", transition.Name)));
                            if (evaluation.IsRatNum)
                            {
                                parikhImage[transition] =
                                    (float)((RatNum)evaluation).Numerator.Int
                                    / (float)((RatNum)evaluation).Denominator.Int;
                            }
                            else
                            {
                                parikhImage[transition] = 0f;
                            }
                        }
                        // Console.WriteLine(String.Join("\n", parikhImage.Where(kvPair => kvPair.Value > 0)));

                        float score =
                            parikhImage.Aggregate(0f, (accumulator, kvPair) => accumulator + kvPair.Value);
                        cachedMarkings[marking] = score;

                        // Console.WriteLine(score);

                        return score;
                    }
                    else // status == Status.UNSATISFIABLE or status == Status.UNKNOWN
                    {
                        cachedMarkings[marking] = null;
                        return null;
                    }
                }
            }

            return QReachabilityHeuristic;
        }

        public static Func<Marking, float?> InitializeBackwardsHeuristic(PetriNet net, Marking initialMarking, bool QReach = true)
        {
            using (Context ctx = new Context(new Dictionary<string, string>
            {
                ["timeout"] = Z3Heuristics.z3TimeOut.ToString(),
            }))
            {
                BoolExpr finalMarkingConstraint =
                    Z3Heuristics.GenerateMarkingConstraint(ctx, net, initialMarking);

                BoolExpr constraint = QReach ?
                    Z3Heuristics.GenerateQReachabilityConstraint(ctx, net) :
                    Z3Heuristics.GenerateMarkingEquationConstraint(ctx, net);

                Optimize minimizer = ctx.MkOptimize();

                minimizer.Add(ctx.MkAnd(constraint, finalMarkingConstraint));
                minimizer.MkMinimize(GenerateParikhImageSumTerm(ctx, net));


                Dictionary<Marking, float?> cachedMarkings = new Dictionary<Marking, float?>();

                float? QCoverabilityHeuristic(Marking marking)
                {
                    if (cachedMarkings.ContainsKey(marking))
                    {
                        return cachedMarkings[marking];
                    }
                    else
                    {
                        minimizer.Push();
                        Marking finalMarking = new Marking();
                        Constraints finalMarkingConstraintsObject = new Constraints();
                        foreach (Place place in net.Places)
                        {
                            finalMarkingConstraintsObject.Add(place, ConstraintOperators.GreaterEqual);
                            finalMarking.Add(place, marking.GetValueOrDefault(place, 0));
                        }

                        MarkingWithConstraints finalMarkingWithConstraints =
                            new MarkingWithConstraints(marking, finalMarkingConstraintsObject);

                        List<MarkingWithConstraints> finalMarkingsList =
                            new List<MarkingWithConstraints>(new MarkingWithConstraints[]
                            {
                                finalMarkingWithConstraints
                            });

                        BoolExpr finalMarkingConstraint =
                            Z3Heuristics.GenerateFinalMarkingConstraint(ctx, finalMarkingsList);

                        minimizer.Add(finalMarkingConstraint);

                        Status status = minimizer.Check();

                        Model model = minimizer.Model;
                        minimizer.Pop();
                        if (status == Status.SATISFIABLE)
                        {
                            Dictionary<UpdateTransition, float> parikhImage = new Dictionary<UpdateTransition, float>();
                            foreach (UpdateTransition transition in net.Transitions)
                            {
                                RealExpr evaluation =
                                    (RealExpr)model.Evaluate(ctx.MkRealConst("times_" + transition.Name + "_fired"));
                                if (evaluation.IsRatNum)
                                {
                                    parikhImage[transition] =
                                        (float)((RatNum)evaluation).Numerator.Int
                                        / (float)((RatNum)evaluation).Denominator.Int;
                                }
                                else
                                {
                                    parikhImage[transition] = 0f;
                                }
                            }

                            float score =
                                parikhImage.Aggregate(0f, (accumulator, kvPair) => accumulator + kvPair.Value);
                            cachedMarkings[marking] = score;

                            return score;
                        }
                        else // status == Status.UNSATISFIABLE or status == Status.UNKNOWN
                        {
                            cachedMarkings[marking] = null;
                            return null;
                        }
                    }
                }

                return QCoverabilityHeuristic;
            }
        }


        public static Func<Marking, float?> InitializeQCoverabilityHeuristicBackwardsCover(PetriNet net,
            Marking initialMarking)
        {
            return InitializeBackwardsHeuristic(net, initialMarking, QReach: true);
        }

        public static Func<Marking, float?> InitializeMarkingEquationBackwardsCover(PetriNet net,
            Marking initialMarking)
        {
            return InitializeBackwardsHeuristic(net, initialMarking, QReach: false);
        }

        private static float? ReadResultFromZ3CLI(string z3Output)
        {
            // clumsily parse the output from z3 TODO use regex?
            string resultStringSat = z3Output.Split(">").Last().Split("\n").Last().Trim(new char[] { ' ', '(', ')' });
            if (resultStringSat == "unsat")
            {
                return null;
            }

            string resultStringParikhImageWeight =
                z3Output.Split(">").Last().Split("\n").First().Trim(new char[] { ' ', '(', ')' });
            float result;
            if (resultStringParikhImageWeight.Contains("/"))
            {
                int numerator = (Int32)Double.Parse(resultStringParikhImageWeight.Split(" ")[1]);
                int denominator = (Int32)Double.Parse(resultStringParikhImageWeight.Split(" ")[2]);
                result = (float)numerator / (float)denominator;
            }
            else
            {
                result = (float)Double.Parse(z3Output.Split(">").Last().Split("\n").First().Trim());
            }

            return result;
        }

        /// <summary>
        /// Initializes an instance of the heuristic function that overapproximates PetriNet reachability via continuous reachability.
        /// Note: make sure to initialize a new heuristic function when you use a different net.
        /// This function makes use of caching to reduce the continuous reachability queries.
        /// </summary>
        /// <param name="net"></param>
        /// <param name="targetMarkings"></param>
        /// <returns>A function that can be applied to a marking: it returns the sum of the parikh image
        /// of the shortest firing sequence that reaches a target marking.</returns>
        public static Func<Marking, float?> InitializeMarkingEquationHeuristic(PetriNet net,
            List<MarkingWithConstraints> targetMarkings)
        {
            using (Context ctx = new Context(new Dictionary<string, string>
            {
                ["timeout"] = Z3Heuristics.z3TimeOut.ToString(),
            }))
            {
                BoolExpr finalMarkingConstraint =
                    Z3Heuristics.GenerateFinalMarkingConstraint(ctx, targetMarkings);
                BoolExpr markingEquationConstraint = Z3Heuristics.GenerateMarkingEquationConstraint(ctx, net);

                Optimize minimizer = ctx.MkOptimize();

                minimizer.Add(markingEquationConstraint, finalMarkingConstraint);
                minimizer.MkMinimize(GenerateParikhImageSumTerm(ctx, net));


                Dictionary<Marking, float?> cachedMarkings = new Dictionary<Marking, float?>();


                float? MarkingEquationHeuristic(Marking marking)
                {
                    if (cachedMarkings.ContainsKey(marking))
                    {
                        return cachedMarkings[marking];
                    }
                    else
                    {
                        BoolExpr initialMarkingConstraint =
                            Z3Heuristics.GenerateMarkingConstraint(ctx, net, marking);


                        minimizer.Push();
                        minimizer.Add(initialMarkingConstraint);

                        Status status = minimizer.Check();

                        Model model = minimizer.Model;

                        minimizer.Pop();
                        if (status == Status.SATISFIABLE)
                        {
                            Dictionary<UpdateTransition, float> parikhImage = GetParikhImageFromModel(ctx, net, model, defaultTransitionTimesFiredVariableName);

                            float score =
                                parikhImage.Aggregate(0f, (accumulator, kvPair) => accumulator + kvPair.Value);
                            cachedMarkings[marking] = score;

                            return score;
                        }
                        else // status == Status.UNSATISFIABLE or status == Status.UNKNOWN
                        {
                            cachedMarkings[marking] = null;
                            return null;
                        }
                    }
                }

                return MarkingEquationHeuristic;
            }
        }

        public static Dictionary<UpdateTransition, float> CalculateParikhImageViaMarkingEquation(PetriNet net,
            Marking initialMarking, List<MarkingWithConstraints> targetMarkings)
        {
            using (Context ctx = new Context())
            {
                BoolExpr markingEquationConstraint = Z3Heuristics.GenerateMarkingEquationConstraint(ctx, net);
                BoolExpr initialMarkingConstraint =
                    Z3Heuristics.GenerateMarkingConstraint(ctx, net, initialMarking);
                BoolExpr finalMarkingConstraint =
                    Z3Heuristics.GenerateFinalMarkingConstraint(ctx, targetMarkings);

                BoolExpr constraintsAnd = ctx.MkAnd(initialMarkingConstraint, finalMarkingConstraint,
                    markingEquationConstraint);



                Optimize minimizer = ctx.MkOptimize();
                minimizer.Add(constraintsAnd);

                minimizer.MkMinimize(GenerateParikhImageSumTerm(ctx, net));
                Status status = minimizer.Check();

                if (status == Status.SATISFIABLE)
                {

                    Model model = minimizer.Model;

                    return GetParikhImageFromModel(ctx, net, model);
                }
                else
                {
                    return null;
                }
            }
        }

        public static Dictionary<UpdateTransition, float> CalculateParikhImageViaQReachability(PetriNet net,
            Marking initialMarking, List<MarkingWithConstraints> targetMarkings, bool doMarkingEQOverN = false)
        {
            using (Context ctx = new Context())
            {
                BoolExpr initialMarkingConstraint =
                    Z3Heuristics.GenerateMarkingConstraint(ctx, net, initialMarking);
                BoolExpr finalMarkingConstraint =
                    Z3Heuristics.GenerateFinalMarkingConstraint(ctx, targetMarkings);

                BoolExpr markingEQConstraint = Z3Heuristics.GenerateQReachabilityConstraint(ctx, net, timesFiredVarsAreInt: doMarkingEQOverN);

                BoolExpr constraintsAnd =
                    ctx.MkAnd(markingEQConstraint, initialMarkingConstraint, finalMarkingConstraint);

                Optimize minimizer = ctx.MkOptimize();
                minimizer.Add(constraintsAnd);

                minimizer.MkMinimize(GenerateParikhImageSumTerm(ctx, net, timesFiredVarsAreInt: doMarkingEQOverN));
                Status status = minimizer.Check();

                // Console.WriteLine(minimizer.ToString());


                if (status == Status.SATISFIABLE)
                {
                    Model model = minimizer.Model;
                    return GetParikhImageFromModel(ctx, net, model, timesFiredVarsAreInt: doMarkingEQOverN);
                }
                else
                {
                    return null;
                }
            }
        }

        public static ArithExpr GenerateParikhImageSumTerm(Context ctx, PetriNet net, string transitionTimesFiredVariableName = "times_{#1}_fired",
        bool timesFiredVarsAreInt = false)
        {
            return GenerateParikhImageSumTerm(ctx, net.Transitions, transitionTimesFiredVariableName, timesFiredVarsAreInt: timesFiredVarsAreInt);
        }

        public static ArithExpr GenerateParikhImageSumTerm(Context ctx, IEnumerable<Transition> transitions, string transitionTimesFiredVariableName = "times_{#1}_fired", bool timesFiredVarsAreInt = false)
        {
            // if there are no transitions, no need to minimize a parikh image
            if (transitions.Count() == 0)
            {
                return ctx.MkInt(0);
            }
            return ctx.MkAdd(
                transitions.Select(transition => MakeVariable(ctx, transitionTimesFiredVariableName.Replace("{#1}", transition.Name), timesFiredVarsAreInt)));
        }

        private static BoolExpr GenerateMarkingEquationConstraint(
            Context ctx,
            PetriNet net,
            string initialMarkingVariableName = defaultInitialMarkingVariableName,
            string finalMarkingVariableName = defaultFinalMarkingVariableName,
            string transitionTimesFiredVariableName = defaultTransitionTimesFiredVariableName,
            bool initialMarkingVarIsInt = true,
            bool finalMarkingVarIsInt = true,
            bool timesFiredVarsAreInt = false)
        {
            List<UpdateTransition> transitions = net.Transitions.Cast<UpdateTransition>().ToList();

            Dictionary<UpdateTransition, ArithExpr> transitionExpressions = transitions.ToDictionary(
                transition => transition,
                transition => MakeVariable(ctx, transitionTimesFiredVariableName.Replace("{#1}", transition.Name), timesFiredVarsAreInt));

            // times_t1_fired >= 0 AND times_t2_fired >= 0 AND ...
            BoolExpr eachTransitionTimesFiredGeq0 =
                ctx.MkAnd(transitionExpressions.Values.Select(expr => ctx.MkGe(expr, ctx.MkInt(0))));

            List<BoolExpr> placesMarkingEquationSatisfied = new List<BoolExpr>();

            // ensure that marking equation is satisfied for each place
            foreach (Place p in net.Places)
            {
                ArithExpr place_initialMarking = MakeVariable(ctx, initialMarkingVariableName + p.Name, initialMarkingVarIsInt);

                ArithExpr place_targetMarking = MakeVariable(ctx, finalMarkingVariableName + p.Name, finalMarkingVarIsInt);

                // finalMarking[p] = initialMarking[p] + times_t1_fired * (t1.post[p] - t1.pre[p]) + times_t2_fired * ...
                var transitionsWithImpact = transitions.Where(transition =>
                {
                    // do not add constraints of the form finalMarking[p] = ... + 0 * times_tn_fired + ...
                    // they slow down z3 but do nothing for us
                    return transition.Post.GetValueOrDefault(p, 0) - transition.Pre.GetValueOrDefault(p, 0) != 0;
                });

                BoolExpr markingEquationSatisfied;

                if (transitionsWithImpact.Count() != 0)
                {
                    ArithExpr changesForPlaceDueToTransitionFires = ctx.MkAdd(transitionsWithImpact.Select(
                        transition =>
                        {
                            int change = transition.Post.GetValueOrDefault(p, 0) -
                                         transition.Pre.GetValueOrDefault(p, 0);
                            ArithExpr transitionTimesFired_times_change = ctx.MkMul(ctx.MkInt(change),
                                MakeVariable(ctx, transitionTimesFiredVariableName.Replace("{#1}", transition.Name), timesFiredVarsAreInt));
                            return transitionTimesFired_times_change;
                        }
                    ));
                    markingEquationSatisfied = ctx.MkEq(place_targetMarking,
                        ctx.MkAdd(place_initialMarking, changesForPlaceDueToTransitionFires));
                }
                else
                {
                    markingEquationSatisfied = ctx.MkEq(place_targetMarking, place_initialMarking);
                }

                placesMarkingEquationSatisfied.Add(markingEquationSatisfied);
            }

            BoolExpr allPlacesMarkingEquationSatisfied = ctx.MkAnd(placesMarkingEquationSatisfied);

            return ctx.MkAnd(allPlacesMarkingEquationSatisfied, eachTransitionTimesFiredGeq0);
        }

        private static ArithExpr MakeVariable(Context context, string name, bool isInt)
        {
            if (isInt)
            {
                return context.MkIntConst(name);
            }
            else
            {
                return context.MkRealConst(name);
            }
        }

        private static BoolExpr GenerateMarkingConstraint(Context ctx, PetriNet net, Marking marking, string markingVariableName = defaultInitialMarkingVariableName,
        bool markingVarIsInt = true)
        {
            return ctx.MkAnd(net.Places.Select(place =>
            {
                int amount = marking.GetValueOrDefault(place, 0);
                Expr placeExpr = null;
                if (markingVarIsInt)
                {
                    placeExpr = ctx.MkIntConst(markingVariableName + place.Name);
                }
                else
                {
                    placeExpr = ctx.MkRealConst(markingVariableName + place.Name);
                }

                IntExpr amountExpr = ctx.MkInt(amount);
                return ctx.MkEq(placeExpr, amountExpr);
            }));
        }

        private static BoolExpr GenerateFinalMarkingConstraint(Context ctx, List<MarkingWithConstraints> finalMarkings, string finalMarkingVariableName = defaultFinalMarkingVariableName)
        {
            // (finalMarking1[p] (>=|=) target[p] AND ...) OR (finalMarking2[p] (>=|=) target[p] AND ...)
            BoolExpr result = ctx.MkOr(finalMarkings.Select(finalMarking => GenerateUpwardMarkingConstraint(ctx, finalMarking, finalMarkingVariableName)));
            return result;
        }

        private static BoolExpr GenerateUpwardMarkingConstraint(Context ctx, MarkingWithConstraints marking, string markingVariableName = defaultFinalMarkingVariableName,
        bool markingVarIsInt = true)
        {
            return ctx.MkAnd(marking.Marking.Select(kvPair =>
            {
                Place place = kvPair.Key;
                int amount = kvPair.Value;
                ConstraintOperators constraint = marking.Constraints[place];

                ArithExpr placeMarkingExpr = null;
                if (markingVarIsInt)
                {
                    placeMarkingExpr = ctx.MkIntConst(markingVariableName + place.Name);
                }
                else
                {
                    placeMarkingExpr = ctx.MkRealConst(markingVariableName + place.Name);
                }
                IntExpr targetAmount = ctx.MkInt(amount);

                if (constraint == ConstraintOperators.GreaterEqual)
                {
                    return ctx.MkGe(placeMarkingExpr, targetAmount);
                }
                else
                {
                    return ctx.MkEq(placeMarkingExpr, targetAmount);
                }
            }));
        }

        private static BoolExpr GenerateQReachabilityConstraint(Context ctx, PetriNet net,
            string initialMarkingVariableName = defaultInitialMarkingVariableName,
            string finalMarkingVariableName = defaultFinalMarkingVariableName,
            string transitionTimesFiredVariableName = defaultTransitionTimesFiredVariableName,
            string firingOrderConstraintVariableName = defaultFiringOrderConstraintVariableName,
            bool initialMarkingVarIsInt = true,
            bool finalMarkingVarIsInt = true,
            bool timesFiredVarsAreInt = false
            )
        {
            return ctx.MkAnd(
                GenerateMarkingEquationConstraint(ctx,
                                                  net,
                                                  initialMarkingVariableName,
                                                  finalMarkingVariableName,
                                                  transitionTimesFiredVariableName,
                                                  initialMarkingVarIsInt: initialMarkingVarIsInt,
                                                  finalMarkingVarIsInt: finalMarkingVarIsInt,
                                                  timesFiredVarsAreInt: timesFiredVarsAreInt),
                //GenerateFiringSetConstraint(ctx, net, true),
                GenerateFiringSetConstraint(ctx, net, false, initialMarkingVariableName, finalMarkingVariableName, transitionTimesFiredVariableName, firingOrderConstraintVariableName, initialMarkingVarIsInt: initialMarkingVarIsInt,
                                                  finalMarkingVarIsInt: finalMarkingVarIsInt, timesFiredVarsAreInt: timesFiredVarsAreInt),
                GenerateFiringSetConstraint(ctx, net, true, initialMarkingVariableName, finalMarkingVariableName, transitionTimesFiredVariableName, firingOrderConstraintVariableName, initialMarkingVarIsInt: initialMarkingVarIsInt,
                                                  finalMarkingVarIsInt: finalMarkingVarIsInt, timesFiredVarsAreInt: timesFiredVarsAreInt)
            );
        }

        // Phi^N_fs in the paper if forward=True, otherwise Phi^(N^-1)_fs
        private static BoolExpr GenerateFiringSetConstraint(
            Context ctx,
            PetriNet net,
            bool forward = true,
            string initialMarkingVariableName = defaultInitialMarkingVariableName,
            string finalMarkingVariableName = defaultFinalMarkingVariableName,
            string transitionTimesFiredVariableName = defaultTransitionTimesFiredVariableName,
            string firingOrderConstraintVariableName = defaultFiringOrderConstraintVariableName,
            bool initialMarkingVarIsInt = true,
            bool finalMarkingVarIsInt = true,
            bool timesFiredVarsAreInt = false)
        {
            // firing amounts
            // "times_" + transition.Name + "_fired"

            // order z
            // "firing_order_" + transition.Name|place.Name
            return ctx.MkAnd(GenerateFiringOrderConstraint(ctx, net, forward, initialMarkingVariableName, finalMarkingVariableName, transitionTimesFiredVariableName, firingOrderConstraintVariableName, timesFiredVarsAreInt: timesFiredVarsAreInt),
                GeneratePlacesMarkedConstraint(ctx,
                                               net,
                                               forward,
                                               initialMarkingVariableName,
                                               finalMarkingVariableName,
                                               transitionTimesFiredVariableName,
                                               firingOrderConstraintVariableName,
                                               initialMarkingVarIsInt: initialMarkingVarIsInt,
                                               finalMarkingVarIsInt: finalMarkingVarIsInt,
                                               timesFiredVarsAreInt: timesFiredVarsAreInt)
            );
        }

        // Phi^N_mk in the paper
        private static BoolExpr GeneratePlacesMarkedConstraint(Context ctx, PetriNet net, bool forward, string initialMarkingVariableName = defaultInitialMarkingVariableName,
            string finalMarkingVariableName = defaultFinalMarkingVariableName,
            string transitionTimesFiredVariableName = defaultTransitionTimesFiredVariableName,
            string firingOrderConstraintVariableName = defaultFiringOrderConstraintVariableName,
            bool initialMarkingVarIsInt = true,
            bool finalMarkingVarIsInt = true,
            bool timesFiredVarsAreInt = false
            )
        {
            List<BoolExpr> placesMarkedConditions = new List<BoolExpr>();

            foreach (Place place in net.Places)
            {
                IntExpr placeFiringOrderExpr =
                    ctx.MkIntConst((forward ? firingOrderConstraintVariableName : "reverse_" + firingOrderConstraintVariableName) + place.Name);
                BoolExpr placeFiringOrderGreater0 = ctx.MkGt(placeFiringOrderExpr, ctx.MkInt(0));


                ArithExpr initialMarkingPlace = null;
                if ((forward && initialMarkingVarIsInt) || (!forward && finalMarkingVarIsInt))
                {
                    initialMarkingPlace = ctx.MkIntConst((forward ? initialMarkingVariableName : finalMarkingVariableName) + place.Name);
                }
                else
                {
                    initialMarkingPlace = ctx.MkRealConst((forward ? initialMarkingVariableName : finalMarkingVariableName) + place.Name);
                }
                BoolExpr initialMarkingPlaceGreater0 = ctx.MkGt(initialMarkingPlace, ctx.MkInt(0));

                BoolExpr placeIsMarkedByTransition = ctx.MkOr(
                    (forward ? net.GetPreSet(place) : net.GetPostSet(place)).Select(transition =>
                    {
                        ArithExpr transitionTimesFiredExpr = MakeVariable(ctx, transitionTimesFiredVariableName.Replace("{#1}", transition.Name), timesFiredVarsAreInt);
                        BoolExpr transitionTimesFiredGreater0 = ctx.MkGt(transitionTimesFiredExpr, ctx.MkInt(0));

                        IntExpr transitionFiringOrderExpr =
                            ctx.MkIntConst((forward ? firingOrderConstraintVariableName : "reverse_" + firingOrderConstraintVariableName) + transition.Name);

                        BoolExpr firingOrderConstraint = ctx.MkLt(transitionFiringOrderExpr, placeFiringOrderExpr);
                        return ctx.MkAnd(transitionTimesFiredGreater0, firingOrderConstraint);
                    }));

                BoolExpr placeMarked = ctx.MkOr(initialMarkingPlaceGreater0, placeIsMarkedByTransition);
                BoolExpr placeFiredImpliesPlaceMarked = ctx.MkImplies(placeFiringOrderGreater0, placeMarked);
                placesMarkedConditions.Add(placeFiredImpliesPlaceMarked);
            }

            return ctx.MkAnd(placesMarkedConditions);
        }

        // Phi^N_dt in the paper
        private static BoolExpr GenerateFiringOrderConstraint(
            Context ctx,
            PetriNet net,
            bool forward,
            string initialMarkingVariableName = defaultInitialMarkingVariableName,
            string finalMarkingVariableName = defaultFinalMarkingVariableName,
            string transitionTimesFiredVariableName = defaultTransitionTimesFiredVariableName,
            string firingOrderConstraintVariableName = defaultFiringOrderConstraintVariableName,
            bool timesFiredVarsAreInt = false)
        {
            List<BoolExpr> firingOrderConstraints = new List<BoolExpr>();

            foreach (UpdateTransition transition in net.Transitions)
            {
                BoolExpr parikhImageGreaterZero =
                    ctx.MkGt(MakeVariable(ctx, transitionTimesFiredVariableName.Replace("{#1}", transition.Name), timesFiredVarsAreInt), ctx.MkInt(0));

                // transition.Pre contains elements of type KeyValuePair<Place, int>
                BoolExpr firingOrderConstraint = ctx.MkAnd((forward ? transition.Pre : transition.Post).Select(
                    prePair =>
                    {
                        IntExpr placeFiringOrderExpr =
                            ctx.MkIntConst((forward ? firingOrderConstraintVariableName : "reverse_" + firingOrderConstraintVariableName) + prePair.Key.Name);
                        IntExpr transitionFiringOrderExpr =
                            ctx.MkIntConst((forward ? firingOrderConstraintVariableName : "reverse_" + firingOrderConstraintVariableName) + transition.Name);
                        BoolExpr placeFiringOrderGreaterThan0 = ctx.MkGt(placeFiringOrderExpr, ctx.MkInt(0));
                        BoolExpr placeFiringOrderLessEqualTransitionFiringOrder =
                            ctx.MkLe(placeFiringOrderExpr, transitionFiringOrderExpr);
                        return ctx.MkAnd(placeFiringOrderGreaterThan0, placeFiringOrderLessEqualTransitionFiringOrder);
                    }));

                BoolExpr constraint = ctx.MkImplies(parikhImageGreaterZero, firingOrderConstraint);
                firingOrderConstraints.Add(constraint);
            }

            return ctx.MkAnd(firingOrderConstraints);
        }

        private static BoolExpr GenerateCycleInitialSmallerFinalConstraint(Context ctx, PetriNet net, string initialMarkingVariableName, string finalMarkingVariableName)
        {
            List<BoolExpr> expressions = new List<BoolExpr>();
            foreach (Place place in net.Places)
            {
                IntExpr initialMarkingVariable = ctx.MkIntConst(initialMarkingVariableName + place.ToString());
                IntExpr finalMarkingVariable = ctx.MkIntConst(finalMarkingVariableName + place.ToString());
                expressions.Add(ctx.MkLe(initialMarkingVariable, finalMarkingVariable));
            }
            return ctx.MkAnd(expressions);
        }

        private static BoolExpr GenerateAtleastOneTransitionConstraint(Context ctx, PetriNet net, string transitionTimesFiredVariableName)
        {
            List<BoolExpr> expressions = new List<BoolExpr>();
            foreach (UpdateTransition transition in net.Transitions)
            {
                BoolExpr transitionFiredNonzeroTimes = ctx.MkGt(ctx.MkRealConst(transitionTimesFiredVariableName.Replace("{#1}", transition.Name)), ctx.MkInt(0));
                expressions.Add(transitionFiredNonzeroTimes);
            }
            return ctx.MkOr(expressions);
        }

        private static Dictionary<Place, float> GetMarkingFromModel(Context ctx, PetriNet net, Model model, string markingVariableName)
        {
            Dictionary<Place, float> placesToTokenNumbers = new Dictionary<Place, float>();
            foreach (Place place in net.Places)
            {
                IntNum evaluation = (IntNum)model.Eval(ctx.MkIntConst(markingVariableName + place.Name));

                placesToTokenNumbers[place] = evaluation.Int;
            }
            return placesToTokenNumbers;
        }

        private static Dictionary<UpdateTransition, float> GetParikhImageFromModel(Context ctx, PetriNet net, Model model, string transitionTimesFiredVariableName = defaultTransitionTimesFiredVariableName, bool timesFiredVarsAreInt = false)
        {
            Dictionary<UpdateTransition, float> parikhImage = new Dictionary<UpdateTransition, float>();
            foreach (UpdateTransition transition in net.Transitions)
            {
                ArithExpr transitionVar = MakeVariable(ctx, transitionTimesFiredVariableName.Replace("{#1}", transition.Name), timesFiredVarsAreInt);

                if (timesFiredVarsAreInt)
                {
                    parikhImage[transition] = (float)((IntNum)model.Evaluate(transitionVar)).Int64;
                }
                else
                {
                    parikhImage[transition] = ((float)
                                               ((RatNum)model.Evaluate(
                                                   transitionVar)).Numerator
                                               .Int /
                                               (float)((RatNum)model.Evaluate(
                                                   transitionVar)).Denominator
                                               .Int);
                }
            }
            return parikhImage;
        }

        public static BoolExpr AtleastOneTransitionInSupportConstraint(Context ctx, PetriNet net, IEnumerable<UpdateTransition> transitions, string parikhImageSupportVariableName)
        {
            return ctx.MkOr(transitions.Select(transition =>
            {
                return ctx.MkGe(ctx.MkIntConst(parikhImageSupportVariableName + transition.Name), ctx.MkInt(1));
            }));
        }

        public static BoolExpr GenerateIndicatorPlacesUnmarkedConstraint(Context ctx, IEnumerable<Place> indicatorPlaces, string markingVariableName)
        {
            return ctx.MkAnd(indicatorPlaces.Select(place => ctx.MkEq(ctx.MkIntConst(markingVariableName + place.Name), ctx.MkInt(0))));
        }

        public static Func<Marking, float?> InitializeHandleHeuristic(PetriNet handleNet, PetriNet cycleNet, IEnumerable<Place> indicatorPlaces = null)
        {
            using (Context ctx = new Context(new Dictionary<string, string>
            {
                ["timeout"] = Z3Heuristics.z3TimeOut.ToString(),
            }))
            {

                BoolExpr cycleStepMarkingReachableConstraint = Z3Heuristics.GenerateQReachabilityConstraint(ctx, cycleNet, "cycleInitialMarking_", "cycleStepMarking_", "transitionTimesFiredCycle_{#1}", "cycleFiringOrder_");
                BoolExpr cycleConstraint = Z3Heuristics.GenerateCycleInitialSmallerFinalConstraint(ctx, cycleNet, "cycleInitialMarking_", "cycleStepMarking_");
                BoolExpr atleastOneTransitionPositiveConstraint = Z3Heuristics.GenerateAtleastOneTransitionConstraint(ctx, cycleNet, "transitionTimesFiredCycle_{#1}");

                BoolExpr cycleOverapproximation = ctx.MkAnd(cycleStepMarkingReachableConstraint, cycleConstraint, atleastOneTransitionPositiveConstraint);

                BoolExpr cycleReachable = Z3Heuristics.GenerateQReachabilityConstraint(ctx, handleNet, "initialMarking_", "cycleInitialMarking_", "transitionTimesFired_{#1}", "firingOrder_");

                BoolExpr markingsGreaterZeroConstraints = ctx.MkAnd(
                    Z3Heuristics.GenerateMarkingGreaterZeroConstraint(handleNet, ctx, "cycleInitialMarking_"),
                    Z3Heuristics.GenerateMarkingGreaterZeroConstraint(cycleNet, ctx, "cycleStepMarking_")
                );


                ArithExpr parikhImageSumTerm = Z3Heuristics.GenerateParikhImageSumTerm(ctx, cycleNet, "transitionTimesFired_{#1}");

                Optimize minimizer = ctx.MkOptimize();
                minimizer.Add(cycleOverapproximation, cycleReachable, markingsGreaterZeroConstraints);

                if (indicatorPlaces != null)
                {
                    Marking indicatorMarking = new Marking(indicatorPlaces.ToDictionary(p => p, p => 1));
                    Constraints indicatorConstraints = new Constraints(indicatorPlaces.ToDictionary(p => p, p => ConstraintOperators.GreaterEqual));
                    MarkingWithConstraints indicatorMarkingWithConstraints = new MarkingWithConstraints(indicatorMarking, indicatorConstraints);

                    BoolExpr indicatorsMarkedConstraint = Z3Heuristics.GenerateUpwardMarkingConstraint(ctx, indicatorMarkingWithConstraints, "cycleStepMarking_");
                    BoolExpr indicatorsUnmarkedInitiallyConstraint = Z3Heuristics.GenerateIndicatorPlacesUnmarkedConstraint(ctx, indicatorPlaces, "cycleInitialMarking_");
                    minimizer.Add(indicatorsMarkedConstraint, indicatorsUnmarkedInitiallyConstraint);
                }

                minimizer.MkMinimize(parikhImageSumTerm);

                return InitializeHeuristicFromOptimizer(handleNet, ctx, minimizer, "initialMarking_", "transitionTimesFired_{#1}");
            }
        }

        public static Func<IEnumerable<MarkingWithConstraints>, Func<Marking, float?>> InitializeCycleHeuristicFactory(
            PetriNet net,
            IEnumerable<Place> indicatorPlaces = null)
        {
            using (Context ctx = new Context(new Dictionary<string, string>
            {
                ["timeout"] = Z3Heuristics.z3TimeOut.ToString(),
            }))
            {
                Func<Marking, float?> factoryFunction(IEnumerable<MarkingWithConstraints> targetMarkings)
                {
                    BoolExpr qReachableConstraint = Z3Heuristics.GenerateQReachabilityConstraint(ctx, net, "initialMarking_", "finalMarking_", "times_{#1}_fired", "firingOrder_");
                    ArithExpr parikhImageSumTerm = Z3Heuristics.GenerateParikhImageSumTerm(ctx, net, "times_{#1}_fired");

                    Optimize minimizer = ctx.MkOptimize();
                    minimizer.MkMinimize(parikhImageSumTerm);
                    minimizer.Add(qReachableConstraint);

                    if (indicatorPlaces != null)
                    {
                        Marking indicatorMarking = new Marking(indicatorPlaces.ToDictionary(p => p, p => 1));
                        Constraints indicatorConstraints = new Constraints(indicatorPlaces.ToDictionary(p => p, p => ConstraintOperators.GreaterEqual));
                        MarkingWithConstraints indicatorMarkingWithConstraints = new MarkingWithConstraints(indicatorMarking, indicatorConstraints);
                        BoolExpr indicatorsMarkedConstraint = Z3Heuristics.GenerateUpwardMarkingConstraint(ctx, indicatorMarkingWithConstraints, "finalMarking_");
                        minimizer.Add(indicatorsMarkedConstraint);
                    }


                    BoolExpr finalMarkingConstraint = Z3Heuristics.GenerateFinalMarkingConstraint(ctx, targetMarkings.ToList(), "finalMarking_");
                    minimizer.Add(finalMarkingConstraint);

                    Func<Marking, float?> heuristic = InitializeHeuristicFromOptimizer(net, ctx, minimizer, "initialMarking_", "times_{#1}_fired");
                    return heuristic;
                }
                return factoryFunction;
            }
        }
    }
}