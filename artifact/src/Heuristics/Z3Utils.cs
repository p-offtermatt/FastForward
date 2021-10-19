using System;
using System.Linq;
using Microsoft.Z3;
using Petri;
using System.Collections.Generic;

namespace Utils
{
    public static class Z3Utils
    {
        public static Model Check(Context ctx, BoolExpr f, Status sat)
        {
            Solver s = ctx.MkSolver();
            s.Assert(f);
            if (s.Check() != sat)
                throw new Exception("Model did not fit status " + sat.ToString());
            if (sat == Status.SATISFIABLE)
                return s.Model;
            else
                return null;
        }

        public static List<string> GetFinalMarkingExpressionsFromModel(Context ctx, Model model, PetriNet net)
        {
            List<string> result = new List<string>();
            foreach (Place place in net.Places)
            {
                int marking = ((IntNum)(model.Evaluate(ctx.MkIntConst("finalMarking_" + place.Name)))).Int;
                result.Add("finalMarking_" + place.Name + ": " + marking.ToString());
            }
            return result;
        }

        public static List<string> GetInitialMarkingExpressionsFromModel(Context ctx, Model model, PetriNet net)
        {
            List<string> result = new List<string>();
            foreach (Place place in net.Places)
            {
                int marking = ((IntNum)(model.Evaluate(ctx.MkIntConst("initialMarking_" + place.Name)))).Int;
                result.Add("initialMarking_" + place.Name + ": " + marking.ToString());
            }
            return result;
        }

        public static Dictionary<UpdateTransition, float> GetParikhImageFromModel(Context ctx, Model model, PetriNet net)
        {
            Dictionary<UpdateTransition, float> parikhImage = new Dictionary<UpdateTransition, float>();
            foreach (UpdateTransition transition in net.Transitions)
            {
                parikhImage[transition] = ((float)((RatNum)model.Evaluate(ctx.MkRealConst("times_" + transition.Name + "_fired"))).Numerator.Int /
                                           ((RatNum)model.Evaluate(ctx.MkRealConst("times_" + transition.Name + "_fired"))).Denominator.Int);
            }
            return parikhImage;
        }

        public static float GetParikhImageWeightEvaluation(Context ctx, Model model, PetriNet net)
        {
            Expr parikhImageSumTerm = Z3Heuristics.GenerateParikhImageSumTerm(ctx, net);
            RatNum evaluation = (RatNum)model.Evaluate(parikhImageSumTerm);
            return (float)evaluation.Numerator.Int / (float)evaluation.Denominator.Int;
        }

        public static Dictionary<string, int> GetFiringOrder(Context ctx, Model model, PetriNet net)
        {
            Dictionary<string, int> firingOrder = new Dictionary<string, int>();
            foreach (Place place in net.Places)
            {
                IntNum evaluation = (IntNum)model.Evaluate(ctx.MkIntConst("firing_order_" + place.Name));
                firingOrder.Add(place.Name, evaluation.Int);
            }
            foreach (UpdateTransition transition in net.Transitions)
            {
                if (model.Evaluate(ctx.MkIntConst("firing_order_" + transition.Name)).IsIntNum)
                {
                    IntNum evaluation = (IntNum)model.Evaluate(ctx.MkIntConst("firing_order_" + transition.Name));
                    firingOrder.Add(transition.Name, evaluation.Int);
                }
            }
            return firingOrder.OrderBy(kvPair => kvPair.Value).ToDictionary(kvPair => kvPair.Key, kvPair => kvPair.Value);
        }
    }
}