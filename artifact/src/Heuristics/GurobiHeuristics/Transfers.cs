using System.Collections.Generic;
using System.Linq;
using Gurobi;
using Utils;

namespace Petri
{
    public static class GurobiTransfers
    {
        public static GRBVar[] CreateTransferTransitionsIndicatorVars(List<TransferTransition> transferTransitions, List<GRBVar> transferTransitionsTimesUpdateFiredVars, GRBModel model)
        {
            GRBVar[] transitionVars = new GRBVar[transferTransitions.Count];

            for (int i = 0; i < transferTransitions.Count; i++)
            {
                TransferTransition transition = transferTransitions[i];
                GRBVar timesFiredVar = transferTransitionsTimesUpdateFiredVars[i];

                GRBVar transitionVar = model.AddVar(0, 1, GurobiConsts.ObjectiveValue, Gurobi.GRB.BINARY, "transferInSupport_" + transition.Name.Truncate(200));
                transitionVars[i] = transitionVar;
                model.AddGenConstrIndicator(transitionVar, 0, timesFiredVar, '=', 0.0, transition.Name.Truncate(200) + "_timesFired_0IfIndicator0");
                model.AddGenConstrIndicator(transitionVar, 1, timesFiredVar, GRB.GREATER_EQUAL, 1.0, transition.Name.Truncate(200) + "_timesFired_0IfIndicator1");
            }

            return transitionVars;
        }

        public static void GenerateTransferConstraints(
                                                        List<Place> places,
                                                        List<TransferTransition> transferTransitions,
                                                        GurobiConsts.Domains gurobiDomain,
                                                        GRBModel model,
                                                        GRBVar[] transferTransitionIndicatorVars,
                                                        GRBVar[] initialMarkingVars,
                                                        GRBVar[] finalMarkingVars)
        {
            Dictionary<Place, GRBLinExpr> transferExpressionPerPlace = new Dictionary<Place, GRBLinExpr>();

            Dictionary<Place, int> placesToIndices = places.Select((place, index) => new { place, index }).ToDictionary(x => x.place, x => x.index);


            for (int i = 0; i < transferTransitions.Count; i++)
            {
                TransferTransition transition = transferTransitions[i];
                GRBVar indicatorVar = transferTransitionIndicatorVars[i];

                switch (transition)
                {
                    case SetTransferTransition setTransfer:
                        GenerateTransferConstraintsForTransition(
                            model,
                            ref transferExpressionPerPlace,
                            placesToIndices,
                            transition.Name,
                            setTransfer.TransferInputs,
                            setTransfer.TransferOutputs,
                            indicatorVar);
                        break;

                    case MultitransferTransition multitransfer:
                        foreach ((Place place, HashSet<Place> transferTargets) in multitransfer.Transfers)
                        {
                            GenerateTransferConstraintsForTransition(
                                model,
                                ref transferExpressionPerPlace,
                                placesToIndices,
                                transition.Name + "#" + place.Name + "#",
                                new HashSet<Place> { place },
                                transferTargets,
                                indicatorVar);
                        }
                        break;
                }
            }

            foreach (Place place in places)
            {
                GRBLinExpr transferSum = transferExpressionPerPlace.GetValueOrDefault(place, new GRBLinExpr());
                int index = placesToIndices[place];
                transferSum.AddTerm(1, initialMarkingVars[index]);
                model.AddConstr(finalMarkingVars[index], '=', transferSum, place.Name.Truncate(200) + "_transferEquation");
            }
        }

        public static void GenerateTransferConstraintsForTransition(
            GRBModel model,
            ref Dictionary<Place, GRBLinExpr> transferExpressionPerPlace,
            Dictionary<Place, int> placesToIndices,
            string transitionName,
            HashSet<Place> transferInputs,
            HashSet<Place> transferOutputs,
            GRBVar indicatorVar)
        {
            GRBLinExpr transferredOut = new GRBLinExpr();
            GRBLinExpr transferredIn = new GRBLinExpr();

            foreach (Place place in transferInputs)
            {
                int placeIndex = placesToIndices[place];
                GRBVar outTransferVar = model.AddVar(
                    0,
                    GurobiConsts.UpperBound,
                    GurobiConsts.ObjectiveValue,
                    GRB.INTEGER,
                    transitionName.Truncate(100) + "_" + place.Name.Truncate(100) + "_transferredOut");
                transferredOut.AddTerm(1, outTransferVar);
                GRBLinExpr curTransferExpressionForPlace = transferExpressionPerPlace.GetValueOrDefault(place, new GRBLinExpr());
                curTransferExpressionForPlace.AddTerm(-1, outTransferVar);
                transferExpressionPerPlace[place] = curTransferExpressionForPlace;
            }

            foreach (Place place in transferOutputs)
            {
                int placeIndex = placesToIndices[place];
                GRBVar inTransferVar = model.AddVar(
                    0,
                    GurobiConsts.UpperBound,
                    GurobiConsts.ObjectiveValue,
                    GRB.INTEGER,
                    transitionName.Truncate(100) + "_" + place.Name.Truncate(100) + "_transferredIn");
                transferredIn.AddTerm(1, inTransferVar);
                GRBLinExpr curTransferExpressionForPlace = transferExpressionPerPlace.GetValueOrDefault(place, new GRBLinExpr());
                curTransferExpressionForPlace.AddTerm(+1, inTransferVar);
                transferExpressionPerPlace[place] = curTransferExpressionForPlace;
            }


            GRBVar transferredOutTotal = model.AddVar(0,
                                                      GurobiConsts.UpperBound,
                                                      GurobiConsts.ObjectiveValue,
                                                      GRB.INTEGER,
                                                      transitionName.Truncate(200) + "_transferredOutTotal");
            GRBVar transferredInTotal = model.AddVar(0,
                                                     GurobiConsts.UpperBound,
                                                     GurobiConsts.ObjectiveValue,
                                                     GRB.INTEGER,
                                                     transitionName.Truncate(200) + "_transferredInTotal");
            model.AddConstr(transferredOutTotal,
                            '=',
                            transferredInTotal,
                            transitionName.Truncate(200) + "transferredIn=transferredOut");

            model.AddConstr(transferredOutTotal,
                            '=',
                            transferredOut,
                            transitionName.Truncate(200) + "_transferredOutTotal=sumOfTransfersOut"
                            );

            model.AddConstr(transferredInTotal,
                            '=',
                            transferredIn,
                            transitionName.Truncate(200) + "_transferredInTotal=sumOfTransfersIn"
                            );

            model.AddGenConstrIndicator(
                indicatorVar,
                0,
                transferredOutTotal,
                '=',
                0,
                transitionName.Truncate(200) + "_updateNotFiredImpliesTransferNotFired");
        }
    }
}