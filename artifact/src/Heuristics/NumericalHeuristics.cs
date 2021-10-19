using System.Collections.Generic;
using System.Linq;
using System;

namespace Petri
{
    public static class NumericalHeuristics
    {
        public static Func<Marking, float?> InitializeEuclideanDistance(PetriNet net, List<MarkingWithConstraints> targetMarkings)
        {
            float? euclideanDistance(Marking cur)
            {
                return targetMarkings.Select(targetMarking => cur.SquaredEuclideanDistance(targetMarking)).Min();
            }

            return euclideanDistance;
        }
    }
}