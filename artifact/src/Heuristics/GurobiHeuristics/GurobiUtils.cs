#if GUROBI
using Gurobi;

namespace Utils
{
    // taken from https://stackoverflow.com/a/2776689
    public static class StringExt
    {
        public static string Truncate(this string value, int maxLength)
        {
            if (string.IsNullOrEmpty(value)) return value;
            return value.Length <= maxLength ? value : value.Substring(0, maxLength);
        }
    }

    public static class GurobiConsts
    {

        public enum Domains
        {
            N,
            Q
        }

        public static double LowerBound = 0.0;
        public static double UpperBound = GRB.INFINITY;

        public static double ObjectiveValue = 0.0;

        public static char EvaluateDomain(Domains domain)
        {
            char gurobiDomain;
            switch (domain)
            {
                case Domains.N:
                    gurobiDomain = GRB.INTEGER;
                    break;
                case Domains.Q:
                    gurobiDomain = GRB.CONTINUOUS;
                    break;
                default:
                    gurobiDomain = GRB.INTEGER;
                    break;
            }

            return gurobiDomain;
        }
    }
}
#endif