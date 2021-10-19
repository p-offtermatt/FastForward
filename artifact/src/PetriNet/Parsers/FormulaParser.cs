using System.Collections.Generic;

namespace Petri
{
    public interface FormulaParser
    {
        List<MarkingWithConstraints> ReadFormula(string filepath);
    }
}