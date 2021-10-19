using System.Collections.Generic;

namespace Petri
{
    public abstract class FullParser : NetParser, FormulaParser
    {
        public abstract List<MarkingWithConstraints> ReadFormula(string filepath);
    }
}