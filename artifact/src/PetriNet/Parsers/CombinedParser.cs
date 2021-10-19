using System;
using System.Collections.Generic;

namespace Petri
{
    public class CombinedParser : FullParser
    {

        private FormulaParser formulaParser;
        private NetParser netParser;

        public CombinedParser(NetParser netParser, FormulaParser formulaParser)
        {
            this.formulaParser = formulaParser;
            this.netParser = netParser;
        }

        public override List<MarkingWithConstraints> ReadFormula(string filepath)
        {
            return formulaParser.ReadFormula(filepath);
        }

        public override Tuple<PetriNet, Marking> ReadNet(string filepath)
        {
            return netParser.ReadNet(filepath);
        }
    }
}