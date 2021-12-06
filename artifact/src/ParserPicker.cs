using System;
using System.Collections.Generic;
using Petri;
using System.Linq;
using System.Diagnostics;
using Benchmark;
using CommandLine;
using SearchAlgorithms;
using System.IO;
using MathNet.Numerics.Statistics;
using CommandLine.Text;
#if GUROBI
using static Petri.GurobiHeuristics;
#endif

namespace PetriTool
{
    public class ParserPicker
    {
        public static NetParser ChooseNetParser(string filepath)
        {
            NetParser result;
            if (filepath.EndsWith(".lola") || filepath.EndsWith(".terminating"))
            {
                result = new LolaParser();
            }
            else if (filepath.EndsWith(".pnml"))
            {
                result = new PNMLParser();
            }
            else if (filepath.EndsWith(".pnet"))
            {
                result = new PnetParser();
            }
            else if (filepath.EndsWith(".xml.tpn"))
            {
                result = new XMLTPNParser();
            }
            else if (filepath.EndsWith(".tts"))
            {
                result = new BinaryTTSParser();
            }
            else if (filepath.EndsWith(".xml"))
            {
                result = new HadaraParser();
            }
            else
            {
                throw new FormatException("Could not determine format of net file " + filepath + "! Make sure the ending is correct");
            }
            return result;
        }

        public static FormulaParser ChooseFormulaParser(string filepath)
        {
            if (filepath == null)
            {
                return null;
            }
            else if (filepath.EndsWith(".formula") || filepath.EndsWith(".task1"))
            {
                return new LolaParser();
            }
            else if (filepath.EndsWith(".prop"))
            {
                return new BinaryTTSParser();
            }
            else if (filepath.EndsWith(".xml"))
            {
                // TODO: enable parsing .xml files
                throw new NotImplementedException();
            }
            else
            {
                Console.WriteLine("Could not determine format of formula file! Make sure the ending is correct! Filename: " + filepath);
                System.Environment.Exit(3);
                return null;
            }
        }


        public static FullParser ChooseFullParser(string net_filepath, string formula_filepath)
        {
            if (net_filepath.EndsWith(".tts"))
            {
                return new BinaryTTSParser();
            }
            else
            {
                NetParser netParser = ChooseNetParser(net_filepath);
                FormulaParser formulaParser = ChooseFormulaParser(formula_filepath);
                return new CombinedParser(netParser, formulaParser);
            }
        }
    }
}