using System;
using CommandLine;
using CommandLine.Text;
using Soundness;
#if GUROBI
#endif

namespace PetriTool
{
    class Program
    {
        static void Main(string[] args)
        {
            // competitive mode:
            // leave out some nice-to-have features like command line parsing and full-blown diagnostic entries
#if COMP
            string method = args[0];
            if (method == "a-star")
            {
                string heuristic = args[1];
                string lolaFilePath = args[2];
                string formulaFilePath = args[3];

                bool prune = false;
                if(args.Count() >= 5){
                    prune = args[4] == "prune";
                }

                AStarQueryOptions options = new AStarQueryOptions();
                options.chosenHeuristic = heuristic;
                options.netFilePath = lolaFilePath;
                options.formulaFilePath = formulaFilePath;
                options.prune = prune;
                options.competitiveMode = true;

                SingleQueryWithHeuristic(options);
            }
            else if (method == "best-first")
            {
                string heuristic = args[1];
                string lolaFilePath = args[2];
                string formulaFilePath = args[3];

                bool prune = false;
                if(args.Count() >= 5){
                    prune = args[4] == "prune";
                }

                BestFirstQueryOptions options = new BestFirstQueryOptions();
                options.chosenHeuristic = heuristic;
                options.netFilePath = lolaFilePath;
                options.formulaFilePath = formulaFilePath;
                options.prune = prune;
                options.competitiveMode = true;
                SingleQueryWithHeuristic(options);
            }
            else
            {
                Console.WriteLine("Input was not in right format for competitive mode... do not know method " + method);
            }

#else
            var parser = new CommandLine.Parser(with => with.HelpWriter = null);

            var parserResult = parser
                .ParseArguments<AStarQueryOptions, WitnessCheckOptions, BestFirstQueryOptions, TranslationOptions, CalculateHeuristicOptions, CalculateHeuristicSupportOptions, ComputeNetStatisticsOptions, GenerateInstanceOptions, AStarUnityFrontierOptions, BestFirstUnityFrontierOptions, SaturationSearchOptions, WFTransformationOptions, ContinuousSoundnessOptions, SoundnessOptions, TranslateWFOptions>(args);
            parserResult.WithParsed<AStarQueryOptions>(SearchQueryEntryPoints.SingleQueryWithHeuristic)
                .WithParsed<BestFirstQueryOptions>(SearchQueryEntryPoints.SingleQueryWithHeuristic)
                .WithParsed<WitnessCheckOptions>(UtilityEntrypoints.WitnessCheck)
                .WithParsed<TranslationOptions>(UtilityEntrypoints.Translate)
                .WithParsed<CalculateHeuristicOptions>(UtilityEntrypoints.CalculateHeuristicDistance)
                .WithParsed<CalculateHeuristicSupportOptions>(UtilityEntrypoints.CalculateHeuristicSupport)
                .WithParsed<ComputeNetStatisticsOptions>(UtilityEntrypoints.ComputeNetStatistics)
                .WithParsed<GenerateInstanceOptions>(UtilityEntrypoints.GenerateInstance)
                .WithParsed<AStarUnityFrontierOptions>(SearchQueryEntryPoints.SingleQueryWithHeuristic)
                .WithParsed<BestFirstUnityFrontierOptions>(SearchQueryEntryPoints.SingleQueryWithHeuristic)
                .WithParsed<SaturationSearchOptions>(SearchQueryEntryPoints.SaturationSearch)
                .WithParsed<WFTransformationOptions>(UtilityEntrypoints.TransformToWFNet)
                .WithParsed<ContinuousSoundnessOptions>(SoundnessChecker.VerifyContinuousSoundness)
                .WithParsed<SoundnessOptions>(SoundnessChecker.VerifySoundness)
                .WithParsed<TranslateWFOptions>(UtilityEntrypoints.TranslateWFNet)
                .WithNotParsed((_) => DisplayHelp(parserResult));
#endif
            System.Environment.Exit(0);
        }

        static void DisplayHelp<T>(ParserResult<T> result)
        {
            var helpText = HelpText.AutoBuild(result, h =>
            {
                h.AddEnumValuesToHelpText = true;
                return HelpText.DefaultParsingErrorsHandler(result, h);
            }, e => e);
            Console.WriteLine(helpText);
            System.Environment.Exit(1);
        }
    }
}