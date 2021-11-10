using CommandLine;
using Petri;
using System;

namespace PetriTool
{
    [Verb("benchmark", HelpText = "run benchmarks on all .lola/.formula files in a folder")]
    public class BenchmarkOptions
    {
        [Option('s', "sample-number",
            HelpText = "How often each benchmark should be run",
            Default = 5)]
        public int sampleNumber { get; set; }

        [OptionAttribute('i', "timeout-tolerance",
            HelpText = "How often a benchmark is allowed to time out before it is designated as 'timeout'",
            Default = 0)]
        public int allowedTimeOutsPerBenchmark { get; set; }

        [Value(0, MetaName = "directory-name", HelpText = "The directory that contains the benchmark files to execute.",
            Required = true)]
        public string directoryPath { get; set; }

        [Value(1, MetaName = "outfile", HelpText = "The file that the output of this utility should be written to.",
            Required = true)]
        public string outfilePath { get; set; }
    }

    public enum GenerationMode
    {
        Reachability,
        Coverability
    }

    [Verb("generate", HelpText =
    "Generate a new reachability/coverability instance from a specified net. " +
    "Starting from the initial marking, a specified number of times an enabled transition is chosen uniformly at random," +
    "the transition is fired, and the process is repeated on the resulting marking." +
    "Finally, the net and the target are written to a desired output path in a given format."
    )]
    public class GenerateInstanceOptions : NetFilepathOption, OutputFormatOptions
    {
        [Value(1, MetaName = "witness-length", HelpText = "How many transitions should be fired to generate the final marking.")]
        public int length { get; set; }

        [Value(2, MetaName = "mode", HelpText = "Whether the target should be a coverability or reachability target.")]
        public GenerationMode generationMode { get; set; }

        public OutputFormat outputFormat { get; set; }
        public string outputFilePath { get; set; }

        [Option('s', "seed", HelpText = "Sets the seed for the random generation of instances. With the same input net, length, mode and seed, the same problem instance will be generated.", Default = null, Required = false)]
        public int? seed { get; set; }

        [Option('p', "forward-pruning", HelpText = "If set, forward-prunes the instance before outputting it.", Default = false, Required = false)]
        public bool forwardPrune { get; set; }

        [Option('b', "backward-pruning", HelpText = "If set, backward-prunes the instance before outputting it.", Default = false, Required = false)]
        public bool backwardPrune { get; set; }
    }

    public class NetFilepathOption
    {
        [Value(0, MetaName = "net-file", HelpText = "The path to a file that contains the Petri Net system. Currently supports .lola, .pnml, .pnet and .tts formats.",
            Required = true)]
        public string netFilePath { get; set; }

        private bool _parameterizedInitialState;

        [Option("single", HelpText = "Only used when parsing .tts instances. Normally, the initial state is assumed to be 0/0, which means state 0 and arbitrarily many tokens in counter 0." +
        "When this option is enabled, the initial state is instead assumed to be 0|0, i.e. state 0 and 1 token in counter 0.", Required = false, Default = false)]
        public bool parameterizedInitialState
        {
            get { return _parameterizedInitialState; }
            set
            {
                // sets the static flag in TTSParser
                // it should only be modified here (outside of tests)
                TTSParser.ParameterizedInitial = !value;
                _parameterizedInitialState = value;
            }
        }
    }

    /// <summary>
    /// Governs the translation to a workflow net. Depending on the option chosen,
    /// the formula and/or the net may be modified.
    /// </summary>
    public enum WorkflowTranslation
    {
        /// <summary>
        /// From initial:1, is final:1 *coverable*?
        /// </summary>
        Coverability,

        /// <summary>
        /// Is the net 1-sound?
        /// </summary>
        Soundness,

        /// <summary>
        /// From initial:1, is final:1 *reachable*?
        /// </summary>
        Reachability,
        
        /// <summary>
        /// Does there exist k such that from initial:k, final:k is *reachable*?
        /// </summary>
        StructuralReachability
    }

    [Verb("translate-wf",
        HelpText = "Translates a workflow net to .lola format and provides a .formula file for checking soundness.")]
    public class TranslateWFOptions : NetFilepathOption, OutputPathOptions
    {

        public string outputFilePath { get; set; }

        [Option('m', "mode",
            HelpText = "Whether to write a formula to check soundness, reachability (of final place), or coverability (of final place)",
            Required = true)]
        public WorkflowTranslation translationMode { get; set; }
    }

    [Verb("add-indicator-places", HelpText = "Add indicator places that ensure each token from the input is used.")]
    public class AddIndicatorPlacesOptions : NetFilepathOption, FormuleFilepathOptions
    {
        public string formulaFilePath { get; set; }

        [Value(2, MetaName = "output-dir", HelpText = "The directory to write the result files to.",
           Required = true)]
        public string outputDir { get; set; }
    }

    [Verb("remove-place", HelpText = "Remove a place from the net and rewrite it to a different file.")]
    public class RemovePlaceOptions : NetFilepathOption, FormuleFilepathOptions
    {
        public string formulaFilePath { get; set; }

        [Value(2, MetaName = "output-dir", HelpText = "The directory to write the result files to.",
           Required = true)]
        public string outputDir { get; set; }

        [Value(3, MetaName = "removed-place", HelpText = "The place to remove.",
            Required = true)]
        public string place { get; set; }
    }

    [Verb("calculate-heuristic", HelpText = "Calculates the heuristic value for a given Petri net, initial marking and final markings.")]
    public class CalculateHeuristicOptions : NetFilepathOption, FormuleFilepathOptions, HeuristicOption, PruningOptions
    {
        public string chosenHeuristic { get; set; }
        public string formulaFilePath { get; set; }
        public bool prune { get; set; }
    }

    [Verb("calculate-support", HelpText = "Calculates the heuristic support for a given Petri net, initial marking and final markings.")]
    public class CalculateHeuristicSupportOptions : NetFilepathOption, FormuleFilepathOptions, PruningOptions
    {
        public string formulaFilePath { get; set; }
        public bool prune { get; set; }
    }

    public interface PruningOptions
    {
        [Option('p', "prune",
            HelpText = @"Use this option if you want the net to be pruned via removal of an unmarked siphon (sign analysis)
                            before running the query.",
            Default = false)]
        public bool prune { get; set; }
    }

    public class SingleQueryOptions : NetFilepathOption, FormuleFilepathOptions, PruningOptions
    {

        [Value(1, MetaName = "formula-file",
            HelpText = "The path to a file that contains the target markings in the .formula format (this format is part of lola).",
            Required = true)]
        public string formulaFilePath { get; set; }

        public bool prune { get; set; }

        [Option('b', "backward-pruning",
                HelpText = @"Use this option if you want the net to be pruned via backward graph reachability before running the query.")]
        public bool backward_pruning { get; set; }

        [Option("precompute-max-tokens",
                HelpText = @"Should only be used with a pseudo-communication free Petri net, i.e. one where
                each transition has only one post place, and each place has a clone transition.", Default = false)]
        public bool precompute_max { get; set; }

        [Option('v', "verbose", HelpText = @"Use this option if you want more extensive output.", Default = false)]
        public bool verbose { get; set; }
    }

    public interface DebugOption
    {
        [Option('v', "verbose", HelpText = @"use this option if you want 
        more verbose output.", Default = false)]
        public bool verbose { get; set; }
    }

    public interface HeuristicOption
    {
        [Option('h', "heuristic",
            HelpText = @"Use this option to choose a heuristic. Call with 'list-heuristics' to see possible values.",
            Default = "list-heuristics",
            Required = true)]
        public string chosenHeuristic { get; set; }
    }


    [Verb("a-star", HelpText = "Run A* for Petri Nets.")]
    public class AStarQueryOptions : SingleQueryOptions, HeuristicOption
    {
        public string chosenHeuristic { get; set; }

        // Is only set programmatically, not via parsing input parameters.
        // Enabling this means output will not be as detailed, but much faster. 
        public bool competitiveMode { get; set; }
    }

    public class UnityFrontierOptions : SingleQueryOptions, HeuristicOption
    {
        public string chosenHeuristic { get; set; }

        // Is only set programmatically, not via parsing input parameters.
        // Enabling this means output will not be as detailed, but much faster. 
        public bool competitiveMode { get; set; }
    }

    [Verb("a-star-unity-frontier", HelpText = "Run A* with unified frontier for Petri Nets.")]
    public class AStarUnityFrontierOptions : UnityFrontierOptions
    {

    }

    [Verb("best-first-unity-frontier", HelpText = "Run GBFS with unified frontier for Petri Nets.")]
    public class BestFirstUnityFrontierOptions : UnityFrontierOptions
    {

    }

    [Verb("best-first", HelpText = "Run Best First Search for Petri Nets.")]
    public class BestFirstQueryOptions : SingleQueryOptions, HeuristicOption
    {
        public string chosenHeuristic { get; set; }
        public bool competitiveMode { get; set; }
    }

    [Verb("witness-check", HelpText = "Tests whether a given witness is actually a witness for the given net and target.")]
    public class WitnessCheckOptions : NetFilepathOption, FormuleFilepathOptions
    {
        [Value(1, MetaName = "formula-file",
            HelpText = "The path to a file that contains the target markings in the .formula format (this format is part of lola).",
            Required = true)]
        public string formulaFilePath { get; set; }

        [Value(2, MetaName = "witness", HelpText = "The witness sequence to be checked, in the form of a quote-enclosed, comma separated list of transition names.", Required = true)]
        public string witness { get; set; }
    }

    public interface OutputFormatOptions : OutputPathOptions
    {
        [Option('f', "output-format", HelpText = "The format to which to write the output.", Required = true)]
        public OutputFormat outputFormat { get; set; }
    }

    public interface OutputPathOptions
    {
        [Option('o', "output-path", HelpText = "The filepath that the output should be written to. The extension is chose automatically. If the chosen format uses multiple files to represent e.g. nets and targets, all necessary files will be written with the same filename, but different extensions.", Required = true)]
        public string outputFilePath { get; set; }
    }

    public interface FormuleFilepathOptions
    {
        [Value(1, MetaName = "formula-inputfile", HelpText = "The path to a file containing a formula in one of the supported formats. Note that the file ending should match the file content, since it determines how the file will be parsed. If no file is given, only the net will be parsed. Depending on the chosen output format, this might not be possible.", Required = false)]
        public string formulaFilePath { get; set; }
    }

    [Verb("translate", HelpText = "Translates nets and formulas from and to the different formats supported by this tool. Note: The output file extension is chosen automatically.")]
    public class TranslationOptions : NetFilepathOption, OutputFormatOptions, FormuleFilepathOptions
    {

        [Value(1, MetaName = "net-inputfile", HelpText = "The path to a file containing a net in one of the supported formats. Note that the file ending should match the file content, since it determines how the file will be parsed.", Required = true)]
        public new string netFilePath { get; set; }

        [Value(2, MetaName = "formula-inputfile", HelpText = "The path to a file containing a formula in one of the supported formats. Note that the file ending should match the file content, since it determines how the file will be parsed. If no file is given, only the net will be parsed. Depending on the chosen output format, this might not be possible.", Required = false)]
        public string formulaFilePath { get; set; }

        public OutputFormat outputFormat { get; set; }
        public string outputFilePath { get; set; }

        [Option("forward-prune",
            HelpText = @"Use this option to perform forward pruning on the net before outputting it in the output format.",
            Required = false,
            Default = false)]
        public bool forwardPrune { get; set; }

        [Option("backward-prune",
            HelpText = @"Use this option to perform backward pruning on the net before outputting it in the output format.",
            Required = false,
            Default = false)]
        public bool backwardPrune { get; set; }

        [Option("transform-wf-net",
            HelpText = @"Use this option to attempt to translate the net into a workflow net while translating. Makes use of the KHA algorithm, see 
            Theorem 5.1, Fundamentals of Control Flow in Workflows, Kiepuszewski et al., 2007.",
            Required = false,
            Default = false)]
        public bool transformWFNet { get; set; }
    }

    [Verb("statistics", HelpText = "Computes several metrics of a given Petri net.")]
    public class ComputeNetStatisticsOptions : NetFilepathOption, FormuleFilepathOptions
    {
        [Value(0, MetaName = "net-inputfile", HelpText = "The path to a file containing a net in one of the supported formats. Note that the file ending should match the file content, since it determines how the file will be parsed.", Required = true)]
        public new string netFilePath { get; set; }

        [Value(1, MetaName = "formula-inputfile", HelpText = "The path to a file containing a formula in one of the supported formats. Note that the file ending should match the file content, since it determines how the file will be parsed. If no file is given, only the net will be parsed. Depending on the chosen output format, this might not be possible.", Required = false)]
        public string formulaFilePath { get; set; }

        [Option('p', "prune", HelpText = "Get statistics about pruning if flag is given, otherwise only get statistics about net itself.")]
        public bool prune { get; set; } = false;


        [Option('m', "monotonic", HelpText = "If set, will check for monotonicity with respect to a given degree.")]
        public int monotonicityDegree { get; set; } = Int32.MinValue;

        [Option("int-sound", HelpText = "If present and assuming that the net is a workflow net, checks integer soundness")]
        public bool checkIntegerSoundness { get; set; } = false;

        [Option("cont-sound", HelpText = "If present and assuming that the net is a workflow net, checks continuous soundness")]
        public bool checkContinuousSoundness { get; set; } = false;
    }

    [Verb("saturation-search", HelpText = "Performs saturation search.")]
    public class SaturationSearchOptions : NetFilepathOption, FormuleFilepathOptions, PruningOptions
    {
        public string formulaFilePath { get; set; }
        public bool prune { get; set; }

        [Option('v', "verbose", HelpText = @"Use this option if you want more extensive output.", Default = false)]
        public bool verbose { get; set; }
    }

    [Verb("transform-wf", HelpText = @"Transforms the input net into a workflow net.  Makes use of the KHA 
    algorithm, see Theorem 5.1, Fundamentals of Control Flow in Workflows, Kiepuszewski et al., 2007.
    Nets that cannot be transformed in a proper way, i.e. because they are not
    free-choice, will be left as-is.
    The initial place is assumed to be given as the initial marking (with a single token).
    If there is not an initial marking of this form and there is a unique source place, 
    that place is assumed to be the initial place.
    If there is also no unique source place, the procedure aborts. 
    ")]
    public class WFTransformationOptions : NetFilepathOption
    {
        [Option('o', "output-path", HelpText = @"The path that the output file should be written to.
        The extension should not be given explicitly, as .lola will be appended to the path automatically.",
        Required = true)]
        public string outputFilePath { get; set; }
    }

    [Verb("continuous-sound", HelpText = @"Checks whether the net is (generalized) continuous sound.")]
    public class ContinuousSoundnessOptions : NetFilepathOption
    {
    }

    [Verb("soundness", HelpText = @"Checks whether the net is k-sound for a given range of indices.")]
    public class SoundnessOptions : NetFilepathOption
    {
        [Option("start_index", Default = 1, HelpText = "The smallest index i for which to check i-soundness. Defaults to 1.")]
        public int startIndex { get; set; }

        [Option("stop_index", Default = 1, HelpText = @"The largest index i for which to check i-soundness. 
If 0, checks up to infinity, but may stop early if invariants determine there exists no i for which the net is sound. Defaults to 0.")]
        public int stopIndex { get; set; }
    }
}