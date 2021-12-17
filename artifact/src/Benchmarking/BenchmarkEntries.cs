using System.Collections.Generic;
using System;
using System.Linq;
using SearchAlgorithms;
using Newtonsoft.Json;
using System.Text;
using System.IO;

namespace Benchmark
{
    public class BenchmarkEntry
    {
        public long timeInQuery { get; set; }
        public int numberOfPlaces { get; set; }
        public int numberOfTransitions { get; set; }
    }

    public class BenchmarkEntryWithHeuristics : BenchmarkEntry
    {

        public long timeHeuristicInit { get; set; }

        public long timeInHeuristicCalculation { get; set; } = 0;

        public string ToJSON()
        {
            return Newtonsoft.Json.JsonConvert.SerializeObject(this);
        }
    }

    public class SearchBenchmarkEntry : BenchmarkEntryWithHeuristics
    {
        public long timeTakenPruning { get; set; }

        public int numberOfPlacesAfterForwardPruning { get; set; }

        public int numberOfTransitionsAfterForwardPruning { get; set; }

        public int numberOfPlacesAfterBackwardPruning { get; set; }

        public int numberOfTransitionsAfterBackwardPruning { get; set; }

        public int numberOfTransitionsInHeuristicSupport { get; set; }

        public long timeInAlgorithm { get; set; }

        public string path { get; set; }

        public int ExpandedNodes { get; set; } = 0;

        public int timesHeuristicCalculated { get; set; } = 0;

        public long timeTakenForwardPruning { get; set; }

        public long timeTakenBackwardPruning { get; set; }

        public long timeForNetParsing { get; set; }

        public long timeForFormulaParsing { get; set; }

        public new string ToJSON()
        {
#if COMP
            return "{\"path\": \""+ this.path +"\"}";
#else
            return Newtonsoft.Json.JsonConvert.SerializeObject(this);
#endif
        }
    }

    public class SaturationSearchBenchmarkEntry : SearchBenchmarkEntry
    {
        public long numberOfIterations { get; set; } = 0;
        public string transitionSetSizePerIteration { get; set; } = "";
    }

    public class SoundnessBenchmarkEntry : BenchmarkEntry
    {
        public bool isContinuousSound { set; get; }

        public bool isSound { set; get; }

        public int soundNumber;

        public string counterexampleMarking { set; get; }

        public string ToJSON()
        {
            return Newtonsoft.Json.JsonConvert.SerializeObject(this);
        }
    }

    public class ContinuousSoundnessBenchmarkEntry : BenchmarkEntry
    {
        public bool isZBounded { set; get; }
        public string ZBoundednessCounterexample { set; get; }
        public bool isContinuousSound { set; get; }
        public string continuousSoundnessCounterexample { set; get; }

        public string ToJSON()
        {
            return Newtonsoft.Json.JsonConvert.SerializeObject(this);
        }
    }

    public class SoundnessViaTransitionBenchmarkEntry : BenchmarkEntry
    {

        public int checkedIndex { set; get; }
        public bool allTransitionsExpressible { set; get; }
        public string counterexampleTransition { set; get; }

        public int numberOfCoverableTransitions { set; get; }

        public string uncoverableTransitions { set; get; }

        public long timeForRemovingUncoverableTransitions;
        public long timeForCheckingTransitionExpression;

        public string ToJSON()
        {
            return Newtonsoft.Json.JsonConvert.SerializeObject(this);
        }
    }

    public class LoopFindingBenchmarkEntry : SearchBenchmarkEntry
    {
        public long timeFindingHandle { set; get; } = 0;
        public long timeFindingLoop { set; get; } = 0;

        public string handle { set; get; }

        public string loop { set; get; }

        public string cycleInitialMarking { set; get; }
        public string cycleStepMarking { set; get; }

        public new string ToJSON()
        {
#if COMP
            return "{\"path\": \"" + this.path + "\", \"handle\": \""+ this.handle +"\", \"loop\": \""+ this.loop +"\"}";
#else
            return Newtonsoft.Json.JsonConvert.SerializeObject(this);
#endif
        }
    }

    public class UnrollingBenchmarkEntry : SearchBenchmarkEntry
    {
        public long timeFindingFirstBound { set; get; } = 0;
        public long timeFindingPlaceBounds { set; get; } = 0;

        public int pathLengthBound { set; get; }

        public long timeSpentUnrolling { set; get; } = 0;

        public string bestUnrolledSolutionMarking { set; get; }
        public string bestUnrolledTransitionSequence { set; get; }

        public double bestUnrolledHeuristicValue { set; get; }

        public string heuristicTransitions { set; get; }

        public string heuristicFinalMarking { set; get; }


        public new string ToJSON()
        {
#if COMP
            return "{\"path\": \""+ this.path +"\"}";
#else
            return Newtonsoft.Json.JsonConvert.SerializeObject(this);
#endif
        }
    }

    public class NetStatisticsEntry
    {
        public int places;
        public int transitions;


        public double fractionOfBioTransitions;

        public int bioTransitions;

        public int numberOfSelfLoops;
        public int numberOfNiceSelfLoops;


        // statistics about the number of places in the pre of transitions

        public double minimalPre;
        public double maximalPre;
        public double medianPre;
        public double meanPre;
        public double firstQuartilePre;
        public double thirdQuartilePre;

        // statistics about the number of places in the post of transitions

        public double minimalPost;
        public double maximalPost;
        public double medianPost;
        public double meanPost;
        public double firstQuartilePost;
        public double thirdQuartilePost;

        // statistics about in degree of places
        public double minimalInDeg;
        public double maximalInDeg;
        public double medianInDeg;
        public double meanInDeg;
        public double firstQuartileInDeg;
        public double thirdQuartileInDeg;

        // statistics about out degree of places
        public double minimalOutDeg;
        public double maximalOutDeg;
        public double medianOutDeg;
        public double meanOutDeg;
        public double firstQuartileOutDeg;
        public double thirdQuartileOutDeg;

        // statistics about pruning
        public int placesAfterPruning;
        public int transitionsAfterPruning;

        public double timeTakenPruning;

        public double fractionOfPlacesPruned;

        public double fractionOfTransitionsPruned;

        public long timeTakenForwardPruning;

        public int placesAfterForwardPruning;

        public int transitionsAfterForwardPruning;

        public long timeTakenBackwardPruning;

        public int placesAfterBackwardPruning;

        public int transitionsAfterBackwardPruning;

        // Monotonicity

        public string checkedMonotonicityDegree;

        public string monotonicPlaceOrder;

        public bool isMonotonic;

        // Net classes

        // State Machine Nets: Transitions have one input and output

        public bool isStateMachineNet;
        public double fractionOfStateMachineTransitions;
        public int numberOfStateMachineTransitions;

        // Marked graph: Places have (at most) one incoming and one outgoing transition

        public bool isMarkedGraph;

        public double fractionOfMarkedGraphPlaces;
        public int numberOfMarkedGraphPlaces;

        // Workflow net: dedicated input/output place, all transitions & places lie on some graph path from initial to final

        public bool isWorkflowNet;

        public string sourcePlaces;
        public string sinkPlaces;

        // Free Choice: Any places share either no outgoing transitions, or all of them
        public bool isFreeChoice;

        // Input files

        public string netFile;
        public string formulaFile;

        // Weights

        public double meanWeight;
        public double medianWeight;
        public double minimalWeight;
        public double maximalWeight;
        public double firstQuartileWeight;
        public double thirdQuartileWeight;

        // Integer Boundedness

        public string integerBoundednessCounterexample;

        // Check Integer Boundedness with WF short-circuit edge
        public string wfIntegerBoundednessCounterexample;

        public int wfIntegerBoundednessCounterexampleSupportSize;
        public double wfIntegerBoundednessCounterexampleImageSize;

        public double timeForWFIntegerBoundednessCounterexample;

        // Continuous Soundness
        public bool isContinuousSound;

        public string continuousSoundnessCounterexample;

        public long timeForContinuousSoundness;

        // Integer Soundness
        public bool isIntegerSound;

        public string integerSoundnessCounterexample;

        public long timeForIntegerSoundness;

        public string ToJSON()
        {
            return Newtonsoft.Json.JsonConvert.SerializeObject(this);
        }
    }
}