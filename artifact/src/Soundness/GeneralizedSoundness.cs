using PetriTool;
using Petri;
using System;
using System.Diagnostics;
using Benchmark;
using System.Linq;

namespace Soundness
{
    public static class GeneralizedSoundness
    {
        public static void VerifyContinuousSoundness(VerifyContinuousSoundnessOptions options)
        {
            SoundnessBenchmarkEntry benchmarkEntry = new SoundnessBenchmarkEntry();

            NetParser parser = ParserPicker.ChooseNetParser(options.netFilePath);
            (PetriNet net, Marking initialMarking) = parser.ReadNet(options.netFilePath);

            benchmarkEntry.numberOfPlaces = net.Places.Count;
            benchmarkEntry.numberOfTransitions = net.Transitions.Count;

            Stopwatch queryWatch = Stopwatch.StartNew();
            var (isSound, counterexample) = Z3Heuristics.IsContinuousSound_ViaContinuousReach(net, initialMarking);
            queryWatch.Stop();
            benchmarkEntry.timeInQuery = queryWatch.ElapsedMilliseconds;

            benchmarkEntry.isContinuousSound = isSound;
            if (!isSound)
            {
                benchmarkEntry.counterexampleMarking = String.Join(", ", counterexample.Where(pair => pair.Value > 0));
            }
            Console.WriteLine(benchmarkEntry.ToJSON());
        }
    }
}