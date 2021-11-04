using PetriTool;
using Petri;
using System;
using System.Diagnostics;
using Benchmark;
using System.Linq;

namespace Soundness
{
    public static class SoundnessChecker
    {
        public static void VerifyContinuousSoundness(ContinuousSoundnessOptions options)
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

        public static void VerifySoundness(SoundnessOptions options)
        {
            return;
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

        public static bool CheckSoundness(PetriNet net, Place initialPlace, Place finalPlace, int startIndex, int stopIndex)
        {
            for (int i = startIndex; i <= stopIndex; i++)
            {
                Marking initialMarking = new Marking();
                initialMarking[initialPlace] = i;


                PetriNetUtils.PetriNetAStar(net, initialMarking, )
            }
            return false;
        }
    }
}