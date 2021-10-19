using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.IO;

namespace Petri
{
    public abstract class TTSParser : FullParser
    {
        protected int NumStates;
        protected int NumCounters;

        // todo set this programmatically. 
        // false means initial state is 0|0 (state 0 and counter 0 has 1 token)
        // true means initial state is 0/0 (staet 0 and counter 0 has arbitrarily many tokens)
        public static bool ParameterizedInitial = true;

        /// <summary>
        /// Reads a Petri net and initial marking from a file in .tts format.
        /// </summary>
        /// <returns>A tuple containing the net and the initial marking.</returns>
        public override Tuple<PetriNet, Marking> ReadNet(String filepath)
        {
            StreamReader reader = new StreamReader(filepath);
            string line = reader.ReadLine();
            while (line.StartsWith('#') || line.Trim() == "")
            {
                line = reader.ReadLine();
            }

            string firstLine = line;

            (int numStates, int numCounters) = GetVASSDimensions(firstLine);
            this.NumStates = numStates;
            this.NumCounters = numCounters;

            IEnumerable<Place> places = GeneratePlaces(numStates, numCounters);

            HashSet<Transition> transitions = new HashSet<Transition>(); // -1 due to declaration line

            int curIndex = 0;

            // skip first declaration line
            while ((line = reader.ReadLine()) != null)
            {
                line = line.Trim();

                if (line == "" || line.StartsWith("#"))
                {
                    continue;
                }
                // read line of form "0 1 -> 1 1 transfers", for now read everything but multitransfers
                string[] contents = line.Trim().Split(new char[0], 6, StringSplitOptions.RemoveEmptyEntries);
                int sourceState = Int32.Parse(contents[0]);
                int sourceCounter = Int32.Parse(contents[1]);
                string transitionType = contents[2];
                int targetState = Int32.Parse(contents[3]);
                int targetCounter = Int32.Parse(contents[4]);

                Place sourceCounterPlace = GetCounterPlace(sourceCounter);
                Place targetCounterPlace = GetCounterPlace(targetCounter);

                Dictionary<Place, int> pre = GetPreForState(sourceState);
                Dictionary<Place, int> post = GetPostForState(targetState);

                switch (transitionType)
                {
                    case "+>":
                        pre[GetCounterPlace(sourceCounter)] = pre.GetValueOrDefault(GetCounterPlace(sourceCounter), 0) + 1;

                        post[GetCounterPlace(targetCounter)] = post.GetValueOrDefault(GetCounterPlace(targetCounter), 0) + 1;
                        post[GetCounterPlace(sourceCounter)] = post.GetValueOrDefault(GetCounterPlace(sourceCounter), 0) + 1;
                        transitions.Add(new UpdateTransition(
                            "update_" + curIndex,
                            pre,
                            post
                        ));
                        break;
                    case "~>": //transfer
                        transitions.Add(new SetTransferTransition(
                                "transfer_" + curIndex,
                                new HashSet<Place> { sourceCounterPlace },
                                new HashSet<Place> { GetCounterPlace(targetCounter) },
                                pre,
                                post
                            ));
                        break;
                    case "->": // update (or multitransfer with update part)
                        pre[sourceCounterPlace] = pre.GetValueOrDefault(sourceCounterPlace, 0) + 1;
                        post[targetCounterPlace] = post.GetValueOrDefault(targetCounterPlace, 0) + 1;
                        if (contents.Length == 6) // additional stuff -> it's actually a multitransfer of the form "1 1 -> 1 2    2 ~> 3    1 ~> 1"...
                        {
                            string additionalContent = contents[5];

                            Dictionary<Place, HashSet<Place>> placesToTransferTargets = new Dictionary<Place, HashSet<Place>>();

                            var matches = Regex.Matches(additionalContent, @"([0-9]+) ~> ([0-9]+)");
                            foreach (Match match in matches)
                            {
                                int inputCounter = Int32.Parse(match.Groups[1].Value);
                                int outputCounter = Int32.Parse(match.Groups[2].Value);

                                Place inputPlace = GetCounterPlace(inputCounter);
                                Place outputPlace = GetCounterPlace(outputCounter);

                                HashSet<Place> curTransferTargetForPlaces = placesToTransferTargets.GetValueOrDefault(inputPlace, new HashSet<Place>());
                                curTransferTargetForPlaces.Add(outputPlace);
                                placesToTransferTargets[inputPlace] = curTransferTargetForPlaces;
                            }

                            transitions.Add(
                                new MultitransferTransition("multitransfer_" + curIndex,
                                placesToTransferTargets,
                                pre,
                                post)
                            );
                        }
                        else if (contents.Length == 5) // just update transition, no additional stuff
                        {
                            transitions.Add(new UpdateTransition("update_" + curIndex, pre, post));
                        }
                        break;

                }
                curIndex++;
            }



            Place counterZeroPlace = GetCounterPlace(0);

            // add additional, artificial loop transition
            if (ParameterizedInitial)
            {
                transitions.Add(new UpdateTransition(
                    "t_" + counterZeroPlace.Name,
                    new Dictionary<Place, int>(),
                    new Dictionary<Place, int> { [counterZeroPlace] = 1 })
                );
            }

            PetriNet net = new PetriNet(places, transitions);

            Marking initialMarking = new Marking(GetPostForState(0));
            initialMarking[counterZeroPlace] = 1;

            return new Tuple<PetriNet, Marking>(net, initialMarking);
        }

        public override List<MarkingWithConstraints> ReadFormula(String filepath)
        {
            IEnumerable<string> lines = System.IO.File.ReadAllLines(filepath);
            lines = lines.Where(line => line.Trim() != "");

            if (lines.Count() >= 2)
            {
                throw new FormatException("Input file not in right format - there should not be multiple lines!");
            }

            string line = lines.First();

            string[] parts = line.Split("|");
            int stateNumber = Int32.Parse(parts[0]);

            Marking stateMarking = GetMarkingForState(stateNumber);

            string[] counters = parts[1].Split(",");

            foreach (string counter in counters)
            {
                int counterNumber = Int32.Parse(counter);
                Place counterPlace = GetCounterPlace(counterNumber);
                stateMarking[counterPlace] = stateMarking.GetValueOrDefault(counterPlace, 0) + 1;
            }

            Constraints constraints = new Constraints(stateMarking.Keys.ToDictionary(place => place, place => ConstraintOperators.GreaterEqual));
            return new List<MarkingWithConstraints> { new MarkingWithConstraints(stateMarking, constraints) };
        }

        private Tuple<int, int> GetVASSDimensions(string declarationLine)
        {
            // splits on whitespace
            string[] contents = declarationLine.Split(new char[0], StringSplitOptions.RemoveEmptyEntries);
            int numStates = Int32.Parse(contents[0]);
            int numCounters = Int32.Parse(contents[1]);
            return new Tuple<int, int>(numStates, numCounters);
        }

        private IEnumerable<Place> GeneratePlaces(int numStates, int numCounters)
        {
            return GenerateCounterPlaces(numCounters).Union(GenerateStatePlaces(numStates));
        }

        protected abstract HashSet<Place> GenerateStatePlaces(int numStates);

        private HashSet<Place> GenerateCounterPlaces(int numCounters)
        {
            HashSet<Place> result = new HashSet<Place>(numCounters);
            for (int i = 0; i < numCounters; i++)
            {
                result.Add(GetCounterPlace(i));
            }
            return result;
        }

        public abstract Dictionary<Place, int> GetPreForState(int state);
        public abstract Dictionary<Place, int> GetPostForState(int state);

        public Marking GetMarkingForState(int state) => new Petri.Marking(GetPreForState(state));

        public Place GetCounterPlace(int counterNum)
        {
            return new Place("counter" + counterNum);
        }

        public abstract int GetStateNumberFromMarking(Marking marking);

        public Dictionary<int, int> GetCounterValuesFromMarking(Marking marking)
        {
            Dictionary<int, int> result = new Dictionary<int, int>();
            for (int curCounter = 0; curCounter < NumCounters; curCounter++)
            {
                Place place = GetCounterPlace(curCounter);
                result[curCounter] = marking.GetValueOrDefault(place, 0);
            }
            return result;
        }

        public Marking GetMarking(int state, params int[] counters)
        {
            Dictionary<Place, int> result = GetMarkingForState(state);
            foreach (int counterNum in counters)
            {
                Place counterPlace = GetCounterPlace(counterNum);
                result[counterPlace] = result.GetValueOrDefault(counterPlace, 0) + 1;
            }

            Marking marking = new Marking(result);
            return marking;
        }
    }
}