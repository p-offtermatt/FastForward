using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace Petri
{
    public class PnetParser : NetParser
    {
        /// <summary>
        /// Reads a Petri net and initial marking from a file in .pnml format.
        /// </summary>
        /// <returns>A tuple containing the net and the initial marking.</returns>
        public override Tuple<PetriNet, Marking> ReadNet(String filepath)
        {
            string text = System.IO.File.ReadAllText(filepath).Trim();

            var lines = text.Split("\n");
            var filteredLines = lines.Where(line => !line.Trim().StartsWith("//"));
            text = String.Join("\n", filteredLines);

            // extract places
            var placeBlockMatch = Regex.Match(text, @"places\s*{([^{}}]*)}").Groups[1].Value;

            var placeMatches = Regex.Matches(placeBlockMatch, @"[^{}\s]+");

            HashSet<Place> places = new HashSet<Place>();
            foreach (Match placeMatch in placeMatches)
            {
                places.Add(new Place(placeMatch.Value));
            }

            // extract transition
            var transitionBlockMatch = Regex.Match(text, @"transitions\s*{([^{}}]*)}").Groups[1].Value;


            var transitionMatches = Regex.Matches(transitionBlockMatch, @"[^{}\s]+");

            HashSet<UpdateTransition> transitions = new HashSet<UpdateTransition>();

            foreach (Match transitionMatch in transitionMatches)
            {
                string transitionName = transitionMatch.Value;

                // find arc(s) labelled with this transition
                Match preMatch = Regex.Match(text, @"^([^\n]+)->\s*" + transitionName + @"[^\n]*$", RegexOptions.Multiline);
                Match postMatch = Regex.Match(text, @"^[^\n]*" + transitionName + @"\s*->([^\n]+)$", RegexOptions.Multiline);


                IEnumerable<string> prePlaces = new HashSet<string>();
                IEnumerable<string> postPlaces = new HashSet<string>();

                if (preMatch.Success)
                {

                    var preBlock = preMatch.Groups[1].Value;

                    prePlaces = Regex.Matches(preBlock, @"[^\s{}]+").Select(match => match.Value);
                }

                if (postMatch.Success)
                {

                    var postBlock = postMatch.Groups[1].Value;

                    postPlaces = Regex.Matches(postBlock, @"[^\s{}]+").Select(match => match.Value);
                }

                UpdateTransition transition = new UpdateTransition(transitionName,
                    prePlaces.ToDictionary(placeName => new Place(placeName), _ => 1),
                    postPlaces.ToDictionary(placeName => new Place(placeName), _ => 1));

                transitions.Add(transition);

            }

            // extract initial marking
            var markingBlockMatch = Regex.Match(text, @"initial\s*{([^{}}]*)}").Groups[1].Value;

            var markedPlaceMatches = Regex.Matches(markingBlockMatch, @"[^\s{}]*").Where(match => match.Value != "");

            Marking initialMarking = new Marking(markedPlaceMatches.ToDictionary(match => new Place(match.Value), _ => 1));

            PetriNet resultNet = new PetriNet(places, transitions);
            return new Tuple<PetriNet, Marking>(resultNet, initialMarking);

        }

        public Tuple<IEnumerable<IEnumerable<Transition>>, IEnumerable<Transition>> ReadCycleConditions(PetriNet net, String filepath)
        {
            string text = System.IO.File.ReadAllText(filepath).Trim();

            // extract liveness property block
            var livenessBlock = Regex.Match(text, @"^[^\n]*liveness property[^\n]*{([^{}]*)}", RegexOptions.Multiline).Groups[1].Value;

            var greaterZeroMatches = Regex.Matches(text, @"(([^\s]+\s*\+\s*)*[^\s]+)\s*((>\s*0)|(>=\s*1))");
            var equalZeroMatches = Regex.Matches(text, @"(([^\s]+\s*\+\s*)*[^\s]+)\s*=\s*0");


            // from the transitions in each of these sets, at least one should be greater than zero
            HashSet<IEnumerable<Transition>> atleastOneGreaterZeroTransitionSets = new HashSet<IEnumerable<Transition>>();

            HashSet<Transition> equalZeroTransitions = new HashSet<Transition>();

            foreach (Match equalZeroMatch in equalZeroMatches)
            {
                string involvedTransitionsBlock = equalZeroMatch.Groups[1].Value;
                MatchCollection transitionMatches = Regex.Matches(involvedTransitionsBlock, @"([^\s\+]+)");
                IEnumerable<string> namesOfInvolvedTransitions = transitionMatches.Select(match => match.Value);
                IEnumerable<Transition> involvedTransitions = namesOfInvolvedTransitions.Select(name => net.GetTransitionByName(name));

                equalZeroTransitions.UnionWith(involvedTransitions);
            }

            foreach (Match greaterZeroMatch in greaterZeroMatches)
            {
                string involvedTransitionsBlock = greaterZeroMatch.Groups[1].Value;

                MatchCollection transitionMatches = Regex.Matches(involvedTransitionsBlock, @"([^\s\+]+)");
                IEnumerable<string> namesOfInvolvedTransitions = transitionMatches.Select(match => match.Value);
                IEnumerable<Transition> involvedTransitions = namesOfInvolvedTransitions.Where(name => equalZeroTransitions.All(transition => transition.Name != name)).Select(name => net.GetTransitionByName(name));

                atleastOneGreaterZeroTransitionSets.Add(involvedTransitions);
            }

            return new Tuple<IEnumerable<IEnumerable<Transition>>, IEnumerable<Transition>>(atleastOneGreaterZeroTransitionSets, equalZeroTransitions);
        }
    }
}