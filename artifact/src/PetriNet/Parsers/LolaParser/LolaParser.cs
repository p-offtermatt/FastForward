using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace Petri
{
    public class LolaParser : FullParser
    {
        public Tuple<PetriNet, Marking> ReadNetFromString(String text)
        {
            var placeBlockMatch = Regex.Match(text, @"PLACE([^;]*;)");

            if (!placeBlockMatch.Success)
            {
                Console.WriteLine("Error: Can not find marking block - needs to start with 'MARKING'!");
            }

            var placeBlock = placeBlockMatch.Groups[1].Value;
            var placeMatches = Regex.Matches(placeBlock, @"([^\s,;]*)[,;]");

            List<Place> places = placeMatches.Select(match => new Place(match.Groups[1].Value)).ToList();


            // handle marking
            var markingBlockMatch = Regex.Match(text, @"MARKING([^;]*)");

            if (!markingBlockMatch.Success)
            {
                Console.WriteLine("Error: Can not find marking block - needs to start with 'MARKING'!");
                System.Environment.Exit(1);
            }

            var markingBlock = markingBlockMatch.Groups[1].Value;
            markingBlock = markingBlock.Trim();

            string[] markings = markingBlock.Split(",");

            Marking initialMarking;

            if (markingBlock != "")
            {
                initialMarking = new Marking(markings.ToDictionary(
                    string_marker => new Place(string_marker.Trim().Split(":")[0].Trim()),
                    string_marker => Int32.Parse(string_marker.Trim().Split(":")[1].Trim())
                ));
            }
            else
            {
                initialMarking = new Marking();
            }

            // all blocks after these first two are transition blocks

            List<UpdateTransition> transitions = new List<UpdateTransition>();

            MatchCollection transitionBlockMatches = Regex.Matches(text, @"TRANSITION *([^\n]*)\n\s*CONSUME\s*([^;]*);\s*PRODUCE\s*([^;]*);");

            foreach (Match transitionBlockMatch in transitionBlockMatches)
            {
                var name = transitionBlockMatch.Groups[1].Value;
                var consumeBlock = transitionBlockMatch.Groups[2].Value;
                var produceBlock = transitionBlockMatch.Groups[3].Value;

                Marking pre;

                if (consumeBlock.Length == 0)
                {
                    pre = new Marking();
                }
                else
                {
                    string[] prePlaceMarkings = consumeBlock.Split(',');
                    pre = new Marking(prePlaceMarkings.ToDictionary(
                        marking => new Place(marking.Split(':')[0].Trim()),
                        marking => Int32.Parse(marking.Split(':')[1].Trim())));
                    pre.RemovePlacesWithNoTokens();
                }

                Marking post;

                if (produceBlock.Length == 0)
                {
                    post = new Marking();
                }
                else
                {
                    string[] postPlaceMarkings = produceBlock.Split(',');
                    post = new Marking(postPlaceMarkings.ToDictionary(
                        marking => new Place(marking.Split(':')[0].Trim()),
                        marking => Int32.Parse(marking.Split(':')[1].Trim())));
                    post.RemovePlacesWithNoTokens();
                }

                UpdateTransition t = new UpdateTransition(name, pre, post);
                transitions.Add(t);
            }

            PetriNet resultNet = new PetriNet(places, transitions);

            // initialMarking.RemovePlacesWithNoTokens();

            return new Tuple<PetriNet, Marking>(resultNet, initialMarking);
        }

        /// <summary>
        /// Reads a Petri net and initial marking from a file in .lola format.
        /// </summary>
        /// <returns>A tuple containing the net and the initial marking.</returns>
        public override Tuple<PetriNet, Marking> ReadNet(String filepath)
        {
            string text = System.IO.File.ReadAllText(filepath).Trim();

            return ReadNetFromString(text);
        }

        public override List<MarkingWithConstraints> ReadFormula(String filepath)
        {
            string text = System.IO.File.ReadAllText(filepath).Trim();

            // expression is a disjunction of conjunctions -> we just care about finding the innermost parentheses,
            // whatever is between them must be a conjunction
            Regex conjunction_pattern = new Regex(@"\([^()]*\)");
            MatchCollection conjunctions = conjunction_pattern.Matches(text);

            List<MarkingWithConstraints> result = new List<MarkingWithConstraints>();

            foreach (Match conjunction in conjunctions)
            {

                Regex comparison_pattern = new Regex(@"([^(\s]*)\s*(>?=)\s*(\d*)");
                MatchCollection comparisons = comparison_pattern.Matches(conjunction.Value);

                Constraints constraints = new Constraints(comparisons.ToDictionary(
                    x => new Place(x.Groups[1].Value),
                    x => x.Groups[2].Value == ">=" ? ConstraintOperators.GreaterEqual :
                          x.Groups[2].Value == "=" ? ConstraintOperators.Equal :
                          throw new System.ArgumentException("Could not understand comparison operator " + x.Groups[2].Value)));

                Marking marking = new Marking(comparisons.ToDictionary(
                    x => new Place(x.Groups[1].Value),
                    x => Int32.Parse(x.Groups[3].Value)
                ));

                MarkingWithConstraints target = new MarkingWithConstraints(marking, constraints);
                result.Add(target);
            }

            return result;

        }
    }
}