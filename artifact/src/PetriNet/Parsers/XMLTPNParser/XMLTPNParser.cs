using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace Petri
{
    public class XMLTPNParser : NetParser
    {
        public Tuple<PetriNet, Marking> ReadNetFromString(String text)
        {
            // handle places and initial marking
            String placePattern = @"^place\s*""(.*)""(?:\s*init\s*([0-9]*))?;";
            var placeMatches = Regex.Matches(text, placePattern, RegexOptions.Multiline | RegexOptions.IgnoreCase);

            List<Place> places = new List<Place>();

            Marking initialMarking = new Marking();

            foreach (Match match in placeMatches)
            {
                var placeName = match.Groups[1].Value;
                Place place = new Place(placeName);
                places.Add(place);

                if (match.Groups[2].Success)
                {
                    // Place is initially marked
                    var tokenCount = Int32.Parse(match.Groups[2].Value);
                    initialMarking.Add(place, tokenCount);
                }
            }

            // handle transitions
            List<UpdateTransition> transitions = new List<UpdateTransition>();

            MatchCollection transitionBlockMatches =
                Regex.Matches(text,
                    @"^trans\s*([^\n\r]*)\s*in ((?:""[^""]*""\s*)*)\s*out ((?:""[^""]*""\s*)*);",
                    RegexOptions.Multiline | RegexOptions.IgnoreCase
                );

            String transitionPlacePattern = @"""([^""\n]*)""";

            foreach (Match transitionBlockMatch in transitionBlockMatches)
            {
                var name = transitionBlockMatch.Groups[1].Value;
                var consumeBlock = transitionBlockMatch.Groups[2].Value;
                var produceBlock = transitionBlockMatch.Groups[3].Value;

                MatchCollection prePlaceMatches = Regex.Matches(consumeBlock, transitionPlacePattern, RegexOptions.Multiline | RegexOptions.IgnoreCase);
                Dictionary<Place, int> pre = new Dictionary<Place, int>();

                foreach(Match placeMatch in prePlaceMatches){
                    Place place = new Place(placeMatch.Groups[1].Value);
                    pre[place] = pre.GetValueOrDefault(place, 0) + 1;
                }
                


                MatchCollection postPlaceMatches = Regex.Matches(produceBlock, transitionPlacePattern, RegexOptions.Multiline | RegexOptions.IgnoreCase);
                Dictionary<Place, int> post = new Dictionary<Place, int>();

                foreach(Match placeMatch in postPlaceMatches){
                    Place place = new Place(placeMatch.Groups[1].Value);
                    post[place] = post.GetValueOrDefault(place, 0) + 1;
                }

                UpdateTransition transition = new UpdateTransition(name, pre, post);
                transitions.Add(transition);
            }

            PetriNet resultNet = new PetriNet(places, transitions);

            // initialMarking.RemovePlacesWithNoTokens();

            return new Tuple<PetriNet, Marking>(resultNet, initialMarking);
        }

        /// <summary>
        /// Reads a Petri net and initial marking from a file in .xml.tpn format.
        /// </summary>
        /// <returns>A tuple containing the net and the initial marking.</returns>
        public override Tuple<PetriNet, Marking> ReadNet(String filepath)
        {
            string text = System.IO.File.ReadAllText(filepath).Trim();

            return ReadNetFromString(text);
        }
    }
}