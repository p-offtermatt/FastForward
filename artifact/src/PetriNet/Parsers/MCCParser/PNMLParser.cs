using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections;
using System.Xml;

namespace Petri
{
    public class PNMLParser : NetParser
    {
        /// <summary>
        /// Reads a Petri net and initial marking from a file in .pnml format.
        /// </summary>
        /// <returns>A tuple containing the net and the initial marking.</returns>
        public override Tuple<PetriNet, Marking> ReadNet(String filepath)
        {

            XmlDocument netDoc = new XmlDocument();
            netDoc.Load(filepath);

            // create ns manager
            XmlNamespaceManager xmlnsManager = new XmlNamespaceManager(netDoc.NameTable);
            xmlnsManager.AddNamespace("def", "");


            var placeNodes = netDoc.SelectNodes("//def:place", xmlnsManager);

            // depending on the details of the xml,
            // need a different namespace.
            // solution: try possibilities until one yields any place nodes
            if (placeNodes.Count == 0)
            {
                xmlnsManager.AddNamespace("def", "http://www.pnml.org/version-2009/grammar/pnml");
                placeNodes = netDoc.SelectNodes("//def:place", xmlnsManager);
            }



            Marking initialMarking = new Marking();

            List<Place> places = new List<Place>(placeNodes.Count);

            foreach (XmlNode node in placeNodes)
            {
                var markingNode = node.SelectSingleNode(".//def:initialMarking", xmlnsManager);
                var name = node.Attributes["id"].Value;
                var initialTokenAmount = markingNode != null ? markingNode.InnerText : null;
                Place place = new Place(name);
                places.Add(place);
                if (initialTokenAmount != null)
                {
                    initialMarking[place] = Int32.Parse(initialTokenAmount);
                }
            }

            var transitionNodes = netDoc.SelectNodes("//def:transition", xmlnsManager);
            Dictionary<String, UpdateTransition> transitions = new Dictionary<string, UpdateTransition>(transitionNodes.Count);

            foreach (XmlNode transitionNode in transitionNodes)
            {
                var name = transitionNode.Attributes["id"].Value;
                Marking pre = new Marking();
                Marking post = new Marking();
                transitions[name] = new UpdateTransition(name, pre, post);
            }

            foreach (XmlNode arcNode in netDoc.SelectNodes("//def:arc", xmlnsManager))
            {
                var sourceName = arcNode.Attributes["source"].Value;
                var targetName = arcNode.Attributes["target"].Value;

                var inscriptionNode = arcNode.SelectSingleNode(".//def:inscription", xmlnsManager);
                int weight;
                if (inscriptionNode != null)
                {
                    var textNode = inscriptionNode.SelectSingleNode(".//def:text", xmlnsManager);
                    weight = Int32.Parse(textNode.InnerText);
                }
                else
                {
                    weight = 1;
                }

                // exactly one of source and target will be a transition and one will be a place
                if (transitions.ContainsKey(sourceName))
                {
                    // source is a transition
                    UpdateTransition transition = transitions[sourceName];
                    Place targetPlace = new Place(targetName);
                    transition.Post[targetPlace] = transition.Post.GetValueOrDefault(targetPlace, 0) + weight;
                }
                else
                {
                    // target is a transition
                    UpdateTransition transition = transitions[targetName];
                    Place sourcePlace = new Place(sourceName);
                    transition.Pre[sourcePlace] = transition.Pre.GetValueOrDefault(sourcePlace, 0) + weight;
                }
            }

            PetriNet net = new PetriNet(places, transitions.Values.ToList());

            return new Tuple<PetriNet, Marking>(net, initialMarking);
        }
    }
}