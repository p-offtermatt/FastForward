using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections;
using System.Xml;

namespace Petri
{
    public class HadaraParser : NetParser
    {
        /// <summary>
        /// Reads a Petri net and initial marking from a file in .xml (Hadara, see https://github.com/LoW12/Hadara-AdSimul) format.
        /// </summary>
        /// <returns>A tuple containing the net and the initial marking.</returns>
        public override Tuple<PetriNet, Marking> ReadNet(String filepath)
        {

            XmlDocument netDoc = new XmlDocument();
            netDoc.Load(filepath);

            // create ns manager
            XmlNamespaceManager xmlnsManager = new XmlNamespaceManager(netDoc.NameTable);
            xmlnsManager.AddNamespace("def", "");


            var placeNodes = netDoc.SelectNodes("//def:CGraph[@Type='Place']", xmlnsManager);

            List<Place> places = new List<Place>(placeNodes.Count);

            foreach (XmlNode node in placeNodes)
            {
                var name = node.Attributes["Label"].Value;
                Place place = new Place(name);
                places.Add(place);
            }

            var transitionNodes = netDoc.SelectNodes("//def:CGraph[@Type='Transition']", xmlnsManager);
            Dictionary<String, UpdateTransition> transitions = new Dictionary<string, UpdateTransition>(transitionNodes.Count);

            foreach (XmlNode transitionNode in transitionNodes)
            {
                var name = transitionNode.Attributes["Label"].Value;
                Marking pre = new Marking();
                Marking post = new Marking();
                transitions[name] = new UpdateTransition(name, pre, post);
            }

            foreach (XmlNode arcNode in netDoc.SelectNodes("//def:CArc[@Type='Arc']", xmlnsManager))
            {
                var sourceName = arcNode.Attributes["Source"].Value;
                var targetName = arcNode.Attributes["Target"].Value;

                int weight = Int32.Parse(arcNode.InnerXml);

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

            return new Tuple<PetriNet, Marking>(net, new Marking());
        }
    }
}