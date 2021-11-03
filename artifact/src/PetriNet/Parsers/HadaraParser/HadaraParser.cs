using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections;
using System.Xml;
using System.IO;

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

            FileStream fs = new FileStream(filepath, FileMode.Open, FileAccess.Read);

            XmlReaderSettings settings = new XmlReaderSettings();
            settings.ConformanceLevel = ConformanceLevel.Fragment;
            List<Place> places = new List<Place>();
            Dictionary<String, UpdateTransition> transitions = new Dictionary<string, UpdateTransition>();

            using (XmlReader reader = XmlReader.Create(fs, settings))
            {
                while (reader.Read())
                {
                    if (reader.NodeType != XmlNodeType.Element)
                    {
                        continue;
                    }

                    if (reader.Name == "CGraph" || reader.Name == "CArc")
                    {
                        switch (reader.GetAttribute("Type"))
                        {
                            case "Place":
                                {
                                    var name = reader.GetAttribute("Label");
                                    Place place = new Place(name);
                                    places.Add(place);
                                    break;
                                }
                            case "Transition":
                                {
                                    var name = reader.GetAttribute("Label");
                                    Marking pre = new Marking();
                                    Marking post = new Marking();
                                    transitions[name] = new UpdateTransition(name, pre, post);
                                    break;
                                }
                            case "Arc":
                                {
                                    var sourceName = reader.GetAttribute("Source");
                                    var targetName = reader.GetAttribute("Target");

                                    int weight = reader.ReadElementContentAsInt();

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
                                    break;
                                }

                        }
                    }
                }

            }

            PetriNet net = new PetriNet(places, transitions.Values.ToList());

            return new Tuple<PetriNet, Marking>(net, new Marking());
        }
    }
}