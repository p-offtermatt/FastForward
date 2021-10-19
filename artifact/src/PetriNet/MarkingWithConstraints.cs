using System.Collections.Generic;
using System;
using System.Linq;
using System.Text;

namespace Petri
{
    public class MarkingWithConstraints
    {
        public Constraints Constraints;
        public Marking Marking;

        public MarkingWithConstraints(Marking marking, Constraints constraints)
        {
            this.Marking = marking;
            this.Constraints = constraints;
        }

        public String ToLola()
        {
            return String.Join(" AND \n",
                                this.Marking.Keys.Select(
                                    place =>
                                        place.ToString() + " " + (this.Constraints[place] == ConstraintOperators.Equal ? "=" : ">=") + " " + this.Marking[place].ToString()));
        }

        /// <summary>
        /// Outputs this marking as a string fit for writing into a .prop file (the target file format for TTS).
        /// The output will be a direct conversion of the Petri Net to a VASS; i.e. disregarding any VASS structure the net may have.
        /// </summary>
        /// <returns></returns>
        public String ToTTS_PN()
        {
            // TODO need to take into account BFC has only +1/-1 updates
            throw new NotImplementedException();
        }

        /// <summary>
        /// Outputs this marking as a string fit for writing to a .prop file (the target file format for TTS).
        /// The output will be a backtranslation of the VASS that is encoded in the Petri Net.
        /// </summary>
        /// <param name="parser">The parser used to read the net from a file. This is needed because it contains some meta-information about the net that is stored during the parsing process,
        /// i.e. the way in which states are stored.</param>
        /// <returns>This marking as a target in a string of the .prop format.</returns>
        public string ToTTS_VASS(TTSParser parser)
        {
            if (this.Constraints.Values.Any(constraint => constraint == ConstraintOperators.Equal))
            {
                throw new Exception("The TTS format can only represent coverability targets, but the target marking is " + this.ToString());
            }
            Dictionary<int, int> counterValues = parser.GetCounterValuesFromMarking(this.Marking);
            int stateNumber = parser.GetStateNumberFromMarking(this.Marking);

            StringBuilder builder = new StringBuilder(counterValues.Count);

            builder.Append(stateNumber);
            builder.Append("|");

            bool first = true;

            foreach ((int counter, int value) in counterValues)
            {
                for (int i = 0; i < value; i++)
                {
                    if (!first)
                    {
                        builder.Append(",");
                    }
                    builder.Append(counter.ToString());
                    first = false;
                }
            }
            return builder.ToString();
        }

        /// <summary>
        /// Generates a marking with constraints that ensures any marking
        /// that satisfies the returned marking with constraints covers the input marking.
        /// </summary>
        /// <param name="marking"></param>
        /// <returns></returns>
        public static MarkingWithConstraints MakeCoveringMarking(Marking marking)
        {
            Constraints constraints = new Constraints();
            foreach (Place place in marking.Keys)
            {
                constraints[place] = ConstraintOperators.GreaterEqual;
            }
            return new MarkingWithConstraints(marking, constraints);
        }

        /// <summary>
        /// Checks whether the given marking with constraints represents a reachability query in the net.
        /// False indicates it is not a reachability query, i.e. not every place of the net is constrained
        /// with ConstraintOperators.Equals to be equal to some value.
        /// </summary>
        /// <param name="net"></param>
        /// <param name="markingWithConstraints"></param>
        /// <returns>True if the marking with constraints fixes every place to be equal to some value (potentially 0),
        /// False if it is not the case.</returns>
        public static bool IsReachabilityQueryForNet(PetriNet net, MarkingWithConstraints markingWithConstraints)
        {
            foreach (Place place in net.Places)
            {
                if (markingWithConstraints.Constraints.GetValueOrDefault(place, ConstraintOperators.GreaterEqual) != ConstraintOperators.Equal)
                {
                    return false;
                }
            }
            return true;
        }

        /// <summary>
        /// Returns all places that are required by this marking to be equal to zero in the target.
        /// </summary>
        /// <returns></returns>
        public IEnumerable<Place> GetZeroPlaces()
        {
            return this.Marking.Keys.Where(place => this.Marking[place] == 0 && this.Constraints[place] == ConstraintOperators.Equal);
        }

        public override string ToString()
        {
            string result_string = "";
            foreach (KeyValuePair<Place, ConstraintOperators> pair in this.Constraints)
            {
                result_string +=
                    pair.Key
                    + " " +
                    (pair.Value == ConstraintOperators.Equal ? "=" : ">=")
                    + " " +
                    this.Marking.GetValueOrDefault(pair.Key, 0)
                    + ", ";
            }
            return result_string;
        }

        public bool SatisfiedByMarking(Marking m)
        {
            if (m == null)
            {
                // if this marking is trivially satisfied by anything, return true.
                if (this.Constraints.All((kvPair) => kvPair.Value == ConstraintOperators.GreaterEqual) && this.Marking.All(kvPair => kvPair.Value == 0))
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
            foreach (Place place in m.Keys.Concat(this.Marking.Keys))
            {

                if (this.Constraints.GetValueOrDefault(place, ConstraintOperators.GreaterEqual) == ConstraintOperators.Equal)
                {
                    if (this.Marking.GetValueOrDefault(place, 0) != m.GetValueOrDefault(place, 0))
                    {
                        return false;
                    }
                }
                else if (this.Constraints.GetValueOrDefault(place, ConstraintOperators.GreaterEqual) == ConstraintOperators.GreaterEqual)
                {
                    if (this.Marking.GetValueOrDefault(place, 0) > m.GetValueOrDefault(place, 0))
                    {
                        return false;
                    }
                }
                else
                {
                    throw new ArgumentException("Place " + place.ToString() + " has constraint " +
                    this.Constraints[place].ToString() + ", which is not expected!");
                }
            }

            return true;
        }

        /// <summary>
        /// Transforms a given list of MarkingWithConstraints into a single lola formula.
        /// Multiple formulas are joined by "or" conditions.
        /// </summary>
        /// <param name="markings">A list of target markings.</param>
        /// <returns>
        /// A Lola formula representing reachability of any target marking 
        /// in the form of a string.
        /// </returns>
        public static string ListToLola(List<MarkingWithConstraints> markings)
        {
            String result = "EF (";
            result += String.Join(" OR ", markings.Select(marking => "(" + marking.ToLola() + ")"));
            result += ")";
            return result;
        }

        public String ToDotspec()
        {
            StringBuilder builder = new StringBuilder(this.Marking.Count * 3 * 2);
            builder.AppendJoin(",", this.Marking.Select(kvPair =>
            {
                Place place = kvPair.Key;
                int value = kvPair.Value;
                ConstraintOperators constraint = this.Constraints.GetValueOrDefault(place, ConstraintOperators.GreaterEqual);
                return place.ToString() +
                       (constraint == ConstraintOperators.GreaterEqual ? ">=" : "=") +
                       value.ToString();
            }));
            return builder.ToString();
        }

        public static String ListToDotspec(List<MarkingWithConstraints> markings)
        {
            StringBuilder builder = new StringBuilder(markings.Count * 4);
            builder.AppendJoin("\n", markings.Select(marking => marking.ToDotspec()));
            return builder.ToString();
        }

        public static MarkingWithConstraints AsCoverability(Marking marking)
        {
            Constraints constraints = new Constraints(marking.ToDictionary(kvPair => kvPair.Key, kvPair => ConstraintOperators.GreaterEqual));
            return new MarkingWithConstraints(marking, constraints);
        }

        public string ToTTS_PN(Dictionary<Place, int> placesToCounterNumDict, bool initialMarking) => this.Marking.ToTTS_PN(placesToCounterNumDict, initialMarking);

        public override int GetHashCode()
        {
            return this.Marking.GetHashCode() + this.Constraints.GetHashCode();
        }

        public override bool Equals(object obj)
        {
            if ((obj == null) || !this.GetType().Equals(obj.GetType()))
            {
                return false;
            }
            else
            {
                MarkingWithConstraints other = (MarkingWithConstraints)obj;
                return this.Marking.Equals(other.Marking) && this.Constraints.Equals(other.Constraints);
            }
        }
    }
}