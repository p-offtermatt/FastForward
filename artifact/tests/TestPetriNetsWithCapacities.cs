using Xunit;
using System.Collections.Generic;
using Petri;
using System.Linq;

namespace Testing
{


    public class TestCapacities
    {

        [Fact]
        public void TestConstructionFromNet()
        {
            List<UpdateTransition> transitions = new List<UpdateTransition>(new UpdateTransition[] {
                new UpdateTransition("t1",
                new Dictionary<Place, int>{
                    [new Place("x0")] = 1,
                    [new Place("x1")] = 1,
                    [new Place("x2")] = 1,
                },
                new Dictionary<Place, int>{
                    [new Place("x2")] = 1,
                    [new Place("x4")] = 1,
                }),
                new UpdateTransition("t2",
                new Dictionary<Place, int>{
                    [new Place("x3")] = 1,
                },
                new Dictionary<Place, int>{
                    [new Place("x0")] = 1,
                    [new Place("x2")] = 1,
                }),
                new UpdateTransition("t0",
                new Dictionary<Place, int>{
                    [new Place("x0")] = 1,
                    [new Place("x1")] = 1,
                    [new Place("x2")] = 1,
                },
                new Dictionary<Place, int>{
                    [new Place("x1")] = 1,
                    [new Place("x3")] = 1,
                }),
                new UpdateTransition("t3",
                new Dictionary<Place, int>{
                    [new Place("x4")] = 1,
                },
                new Dictionary<Place, int>{
                    [new Place("x0")] = 1,
                    [new Place("x1")] = 1,
                }),
                new UpdateTransition("t_x0",
                new Dictionary<Place, int>(),
                new Dictionary<Place, int>{
                    [new Place("x0")] = 1,
                }),
            });

            List<Place> places = new List<Place>(new Place[] {new Place("x0"),
                                                                      new Place("x1"),
                                                                      new Place("x2"),
                                                                      new Place("x3"),
                                                                      new Place("x4")});

            PetriNet net = new PetriNet(places, transitions);
            PetriNetWithCapacities netFromCopyConstructor = new PetriNetWithCapacities(net);
            PetriNetWithCapacities netFromStandardConstructor = new PetriNetWithCapacities(places, transitions);
            Assert.Equal(netFromCopyConstructor, netFromStandardConstructor);

            netFromStandardConstructor.SetCapacity(new Place("x0"), 4);

            Assert.NotEqual(netFromCopyConstructor, netFromStandardConstructor);

            PetriNetWithCapacities netFromCapaConstructor = new PetriNetWithCapacities(places, transitions,
                places.ToDictionary(place => place, place => place.Name == "x0" ? 4 : int.MaxValue));

            Assert.Equal(netFromCapaConstructor, netFromStandardConstructor);

            Marking marking = new Marking();
            marking[new Place("x0")] = 4;

            Marking expectedMarking = new Marking(marking);
            expectedMarking[new Place("x0")] = 5;

            UpdateTransition transition = new UpdateTransition("t_x0",
                new Dictionary<Place, int>(),
                new Dictionary<Place, int>
                {
                    [new Place("x0")] = 1,
                });

            var actualMarking = PetriNetUtils.DistancelessSuccessorFunction(marking, transition).First();

            Assert.Equal(expectedMarking, actualMarking);

            Assert.Empty(PetriNetUtils.DistanceSuccessorFunctionWithCapacities(netFromCapaConstructor, marking, transition));
        }
    }
}
