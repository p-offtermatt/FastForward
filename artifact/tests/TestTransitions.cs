using Xunit;
using Petri;
using System.Collections.Generic;
using System.Collections;
using System.Linq;
using Xunit.Abstractions;


namespace Testing
{

    public class TestTransitions
    {

        private readonly ITestOutputHelper output;

        public TestTransitions(ITestOutputHelper output)
        {
            this.output = output;
        }

        static Place p1 = new Place("p1");
        static Place p2 = new Place("p2");
        static Place p3 = new Place("p3");

        static UpdateTransition update1 = new UpdateTransition("t1", new Dictionary<Place, int> { [p1] = 2, [p2] = 1 }, new Dictionary<Place, int> { [p1] = 1, [p3] = 2 });
        static UpdateTransition update2 = new UpdateTransition("t2", new Dictionary<Place, int> { [p1] = 3, [p2] = 2 }, new Dictionary<Place, int> { [p2] = 2, [p3] = 5 });

        static SetTransferTransition transfer1 = new SetTransferTransition("transfer1", new HashSet<Place> { p1, p3 }, new HashSet<Place> { p2 }, update1.Pre, update1.Post);
        static SetTransferTransition transfer2 = new SetTransferTransition("transfer2", new HashSet<Place> { p1, p3 }, new HashSet<Place> { p2 }, new Dictionary<Place, int>(), new Dictionary<Place, int>());

        static SetTransferTransition transfer3 = new SetTransferTransition("transfer3",
        new HashSet<Place>(),
        new HashSet<Place>(),
        new Dictionary<Place, int> { [p1] = 0 },
        new Dictionary<Place, int> { [p1] = 1 });


        [Fact]
        public void CopyUpdateTransitionTest()
        {
            UpdateTransition copy = (UpdateTransition)update1.CopyThis();
            copy.Pre = null;
            copy.Post[p1] = 4;

            Assert.Null(copy.Pre);
            Assert.Equal(copy.Post, new Dictionary<Place, int> { [p1] = 4, [p3] = 2 });

            Assert.Equal(update1.Pre, new Dictionary<Place, int> { [p1] = 2, [p2] = 1 });
            Assert.Equal(update1.Post, new Dictionary<Place, int> { [p1] = 1, [p3] = 2 });
        }

        [Fact]
        public void CopySetTransferTransitionTest()
        {
            SetTransferTransition copy = (SetTransferTransition)transfer1.CopyThis();
            copy.TransferInputs = new HashSet<Place> { p3 };
            copy.TransferOutputs = new HashSet<Place> { p1, p2 };
            copy.UpdateBehaviour.Pre = new Dictionary<Place, int>();
            copy.UpdateBehaviour.Post[p1] = 0;

            Assert.Equal(copy.TransferInputs, new HashSet<Place> { p3 });
            Assert.Equal(copy.TransferOutputs, new HashSet<Place> { p1, p2 });
            Assert.Equal(copy.UpdateBehaviour.Pre, new Dictionary<Place, int>());
            Assert.Equal(copy.UpdateBehaviour.Post[p1], 0);

            Assert.Equal(transfer1.TransferInputs, new HashSet<Place> { p1, p3 });
            Assert.Equal(transfer1.TransferOutputs, new HashSet<Place> { p2 });
            Assert.Equal(transfer1.UpdateBehaviour.Pre, new Dictionary<Place, int> { [p1] = 2, [p2] = 1 });
            Assert.Equal(transfer1.UpdateBehaviour.Post, new Dictionary<Place, int> { [p1] = 1, [p3] = 2 });
        }

        [Fact]
        public void TransferPrePostTest()
        {
            Assert.Equal(new HashSet<Place> { p1, p2, p3 }, transfer1.GetPrePlaces());
            Assert.Equal(new HashSet<Place> { p1, p2, p3 }, transfer1.GetPostPlaces());

            Assert.Equal(new HashSet<Place> { p1, p3 }, transfer2.GetPrePlaces());
            Assert.Equal(new HashSet<Place> { p2 }, transfer2.GetPostPlaces());

            Assert.Equal(new HashSet<Place> { }, transfer3.GetPrePlaces());
            Assert.Equal(new HashSet<Place> { p1 }, transfer3.GetPostPlaces());
        }

        [Fact]
        public void TransferEqualityTest()
        {
            Assert.NotEqual(transfer1, transfer2);
            Assert.NotEqual(transfer1.GetHashCode(), transfer2.GetHashCode());
            SetTransferTransition copy = (SetTransferTransition)transfer1.CopyThis();
            Assert.Equal(transfer1, copy);
            Assert.Equal(transfer1.GetHashCode(), copy.GetHashCode());
            Assert.NotEqual(transfer2, copy);
            Assert.NotEqual(transfer2.GetHashCode(), copy.GetHashCode());

            copy.TransferInputs = new HashSet<Place> { p2, p3 };
            Assert.NotEqual(transfer1, copy);
            Assert.NotEqual(transfer1.GetHashCode(), copy.GetHashCode());

            transfer1.TransferInputs = new HashSet<Place> { p2, p3 };
        }

        [Fact]
        public void TransferRemovePlaceTest()
        {
            SetTransferTransition copy = (SetTransferTransition)transfer2.CopyThis();
            Assert.False(copy.RemovePlaceFromPost(p1));
            Assert.True(copy.RemovePlaceFromPost(p2));
            Assert.Equal(transfer2.GetPostPlaces().Except(new HashSet<Place> { p2 }).ToHashSet(), copy.GetPostPlaces());

            Assert.True(copy.RemovePlaceFromPre(p1));
            Assert.False(copy.RemovePlaceFromPre(p2));
            Assert.Equal(transfer2.GetPrePlaces().Except(new HashSet<Place> { p1 }).ToHashSet(), copy.GetPrePlaces());
        }

        [Fact]
        public void UpdateRemovePlaceTest()
        {
            UpdateTransition copy = (UpdateTransition)update1.CopyThis();
            Assert.False(copy.RemovePlaceFromPre(p3));
            Assert.True(copy.RemovePlaceFromPre(p2));
            Assert.Equal(update1.GetPrePlaces().Except(new HashSet<Place> { p2 }).ToHashSet(), copy.GetPrePlaces());

            Assert.True(copy.RemovePlaceFromPost(p1));
            Assert.False(copy.RemovePlaceFromPost(p2));
            Assert.Equal(update1.GetPostPlaces().Except(new HashSet<Place> { p1 }).ToHashSet(), copy.GetPostPlaces());
        }

        [Fact]
        public void GuardTest()
        {
            Assert.Equal(new Dictionary<Place, int> { [p1] = 2, [p2] = 1 }, update1.GetGuard());
            Assert.Equal(new Dictionary<Place, int> { [p1] = 3, [p2] = 2 }, update2.GetGuard());

            Assert.Equal(new Dictionary<Place, int> { [p1] = 2, [p2] = 1 }, transfer1.GetGuard());
            Assert.Equal(new Dictionary<Place, int>(), transfer2.GetGuard());

        }
    }
}