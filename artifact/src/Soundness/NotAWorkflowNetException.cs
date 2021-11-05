using System;
using System.Collections.Generic;
using Petri;

namespace Soundness
{
    [Serializable]
    public class NotAWorkflowNetException : Exception
    {
        public NotAWorkflowNetException() : base() { }
        public NotAWorkflowNetException(string message) : base(message) { }
        public NotAWorkflowNetException(string message, Exception inner) : base(message, inner) { }

        // A constructor is needed for serialization when an
        // exception propagates from a remoting server to the client.
        protected NotAWorkflowNetException(System.Runtime.Serialization.SerializationInfo info,
            System.Runtime.Serialization.StreamingContext context) : base(info, context) { }

        public NotAWorkflowNetException(IEnumerable<Place> sources, IEnumerable<Place> sinks) : base("Not a workflow net; source places: " + String.Join(", ", sources + "\n sink places: " + String.Join(", ", sinks)))
        { }
    }
}