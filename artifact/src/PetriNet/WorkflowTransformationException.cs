using System;

namespace Petri
{
    [Serializable]
    public class WorkflowException : Exception
    {
        public WorkflowException() : base() { }
        public WorkflowException(string message) : base(message) { }
        public WorkflowException(string message, Exception inner) : base(message, inner) { }

        // A constructor is needed for serialization when an
        // exception propagates from a remoting server to the client.
        protected WorkflowException(System.Runtime.Serialization.SerializationInfo info,
            System.Runtime.Serialization.StreamingContext context) : base(info, context) { }
    }
}