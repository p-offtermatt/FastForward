
namespace HeuristicFrontier
{
    /// <summary>
    /// Stores the nodes of the frontier using similar semantics (not inner structure!) to that of a priority queue, but does not compute their individual heuristic values.
    /// Instead, when one asks to get the node with the lowest score (cost from start + heuristic score), it evaluates this as one heuristic call on the whole frontier.
    /// </summary>
    public abstract class HeuristicFrontier<T>
    {

        /// <summary>
        /// Adds a node to the heuristic frontier.
        /// </summary>
        /// <param name="node">The node.</param>
        /// <param name="score">How far the node is from the start (this is needed since the actual score is computed as [distance from start + heuristic score])</param>
        public abstract void Add(T node, float score);
        public abstract (double, T) PopMinNode();

        public abstract void UpdateNodeScore(T node, float newScore);

        public abstract bool Contains(T node);
    }
}