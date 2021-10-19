using System;
using System.Collections.Generic;

namespace PetriTool
{
    public static class Utils
    {
        /// <summary>
        /// From https://stackoverflow.com/a/648240. Produces a uniformly-randomly chosen element from the given source sequence.
        /// </summary>
        /// <param name="source">A sequence of elements.</param>
        /// <param name="rng">An instance of the Random class.</param>
        /// <typeparam name="T">The type of elements in the source sequence.</typeparam>
        /// <returns>An element chosen uniformly at random from the source sequence.</returns>
        public static T RandomElement<T>(this IEnumerable<T> source,
                                     Random rng)
        {
            T current = default(T);
            int count = 0;
            foreach (T element in source)
            {
                count++;
                if (rng.Next(count) == 0)
                {
                    current = element;
                }
            }
            if (count == 0)
            {
                throw new InvalidOperationException("Sequence was empty");
            }
            return current;
        }
    }
}