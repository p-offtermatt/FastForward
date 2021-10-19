using System;
using System.Collections.Generic;
using System.Linq;

namespace Testing
{
    public class Utils
    {

        public static string GetPathForTestfile(string filename)
        {
            string basePath = AppContext.BaseDirectory.Substring(0, AppContext.BaseDirectory.LastIndexOf("bin"));

            return basePath + "/testfiles/" + filename;
        }

        /// From https://stackoverflow.com/a/8376120
        /// Iterates over all possible choices of 'choose' elements from 'items'
        public IEnumerable<IList<T>> Combinations<T>(IEnumerable<T> items, int choose)
        {
            if (items == null) throw new ArgumentNullException("items");

            var itemsList = items.ToList();
            int n = itemsList.Count;

            if (n < 1) throw new ArgumentException("Must contain at least one item.", "items");
            if (choose <= 0 || choose >= n) throw new ArgumentOutOfRangeException("choose");

            var indices = Enumerable.Range(0, choose).ToArray();

            bool moreWork = true;
            while (moreWork)
            {
                yield return indices.Select(i => itemsList[i]).ToList();

                moreWork = false;
                for (int i = choose - 1; i >= 0; i--)
                {
                    if (indices[i] < n - choose + i)
                    {
                        indices[i]++;
                        for (int j = i + 1; j < choose; j++)
                        {
                            indices[j] = indices[i] - i + j;
                        }
                        moreWork = true;
                        break;
                    }
                }
            }
        }
    }
}