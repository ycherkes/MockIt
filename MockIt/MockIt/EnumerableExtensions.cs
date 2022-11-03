using MockIt.ThirdParty;
using System;
using System.Collections.Generic;
using System.Linq;

namespace MockIt
{
    public static class EnumerableExtensions
    {
        public static IEnumerable<T> DistinctBy<T, TKey>(this IEnumerable<T> items, Func<T, TKey> property)
        {
            return items.GroupBy(property).Select(x => x.First());
        }

        public static IEnumerable<T> DistinctBy<T, TKey>(this IEnumerable<T> items, Func<T, TKey> property, IEqualityComparer<TKey> comparer)
        {
            return items.GroupBy(property, comparer).Select(x => x.First());
        }

        public static IEnumerable<T> DistinctBy<T, TKey>(this IEnumerable<T> items, Func<T, TKey> property, Func<TKey, TKey, bool> comparer)
        {
            return items.GroupBy(property, new EqualityComparer<TKey>(comparer)).Select(x => x.First());
        }

        public static IEnumerable<TreeNode<T>> Find<T>(this IEnumerable<TreeNode<T>> items, Func<TreeNode<T>, bool> predicate) where T : IEquatable<T>
        {
            var result = Enumerable.Empty<TreeNode<T>>();

            result = items.Aggregate(result, (current, item) => current.Concat(item.FindTreeNodes(predicate)));

            return result.ToArray();
        }

        // This comparer can not be used for any hash collections
        private class EqualityComparer<T> : IEqualityComparer<T>
        {
            private readonly Func<T, T, bool> _comparer;

            public EqualityComparer(Func<T, T, bool> comparer)
            {
                _comparer = comparer;
            }

            public bool Equals(T x, T y)
            {
                return _comparer(x, y);
            }

            public int GetHashCode(T obj)
            {
                return 0;
            }
        }
    }
}
