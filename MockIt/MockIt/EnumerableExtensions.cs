using System;
using System.Collections.Generic;
using System.Linq;
using MockIt.ThirdParty;

namespace MockIt
{
    public static class EnumerableExtensions
    {
        public static IEnumerable<T> DistinctBy<T, TKey>(this IEnumerable<T> items, Func<T, TKey> property, bool last = false)
        {
            Func<IGrouping<TKey, T>, T> firstOrLastFunc = x => last ? x.Last() : x.First();
            return items.GroupBy(property).Select(firstOrLastFunc);
        }

        public static IEnumerable<TreeNode<T>> Find<T>(this IEnumerable<TreeNode<T>> items, Func<TreeNode<T>, bool> predicate)
        {
            var result = Enumerable.Empty<TreeNode<T>>();

            result = items.Aggregate(result, (current, item) => current.Concat(item.FindTreeNodes(predicate)));

            return result.ToArray();
        }

        public static IEnumerable<TreeNode<T>> FindChild<T>(this IEnumerable<TreeNode<T>> items, Func<TreeNode<T>, bool> predicate)
        {
            var result = Enumerable.Empty<TreeNode<T>>();

            result = items.Aggregate(result, (current, item) => current.Concat(item.FindChildTreeNodes(predicate)));

            return result.ToArray();
        }
    }
}
