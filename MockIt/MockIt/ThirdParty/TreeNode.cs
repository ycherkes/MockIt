using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace MockIt.ThirdParty
{
    public class TreeNode<T> : IEnumerable<TreeNode<T>> where T : IEquatable<T>
    {
        public T Data { get; set; }
        public TreeNode<T> Parent { get; set; }
        public ICollection<TreeNode<T>> Children { get; set; }

        public bool IsRoot => Parent == null;

        public bool IsLeaf => Children.Count == 0;

        public int Level
        {
            get
            {
                if (IsRoot)
                    return 0;
                return Parent.Level + 1;
            }
        }


        public TreeNode(T data)
        {
            Data = data;
            Children = new LinkedList<TreeNode<T>>();

            ElementsIndex = new LinkedList<TreeNode<T>>();
            ElementsIndex.Add(this);
        }

        public TreeNode<T> AddChild(T child)
        {
            var childNode = new TreeNode<T>(child) { Parent = this };
            Children.Add(childNode);
            RegisterChildForSearch(childNode);

            return childNode;
        }

        public override string ToString()
        {
            return Data != null ? Data.ToString() : "[data null]";
        }


        #region searching

        private ICollection<TreeNode<T>> ElementsIndex { get; }

        private void RegisterChildForSearch(TreeNode<T> node)
        {
            ElementsIndex.Add(node);
            Parent?.RegisterChildForSearch(node);
        }

        public TreeNode<T> FindTreeNode(Func<TreeNode<T>, bool> predicate)
        {
            return ElementsIndex.FirstOrDefault(predicate);
        }

        public IEnumerable<TreeNode<T>> FindTreeNodes(Func<TreeNode<T>, bool> predicate)
        {
            return ElementsIndex.Where(predicate);
        }

        public IEnumerable<TreeNode<T>> FindChildTreeNodes(Func<TreeNode<T>, bool> predicate)
        {
            return ElementsIndex.Where(x => x != this).Where(predicate);
        }

        #endregion


        #region iterating

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        public IEnumerator<TreeNode<T>> GetEnumerator()
        {
            yield return this;
            foreach (var anyChild in Children.SelectMany(directChild => directChild))
            {
                yield return anyChild;
            }
        }

        #endregion
    }
}
