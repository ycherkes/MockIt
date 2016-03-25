using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using MockIt.ThirdParty;

namespace MockIt
{
    public class SutInfo
    {
        public SymbolInfo SymbolInfo { get; set; }

        public IReadOnlyCollection<TreeNode<DependencyField>> InjectedFields { get; set; }
    }
}