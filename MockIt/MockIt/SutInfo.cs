using Microsoft.CodeAnalysis;
using MockIt.ThirdParty;
using System.Collections.Generic;

namespace MockIt
{
    public class SutInfo
    {
        public SymbolInfo SymbolInfo { get; set; }

        public IReadOnlyCollection<TreeNode<DependencyField>> InjectedFields { get; set; }
        public SemanticModel SemanticModel { get; set; }
        public SyntaxToken? Identifier { get; set; }
    }
}