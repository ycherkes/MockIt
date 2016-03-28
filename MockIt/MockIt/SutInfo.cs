using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using MockIt.ThirdParty;

namespace MockIt
{
    public class SutInfo
    {
        public SymbolInfo SymbolInfo { get; set; }

        public IReadOnlyCollection<TreeNode<DependencyField>> InjectedFields { get; set; }
        public SemanticModel SemanticModel { get; set; }
        public IdentifierNameSyntax Identifier { get; set; }

        //public SyntaxNode[] Nodes { get; set; }
    }
}