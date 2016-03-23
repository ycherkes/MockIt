using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using MockIt.ThirdParty;

namespace MockIt
{
    public class SutInfo
    {
        public SymbolInfo SymbolInfo { get; set; }

        //public FieldDeclarationSyntax[] DeclaredFields { get; set; }
        //public DependencyField[] InjectedFields { get; set; }
        public IReadOnlyCollection<TreeNode<DependencyField>> InjectedFields { get; set; }
        //public IReadOnlyCollection<MemberDeclarationSyntax> ImplicitDependencies { get; set; }
    }

    public class DependencyField
    {
        public FieldDeclarationSyntax Field { get; set; }
        public bool IsInjectedFromConstructor { get; set; }
        public ExpressionStatementSyntax SetupExpression { get; set; }
        public MemberAccessExpressionSyntax SetupIdentifierNode { get; set; }

        //public FieldDeclarationSyntax ReturnsField { get; set; }
        //public SyntaxNode SetupIdentifierNode { get; set; }
        //public TypeSyntax FieldTypeSyntax { get; set; }
        //public ExpressionStatementSyntax NewExpression { get; set; }
        //public string SetupExpression { get; set; }
    }

    //public class ImplicitDependencyField
    //{
    //    public FieldDeclarationSyntax Field { get; set; }
    //    public bool IsAlreadyDeclared { get; set; }
    //    public ExpressionStatementSyntax NewExpression { get; set; }
    //    public ExpressionStatementSyntax SetupExpression { get; set; }
    //}
}