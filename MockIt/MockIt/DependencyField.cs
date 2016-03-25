using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace MockIt
{
    public class DependencyField
    {
        public FieldDeclarationSyntax Field { get; set; }
        public bool IsInjectedFromConstructor { get; set; }
        public ExpressionStatementSyntax SetupExpression { get; set; }
        public MemberAccessExpressionSyntax SetupIdentifierNode { get; set; }
    }
}