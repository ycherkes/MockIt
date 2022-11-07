using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace MockIt.Model
{
    public class SetupsInfo
    {
        public ExpressionStatementSyntax Expression { get; set; }
        public VariableDeclarationSyntax ReturnsFieldOrVariable { get; set; }
        public VariableDeclarationSyntax ParentFieldOrVariable { get; set; }
        public MemberAccessExpressionSyntax SetupIdentifierNode { get; set; }
    }
}