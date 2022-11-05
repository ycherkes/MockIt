using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace MockIt
{
    public class SetupsInfo
    {
        public ExpressionStatementSyntax Expression { get; set; }
        public VariableDeclarationSyntax ReturnsField { get; set; }
        public VariableDeclarationSyntax ParentField { get; set; }
        public MemberAccessExpressionSyntax SetupIdentifierNode { get; set; }
    }
}