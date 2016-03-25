using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace MockIt
{
    public class SetupsInfo
    {
        public ExpressionStatementSyntax Expression { get; set; }
        public FieldDeclarationSyntax ReturnsField { get; set; }
        public FieldDeclarationSyntax ParentField { get; set; }
        public MemberAccessExpressionSyntax SetupIdentifierNode { get; set; }
    }
}