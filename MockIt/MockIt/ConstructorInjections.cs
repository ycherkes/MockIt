using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace MockIt
{
    public class ConstructorInjections
    {
        public FieldDeclarationSyntax NewField { get; set; }
        public ExpressionStatementSyntax NewExpression { get; set; }
        public SyntaxNodeOrToken CreationArgument { get; set; }
    }

    public class ChainOfCallsInjections
    {
        public FieldDeclarationSyntax NewField { get; set; }
        public ExpressionStatementSyntax NewExpression { get; set; }
        public ExpressionStatementSyntax SetupExpression { get; set; }
    }
}