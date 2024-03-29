using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace MockIt.Model
{
    public class ConstructorInjections
    {
        public FieldDeclarationSyntax NewField { get; set; }
        public SyntaxNode NewExpression { get; set; }
        public ArgumentSyntax CreationArgument { get; set; }
    }
}