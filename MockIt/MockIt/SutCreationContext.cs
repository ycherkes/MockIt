using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace MockIt
{
    public class SutCreationContext
    {
        public SutCreationContextType ContextType { get; set; }
        public BaseMethodDeclarationSyntax MethodSyntax { get; set; }
    }

    public enum SutCreationContextType
    {
        Constructor,
        InitMethod,
        Method
    }
}
