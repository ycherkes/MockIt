using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;

namespace MockIt
{
    public class SutCreationContextContainer
    {
        public FieldDeclarationSyntax[] Fields { get; set; } = Array.Empty<FieldDeclarationSyntax>();
        public SutCreationContext[] Contexts { get; set; } = Array.Empty<SutCreationContext>();
    }
    public class SutCreationContext
    {
        public SutCreationContextType ContextType { get; set; }
        public BaseMethodDeclarationSyntax MethodSyntax { get; set; }

        public LocalDeclarationStatementSyntax[] DeclaredVariables { get; set; } = Array.Empty<LocalDeclarationStatementSyntax>();
    }

    public enum SutCreationContextType
    {
        Constructor,
        InitMethod,
        Method
    }
}
