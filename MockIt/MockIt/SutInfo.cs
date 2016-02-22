using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace MockIt
{
    public class SutInfo
    {
        public SymbolInfo SymbolInfo { get; set; }

        public FieldDeclarationSyntax[] DeclaredFields { get; set; }
    }
}