using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;

namespace MockIt.Model
{
    public class FieldOrLocalVariables
    {
        public ISymbol MethodOrPropertySymbol { get; set; }
        public IEnumerable<FieldOrLocalVariableSetups> FieldOrLocalVariablesToSetup { get; set; }
        public ExpressionSyntax Expression { get; set; }
    }
}