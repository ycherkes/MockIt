using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace MockIt
{
    public class Fields
    {
        public ISymbol MethodOrPropertySymbol { get; set; }
        public IEnumerable<FieldsSetups> FieldsToSetup { get; set; }
        public ExpressionSyntax Expression { get; set; }
    }
}