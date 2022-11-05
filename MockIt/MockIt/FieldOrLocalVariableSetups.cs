using Microsoft.CodeAnalysis;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace MockIt
{
    public class FieldOrLocalVariableSetups
    {
        public IEnumerable<string> FieldOrLocalVariable { get; set; }
        public IImmutableDictionary<string, ITypeSymbol> Substitutions { get; set; }
        public IImmutableDictionary<string, ITypeSymbol> SutSubstitutions { get; internal set; }
    }
}