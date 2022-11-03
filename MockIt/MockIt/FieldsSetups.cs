using Microsoft.CodeAnalysis;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace MockIt
{
    public class FieldsSetups
    {
        public IEnumerable<string> Field { get; set; }
        public IImmutableDictionary<string, ITypeSymbol> Substitutions { get; set; }
        public IImmutableDictionary<string, ITypeSymbol> SutSubstitutions { get; internal set; }
    }
}