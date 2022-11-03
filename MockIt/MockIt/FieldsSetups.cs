using Microsoft.CodeAnalysis;
using System.Collections.Generic;

namespace MockIt
{
    public class FieldsSetups
    {
        public IEnumerable<string> Field { get; set; }
        public Dictionary<string, ITypeSymbol> Substitutions { get; set; }
        public Dictionary<string, ITypeSymbol> SutSubstitutions { get; internal set; }
    }
}