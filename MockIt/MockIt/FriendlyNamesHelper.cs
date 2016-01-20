using System.Collections.Generic;
using Microsoft.CodeAnalysis;

namespace MockIt
{
    public class FriendlyNamesHelper
    {
        private static readonly Dictionary<string, string> primitiveTypes = new Dictionary<string, string>
        {
            { typeof(object).Name,  "object"} ,
            { typeof(string).Name,  "string"} ,
            { typeof(bool).Name,    "bool"} ,
            { typeof(byte).Name,    "byte"} ,
            { typeof(char).Name,    "char"} ,
            { typeof(decimal).Name, "decimal"} ,
            { typeof(double).Name,  "double "} ,
            { typeof(short).Name,   "short"} ,
            { typeof(int).Name,     "int"} ,
            { typeof(long).Name,    "long"} ,
            { typeof(sbyte).Name,   "sbyte"} ,
            { typeof(float).Name,   "float"} ,
            { typeof(ushort).Name,  "ushort"} ,
            { typeof(uint).Name,    "uint"} ,
            { typeof(ulong).Name,   "ulong"}
        };

      
        public static string GetSimpleTypeName(ISymbol type)
        {
            string result;

            return primitiveTypes.TryGetValue(type.Name, out result) ? result : type.Name;
        }
    }
}