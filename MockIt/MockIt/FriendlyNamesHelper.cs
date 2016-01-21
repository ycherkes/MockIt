#region License
// Copyright (c) Yevhen Cherkes
// 
// Licensed under the Apache License, Version 2.0 (the "License"); 
// you may not use this file except in compliance with the License. 
// You may obtain a copy of the License at 
// 
// http://www.apache.org/licenses/LICENSE-2.0 
// 
// Unless required by applicable law or agreed to in writing, software 
// distributed under the License is distributed on an "AS IS" BASIS, 
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
// See the License for the specific language governing permissions and 
// limitations under the License.
// 
// The latest version of this file can be found at https://github.com/ycherkes/MockIt
#endregion
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