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

using MockIt.Extensions;
using System;

namespace MockIt
{
    public class NameGenerator
    {
        private readonly string _variableNameTemplate;
        private readonly string _fieldNameTemplate;
        private readonly Func<string, string> _variableNameAdapter;
        private readonly Func<string, string> _fieldNameAdapter;

        public NameGenerator(string variableNameTemplate, string fieldNameTemplate)
        {
            _variableNameTemplate = variableNameTemplate ?? throw new ArgumentNullException(nameof(variableNameTemplate));
            _fieldNameTemplate = fieldNameTemplate ?? throw new ArgumentNullException(nameof(fieldNameTemplate));

            _variableNameAdapter = _variableNameTemplate.StartsWith("{0}")
                ? (Func<string, string>)ToCamelCase
                : ToPascalCase;

            _fieldNameAdapter = fieldNameTemplate.StartsWith("{0}") || fieldNameTemplate.StartsWith("_{0}")
                ? (Func<string, string>)ToCamelCase
                : ToPascalCase;
        }

        public string GetVariableName(string injectedVariableName)
        {
            var adaptedVariableName = _variableNameAdapter(injectedVariableName);
            return string.Format(_variableNameTemplate, adaptedVariableName);
        }


        public string GetFieldName(string injectedFieldName)
        {
            var adaptedFieldName = _fieldNameAdapter(injectedFieldName);
            return string.Format(_fieldNameTemplate, adaptedFieldName);
        }

        private static string ToPascalCase(string name)
        {
            return name.FirstCharToUpperCase();
        }

        private static string ToCamelCase(string name)
        {
            return name.FirstCharToLowerCase();
        }
    }
}