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
using System.Collections.Immutable;
using System.Linq;
using System.Text.RegularExpressions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace MockIt
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class TestMethodDiagnosticAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "MockItMethod";
        internal const string Category = "Usage";
        
        internal static readonly LocalizableString Title = new LocalizableResourceString(nameof(Resources.AnalyzerTitle), Resources.ResourceManager, typeof (Resources));

        internal static readonly LocalizableString MessageFormat = new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager, typeof (Resources));

        internal static readonly LocalizableString Description = new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager, typeof (Resources));

        internal static DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, true, Description);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSemanticModelAction(AnalyzeSemantic);
        }

        private static void AnalyzeSemantic(SemanticModelAnalysisContext semanticContext)
        {
            var testSemanticModel = semanticContext.SemanticModel;

            var methodDecls = TestSemanticHelper.GetTestMethods(testSemanticModel);
            var methods = TestSemanticHelper.GetMethodsToConfigureMocks(methodDecls);
            var properties = TestSemanticHelper.GetPropertiesToConfigureMocks(methodDecls, methods);

            var memberAccessExpressions = methods.Concat(properties)
                                     .DistinctBy(x => x.SpanStart)
                                     .ToArray();

            if (!memberAccessExpressions.Any())
                return;

            var testInitMethodDecl = TestSemanticHelper.GetTestInitializeMethod(testSemanticModel);

            var declaredFields = testInitMethodDecl.Parent.ChildNodes().OfType<FieldDeclarationSyntax>().ToArray();

            var suts = testInitMethodDecl.GetSuts(testSemanticModel, declaredFields);

            var mockableExpressions = memberAccessExpressions.Where(expressionSyntax => !IsNotExpressionNeedsToMock(MocksAnalyzingEngine.GetInvokedMethodsOfMock(expressionSyntax, testSemanticModel, suts)
                                                                                                                                 .SelectMany(x => x.FieldsToSetup
                                                                                                                                                   .SelectMany(y => y.Field))
                                                                                                                                 .Distinct()
                                                                                                                                 .ToArray(), 
                                                                                                                    expressionSyntax))
                                                               .ToArray();

            if (mockableExpressions.Length == 0)
                return;

            foreach (var mockableExpression in mockableExpressions)
            {
                semanticContext.ReportDiagnostic(Diagnostic.Create(Rule, mockableExpression.Parent.GetLocation()));
            }
        }

        private static bool IsNotExpressionNeedsToMock(IReadOnlyCollection<string> mocksInvokations, SyntaxNode expression)
        {
            return mocksInvokations.Count == 0
                    || expression.Parents(n => n is BlockSyntax)
                                  ?.DescendantNodes()
                                  .OfType<InvocationExpressionSyntax>()
                                  .Select(x => x.ToString())
                                  .Any(x => mocksInvokations.Any(e => Regex.IsMatch(x, e + @"\s*\.Setup"))) == true;
        }
    }
}