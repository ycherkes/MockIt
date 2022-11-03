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


using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace MockIt
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class TestMethodDiagnosticAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "MockItMethod";
        private const string Category = "Usage";

        private static readonly LocalizableString Title = new LocalizableResourceString(nameof(Resources.AnalyzerTitle), Resources.ResourceManager, typeof(Resources));

        private static readonly LocalizableString MessageFormat = new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager, typeof(Resources));

        private static readonly LocalizableString Description = new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager, typeof(Resources));

        private static readonly DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, true, Description);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

        public override void Initialize(AnalysisContext context)
        {
            context.EnableConcurrentExecution();
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.Analyze | GeneratedCodeAnalysisFlags.ReportDiagnostics);
            context.RegisterSemanticModelAction(AnalyzeSemantic);
        }

        private static async void AnalyzeSemantic(SemanticModelAnalysisContext semanticContext)
        {
            try
            {
                var testSemanticModel = semanticContext.SemanticModel;

                var methodDeclarations = TestSemanticHelper.GetTestMethods(testSemanticModel);
                var methods = TestSemanticHelper.GetMethodsToConfigureMocks(methodDeclarations);
                var properties = TestSemanticHelper.GetPropertiesToConfigureMocks(methodDeclarations, methods);

                var memberAccessExpressions = methods.Concat(properties)
                                                     .ToArray();

                if (!memberAccessExpressions.Any())
                    return;

                var testInitMethodDecl = TestSemanticHelper.GetSutCreationContexts(testSemanticModel).FirstOrDefault()?.MethodSyntax;

                var declaredFields = testInitMethodDecl?.Parent?.ChildNodes().OfType<FieldDeclarationSyntax>().ToArray();

                if (declaredFields == null) return;

                var suts = testInitMethodDecl.GetSuts(testSemanticModel, declaredFields);
                var sutIdentifiers = suts.Select(x => x.Identifier.Identifier.Text).ToArray();

                memberAccessExpressions = memberAccessExpressions.Where(x => x.DescendantNodesAndSelf()
                                                                              .Any(y => sutIdentifiers.Contains((y as IdentifierNameSyntax)?.Identifier.Text)))
                                                                 .ToArray();

                var express = memberAccessExpressions.Select(async expressionSyntax => new
                {
                    Syntax = expressionSyntax,
                    ToBeMocked = !IsNotExpressionNeedsToMock((await MocksAnalyzingEngine.GetInvokedMethodsOfMock(expressionSyntax, testSemanticModel, suts))
                                                                                        .SelectMany(x => x.FieldsToSetup
                                                                                                          .SelectMany(y => y.Field))
                                                                                        .Distinct()
                                                                                        .ToArray(),
                                                             expressionSyntax)
                });

                var mockableExpressions = (await Task.WhenAll(express)).Where(x => x.ToBeMocked).Select(x => x.Syntax);

                foreach (var mockableExpression in mockableExpressions)
                {
                    var location = mockableExpression.Parent?.GetLocation();
                    if (location != null)
                    {
                        semanticContext.ReportDiagnostic(Diagnostic.Create(Rule, location));
                    }
                }
            }
            catch { }
        }

        private static bool IsNotExpressionNeedsToMock(IReadOnlyCollection<string> mocksInvocations, SyntaxNode expression)
        {
            return mocksInvocations.Count == 0
                    || expression.Parents(n => n is BlockSyntax)
                                  ?.DescendantNodes()
                                  .OfType<InvocationExpressionSyntax>()
                                  .Select(x => x.ToString())
                                  .Any(x => mocksInvocations.Any(e => Regex.IsMatch(x, e + @"\s*\.Setup"))) == true;
        }
    }
}