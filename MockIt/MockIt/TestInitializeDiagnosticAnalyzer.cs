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
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using MockIt.Model;
using System.Collections.Immutable;
using System.Linq;

namespace MockIt
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class TestInitializeDiagnosticAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "MockItInitializer";

        private const string Category = "Usage";

        private static readonly LocalizableString Title = new LocalizableResourceString(nameof(Resources.AnalyzerTitle), Resources.ResourceManager, typeof(Resources));

        private static readonly LocalizableString MessageFormat = new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager, typeof(Resources));

        private static readonly LocalizableString Description = new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager, typeof(Resources));

        private static readonly DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Info, true, Description);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

        public override void Initialize(AnalysisContext context)
        {
            context.EnableConcurrentExecution();
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.Analyze | GeneratedCodeAnalysisFlags.ReportDiagnostics);
            context.RegisterSemanticModelAction(AnalyzeSemantic);
        }

        private static void AnalyzeSemantic(SemanticModelAnalysisContext semanticModelContext)
        {
            try
            {
                var config = semanticModelContext.Options.AnalyzerConfigOptionsProvider.GetOptions(semanticModelContext.SemanticModel.SyntaxTree);
                if (!config.TryGetValue("dotnet_diagnostic.MockItInitializer.mock_variable_name_template", out var mockVariableNameTemplate))
                {
                    mockVariableNameTemplate = "mock{0}";
                }
                if (!config.TryGetValue("dotnet_diagnostic.MockItInitializer.mock_field_name_template", out var mockFieldNameTemplate))
                {
                    mockFieldNameTemplate = "_mock{0}";
                }

                var props = new (string key, string value)[]
                {
                    ("VariableNameTemplate", mockVariableNameTemplate ),
                    ("FieldNameTemplate", mockFieldNameTemplate )
                }.ToImmutableDictionary(x => x.key, x => x.value);

                var sutContext = TestSemanticHelper.GetSutCreationContextContainer(semanticModelContext.SemanticModel);

                var locations = sutContext.Contexts
                                          .Select(x => new { location = GetDiagnosticLocation(semanticModelContext, x), x.ContextType })
                                          .Where(x => x.location != null);


                foreach (var location in locations)
                {
                    semanticModelContext.ReportDiagnostic(Diagnostic.Create(Rule, location.location, props.Add("ContextType", location.ContextType.ToString())));
                }
            }
            catch
            {
                // ignored
            }
        }

        private static Location GetDiagnosticLocation(SemanticModelAnalysisContext semanticModel,
            SutCreationContext context)
        {
            var expression = context.MethodSyntax?.DescendantNodes()
                .OfType<ObjectCreationExpressionSyntax>()
                .FirstOrDefault(x => x.ArgumentList?.Arguments.Count == 0);

            if (expression?.Parent == null)
                return null;

            var symbolInfo = semanticModel.SemanticModel.GetSymbolInfo(expression);

            if (symbolInfo.CandidateReason != CandidateReason.OverloadResolutionFailure)
                return null;

            var invokedSymbol = symbolInfo.CandidateSymbols.OfType<IMethodSymbol>().FirstOrDefault( /*&&((IMethodSymbol) x).Parameters.All(y => y.Type.IsAbstract)*/);

            return invokedSymbol == null ? null : expression.Parent.GetLocation();
        }
    }
}