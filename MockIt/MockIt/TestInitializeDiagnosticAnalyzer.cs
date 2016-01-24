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
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace MockIt
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class TestInitializeDiagnosticAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "MockItInitializer";
        internal const string Category = "Usage";
        // You can change these strings in the Resources.resx file. If you do not want your analyzer to be localize-able, you can use regular strings for Title and MessageFormat.
        internal static readonly LocalizableString Title = new LocalizableResourceString(nameof(Resources.AnalyzerTitle), Resources.ResourceManager, typeof (Resources));

        internal static readonly LocalizableString MessageFormat = new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager, typeof (Resources));

        internal static readonly LocalizableString Description = new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager, typeof (Resources));

        internal static DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Info, true, Description);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSemanticModelAction(AnalyzeSemantic);
        }

        private static void AnalyzeSemantic(SemanticModelAnalysisContext obj)
        {
            var methodDecl = obj.SemanticModel.SyntaxTree.GetRoot()
                                .DescendantNodes()
                                .OfType<MethodDeclarationSyntax>()
                                .FirstOrDefault(x => x.AttributeLists
                                    .Any(y => y.Attributes
                                        .Any(z => new[] { "TestFixtureSetUp", "TestInitialize" }.Contains(((IdentifierNameSyntax) z.Name).Identifier.Text))));

            var expression = methodDecl?.DescendantNodes()
                                        .OfType<ObjectCreationExpressionSyntax>()
                                        .FirstOrDefault(x => x.ArgumentList.Arguments.Count == 0);

            if (expression == null)
                return;

            var symbolInfo = obj.SemanticModel.GetSymbolInfo(expression);

            if (symbolInfo.CandidateReason != CandidateReason.OverloadResolutionFailure)
                return;

            var invokedSymbol = symbolInfo.CandidateSymbols.FirstOrDefault(x => x is IMethodSymbol &&
                                                                           ((IMethodSymbol) x).Parameters.All(
                                                                               y => y.Type.IsAbstract));

            if (invokedSymbol != null)
                obj.ReportDiagnostic(Diagnostic.Create(Rule, expression.Parent.GetLocation()));
        }
    }
}