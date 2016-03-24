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
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace MockIt
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(TestInitializeCodeFixProvider)), Shared]
    public class TestInitializeCodeFixProvider : CodeFixProvider
    {
        public sealed override ImmutableArray<string> FixableDiagnosticIds => ImmutableArray.Create(TestInitializeDiagnosticAnalyzer.DiagnosticId);

        public sealed override FixAllProvider GetFixAllProvider()
        {
            return WellKnownFixAllProviders.BatchFixer;
        }

        public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            var diagnostic = context.Diagnostics.First();
            var diagnosticSpan = diagnostic.Location.SourceSpan;

            var tokens = root.FindToken(diagnosticSpan.Start)
                .Parent.AncestorsAndSelf();

            var creation = tokens.OfType<ExpressionStatementSyntax>()
                .FirstOrDefault();

            // Register a code action that will invoke the fix.
            context.RegisterCodeFix(CodeAction.Create("Make mock", c => MakeMock(context.Document, creation, c), "MockItTool"), diagnostic);
        }

        private static string GetTypeNameFromType(ISymbol symbol)
        {
            return TestSemanticHelper.GetSimpleTypeName(symbol);
        }

        private static async Task<Document> MakeMock(Document document, ExpressionStatementSyntax creation,
            CancellationToken cancellationToken)
        {
            var semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

            var creationExpressionSyntax = creation.Expression.ChildNodes().OfType<ObjectCreationExpressionSyntax>().First();

            var symbolInfo = semanticModel.GetSymbolInfo(creationExpressionSyntax);
            
            var invokedSymbol = (IMethodSymbol)symbolInfo.CandidateSymbols.FirstOrDefault(x => x is IMethodSymbol
                                                                                               /*&& ((IMethodSymbol)x).Parameters.All(y => y.Type.IsAbstract)*/);
            
            var constructorParameters = invokedSymbol.Parameters.Select(x => new ConstructorParameters
            {
                ArgumentName = x.Name,
                TypeName = GetTypeNameFromType((INamedTypeSymbol)x.Type)
            }).ToArray();

            var changes = constructorParameters.MakeConstructorInjections();

            var changedDocument = await ChangesMaker.ApplyConstuctorInjections(document, creation, cancellationToken, changes, creationExpressionSyntax);
            return changedDocument;
        }
    }
}