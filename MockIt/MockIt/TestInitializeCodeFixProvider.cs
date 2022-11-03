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
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

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
            context.RegisterCodeFix(CodeAction.Create("Fill with mocks", c => FillWithMocks(context.Document, creation, c), "MockItTool"), diagnostic);
        }

        private static async Task<Document> FillWithMocks(Document document, ExpressionStatementSyntax creation,
            CancellationToken cancellationToken)
        {
            var semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

            var creationExpressionSyntax = creation.Expression.ChildNodes().OfType<ObjectCreationExpressionSyntax>().FirstOrDefault();

            if (creationExpressionSyntax == null)
                return document;

            var symbolInfo = semanticModel.GetSymbolInfo(creationExpressionSyntax);

            var invokedSymbol = symbolInfo.CandidateSymbols.OfType<IMethodSymbol>().FirstOrDefault(/*&& ((IMethodSymbol)x).Parameters.All(y => y.Type.IsAbstract)*/);

            if ((invokedSymbol?.Parameters.Length ?? 0) == 0)
                return document;

            var constructorParameters = invokedSymbol.Parameters.Select(x => new ConstructorParameters
            {
                ArgumentName = x.Name,
                TypeName = x.Type.GetSimpleTypeName()
            }).ToArray();

            var changes = constructorParameters.MakeConstructorInjections();

            var changedDocument = await ChangesMaker.ApplyConstructorInjections(document, creation, cancellationToken, changes, creationExpressionSyntax);
            return changedDocument;
        }
    }
}