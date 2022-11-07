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
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;
using MockIt.Extensions;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace MockIt
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(TestMethodCodeFixProvider)), Shared]
    public class TestMethodCodeFixProvider : CodeFixProvider
    {
        public sealed override ImmutableArray<string> FixableDiagnosticIds => ImmutableArray.Create(TestMethodDiagnosticAnalyzer.DiagnosticId);

        public sealed override FixAllProvider GetFixAllProvider()
        {
            return WellKnownFixAllProviders.BatchFixer;
        }

        public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            if (root == null) return;

            var diagnostic = context.Diagnostics.First();
            var diagnosticSpan = diagnostic.Location.SourceSpan;

            var tokens = root.FindToken(diagnosticSpan.Start)
                             .Parent
                             ?.AncestorsAndSelf();

            var invocation = tokens?.FirstOrDefault(x => x is ExpressionStatementSyntax || x is LocalDeclarationStatementSyntax);

            if (invocation == null) return;

            context.RegisterCodeFix(CodeAction.Create("Setup mocks with callbacks", c => SetupMocks(context.Document, invocation, c, true), "MockItTool.SetupMocksWithCallbacks"), diagnostic);
            context.RegisterCodeFix(CodeAction.Create("Setup mocks", c => SetupMocks(context.Document, invocation, c, false), "MockItTool.SetupMocks"), diagnostic);
        }

        private static async Task<Document> SetupMocks(Document document, SyntaxNode invocationSyntax, CancellationToken cancellationToken, bool withCallBack)
        {
            var testSemanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

            var sutCreationContext = TestSemanticHelper.GetSutCreationContextContainer(testSemanticModel);

            if (sutCreationContext.Fields.Length == 0 && sutCreationContext.Contexts.All(x => x.DeclaredVariables.Length == 0))
                return document;

            var suts = sutCreationContext.Contexts.SelectMany(c => c.GetSuts(testSemanticModel, sutCreationContext.Fields)).ToArray();

            var memberAccessExpressions = invocationSyntax.DescendantNodes()
                .OfType<ExpressionSyntax>()
                .Where(x => x is InvocationExpressionSyntax || x is MemberAccessExpressionSyntax)
                .Select(expr =>
                {
                    var memberAccess = expr as MemberAccessExpressionSyntax;
                    var expression = !(expr is InvocationExpressionSyntax invocationExpression) ? memberAccess : invocationExpression.Expression;
                    return expression;
                });

            var invokedMethodsOfMocks = await Task.WhenAll(memberAccessExpressions.Select(expressionSyntax => MocksAnalyzingEngine.GetInvokedMethodsOfMock(expressionSyntax, testSemanticModel, suts)));

            var invokedMethodsOfMocksDistinct = invokedMethodsOfMocks.SelectMany(x => x)
                                                                     .DistinctBy(x => string.Join(",", x.FieldOrLocalVariablesToSetup.SelectMany(y => y.FieldOrLocalVariableName.Select(z => z))) + "," + x.MethodOrPropertySymbol)
                                                                     .ToArray();

            if (invokedMethodsOfMocksDistinct.Length == 0)
                return document;

            var editor = await DocumentEditor.CreateAsync(document, cancellationToken).ConfigureAwait(false);

            invocationSyntax.ApplyMethodCodeFixChanges(editor, invokedMethodsOfMocksDistinct, withCallBack);

            return editor.GetChangedDocument();
        }
    }
}