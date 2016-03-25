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
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;

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

            var diagnostic = context.Diagnostics.First();
            var diagnosticSpan = diagnostic.Location.SourceSpan;

            var tokens = root.FindToken(diagnosticSpan.Start)
                             .Parent
                             .AncestorsAndSelf();

            var invokation = tokens.FirstOrDefault(x => x is ExpressionStatementSyntax || x is LocalDeclarationStatementSyntax);

            if (invokation == null) return;

            context.RegisterCodeFix(CodeAction.Create("Make mock", c => MakeMock(context.Document, invokation, c), "MockItTool"), diagnostic);
        }

        private static async Task<Document> MakeMock(Document document, SyntaxNode invokationSyntax,
            CancellationToken cancellationToken)
        {
            var testSemanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

            var testInitMethodDecl = TestSemanticHelper.GetTestInitializeMethod(testSemanticModel);

            var declaredFields = testInitMethodDecl.Parent.ChildNodes().OfType<FieldDeclarationSyntax>().ToArray();

            var suts = testInitMethodDecl.GetSuts(testSemanticModel, declaredFields);

            var memberAccessExpressions = invokationSyntax.DescendantNodes()
                .OfType<ExpressionSyntax>()
                .Where(x => x is InvocationExpressionSyntax || x is MemberAccessExpressionSyntax)
                .Select(expr =>
                {
                    var memberAccess = expr as MemberAccessExpressionSyntax;
                    var invokationExpression = expr as InvocationExpressionSyntax;
                    var expression = invokationExpression == null ? memberAccess : invokationExpression.Expression;
                    return expression;
                });

            var invokedMethodsOfMocks = memberAccessExpressions.SelectMany(expressionSyntax => MocksAnalyzingEngine.GetInvokedMethodsOfMock(expressionSyntax, testSemanticModel, suts))
                                                               .DistinctBy(x => x.MethodOrPropertySymbol)
                                                               .ToArray();

            if (invokedMethodsOfMocks.Length == 0)
                return document;

            var editor = await DocumentEditor.CreateAsync(document, cancellationToken).ConfigureAwait(false);

            ChangesMaker.ApplyChanges(invokationSyntax, editor, invokedMethodsOfMocks);

            return editor.GetChangedDocument();
        }
    }
}