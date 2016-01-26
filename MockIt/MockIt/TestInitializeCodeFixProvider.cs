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
using Microsoft.CodeAnalysis.Editing;

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

        private static string GetTypeNameFromType(INamedTypeSymbol symbol)
        {
            return symbol.IsGenericType 
                ? FriendlyNamesHelper.GetSimpleTypeName(symbol) + "<" + string.Join(", ", symbol.TypeArguments.OfType<INamedTypeSymbol>().Select(GetTypeNameFromType)) + ">"
                : FriendlyNamesHelper.GetSimpleTypeName(symbol);
        }

        private static async Task<Document> MakeMock(Document document, ExpressionStatementSyntax creation,
            CancellationToken cancellationToken)
        {
            var semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);
            var editor = await DocumentEditor.CreateAsync(document, cancellationToken).ConfigureAwait(false);

            var creationExpressionSyntax = creation.Expression.ChildNodes().OfType<ObjectCreationExpressionSyntax>().First();

            var symbolInfo = semanticModel.GetSymbolInfo(creationExpressionSyntax);
            
            var invokedSymbol = (IMethodSymbol)symbolInfo.CandidateSymbols.FirstOrDefault(x => x is IMethodSymbol &&
                                                                                          ((IMethodSymbol)x).Parameters.All(y => y.Type.IsAbstract));
            
            var constructorParameters = invokedSymbol.Parameters.Select(x => new { ArgumentName = x.Name, TypeName = GetTypeNameFromType((INamedTypeSymbol)x.Type) }).ToArray();

            var changes = constructorParameters.Select(x => new
            {
                NewField = SyntaxFactory.FieldDeclaration(
                               SyntaxFactory.VariableDeclaration(
                                    SyntaxFactory.GenericName(
                                        SyntaxFactory.Identifier("Mock"))
                                    .WithTypeArgumentList(
                                        SyntaxFactory.TypeArgumentList(
                                            SyntaxFactory.SingletonSeparatedList<TypeSyntax>(
                                                SyntaxFactory.IdentifierName(x.TypeName)))))
                                .WithVariables(
                                    SyntaxFactory.SingletonSeparatedList(
                                        SyntaxFactory.VariableDeclarator(
                                            SyntaxFactory.Identifier(x.ArgumentName + "Mock")))))
                           .WithModifiers(
                                SyntaxFactory.TokenList(
                                    SyntaxFactory.Token(SyntaxKind.PrivateKeyword))),

                NewExpression = SyntaxFactory.ExpressionStatement(
                                    SyntaxFactory.AssignmentExpression(
                                        SyntaxKind.SimpleAssignmentExpression,
                                        SyntaxFactory.IdentifierName(x.ArgumentName + "Mock"),
                                        SyntaxFactory.ObjectCreationExpression(
                                            SyntaxFactory.GenericName(
                                                SyntaxFactory.Identifier("Mock"))
                                            .WithTypeArgumentList(
                                                SyntaxFactory.TypeArgumentList(
                                                    SyntaxFactory.SingletonSeparatedList<TypeSyntax>(
                                                        SyntaxFactory.IdentifierName(x.TypeName)))))
                                        .WithArgumentList(SyntaxFactory.ArgumentList()))),

                CreationArgument = (SyntaxNodeOrToken)SyntaxFactory.Argument(
                                                        SyntaxFactory.MemberAccessExpression(
                                                            SyntaxKind.SimpleMemberAccessExpression,
                                                            SyntaxFactory.IdentifierName(x.ArgumentName + "Mock"),
                                                            SyntaxFactory.IdentifierName(@"Object")))
            }).ToArray();

            var arguments = (from change in changes
                             from comma in new[] { (SyntaxNodeOrToken)SyntaxFactory.Token(SyntaxKind.CommaToken) }
                             select new { change.CreationArgument, comma })
                            .SelectMany(x => new[] { x.CreationArgument, x.comma })
                            .Take(changes.Length * 2 - 1);

            editor.InsertBefore(creation.Parent.Parent, changes.Select(x => x.NewField));
            editor.InsertBefore(creation, changes.Select((x, i) => changes.Length - 1 == i ? x.NewExpression.WithTrailingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.CarriageReturnLineFeed, SyntaxFactory.CarriageReturnLineFeed)) : x.NewExpression));
            editor.ReplaceNode(creationExpressionSyntax.ArgumentList, SyntaxFactory.ArgumentList(SyntaxFactory.SeparatedList<ArgumentSyntax>(arguments)));

            return editor.GetChangedDocument();
        }
    }
}