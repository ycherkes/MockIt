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
            var semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);
            var editor = await DocumentEditor.CreateAsync(document, cancellationToken).ConfigureAwait(false);

            var testInitMethodDecl = TestSemanticHelper.GetTestInitializeMethod(semanticModel);

            var declaredFields = testInitMethodDecl.Parent.ChildNodes().OfType<FieldDeclarationSyntax>();

            var suts = testInitMethodDecl.GetSuts(semanticModel, declaredFields);

            var memberAccessExpresion = invokationSyntax.DescendantNodes()
                                                        .OfType<ExpressionSyntax>()
                                                        .Where(x => x is InvocationExpressionSyntax || x is MemberAccessExpressionSyntax)
                                                        .Select(expr =>
                                                        {
                                                            var memberAccess = expr as MemberAccessExpressionSyntax;
                                                            var invokationExpression = expr as InvocationExpressionSyntax;
                                                            var expression = invokationExpression == null ? memberAccess : invokationExpression.Expression;
                                                            return expression;
                                                        }).First();


            var isLeftSideOfAssignExpression = memberAccessExpresion.IsLeftSideOfAssignExpression();
            var symbol = semanticModel.GetSymbolInfo(memberAccessExpresion).Symbol;

            var refType = symbol.ContainingType;

            var suitableSut = refType.GetSuitableSut(suts);

            if (suitableSut == null) return document;

            var sutSubstitutions = TestSemanticHelper.GetSubstitutions(refType);

            var suitableSutMember = suitableSut.GetSuitableSutMember(symbol);

            var sourceTree = suitableSutMember.Locations.First().SourceTree;
            var treeRoot = sourceTree.GetRoot();
            var position = suitableSutMember.Locations.First().SourceSpan.Start;
            var parentToken = treeRoot.FindToken(position).Parent;

            var node = parentToken.FirstAncestorOrSelf<MethodDeclarationSyntax>() as MemberDeclarationSyntax 
                        ?? parentToken.FirstAncestorOrSelf<PropertyDeclarationSyntax>();

            var compilation = TestSemanticHelper.GetCompilation(suitableSutMember, semanticModel);

            if (compilation == null) return document;

            var model = compilation.GetSemanticModel(sourceTree);

            var allNodes = node.DescendantNodesAndSelf().ToList();

            var allSyntax = Enumerable.Empty<ExpressionSyntax>().ToList();

            var count = int.MaxValue;

            while (count != allSyntax.Count)
            {
                count = allSyntax.Count;

                var methods = TestSemanticHelper.GetMethodsToConfigureMocks(allNodes);

                var properties = TestSemanticHelper.GetPropertiesToConfigureMocks(allNodes, methods,
                    isLeftSideOfAssignExpression);

                allSyntax.AddRange(methods.Concat(properties).Distinct());
                allSyntax = allSyntax.Distinct().ToList();

                allNodes = allSyntax.SelectMany(syn  => syn.DescendantNodesAndSelf())
                                    .SelectMany(x => GetReferencedNodes(x, model))
                                    .ToList();
            }

            var invokedMethodsOfMocks = GetInvokedMethodsOfMocks(allSyntax, 
                                                                 model, 
                                                                 suitableSut, 
                                                                 semanticModel, 
                                                                 sutSubstitutions);

            ChangesMaker.ApplyChanges(invokationSyntax, editor, invokedMethodsOfMocks);

            return editor.GetChangedDocument();
        }

        private static IEnumerable<SyntaxNode> GetReferencedNodes(SyntaxNode node, SemanticModel model)
        {
            try
            {
                var symbol = model.GetSymbolInfo(node).Symbol;


                if (symbol == null)
                    return Enumerable.Empty<SyntaxNode>();

                return symbol.DeclaringSyntaxReferences
                    .SelectMany(z => z.GetSyntax()
                        .DescendantNodes());
            }
            //todo: determine the semantic model correctly
            catch
            {
                return Enumerable.Empty<SyntaxNode>();
            }
        }

        private static Fields[] GetInvokedMethodsOfMocks(
            IEnumerable<ExpressionSyntax> methodsAndPropertyInvokations, 
            SemanticModel model, 
            SutInfo suitableSut,
            SemanticModel semanticModel, 
            Dictionary<string, ITypeSymbol> sutSubstitutions)
        {
            var invokedMethodsOfMocks = methodsAndPropertyInvokations.Select(x => new
                                                                             {
                                                                                 model.GetSymbolInfo(x).Symbol,
                                                                                 Expression = x
                                                                             })
                                                                     .Select(x => new Fields
                                                                             {
                                                                                 Expression = x.Expression,
                                                                                 MethodOrPropertySymbol = x.Symbol,
                                                                                 FieldsToSetup = GetFieldsToSetup(suitableSut, semanticModel, x.Symbol, sutSubstitutions)
                                                                             })
                                                                     .Where(x => x.FieldsToSetup.Any())
                                                                     .ToArray();
            return invokedMethodsOfMocks;
        }

        private static IEnumerable<FieldsSetups> GetFieldsToSetup(SutInfo suitableSut, 
                                                                  SemanticModel semanticModel, 
                                                                  ISymbol symbol, 
                                                                  Dictionary<string, ITypeSymbol> sutSubstitutions)
        {
            return suitableSut.DeclaredFields.Where(
                z => (z.Declaration.Type as GenericNameSyntax)?.TypeArgumentList.Arguments.Any(y => IsCorrespondingType(semanticModel, y, symbol, sutSubstitutions)) ?? false)
                .Select(z => new FieldsSetups
                {
                    Field = z.Declaration.Variables.Select(f => f.Identifier.ValueText),
                    Substitutions = (z.Declaration.Type as GenericNameSyntax)?.TypeArgumentList
                                                                              .Arguments
                                                                              .Select(y => GetSubstitutions(semanticModel, y))
                                                                              .SelectMany(s => s)
                                                                              .ToDictionary(s => s.Key, s => s.Value), 
                    //?? GetSubstitutions(semanticModel, z.Declaration.Type),
                    SutSubstitutions = sutSubstitutions
                }).ToArray();
        }


        private static bool IsCorrespondingType(
            SemanticModel semanticModel, 
            ExpressionSyntax y, 
            ISymbol x, 
            IReadOnlyDictionary<string, ITypeSymbol> sutSubstitutions)
        {
            var symbol = semanticModel.GetSymbolInfo(y).Symbol;

            var methodSymbol = x as IMethodSymbol;
            var propertySymbol = x as IPropertySymbol;

            var symbolDefinitionsReplacement = TestSemanticHelper.GetReplacedDefinitions(sutSubstitutions, x.ContainingType);

            var ct = methodSymbol != null ? methodSymbol.ReceiverType : propertySymbol?.ContainingType;
            var ctName = TestSemanticHelper.GetSimpleTypeName(ct);
            
            return TestSemanticHelper.GetSimpleTypeName(symbol) == ctName
                    || symbolDefinitionsReplacement.Contains(TestSemanticHelper.GetSimpleTypeName(symbol));
        }

        private static Dictionary<string, ITypeSymbol> GetSubstitutions(SemanticModel semanticModel, ExpressionSyntax y)
        {
            var symbol = semanticModel.GetSymbolInfo(y).Symbol;
            return TestSemanticHelper.GetSubstitutions(symbol);
        }
    }
}