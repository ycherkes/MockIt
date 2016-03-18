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
            var testSemanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

            var testInitMethodDecl = TestSemanticHelper.GetTestInitializeMethod(testSemanticModel);

            var declaredFields = testInitMethodDecl.Parent.ChildNodes().OfType<FieldDeclarationSyntax>();

            var suts = testInitMethodDecl.GetSuts(testSemanticModel, declaredFields);

            //todo handle the case when: _sut.List().Result instead of _sut.List()
            var memberAccessExpression = invokationSyntax.DescendantNodes()
                .OfType<ExpressionSyntax>()
                .Where(x => x is InvocationExpressionSyntax || x is MemberAccessExpressionSyntax)
                .Select(expr =>
                {
                    var memberAccess = expr as MemberAccessExpressionSyntax;
                    var invokationExpression = expr as InvocationExpressionSyntax;
                    var expression = invokationExpression == null ? memberAccess : invokationExpression.Expression;
                    return expression;
                });//.First();


            var invokedMethodsOfMocks = memberAccessExpression.SelectMany(expressionSyntax => GetInvokedMethodsOfMock(expressionSyntax, testSemanticModel, suts))
                                                              .DistinctBy(x => x.MethodOrPropertySymbol)
                                                              .ToArray();

            if (invokedMethodsOfMocks.Length == 0)
                return document;

            var editor = await DocumentEditor.CreateAsync(document, cancellationToken).ConfigureAwait(false);

            ChangesMaker.ApplyChanges(invokationSyntax, editor, invokedMethodsOfMocks);

            return editor.GetChangedDocument();
        }

        private static IEnumerable<Fields> GetInvokedMethodsOfMock(ExpressionSyntax memberAccessExpresion, SemanticModel testSemanticModel, IEnumerable<SutInfo> suts)
        {
            var isLeftSideOfAssignExpression = memberAccessExpresion.IsLeftSideOfAssignExpression();
            var symbol = testSemanticModel.GetSymbolInfo(memberAccessExpresion).Symbol;

            var refType = symbol.ContainingType;

            var suitableSut = refType.GetSuitableSut(suts);

            if (suitableSut == null) return new Fields[0];

            var sutSubstitutions = TestSemanticHelper.GetSubstitutions(refType);

            var suitableSutSymbol = suitableSut.GetSuitableSutSymbol(symbol);
            var sutFirstLocation = suitableSutSymbol.Locations.First();
            var sutSemanticModel = TestSemanticHelper.GetSutSemanticModel(testSemanticModel, suitableSutSymbol, sutFirstLocation);

            if (sutSemanticModel == null)
                return new Fields[0];

            var node = sutFirstLocation.GetMemberNode();

            var allNodes = node.DescendantNodesAndSelf().ToList();

            var allSyntax = new List<ExpressionSyntax>();

            var count = int.MaxValue;

            while (count != allSyntax.Count)
            {
                count = allSyntax.Count;

                var methods = TestSemanticHelper.GetMethodsToConfigureMocks(allNodes);
                var properties = TestSemanticHelper.GetPropertiesToConfigureMocks(allNodes, methods,
                    isLeftSideOfAssignExpression);

                allSyntax.AddRange(methods.Concat(properties).Distinct());
                allSyntax = allSyntax.Distinct().ToList();

                allNodes = allSyntax.SelectMany(syn => syn.DescendantNodesAndSelf())
                                    .SelectMany(x => GetReferencedNodes(x, sutSemanticModel))
                                    .ToList();
            }

            var invokedMethodsOfMocks = GetInvokedMethodsOfMocks(allSyntax,
                sutSemanticModel,
                suitableSut,
                testSemanticModel,
                sutSubstitutions);

            return invokedMethodsOfMocks;
        }

        private static IEnumerable<SyntaxNode> GetReferencedNodes(SyntaxNode node, SemanticModel model)
        {
            var symbolModel = node.GetModelFromNode(model.Compilation);

            if (symbolModel == null)
                return new SyntaxNode[0];

            var symbol = symbolModel.GetSymbolInfo(node).Symbol;


            if (symbol == null)
                return new SyntaxNode[0];

            return symbol.DeclaringSyntaxReferences
                         .SelectMany(z => z.GetSyntax()
                                           .DescendantNodes());
        }

        private static IEnumerable<Fields> GetInvokedMethodsOfMocks(
            IEnumerable<ExpressionSyntax> methodsAndPropertyInvokations, 
            SemanticModel model, 
            SutInfo suitableSut,
            SemanticModel semanticModel, 
            Dictionary<string, ITypeSymbol> sutSubstitutions)
        {
            var invokedMethodsOfMocks = methodsAndPropertyInvokations.Select(x => new
                                                                             {
                                                                                 x.GetModelFromExpression(model)?.GetSymbolInfo(x).Symbol,
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
            var model = y.GetModelFromExpression(semanticModel);
            var symbol = model.GetSymbolInfo(y).Symbol;
            return TestSemanticHelper.GetSubstitutions(symbol);
        }
    }
}