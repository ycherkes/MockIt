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

            var sutSubstitutions = GetSubstitutions(refType);

            var suitableSutMember = ((INamedTypeSymbol)suitableSut.SymbolInfo.Symbol).FindImplementationForInterfaceMember(symbol);

            //for parameterized generic method
            if (suitableSutMember == null)
            {
                var s1 = symbol as IMethodSymbol;
                if (s1?.ConstructedFrom != null)
                    suitableSutMember =
                        ((INamedTypeSymbol)suitableSut.SymbolInfo.Symbol).FindImplementationForInterfaceMember(s1.ConstructedFrom);
            }

            if (suitableSutMember == null) suitableSutMember = symbol;

            var sourceTree = suitableSutMember.Locations.First().SourceTree;
            var treeRoot = sourceTree.GetRoot();
            var position = suitableSutMember.Locations.First().SourceSpan.Start;
            var node = treeRoot.FindToken(position).Parent.FirstAncestorOrSelf<MethodDeclarationSyntax>() as MemberDeclarationSyntax ?? treeRoot.FindToken(position).Parent.FirstAncestorOrSelf<PropertyDeclarationSyntax>();

            var compilation = TestSemanticHelper.GetCompilation(suitableSutMember, semanticModel);

            var model = compilation.GetSemanticModel(sourceTree);

            var methods = TestSemanticHelper.GetMethodsToConfigureMocks(node);

            var properties = TestSemanticHelper.GetPropertiesToConfigureMocks(node, methods, isLeftSideOfAssignExpression);

            var invoks = methods.Concat(properties).ToArray();

            var invokedMethodsOfMocks = invoks
                .Select(x => new { model.GetSymbolInfo(x).Symbol, Expression = x})
                .Select(
                    x =>
                        new Fields
                        {
                            Expression = x.Expression,
                            MethodOrPropertySymbol = x.Symbol,
                            FieldsToSetup =
                                suitableSut.DeclaredFields.Where(
                                    z =>
                                        // todo make corresponding variables determination without name equality ( sut.varname + "Mock" === test.mockVarName )
                                        // removed
                                        //z.Declaration.Variables[0].ToString() == ((MemberAccessExpressionSyntax)x.Expression).Expression.ToString() + "Mock" && 
                                        ((z.Declaration.Type as GenericNameSyntax)?.TypeArgumentList.Arguments.Any(y => IsCorrespondingType(semanticModel, y, x.Symbol, sutSubstitutions)) ?? false /*?? IsCorrespondingType(semanticModel, z.Declaration.Type, x.Symbol, sutSubstitutions)*/))
                                    .Select(z => new FieldsSetups
                                    {
                                        Field = z.Declaration.Variables.Select(f => f.Identifier.ValueText),
                                        Substitutions = (z.Declaration.Type as GenericNameSyntax)?.TypeArgumentList
                                                                                 .Arguments.Select(y => GetSubstitutions(semanticModel, y))
                                                                                 .SelectMany(s => s)
                                                                                 .ToDictionary(s => s.Key, s => s.Value), 
                                                                                 //?? GetSubstitutions(semanticModel, z.Declaration.Type),
                                        SutSubstitutions = sutSubstitutions
                                    })
                                    .ToArray()
                        })
                .Where(x => x.FieldsToSetup.Any())
                .ToArray();

            var setups = invokedMethodsOfMocks.SelectMany(x => x.FieldsToSetup
            .SelectMany(y => y.Field.Select(f => GetSetups(f, x, y))))
            .SelectMany(x =>
            {
                var collections = x as string[] ?? x.ToArray();
                return collections;
            })
            .Distinct()
            .Select(x => SyntaxFactory.ExpressionStatement(SyntaxFactory.ParseExpression(x.Replace("#ToReplace#", "Setup").Replace("#ToReplaceGet#", "SetupGet").Replace("#ToReplaceSet#", "SetupSet"))))
            .ToArray();

            var verifiers = invokedMethodsOfMocks.SelectMany(x => x.FieldsToSetup.SelectMany(y => y.Field)).Distinct().Select(x => SyntaxFactory.ExpressionStatement(SyntaxFactory.ParseExpression(x + ".VerifyAll()"))).ToArray();

            editor.InsertBefore(invokationSyntax, setups.Select((x, i) => setups.Length - 1 == i ? x.WithLeadingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.ElasticMarker)).WithTrailingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.CarriageReturnLineFeed, SyntaxFactory.CarriageReturnLineFeed)) : x.WithLeadingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.ElasticMarker))));

            //todo will try to understand how to add a new line before
            editor.InsertAfter(invokationSyntax, verifiers.Select(x => x.WithLeadingTrivia(SyntaxFactory.ElasticMarker)));

            return editor.GetChangedDocument();
        }
        

        private class Fields
        {
            public ISymbol MethodOrPropertySymbol { get; set; }
            public IEnumerable<FieldsSetups> FieldsToSetup { get; set; }
            public ExpressionSyntax Expression { get; set; }
        }

        private class FieldsSetups
        {
            public IEnumerable<string> Field { get; set; }
            public Dictionary<string, ITypeSymbol> Substitutions { get; set; }
            public Dictionary<string, ITypeSymbol> SutSubstitutions { get; internal set; }
        }

        private static IEnumerable<string> GetSetups(string f, Fields x, FieldsSetups y)
        {
            var methodSymbol = x.MethodOrPropertySymbol as IMethodSymbol;

            if (methodSymbol != null)
            {

                return new[]
                {
                    f + ".#ToReplace#(x => x." +
                    methodSymbol.Name + "(" +
                    string.Join(", ", methodSymbol.Parameters.Select(z => "It.Is<" +
                                                                          GetSimpleTypeName(y.Substitutions,
                                                                              y.SutSubstitutions, z.Type) +
                                                                          ">(" + z.Name + " => " + z.Name +
                                                                          " == default(" +
                                                                          GetSimpleTypeName(y.Substitutions,
                                                                              y.SutSubstitutions, z.Type) +
                                                                          "))")) + "))" +
                    (methodSymbol.ReturnType.ToDisplayString() != "void"
                        ? ".Returns(default(" +
                          GetSimpleTypeName(y.Substitutions, y.SutSubstitutions, methodSymbol.ReturnType) +
                          "))"
                        : "")
                };
            }

            var propertySymbol = (IPropertySymbol)x.MethodOrPropertySymbol;

            var expressions = new List<string>();

            if (!propertySymbol.IsWriteOnly && !x.Expression.IsLeftSideOfAssignExpression())
            {
                var getExpression = f + ".#ToReplaceGet#(x => x." + propertySymbol.Name + ").Returns(default(" +
                                    GetSimpleTypeName(y.Substitutions, y.SutSubstitutions, propertySymbol.Type) + "))";

                expressions.Add(getExpression);
            }

            if (propertySymbol.IsReadOnly || !x.Expression.IsLeftSideOfAssignExpression()) return expressions;

            var setExpression = f + ".#ToReplaceSet#(x => x." + propertySymbol.Name + " = default(" +
                                GetSimpleTypeName(y.Substitutions, y.SutSubstitutions, propertySymbol.Type) + "))";

            expressions.Add(setExpression);

            return expressions;
        }

        private static string GetSimpleTypeName(IReadOnlyDictionary<string, ITypeSymbol> substitutions, IReadOnlyDictionary<string, ITypeSymbol> sutSubstitutions, ITypeSymbol z)
        {
            ITypeSymbol substitution;
            ITypeSymbol sutSubstitution;

            return TestSemanticHelper.GetSimpleTypeName(
                    substitutions.TryGetValue(z.ToString(), out substitution)
                    ? substitution
                    : sutSubstitutions.TryGetValue(z.ToString(), out sutSubstitution)
                    ? sutSubstitution
                    : z);
        }

        private static bool IsCorrespondingType(SemanticModel semanticModel, ExpressionSyntax y, ISymbol x, IReadOnlyDictionary<string, ITypeSymbol> sutSubstitutions)
        {
            var symbol = semanticModel.GetSymbolInfo(y).Symbol;

            var methodSymbol = x as IMethodSymbol;
            var propertySymbol = x as IPropertySymbol;

            if (methodSymbol != null)
            {
                var methodDefinitionsReplacement = GetReplacedDefinitions(sutSubstitutions, methodSymbol);
                return symbol.ToString() == methodSymbol.ReceiverType.ToString() ||
                       methodDefinitionsReplacement.Contains(symbol.ToString());
            }
            var propertyDefinitionsReplacement = GetReplacedDefinitions(sutSubstitutions, propertySymbol);

            return symbol.ToString() == propertySymbol?.ContainingType.ToString() ||
                   propertyDefinitionsReplacement.Contains(symbol.ToString());
        }

        private static IEnumerable<string> GetReplacedDefinitions(IReadOnlyDictionary<string, ITypeSymbol> sutSubstitutions, ISymbol propertySymbol)
        {
            var replacements = sutSubstitutions.Select(kv => new[]
            {
                new {Original = "<" + kv.Key + ">", Replacement = "<" + kv.Value + ">"},
                new {Original = "<" + kv.Key + ",", Replacement = "<" + kv.Value + ","},
                new {Original = ", " + kv.Key + ",", Replacement = ", " + kv.Value + ","},
                new {Original = ", " + kv.Key + ">", Replacement = ", " + kv.Value + ">"}
            });

            var originalType = propertySymbol.ContainingType.ToString();

            var replacedDefinition = replacements.Select(s => s.Aggregate(originalType, (sum, repl) => sum.Replace(repl.Original, repl.Replacement)));
            return replacedDefinition;
        }

        private static Dictionary<string, ITypeSymbol> GetSubstitutions(SemanticModel semanticModel, ExpressionSyntax y)
        {
            var symbol = semanticModel.GetSymbolInfo(y).Symbol;
            return GetSubstitutions(symbol);
        }

        private static Dictionary<string, ITypeSymbol> GetSubstitutions(ISymbol symbol)
        {
            var namedTypeSymbol = symbol as INamedTypeSymbol;

            var emptyDictionary = new Dictionary<string, ITypeSymbol>();

            if (namedTypeSymbol == null) return emptyDictionary;
            var typeParameters = namedTypeSymbol.TypeParameters;
            var typeArguments = namedTypeSymbol.TypeArguments;

            if (typeParameters.Length == 0 || typeArguments.Length == 0)
                return emptyDictionary;

            var typeMap = typeParameters.Zip(typeArguments,
                (parameterSymbol, typeSymbol) => new { Key = parameterSymbol, Value = typeSymbol })
                .ToDictionary(pair => pair.Key.ToString(), pair => pair.Value);

            return typeMap;
        }
    }
}