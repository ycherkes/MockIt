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

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Reflection;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;
//using Toolbelt.DynamicBinderExtension;

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

        private static async Task<Document> MakeMock(Document document, SyntaxNode creation,
            CancellationToken cancellationToken)
        {
            var semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);
            var editor = await DocumentEditor.CreateAsync(document, cancellationToken).ConfigureAwait(false);

            var testInitMethodDecl = semanticModel.SyntaxTree.GetRoot()
                                .DescendantNodes()
                                .OfType<MethodDeclarationSyntax>()
                                .FirstOrDefault(x => x.AttributeLists
                                    .Any(y => y.Attributes
                                        .Any(z => new[] { "TestFixtureSetUp", "TestInitialize" }.Contains(((IdentifierNameSyntax)z.Name).Identifier.Text))));

            var declaredFields = testInitMethodDecl.Parent.ChildNodes().OfType<FieldDeclarationSyntax>();

            var suts = testInitMethodDecl.DescendantNodes()
                .OfType<ObjectCreationExpressionSyntax>()
                .Select(x => new
                {
                    SymbolInfo = semanticModel.GetSymbolInfo(x.Type),
                    DeclaredFields = declaredFields.Where(z => x.ArgumentList.Arguments.Any(y => y.Expression.GetText().ToString() == z.Declaration.Variables.FirstOrDefault()?.Identifier.Text + ".Object")).ToArray()
                })
                .Where(x => x.DeclaredFields.Any())
                .ToArray();

            var invokation = creation.DescendantNodes().OfType<InvocationExpressionSyntax>().First();

            var symbol = (IMethodSymbol)semanticModel.GetSymbolInfo(invokation.Expression).Symbol;

            var refType = symbol.OriginalDefinition.ContainingType;

            var suitableSut = suts.FirstOrDefault(x => (((INamedTypeSymbol)x.SymbolInfo.Symbol).AllInterfaces.Any(y => y == refType || y.ConstructedFrom == refType)));

            
            var suitableSutMember = ((INamedTypeSymbol)suitableSut.SymbolInfo.Symbol).FindImplementationForInterfaceMember(symbol);

            var sourceTree = suitableSutMember.Locations.First().SourceTree;
            var treeRoot = sourceTree.GetRoot();
            var position = suitableSutMember.Locations.First().SourceSpan.Start;
            var node = treeRoot.FindToken(position).Parent.FirstAncestorOrSelf<MethodDeclarationSyntax>();

            var compilation = symbol.ContainingAssembly.Name == semanticModel.Compilation.Assembly.Name
                        ? semanticModel.Compilation
                        : semanticModel.Compilation.ExternalReferences
                            .OfType<CompilationReference>()
                            .Select(x => x.Compilation)
                            .FirstOrDefault(x => x.Assembly.Name == symbol.ContainingAssembly.Name);
            var model = compilation.GetSemanticModel(sourceTree);


            //todo = work with another semanticModel (not in this file)
            var invokedMethodsOfMocks = node.DescendantNodes()
                .OfType<InvocationExpressionSyntax>()
                .Select(x => (IMethodSymbol)model.GetSymbolInfo(x.Expression).Symbol)
                .Select(
                    x =>
                        new
                        {
                            MethodSymbol = x,
                            FieldsToSetup = 
                                suitableSut.DeclaredFields.Where(
                                    z =>
                                        (z.Declaration.Type as GenericNameSyntax)?.TypeArgumentList
                                                                                  .Arguments
                                                                                  .Any(y => IsCorrespondingType(semanticModel, y, x))
                                                                                 ?? false)
                                    .Select(z => new
                                    {
                                            Field = z.Declaration.Variables.Select(f => f.Identifier.ValueText),
                                            Substitutions = (z.Declaration.Type as GenericNameSyntax)?.TypeArgumentList
                                                                                 .Arguments.Select(y => GetSubstitutions(semanticModel, y))
                                                                                 .SelectMany(s => s)
                                                                                 .ToDictionary(s => s.Key.ToString(), s => s.Value)
                                    })
                                    .ToArray()
                        })
                .Where(x => x.FieldsToSetup.Any())
                .ToArray();

            var setups = invokedMethodsOfMocks.SelectMany(x => x.FieldsToSetup
            .SelectMany(y => y.Field.Select(f => f +".#ToReplace#(x => x." + 
            x.MethodSymbol.Name + "(" + 
            string.Join(", ", x.MethodSymbol.Parameters.Select(z => "It.Is<" + 
            GetSimpleTypeName(y.Substitutions, z.Type) +
            ">(" + z.Name + " => " + z.Name + " == default(" +
            GetSimpleTypeName(y.Substitutions, z.Type) +
            "))")) + "))" + (x.MethodSymbol.ReturnType.ToDisplayString() != "void" ? ".Returns(default(" +
            GetSimpleTypeName(y.Substitutions, x.MethodSymbol.ReturnType) +
            "))" : ""))))
            .Distinct()
            .Select(x => SyntaxFactory.ExpressionStatement(SyntaxFactory.ParseExpression(x.Replace("#ToReplace#", "Setup"))))
            .ToArray();

            var verifiers = invokedMethodsOfMocks.SelectMany(x => x.FieldsToSetup.SelectMany(y => y.Field)).Distinct().Select(x => SyntaxFactory.ExpressionStatement(SyntaxFactory.ParseExpression(x + ".VerifyAll()")));

            editor.InsertBefore(creation, setups.Select(x => x.WithLeadingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.ElasticMarker))));
            editor.InsertAfter(creation, verifiers.Select(x => x.WithLeadingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.ElasticMarker))));

            return editor.GetChangedDocument();
        }

        private static string GetSimpleTypeName(IReadOnlyDictionary<string, ITypeSymbol> substitutions, ITypeSymbol z)
        {
            ITypeSymbol substitution;

            return FriendlyNamesHelper.GetSimpleTypeName(substitutions.TryGetValue(z.ToString(), out substitution)
                ? substitution
                : z);
        }

        private static bool IsCorrespondingType(SemanticModel semanticModel, ExpressionSyntax y, IMethodSymbol x)
        {
            var symbol = semanticModel.GetSymbolInfo(y).Symbol;
            //todo for generics have to determine the substitutions it are in the private property ConstructedNamedTypeSymbol.TypeMap
            return symbol.ToString() == x.ReceiverType.ToString() || 
                   (symbol as INamedTypeSymbol)?.ConstructedFrom.ToString() == x.ReceiverType.ToString();
        }

        private static IEnumerable<KeyValuePair<ITypeParameterSymbol, ITypeSymbol>> GetSubstitutions(SemanticModel semanticModel, ExpressionSyntax y)
        {
            var symbol = semanticModel.GetSymbolInfo(y).Symbol;
            var namedTypeSymbol = symbol as INamedTypeSymbol;

            var emptyDictionary = new Dictionary<ITypeParameterSymbol, ITypeSymbol>();

            if (namedTypeSymbol == null) return emptyDictionary;
            var typeParameters = namedTypeSymbol.TypeParameters;
            var typeArguments = namedTypeSymbol.TypeArguments;

            if (typeParameters.Length == 0 || typeArguments.Length == 0)
                return emptyDictionary;

            var typeMap = typeParameters.Zip(typeArguments, (parameterSymbol, typeSymbol) => new { Key = parameterSymbol, Value = typeSymbol })
                                        .ToDictionary(pair => pair.Key, pair => pair.Value);

            return typeMap;
        }
    }
}