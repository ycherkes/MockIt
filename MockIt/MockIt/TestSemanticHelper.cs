using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace MockIt
{
    public static class TestSemanticHelper
    {
        public static MethodDeclarationSyntax GetTestInitializeMethod(SemanticModel semanticModel)
        {
            var methods = GetMethodsWithAttributes(semanticModel, "SetUp", "TestInitialize");

            return methods.FirstOrDefault();
        }

        public static SemanticModel GetModelFromNode(this SyntaxNode node, IEnumerable<SemanticModel> semanticModels)
        {
            return semanticModels.FirstOrDefault(x => x.SyntaxTree == node.SyntaxTree);
        }

        public static MethodDeclarationSyntax[] GetTestMethods(SemanticModel semanticModel)
        {
            var methodDecls = GetMethodsWithAttributes(semanticModel, "Test", "TestMethod");

            return methodDecls.ToArray();
        }

        private static IEnumerable<MethodDeclarationSyntax> GetMethodsWithAttributes(SemanticModel semanticModel, params string[] attributes)
        {
            var methodDecl = semanticModel
                .SyntaxTree
                .GetRoot()
                .DescendantNodes()
                .OfType<MethodDeclarationSyntax>()
                .Where(x => x.AttributeLists
                    .Any(y => y.Attributes
                        .Any(z => z.Name is IdentifierNameSyntax && 
                                  attributes.Contains(((IdentifierNameSyntax) z.Name).Identifier.Text))));

            return methodDecl;
        }

        public static IEnumerable<MemberAccessExpressionSyntax> GetPropertiesToConfigureMocks(IEnumerable<SyntaxNode> nodes,
            IEnumerable<ExpressionSyntax> methods)
        {
            var properties = nodes.SelectMany(node => GetPropertiesToConfigureMocks(node, methods))
                                  .ToArray();

            return properties;
        }

        //public static ExpressionSyntax[] GetMethodsToConfigureMocks(IEnumerable<SyntaxNode> nodes)
        //{
        //    var methods = nodes.SelectMany(GetMethodsToConfigureMocks).ToArray();

        //    return methods;
        //}

        public static IEnumerable<MemberAccessExpressionSyntax> GetPropertiesToConfigureMocks(IEnumerable<SyntaxNode> nodes,
            IEnumerable<ExpressionSyntax> methods, bool isLeftSideOfAssignExpression)
        {
            var properties = nodes.SelectMany(node => GetPropertiesToConfigureMocks(node, methods, isLeftSideOfAssignExpression))
                                  .ToArray();

            return properties;
        }

        public static IEnumerable<MemberAccessExpressionSyntax> GetPropertiesToConfigureMocks(SyntaxNode node,
            IEnumerable<ExpressionSyntax> methods, bool isLeftSideOfAssignExpression = false)
        {
            var property = node as PropertyDeclarationSyntax;

            if (property != null)
            {
                var accessors = property.AccessorList.Accessors;
                var getter = accessors.FirstOrDefault(ad => ad.Kind() == SyntaxKind.GetAccessorDeclaration);
                var setter = accessors.FirstOrDefault(ad => ad.Kind() == SyntaxKind.SetAccessorDeclaration);

                node = isLeftSideOfAssignExpression ? setter : getter;

                if (node == null)
                    return Enumerable.Empty<MemberAccessExpressionSyntax>();
            }

            var properties = node.DescendantNodes()
                                 .OfType<MemberAccessExpressionSyntax>()
                                 .Where(expr => !expr.DescendantNodes(methods.Contains).Any())
                                 .ToArray();

            return properties;
        }

        public static ExpressionSyntax[] GetMethodsToConfigureMocks(IEnumerable<SyntaxNode> nodes)
        {
            var methods = nodes.SelectMany(x => x.DescendantNodes())
                               .OfType<InvocationExpressionSyntax>()
                               .Select(expr => expr.Expression).ToArray();

            return methods;
        }

        public static string GetSimpleTypeName(ISymbol type)
        {
            return type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
        }

        public static Compilation GetCompilation(ISymbol suitableSutMember, SemanticModel semanticModel)
        {
            var compilation = suitableSutMember.ContainingSymbol.ContainingAssembly.Name ==
                              semanticModel.Compilation.Assembly.Name
                ? semanticModel.Compilation
                : semanticModel.Compilation
                               .ExternalReferences
                               .OfType<CompilationReference>()
                               .Select(x => x.Compilation)
                               .FirstOrDefault(x => x.Assembly.Name == suitableSutMember.ContainingSymbol.ContainingAssembly.Name);

            return compilation;
        }

        public static IReadOnlyCollection<SutInfo> GetSuts(this SyntaxNode testInitMethodDecl, SemanticModel semanticModel,
            IEnumerable<FieldDeclarationSyntax> declaredFields)
        {
            var suts = testInitMethodDecl.DescendantNodes()
                .OfType<ObjectCreationExpressionSyntax>()
                .Select(x => new SutInfo
                {
                    SymbolInfo = semanticModel.GetSymbolInfo(x.Type),
                    DeclaredFields =
                        declaredFields.Where(z => x.ArgumentList != null 
                                                  && x.ArgumentList.Arguments.Any(y => IsSuitableDeclaredField(z, y))).ToArray()
                })
                .Where(x => x.DeclaredFields.Any())
                .ToArray();

            return suts;
        }

        public static SutInfo GetSuitableSut(this INamedTypeSymbol refType, IEnumerable<SutInfo> suts)
        {
            var suitableSut =
                suts.FirstOrDefault(
                    x =>
                        x.SymbolInfo.Symbol is INamedTypeSymbol &&
                        (((INamedTypeSymbol)x.SymbolInfo.Symbol).ToDisplayString() == refType.ToDisplayString() ||
                        ((INamedTypeSymbol)x.SymbolInfo.Symbol).ConstructedFrom == refType) ||
                        ((INamedTypeSymbol)x.SymbolInfo.Symbol).AllInterfaces.Any(
                           y => y.ToDisplayString() == refType.ToDisplayString() || 
                           y.ConstructedFrom == refType));

            return suitableSut;
        }

        public static SyntaxNode Parents(this SyntaxNode node, Func<SyntaxNode, bool> criteria)
        {
            while (true)
            {
                if (criteria(node))
                    return node;

                if (node.Parent == null)
                    return null;

                node = node.Parent;
            }
        }

        public static ISymbol GetSuitableSutMember(this SutInfo suitableSut, ISymbol memberSymbol)
        {
            var suitableSutMember =
                ((INamedTypeSymbol)suitableSut.SymbolInfo.Symbol).FindImplementationForInterfaceMember(memberSymbol);


            if (suitableSutMember != null) return suitableSutMember;

            //for parameterized generic method
            var s1 = memberSymbol as IMethodSymbol;

            if (s1?.ConstructedFrom != null)
                suitableSutMember =
                    ((INamedTypeSymbol)suitableSut.SymbolInfo.Symbol).FindImplementationForInterfaceMember(
                        s1.ConstructedFrom);

            return suitableSutMember ?? memberSymbol;
        }

        private static bool IsSuitableDeclaredField(BaseFieldDeclarationSyntax z, ArgumentSyntax y)
        {
            return new[] { z.Declaration.Variables.FirstOrDefault()?.Identifier.Text + ".Object", z.Declaration.Variables.FirstOrDefault()?.Identifier.Text }.Contains(y.Expression.GetText().ToString().Trim());
        }

        public static IEnumerable<string> GetReplacedDefinitions(IReadOnlyDictionary<string, ITypeSymbol> sutSubstitutions, ISymbol typeSymbol)
        {
            var replacements = sutSubstitutions.Select(kv => new[]
            {
                new {Original = "<" + kv.Key + ">", Replacement = "<" + GetSimpleTypeName(kv.Value) + ">"},
                new {Original = "<" + kv.Key + ",", Replacement = "<" + GetSimpleTypeName(kv.Value) + ","},
                new {Original = ", " + kv.Key + ",", Replacement = ", " + GetSimpleTypeName(kv.Value) + ","},
                new {Original = ", " + kv.Key + ">", Replacement = ", " + GetSimpleTypeName(kv.Value) + ">"}
            });

            var originalType = GetSimpleTypeName(typeSymbol);

            var replacedDefinition = replacements.Select(s => s.Aggregate(originalType, (sum, repl) => sum.Replace(repl.Original, repl.Replacement)));
            return replacedDefinition;
        }

        public static Dictionary<string, ITypeSymbol> GetSubstitutions(ISymbol symbol)
        {
            var namedTypeSymbol = symbol as INamedTypeSymbol;

            var emptyDictionary = new Dictionary<string, ITypeSymbol>();

            if (namedTypeSymbol == null) return emptyDictionary;

            var typeParameters = namedTypeSymbol.TypeParameters;
            var typeArguments = namedTypeSymbol.TypeArguments;

            if (typeParameters.Length == 0 || typeArguments.Length == 0)
                return emptyDictionary;

            var typeMap = typeParameters.Zip(typeArguments, (parameterSymbol, typeSymbol) => new
            {
                Key = parameterSymbol,
                Value = typeSymbol
            })
                .ToDictionary(pair => pair.Key.ToString(), pair => pair.Value);

            return typeMap;
        }
    }
}