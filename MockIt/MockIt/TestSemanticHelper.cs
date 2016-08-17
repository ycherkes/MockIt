using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text.RegularExpressions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using MockIt.ThirdParty;

namespace MockIt
{
    public static class TestSemanticHelper
    {
        private static readonly string[] s_xUnitMethodsAttributes;
        private static readonly string[] s_testMethodsAttributes;

        static TestSemanticHelper()
        {
            s_xUnitMethodsAttributes = new [] { "Fact", "Theory" };
            s_testMethodsAttributes = new[] { "Test", "TestMethod" }.Concat(s_xUnitMethodsAttributes)
                                                                    .ToArray();
        }

        public static BaseMethodDeclarationSyntax GetTestInitializeMethod(SemanticModel semanticModel)
        {
            var firstTestMethod = GetMethodsWithAttributes(semanticModel, s_testMethodsAttributes).FirstOrDefault();
            var setupMethods = GetMethodsWithAttributes(semanticModel, "SetUp", "TestInitialize").Cast<BaseMethodDeclarationSyntax>().ToArray();

            if (firstTestMethod == null && !setupMethods.Any())
                return null;

            var classDeclSyntax = Parents(firstTestMethod ?? setupMethods.First(), node => node is ClassDeclarationSyntax);

            var constructors = classDeclSyntax?.ChildNodes()
                                               .Where(n => n.Kind() == SyntaxKind.ConstructorDeclaration)
                                               .OfType<ConstructorDeclarationSyntax>()
                                               .Where(x => x.Modifiers.All(y => y.Text != "static"))
                                               .ToArray() ?? Enumerable.Empty<ConstructorDeclarationSyntax>();

            var methods = constructors.Concat(setupMethods);

            return methods.FirstOrDefault();
        }

        public static SemanticModel GetModelFromNode(this SyntaxNode node, IEnumerable<SemanticModel> semanticModels)
        {
            return semanticModels.FirstOrDefault(x => x.SyntaxTree == node.SyntaxTree);
        }

        public static MethodDeclarationSyntax[] GetTestMethods(SemanticModel semanticModel)
        {
            var methodDecls = GetMethodsWithAttributes(semanticModel, s_testMethodsAttributes);

            return methodDecls.ToArray();
        }

        private static IEnumerable<MethodDeclarationSyntax> GetMethodsWithAttributes(SemanticModel semanticModel, params string[] attributes)
        {
            var methodDecl = semanticModel.SyntaxTree
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
                                 .Where(x => !x.Span.IsEmpty)
                                 .Where(expr => !expr.DescendantNodes(methods.Contains).Any())
                                 .ToArray();

            return properties;
        }

        public static ExpressionSyntax[] GetMethodsToConfigureMocks(IEnumerable<SyntaxNode> nodes)
        {
            var methods = nodes.SelectMany(x => x.DescendantNodes())
                               .Where(x => !x.Span.IsEmpty)
                               .OfType<InvocationExpressionSyntax>()
                               .Select(expr => expr.Expression).ToArray();

            return methods;
        }

        public static string GetSimpleTypeName(this ISymbol type)
        {
            return type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
        }

        public static Compilation GetCompilation(this SemanticModel semanticModel, ISymbol suitableSutMember)
        {
            var compilation = IsAssembliesEqual(suitableSutMember, semanticModel)
                ? semanticModel.Compilation
                : GetExternalCompilation(semanticModel.Compilation, suitableSutMember);

            return compilation;
        }

        public static Compilation GetExternalCompilation(this Compilation compilation, ISymbol suitableSutMember)
        {
            return compilation.ExternalReferences
                              .OfType<CompilationReference>()
                              .Select(x => x.Compilation)
                              .FirstOrDefault(x => IsAssembliesEqual(x.Assembly, suitableSutMember.ContainingSymbol.ContainingAssembly));
        }

        public static MemberDeclarationSyntax GetMemberNode(this Location location)
        {
            var sourceTree = location.SourceTree;
            var treeRoot = sourceTree.GetRoot();
            var position = location.SourceSpan.Start;
            var parentNode = treeRoot.FindToken(position).Parent;
            var node = parentNode.FirstAncestorOrSelf<MethodDeclarationSyntax>() as MemberDeclarationSyntax
                       ?? parentNode.FirstAncestorOrSelf<PropertyDeclarationSyntax>();

            return node;
        }

        public static SemanticModel GetModelFromNode(this SyntaxNode node, Compilation sutCompilation)
        {
            return node.SyntaxTree.GetModelFromSyntaxTree(sutCompilation);

        }

        public static SemanticModel GetModelFromSyntaxTree(this SyntaxTree tree, Compilation sutCompilation)
        {
            var compilation = sutCompilation.ContainsSyntaxTree(tree)
                                ? sutCompilation
                                : sutCompilation.ExternalReferences
                                                .OfType<CompilationReference>()
                                                .Select(x => x.Compilation).FirstOrDefault(c => c.ContainsSyntaxTree(tree));

            var model = compilation?.GetSemanticModel(tree);

            return model;

        }

        public static SemanticModel GetSutSemanticModel(SemanticModel testSemanticModel, ISymbol suitableSutSymbol, Location sutFirstLocation)
        {
            var compilation = testSemanticModel.GetCompilation(suitableSutSymbol);
            var sutSemanticModel = compilation?.GetSemanticModel(sutFirstLocation.SourceTree);
            return sutSemanticModel;
        }

        private static bool IsAssembliesEqual(ISymbol sutSymbol, SemanticModel semanticModel)
        {
            return IsAssembliesEqual(sutSymbol.ContainingSymbol.ContainingAssembly, semanticModel.Compilation.Assembly);
        }

        private static bool IsAssembliesEqual(IAssemblySymbol firstSymbol, IAssemblySymbol secondSymbol)
        {
            return firstSymbol.Name == secondSymbol.Name;
        }

        //todo .. and implicit fields also
        public static IReadOnlyCollection<SutInfo> GetSuts(this SyntaxNode testInitMethodDecl, SemanticModel semanticModel,
            IReadOnlyCollection<FieldDeclarationSyntax> declaredFields, bool fillImplicitDependencies = true)
        {
            var suts = testInitMethodDecl.DescendantNodes()
                .OfType<ObjectCreationExpressionSyntax>()
                .Select(x => new
                {
                    Expression = x,
                    SymbolInfo = semanticModel.GetSymbolInfo(x.Type)
                })
                .Where(x => x.SymbolInfo.Symbol != null)
                .Where(x => x.SymbolInfo.Symbol.Locations.Any(y => y.IsInSource && y.SourceTree != null))
                .Select(x => new SutInfo
                {
                    SymbolInfo = x.SymbolInfo,
                    Identifier = x.Expression.Parent.DescendantNodes().OfType<IdentifierNameSyntax>().FirstOrDefault(),
                    SemanticModel = GetSutSemanticModel(semanticModel, x.SymbolInfo.Symbol, x.SymbolInfo.Symbol.Locations.First()),
                    //excplicitly declared fields
                    InjectedFields = declaredFields.Where(z => x.Expression.ArgumentList != null
                                                                       && x.Expression.ArgumentList.Arguments.Any(y => IsSuitableDeclaredField(z, y)))
                                                            .Select(y => new DependencyField
                                                            {
                                                                Field = y,
                                                                IsInjectedFromConstructor = true
                                                            })
                                                            .Select(y => new TreeNode<DependencyField>(y))
                                                           .ToArray()
                })
                .Where(x => x.InjectedFields.Any())
                .ToArray();

            if (!fillImplicitDependencies) return suts;

            var implicitDependencies = GetTestInitSetups(testInitMethodDecl, declaredFields);

            var allInjectedFields = suts.SelectMany(sutInfo => sutInfo.InjectedFields);

            foreach (var field in allInjectedFields)
            {
                FillSetupsTree(field, implicitDependencies);
            }

            return suts;
        }

        private static IReadOnlyCollection<SetupsInfo> GetTestInitSetups(SyntaxNode testInitMethodDecl, IReadOnlyCollection<FieldDeclarationSyntax> declaredFields)
        {
            var result =  testInitMethodDecl.DescendantNodes()
                .OfType<ExpressionStatementSyntax>()
                .Select(x => new
                {
                    Expression = x,
                    Match = Regex.Match(x.ToString(), @"(.+)\s*\.(Setup){1}(Get){0,1}\(\s*(?<varName>\w+)\s*=>\s*\k<varName>\.(\w.+)((\(.*\){1})|'')\)\s*.Returns\((.+)\.Object\)")
                })
                .Where(x => x.Match.Success)
                .Select(x => new SetupsInfo
                {
                    Expression = x.Expression,
                    ParentField = declaredFields.FirstOrDefault(y => y.Declaration.Variables.Any(z => z.Identifier.Text == x.Match.Groups[1].Value.Trim())),
                    SetupIdentifierNode = x.Expression.DescendantNodes().OfType<MemberAccessExpressionSyntax>().FirstOrDefault(y => x.Match.Groups[4].Value.Trim() == y.Name.ToString() || x.Match.Groups[4].Value.Trim().StartsWith(y.Name.ToString() + "(")),
                    ReturnsField = declaredFields.FirstOrDefault(y => y.Declaration.Variables.Any(z => z.Identifier.Text == x.Match.Groups[7].Value.Trim())),
                })
                .ToArray();

            return result;
        }

        private static void FillSetupsTree(TreeNode<DependencyField> parentFieldSyntax, IReadOnlyCollection<SetupsInfo> setupsInfos)
        {
            var fieldSetups = setupsInfos.Where(x => x.ParentField == parentFieldSyntax.Data.Field).Select(x => new DependencyField
            {
                Field = x.ReturnsField,
                IsInjectedFromConstructor = false,
                SetupExpression = x.Expression,
                SetupIdentifierNode = x.SetupIdentifierNode
            }).ToArray();

            var addedSetups = fieldSetups.Select(parentFieldSyntax.AddChild);

            foreach (var nodeFromDependency in addedSetups)
            {
                FillSetupsTree(nodeFromDependency, setupsInfos);
            }
        }

        public static SutInfo GetSuitableSut(this INamedTypeSymbol refType, IEnumerable<SutInfo> suts)
        {
            var suitableSut = suts.Where(x => x.SymbolInfo.Symbol is INamedTypeSymbol)
                                  .Select(x => new { Symbol = (INamedTypeSymbol) x.SymbolInfo.Symbol, sut = x})
                                  .FirstOrDefault(x => x.Symbol.ToDisplayString() == refType.ToDisplayString() ||
                                                       x.Symbol.ConstructedFrom == refType ||
                                                       x.Symbol.AllInterfaces.Any(y => y.ToDisplayString() == refType.ToDisplayString() ||
                                                                                       y.ConstructedFrom == refType));

            return suitableSut.sut;
        }

        public static SyntaxNode Parents(this SyntaxNode node, Func<SyntaxNode, bool> criteria)
        {
            while (true)
            {
                if (criteria(node))
                    return node;

                if (node?.Parent == null)
                    return null;

                node = node.Parent;
            }
        }

        public static ISymbol GetSuitableSutSymbol(this SutInfo suitableSut, ISymbol memberSymbol)
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

        public static IReadOnlyCollection<ReplacementInfo> GetReplacedDefinitions(IReadOnlyDictionary<string, ITypeSymbol> sutSubstitutions, ISymbol typeSymbol)
        {
            var replacements = sutSubstitutions.Select(kv => new[]
            {
                new {Original = "<" + kv.Key + ">", Replacement = "<" + GetSimpleTypeName(kv.Value) + ">"},
                new {Original = "<" + kv.Key + ",", Replacement = "<" + GetSimpleTypeName(kv.Value) + ","},
                new {Original = ", " + kv.Key + ",", Replacement = ", " + GetSimpleTypeName(kv.Value) + ","},
                new {Original = ", " + kv.Key + ">", Replacement = ", " + GetSimpleTypeName(kv.Value) + ">"}
            });

            var originalType = GetSimpleTypeName(typeSymbol);

            var replacedDefinition = replacements.Select(s => s.Aggregate(originalType, (sum, repl) => sum.Replace(repl.Original, repl.Replacement)))
                                                 .Select(x => new ReplacementInfo { IsReplaced = x != originalType, Result = x})
                                                 .ToArray();

            return replacedDefinition;
        }

        private static Tuple<ImmutableArray<ITypeParameterSymbol>, ImmutableArray<ITypeSymbol>> GetGenericInfo(ISymbol symbol)
        {
            var namedTypeSymbol = symbol as INamedTypeSymbol;

            if(namedTypeSymbol != null)
                return new Tuple<ImmutableArray<ITypeParameterSymbol>, ImmutableArray<ITypeSymbol>>(namedTypeSymbol.TypeParameters, namedTypeSymbol.TypeArguments);

            var methodSymbol = symbol as IMethodSymbol;

            if (methodSymbol != null)
                return new Tuple<ImmutableArray<ITypeParameterSymbol>, ImmutableArray<ITypeSymbol>>(methodSymbol.TypeParameters, methodSymbol.TypeArguments);

            return new Tuple<ImmutableArray<ITypeParameterSymbol>, ImmutableArray<ITypeSymbol>>(ImmutableArray<ITypeParameterSymbol>.Empty, ImmutableArray<ITypeSymbol>.Empty);

        }

        public static Dictionary<string, ITypeSymbol> GetSubstitutions(ISymbol symbol)
        {
            var genericInfo = GetGenericInfo(symbol);

            var typeParameters = genericInfo.Item1;
            var typeArguments = genericInfo.Item2;

            if (typeParameters.Length == 0 || typeArguments.Length == 0)
                return new Dictionary<string, ITypeSymbol>();

            var typeMap = typeParameters.Zip(typeArguments, (parameterSymbol, typeSymbol) => new
            {
                Key = parameterSymbol,
                Value = typeSymbol
            })
            .ToDictionary(pair => pair.Key.ToString(), pair => pair.Value);

            return typeMap;
        }

        public static SemanticModel GetModelFromExpression(this ExpressionSyntax x, SemanticModel dependantModel)
        {
            return x.SyntaxTree.GetModelFromSyntaxTree(dependantModel.Compilation);
        }
    }
}