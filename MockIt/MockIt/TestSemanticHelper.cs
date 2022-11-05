using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using MockIt.ThirdParty;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace MockIt
{
    public static class TestSemanticHelper
    {
        private static readonly HashSet<string> TestMethodsAttributes = new HashSet<string> { "Test", "TestMethod", "Fact", "Theory" };
        private static readonly HashSet<string> SetupMethodsAttributes = new HashSet<string> { "SetUp", "TestInitialize" };
        private static readonly HashSet<string> TestFrameworkUsings = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
        {
            "Microsoft.VisualStudio.TestTools.UnitTesting",
            "Xunit",
            "NUnit.Framework"
        };

        public static SutCreationContext[] GetSutCreationContexts(SemanticModel semanticModel)
        {
            var allMethods = GetMethods(semanticModel).ToArray();
            var setupMethods = WithAttributes(allMethods, SetupMethodsAttributes).Cast<BaseMethodDeclarationSyntax>().ToArray();
            var csu = semanticModel.SyntaxTree.GetRoot() as CompilationUnitSyntax;
            var usings = csu?.Usings.FirstOrDefault(x => TestFrameworkUsings.Contains(x.Name.ToFullString()));
            var firstTestMethod = WithAttributes(allMethods, TestMethodsAttributes).FirstOrDefault();

            if (allMethods.Length == 0 || usings == null)
                return Array.Empty<SutCreationContext>();

            var classDeclSyntax = firstTestMethod == null && setupMethods.Length == 0
                                    ? csu?.DescendantNodes().OfType<ClassDeclarationSyntax>().FirstOrDefault()
                                    : Parents(firstTestMethod ?? setupMethods.First(), node => node is ClassDeclarationSyntax);

            var constructors = classDeclSyntax?.ChildNodes()
                                               .Where(n => n.IsKind(SyntaxKind.ConstructorDeclaration))
                                               .OfType<ConstructorDeclarationSyntax>()
                                               .Where(x => x.Modifiers.All(y => y.Text != "static"))
                                               .ToArray() ?? Array.Empty<ConstructorDeclarationSyntax>();

            var methodsExcludingSetupMethods = allMethods.Except(setupMethods).Where(x => x.Parent == classDeclSyntax);

            return setupMethods.Select(x => new SutCreationContext
            {
                ContextType = SutCreationContextType.InitMethod,
                MethodSyntax = x
            }).Concat(constructors.Select(x => new SutCreationContext
            {
                ContextType = SutCreationContextType.Constructor,
                MethodSyntax = x
            })).Concat(methodsExcludingSetupMethods.Select(x => new SutCreationContext
            {
                ContextType = SutCreationContextType.Method,
                MethodSyntax = x
            })).ToArray();
        }

        public static SutCreationContextContainer GetSutCreationContextContainer(SemanticModel semanticModel)
        {
            var allMethods = GetMethods(semanticModel).ToArray();
            var setupMethods = WithAttributes(allMethods, SetupMethodsAttributes).Cast<BaseMethodDeclarationSyntax>().ToArray();
            var csu = semanticModel.SyntaxTree.GetRoot() as CompilationUnitSyntax;
            var usings = csu?.Usings.FirstOrDefault(x => TestFrameworkUsings.Contains(x.Name.ToFullString()));
            var firstTestMethod = WithAttributes(allMethods, TestMethodsAttributes).FirstOrDefault();

            if (allMethods.Length == 0 || usings == null)
                return new SutCreationContextContainer();

            var classDeclSyntax = firstTestMethod == null && setupMethods.Length == 0
                                    ? csu?.DescendantNodes().OfType<ClassDeclarationSyntax>().FirstOrDefault()
                                    : Parents(firstTestMethod ?? setupMethods.First(), node => node is ClassDeclarationSyntax);

            var constructors = classDeclSyntax?.ChildNodes()
                                               .Where(n => n.IsKind(SyntaxKind.ConstructorDeclaration))
                                               .OfType<ConstructorDeclarationSyntax>()
                                               .Where(x => x.Modifiers.All(y => y.Text != "static"))
                                               .ToArray() ?? Array.Empty<ConstructorDeclarationSyntax>();

            var methodsExcludingSetupMethods = allMethods.Except(setupMethods).Where(x => x.Parent == classDeclSyntax);

            var contexts = setupMethods.Select(x => new SutCreationContext
            {
                ContextType = SutCreationContextType.InitMethod,
                MethodSyntax = x
            }).Concat(constructors.Select(x => new SutCreationContext
            {
                ContextType = SutCreationContextType.Constructor,
                MethodSyntax = x
            })).Concat(methodsExcludingSetupMethods.Select(x => new SutCreationContext
            {
                ContextType = SutCreationContextType.Method,
                MethodSyntax = x
            })).ToArray();

            foreach (var context in contexts)
            {
                context.DeclaredVariables = context.MethodSyntax.Body?.ChildNodes().OfType<LocalDeclarationStatementSyntax>().ToArray() ?? Array.Empty<LocalDeclarationStatementSyntax>();
            }

            return new SutCreationContextContainer
            {
                Contexts = contexts,
                Fields = contexts.FirstOrDefault()?.MethodSyntax?.Parent?.ChildNodes().OfType<FieldDeclarationSyntax>().ToArray() ?? Array.Empty<FieldDeclarationSyntax>()
            };
        }

        private static IEnumerable<MethodDeclarationSyntax> WithAttributes(IEnumerable<MethodDeclarationSyntax> methods, HashSet<string> attributes)
        {
            var methodDecl = methods.Where(x => x.AttributeLists
                                                 .Any(y => y.Attributes
                                                            .Any(z => z.Name is IdentifierNameSyntax syntax && attributes.Contains(syntax.Identifier.Text))));

            return methodDecl;
        }

        public static IEnumerable<MethodDeclarationSyntax> GetMethods(SemanticModel semanticModel)
        {
            var methodDecl = semanticModel.SyntaxTree
                                          .GetRoot()
                                          .DescendantNodes()
                                          .OfType<MethodDeclarationSyntax>()
                                          .Where(x => x.Modifiers.All(y => y.Text != "static"));

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

        private static IEnumerable<MemberAccessExpressionSyntax> GetPropertiesToConfigureMocks(SyntaxNode node,
            IEnumerable<ExpressionSyntax> methods, bool isLeftSideOfAssignExpression = false)
        {
            if (node is PropertyDeclarationSyntax property)
            {
                var accessors = property.AccessorList?.Accessors;
                var getter = accessors?.FirstOrDefault(ad => ad.IsKind(SyntaxKind.GetAccessorDeclaration));
                var setter = accessors?.FirstOrDefault(ad => ad.IsKind(SyntaxKind.SetAccessorDeclaration));

                node = isLeftSideOfAssignExpression ? setter : getter;

                if (node == null)
                    return Array.Empty<MemberAccessExpressionSyntax>();
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
            return (type as INamedTypeSymbol)?.IsAnonymousType == true ? "object" : type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
        }

        private static Compilation GetCompilation(this SemanticModel semanticModel, ISymbol suitableSutMember)
        {
            var compilation = IsAssemblyEqual(suitableSutMember, semanticModel)
                ? semanticModel.Compilation
                : GetExternalCompilation(semanticModel.Compilation, suitableSutMember);

            return compilation;
        }

        private static Compilation GetExternalCompilation(this Compilation compilation, ISymbol suitableSutMember)
        {
            return compilation.ExternalReferences
                              .OfType<CompilationReference>()
                              .Select(x => x.Compilation)
                              .FirstOrDefault(x => IsEqual(x.Assembly, suitableSutMember.ContainingSymbol.ContainingAssembly));
        }

        public static async Task<MemberDeclarationSyntax> GetMemberNode(this Location location)
        {
            var sourceTree = location.SourceTree;

            if (sourceTree == null) return null;

            var treeRoot = await sourceTree.GetRootAsync();
            var position = location.SourceSpan.Start;
            var parentNode = treeRoot.FindToken(position).Parent;
            var node = parentNode?.FirstAncestorOrSelf<MethodDeclarationSyntax>() as MemberDeclarationSyntax
                       ?? parentNode?.FirstAncestorOrSelf<PropertyDeclarationSyntax>();

            return node;
        }

        public static SemanticModel GetModelFromNode(this SyntaxNode node, Compilation sutCompilation)
        {
            return node.SyntaxTree.GetModelFromSyntaxTree(sutCompilation);
        }

        private static SemanticModel GetModelFromSyntaxTree(this SyntaxTree tree, Compilation sutCompilation)
        {
            var compilation = sutCompilation.ContainsSyntaxTree(tree)
                                ? sutCompilation
                                : sutCompilation.ExternalReferences
                                                .OfType<CompilationReference>()
                                                .Select(x => x.Compilation)
                                                .FirstOrDefault(c => c.ContainsSyntaxTree(tree));

            var model = compilation?.GetSemanticModel(tree);

            return model;

        }

        private static SemanticModel GetSutSemanticModel(SemanticModel testSemanticModel, ISymbol suitableSutSymbol, Location sutFirstLocation)
        {
            if (sutFirstLocation.SourceTree == null) return null;
            var compilation = testSemanticModel.GetCompilation(suitableSutSymbol);
            var sutSemanticModel = compilation?.GetSemanticModel(sutFirstLocation.SourceTree);
            return sutSemanticModel;
        }

        private static bool IsAssemblyEqual(ISymbol sutSymbol, SemanticModel semanticModel)
        {
            return IsEqual(sutSymbol.ContainingSymbol.ContainingAssembly, semanticModel.Compilation.Assembly);
        }

        private static bool IsEqual(IAssemblySymbol firstSymbol, IAssemblySymbol secondSymbol)
        {
            return firstSymbol.Name == secondSymbol.Name;
        }

        //todo .. and implicit fields also

        public static IReadOnlyCollection<SutInfo> GetSuts(this SutCreationContext context,
            SemanticModel semanticModel,
            IReadOnlyCollection<FieldDeclarationSyntax> declaredFields)
        {
            //const bool fillImplicitDependencies = true;
            var suts = context.MethodSyntax.DescendantNodes()
                .OfType<ObjectCreationExpressionSyntax>()
                .Select(x => new
                {
                    Expression = x,
                    SymbolInfo = semanticModel.GetSymbolInfo(x.Type)
                })
                .Where(x => x.SymbolInfo.Symbol?.Locations.Any(y => y.IsInSource && y.SourceTree != null) == true)
                .Select(x => new SutInfo
                {
                    SymbolInfo = x.SymbolInfo,
                    Identifier = ((VariableDeclarationSyntax)x.Expression.Parents(n => n is VariableDeclarationSyntax))?.Variables.FirstOrDefault()?.Identifier ?? x.Expression.Parent?.DescendantNodes().OfType<IdentifierNameSyntax>().Select(i => i.Identifier).FirstOrDefault(),
                    SemanticModel = GetSutSemanticModel(semanticModel, x.SymbolInfo.Symbol, x.SymbolInfo.Symbol?.Locations.First()),
                    //explicitly declared fields or variables
                    InjectedDependencies = declaredFields.Select(d => d.Declaration).Concat(context.DeclaredVariables.Select(v => v.Declaration)).Where(z => x.Expression.ArgumentList != null
                                                                       && x.Expression.ArgumentList.Arguments.Any(y => IsSuitableDeclaredVariable(z, y)))
                                                            .Select(y => new Dependency
                                                            {
                                                                FieldOrLocalVariable = y,
                                                                IsInjectedFromConstructor = true
                                                            })
                                                            .Select(y => new TreeNode<Dependency>(y))
                                                           .ToArray()
                })
                .Where(x => x.InjectedDependencies.Any())
                .ToArray();

            //if (!fillImplicitDependencies) return suts;

            var implicitDependencies = GetTestInitSetups(context.MethodSyntax, declaredFields);

            var allInjectedFields = suts.SelectMany(sutInfo => sutInfo.InjectedDependencies);

            foreach (var field in allInjectedFields)
            {
                FillSetupsTree(field, field, implicitDependencies);
            }

            return suts;
        }

        private static IReadOnlyCollection<SetupsInfo> GetTestInitSetups(SyntaxNode testInitMethodDecl, IReadOnlyCollection<FieldDeclarationSyntax> declaredFields)
        {
            var result = testInitMethodDecl.DescendantNodes()
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
                    ParentFieldOrVariable = declaredFields.FirstOrDefault(y => y.Declaration.Variables.Any(z => z.Identifier.Text == x.Match.Groups[1].Value.Trim()))?.Declaration,
                    SetupIdentifierNode = x.Expression.DescendantNodes().OfType<MemberAccessExpressionSyntax>().FirstOrDefault(y => x.Match.Groups[4].Value.Trim() == y.Name.ToString() || x.Match.Groups[4].Value.Trim().StartsWith(y.Name + "(")),
                    ReturnsFieldOrVariable = declaredFields.FirstOrDefault(y => y.Declaration.Variables.Any(z => z.Identifier.Text == x.Match.Groups[7].Value.Trim()))?.Declaration,
                })
                .ToArray();

            return result;
        }

        private static void FillSetupsTree(TreeNode<Dependency> rootDependencySyntax, TreeNode<Dependency> parentDependencySyntax, IReadOnlyCollection<SetupsInfo> setupsInfos)
        {
            var fieldSetups = setupsInfos.Where(x => x.ParentFieldOrVariable == parentDependencySyntax.Data.FieldOrLocalVariable).Select(x => new Dependency
            {
                FieldOrLocalVariable = x.ReturnsFieldOrVariable,
                IsInjectedFromConstructor = false,
                SetupExpression = x.Expression,
                SetupIdentifierNode = x.SetupIdentifierNode
            }).ToArray();

            var addedSetups = new List<TreeNode<Dependency>>();

            foreach (var setup in fieldSetups)
            {
                if (rootDependencySyntax.FindTreeNode(y => Equals(y.Data, setup)) != default) continue;

                addedSetups.Add(parentDependencySyntax.AddChild(setup));
            }

            foreach (var nodeFromDependency in addedSetups)
            {
                FillSetupsTree(rootDependencySyntax, nodeFromDependency, setupsInfos);
            }
        }

        public static SutInfo GetSuitableSut(this INamedTypeSymbol refType, IEnumerable<SutInfo> suts)
        {
            var suitableSut = suts.Where(x => x.SymbolInfo.Symbol is INamedTypeSymbol)
                                  .Select(x => new { Symbol = (INamedTypeSymbol)x.SymbolInfo.Symbol, sut = x })
                                  .FirstOrDefault(x => x.Symbol.ToDisplayString() == refType.ToDisplayString() ||
                                                       x.Symbol.ConstructedFrom.Equals(refType, SymbolEqualityComparer.Default) ||
                                                       x.Symbol.AllInterfaces.Any(y => y.ToDisplayString() == refType.ToDisplayString() ||
                                                                                       y.ConstructedFrom.Equals(refType, SymbolEqualityComparer.Default)));

            return suitableSut?.sut;
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
            var suitableSutMember = ((INamedTypeSymbol)suitableSut.SymbolInfo.Symbol)?.FindImplementationForInterfaceMember(memberSymbol);


            if (suitableSutMember != null) return suitableSutMember;

            //for parameterized generic method
            var s1 = memberSymbol as IMethodSymbol;

            if (s1?.ConstructedFrom != null)
                suitableSutMember = ((INamedTypeSymbol)suitableSut.SymbolInfo.Symbol)?.FindImplementationForInterfaceMember(s1.ConstructedFrom);

            return suitableSutMember ?? memberSymbol;
        }

        private static bool IsSuitableDeclaredVariable(VariableDeclarationSyntax z, ArgumentSyntax y)
        {
            var variableIdentifier = z.Variables.FirstOrDefault()?.Identifier.Text;
            return new[]
            {
                variableIdentifier + ".Object",
                variableIdentifier
            }
            .Contains(y.Expression.GetText().ToString().Trim());
        }

        private static bool IsSuitableDeclaredField(BaseFieldDeclarationSyntax z, ArgumentSyntax y)
        {
            var variableIdentifier = z.Declaration.Variables.FirstOrDefault()?.Identifier.Text;
            return new[]
            {
                variableIdentifier + ".Object",
                variableIdentifier
            }
            .Contains(y.Expression.GetText().ToString().Trim());
        }

        /// <summary>
        /// todo: replace string manipulations with syntax
        /// </summary>
        /// <param name="sutSubstitutions"></param>
        /// <param name="typeSymbol"></param>
        /// <returns></returns>
        public static IReadOnlyCollection<ReplacementInfo> GetReplacedDefinitions(IImmutableDictionary<string, ITypeSymbol> sutSubstitutions, ISymbol typeSymbol)
        {
            var replacements = sutSubstitutions.Select(kv => new { Original = kv.Key, Substitution = GetSimpleTypeName(kv.Value) })
                .Select(kv =>
                    new[]
                    {
                        new {Original = "<" + kv.Original + ">", Replacement = "<" + kv.Substitution + ">"},
                        new {Original = "<" + kv.Original + ",", Replacement = "<" + kv.Substitution +  ","},
                        new {Original = ", " + kv.Original + ",", Replacement = ", " + kv.Substitution + ","},
                        new {Original = ", " + kv.Original + ">", Replacement = ", " + kv.Substitution + ">"}
                    });

            var originalType = GetSimpleTypeName(typeSymbol);

            var replacedDefinition = replacements.Select(s => s.Aggregate(originalType, (sum, repl) => sum.Replace(repl.Original, repl.Replacement)))
                                                 .Select(x => new ReplacementInfo { IsReplaced = x != originalType, Result = x })
                                                 .ToArray();

            return replacedDefinition;
        }

        private static (ImmutableArray<ITypeParameterSymbol> typeParameters, ImmutableArray<ITypeSymbol> typeArguments) GetGenericInfo(ISymbol symbol)
        {
            if (symbol is INamedTypeSymbol namedTypeSymbol)
                return (namedTypeSymbol.TypeParameters, namedTypeSymbol.TypeArguments);

            if (symbol is IMethodSymbol methodSymbol)
                return (methodSymbol.TypeParameters, methodSymbol.TypeArguments);

            return (ImmutableArray<ITypeParameterSymbol>.Empty, ImmutableArray<ITypeSymbol>.Empty);

        }

        public static IImmutableDictionary<string, ITypeSymbol> GetSubstitutions(ISymbol symbol)
        {
            var (typeParameters, typeArguments) = GetGenericInfo(symbol);

            if (typeParameters.Length == 0 || typeArguments.Length == 0)
                return ImmutableDictionary<string, ITypeSymbol>.Empty;

            var typeMap = typeParameters.Zip(typeArguments, (parameterSymbol, typeSymbol) => new
            {
                Key = parameterSymbol,
                Value = typeSymbol
            }).ToImmutableDictionary(x => x.Key.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat), x => x.Value);

            return typeMap;
        }

        public static SemanticModel GetModelFromExpression(this ExpressionSyntax x, SemanticModel dependentModel)
        {
            return x.SyntaxTree.GetModelFromSyntaxTree(dependentModel.Compilation);
        }
    }
}