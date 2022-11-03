using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using MockIt.ThirdParty;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading.Tasks;

namespace MockIt
{
    public static class MocksAnalyzingEngine
    {
        private const int MaxDepth = 10;
        public static async Task<IEnumerable<Fields>> GetInvokedMethodsOfMock(ExpressionSyntax memberAccessExpression, SemanticModel testSemanticModel, IEnumerable<SutInfo> suts)
        {
            var isLeftSideOfAssignExpression = memberAccessExpression.IsLeftSideOfAssignExpression();
            var symbol = testSemanticModel.GetSymbolInfo(memberAccessExpression).Symbol;

            if (symbol == null) return Array.Empty<Fields>();

            var refType = symbol.ContainingType;

            var suitableSut = refType.GetSuitableSut(suts);

            if (suitableSut == null) return Array.Empty<Fields>();

            var sutSubstitutionsByInterface = TestSemanticHelper.GetSubstitutions(refType);
            var sutSubstitutionsByConcreteType = TestSemanticHelper.GetSubstitutions(suitableSut.SymbolInfo.Symbol);
            var symbolSubstitutions = TestSemanticHelper.GetSubstitutions(symbol);
            var sutSubstitutions = sutSubstitutionsByInterface.Concat(sutSubstitutionsByConcreteType)
                                                              .Concat(symbolSubstitutions)
                                                              .DistinctBy(x => x.Key)
                                                              .ToImmutableDictionary(x => x.Key, x => x.Value);

            if (suitableSut.SemanticModel == null)
                return Array.Empty<Fields>();

            var suitableSutSymbol = suitableSut.GetSuitableSutSymbol(symbol);
            var sutFirstLocation = suitableSutSymbol.Locations.First();
            var node = await sutFirstLocation.GetMemberNode();

            if (node == null)
            {
                return Array.Empty<Fields>();
            }

            var allNodes = node.DescendantNodesAndSelf().Where(x => !x.Span.IsEmpty).ToList();

            var allSyntax = new List<ExpressionSyntax>();

            var count = int.MaxValue;
            var iteration = 0;
            while (count != allSyntax.Count || ++iteration > MaxDepth)
            {
                count = allSyntax.Count;

                var methods = TestSemanticHelper.GetMethodsToConfigureMocks(allNodes).ToArray();
                var properties = TestSemanticHelper.GetPropertiesToConfigureMocks(allNodes, methods, isLeftSideOfAssignExpression).ToArray();

                allSyntax.AddRange(methods.Concat(properties).Distinct());
                allSyntax = allSyntax.Distinct().ToList();

                allNodes = allSyntax.SelectMany(syn => syn.DescendantNodesAndSelf().Where(x => !x.Span.IsEmpty))
                                    .SelectMany(x => GetReferencedNodes(x, suitableSut))
                                    .ToList();
            }

            var invokedMethodsOfMocks = GetInvokedMethodsOfMocks(allSyntax,
                                                                 suitableSut.SemanticModel,
                                                                 suitableSut,
                                                                 testSemanticModel,
                                                                 sutSubstitutions);

            invokedMethodsOfMocks = invokedMethodsOfMocks.Where(HaveMissingSetups(suitableSut));

            return invokedMethodsOfMocks;
        }

        private static Func<Fields, bool> HaveMissingSetups(SutInfo suitableSut)
        {
            return syntax => !ExistsInSetups(syntax, suitableSut.InjectedFields);
        }

        private static bool ExistsInSetups(Fields fields, IEnumerable<TreeNode<DependencyField>> injectedFields)
        {
            return injectedFields.Any(x => x.FindTreeNodes(y => y.Parent != null
                                                                && IsMemberEquals(fields.MethodOrPropertySymbol, y.Data.SetupIdentifierNode?.Name)
                                                                && fields.FieldsToSetup.Any(z => z.Field.Any(w => y.Parent.Data.Field.Declaration.Variables.Any(t => t.Identifier.Text == w)))).Any());
        }

        private static IEnumerable<SyntaxNode> GetReferencedNodes(SyntaxNode node, SutInfo sutInfo)
        {
            var symbolModel = node.GetModelFromNode(sutInfo.SemanticModel.Compilation);

            if (symbolModel == null)
                return Array.Empty<SyntaxNode>();

            var symbol = symbolModel.GetSymbolInfo(node).Symbol;

            if (symbol == null || symbol.Locations.All(x => !x.IsInSource))
                return Array.Empty<SyntaxNode>();

            var symbolType = symbol as INamedTypeSymbol;

            //constraint to get nodes for current sut type or base type of it
            if (sutInfo.SymbolInfo.Symbol is INamedTypeSymbol sutType
                && !IsEqual(symbolType, sutType)
                && !IsEqual(symbol.ContainingType, sutType)
                && !IsEqual(symbol.ContainingType?.ConstructedFrom, sutType.ConstructedFrom)
                && !IsEqual(symbolType, sutType.BaseType)
                && !IsEqual(symbol.ContainingType, sutType.BaseType)
                && !IsEqual(symbol.ContainingType?.ConstructedFrom, sutType.BaseType?.ConstructedFrom))
            {
                return Array.Empty<SyntaxNode>();
            }

            return symbol.DeclaringSyntaxReferences
                         .SelectMany(z => z.GetSyntax()
                         .DescendantNodes());
        }

        private static bool IsEqual(INamedTypeSymbol type1, INamedTypeSymbol type2)
        {
            return SymbolEqualityComparer.Default.Equals(type1, type2);
            //type1?.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat) ==
            //       type2?.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
        }

        private static IEnumerable<Fields> GetInvokedMethodsOfMocks(
            IEnumerable<ExpressionSyntax> methodsAndPropertyInvocations,
            SemanticModel model,
            SutInfo suitableSut,
            SemanticModel semanticModel,
            IImmutableDictionary<string, ITypeSymbol> sutSubstitutions)
        {
            var invokedMethodsOfMocks = methodsAndPropertyInvocations.Select(x => new
            {
                x.GetModelFromExpression(model)?.GetSymbolInfo(x).Symbol,
                Expression = x
            }).SelectMany(x => new[] {new Fields
                {
                    Expression = x.Expression,
                    MethodOrPropertySymbol = x.Symbol,
                    FieldsToSetup = GetFieldsToSetup(suitableSut, semanticModel, x.Symbol, sutSubstitutions)
                }}.Concat(GetUsings(x.Expression, x.Symbol, suitableSut, semanticModel, sutSubstitutions)))
                .Where(x => x.FieldsToSetup.Any())
                .ToArray();
            return invokedMethodsOfMocks;
        }

        private static IEnumerable<Fields> GetUsings(ExpressionSyntax expression,
            ISymbol symbol,
            SutInfo suitableSut,
            SemanticModel semanticModel,
            IImmutableDictionary<string, ITypeSymbol> sutSubstitutions)
        {
            var syntax = expression.Parents(x => (x as UsingStatementSyntax)?.Declaration
                                                                             ?.Variables
                                                                             .Any(y => y.DescendantNodes()
                                                                                        .Any(z => z == expression)) == true) as UsingStatementSyntax;

            if (syntax == null) return Enumerable.Empty<Fields>();

            var methodSymbol = symbol as IMethodSymbol;
            var propertySymbol = symbol as IPropertySymbol;

            if (methodSymbol == null && propertySymbol == null)
                return Array.Empty<Fields>();

            var containingType = methodSymbol?.ReturnType ?? propertySymbol?.GetMethod?.ReturnType;

            var disposable = containingType?.Interfaces.FirstOrDefault(x => x.Name == "IDisposable");

            if (disposable == null)
                return Enumerable.Empty<Fields>();

            var disposeMethod = disposable.GetMembers("Dispose").First();

            return syntax.Declaration?.Variables.Select(x => new Fields
            {
                Expression = expression,
                MethodOrPropertySymbol = disposeMethod,
                FieldsToSetup = GetFieldsToSetup(suitableSut, semanticModel, containingType, sutSubstitutions)
            }) ?? Array.Empty<Fields>();
        }

        private static IEnumerable<FieldsSetups> GetFieldsToSetup(SutInfo suitableSut,
            SemanticModel semanticModel,
            ISymbol symbol,
            IImmutableDictionary<string, ITypeSymbol> sutSubstitutions)
        {
            var suitableFields = suitableSut.InjectedFields.Find(IsSuitableField(semanticModel, symbol, sutSubstitutions));

            return suitableFields
                .Select(z =>
                {
                    return new FieldsSetups
                    {
                        Field = z.Data.Field.Declaration.Variables.Select(f => f.Identifier.ValueText),
                        Substitutions = (z.Data.Field.Declaration.Type as GenericNameSyntax)?.TypeArgumentList
                            .Arguments
                            .Select(y => GetSubstitutions(semanticModel, y))
                            .SelectMany(s => s)
                            .ToImmutableDictionary(s => s.Key, s => s.Value),
                        SutSubstitutions = sutSubstitutions
                    };
                }).ToArray();
        }

        private static Func<TreeNode<DependencyField>, bool> IsSuitableField(SemanticModel semanticModel,
            ISymbol symbol,
            IImmutableDictionary<string, ITypeSymbol> sutSubstitutions)
        {
            return z => (z.Data.Field?.Declaration.Type as GenericNameSyntax)?.TypeArgumentList
                                                                              .Arguments
                                                                              .Any(y => IsSuitableType(semanticModel, y, symbol, sutSubstitutions)) ?? false;
        }

        private static bool IsSuitableType(SemanticModel semanticModel,
            ExpressionSyntax y,
            ISymbol x,
            IImmutableDictionary<string, ITypeSymbol> sutSubstitutions)
        {
            var symbol = semanticModel.GetSymbolInfo(y).Symbol;

            return IsSuitableType(symbol, x, sutSubstitutions);
        }

        private static bool IsSuitableType(ISymbol symbol,
            ISymbol x,
            IImmutableDictionary<string, ITypeSymbol> sutSubstitutions)
        {
            var methodSymbol = x as IMethodSymbol;
            var propertySymbol = x as IPropertySymbol;
            var typeSymbol = x as INamedTypeSymbol;

            if (methodSymbol == null && propertySymbol == null && typeSymbol == null)
                return false;

            var ct = methodSymbol != null ? methodSymbol.ReceiverType : propertySymbol?.ContainingType ?? typeSymbol;
            var symbolDefinitionsReplacement = TestSemanticHelper.GetReplacedDefinitions(sutSubstitutions, ct);

            var ctName = ct.GetSimpleTypeName();

            return symbol.GetSimpleTypeName() == ctName
                   || symbolDefinitionsReplacement.Select(z => z.Result).Contains(symbol.GetSimpleTypeName())
                   || (symbol as INamedTypeSymbol)?.AllInterfaces.Any(z => IsSuitableType(z, x, sutSubstitutions)) == true;
        }

        private static bool IsMemberEquals(ISymbol methodOrPropertySymbol, SimpleNameSyntax name)
        {
            return methodOrPropertySymbol.Name == name.Identifier.Text;
        }

        private static IImmutableDictionary<string, ITypeSymbol> GetSubstitutions(SemanticModel semanticModel, ExpressionSyntax y)
        {
            var model = y.GetModelFromExpression(semanticModel);
            var symbol = model.GetSymbolInfo(y).Symbol;
            return TestSemanticHelper.GetSubstitutions(symbol);
        }
    }
}