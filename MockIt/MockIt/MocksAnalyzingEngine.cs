using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using MockIt.ThirdParty;

namespace MockIt
{
    public class MocksAnalyzingEngine
    {
        public static IEnumerable<Fields> GetInvokedMethodsOfMock(ExpressionSyntax memberAccessExpresion, SemanticModel testSemanticModel, IEnumerable<SutInfo> suts)
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
                var properties = TestSemanticHelper.GetPropertiesToConfigureMocks(allNodes, methods, isLeftSideOfAssignExpression);

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

            invokedMethodsOfMocks = invokedMethodsOfMocks.Where(DontHaveSetups(suitableSut, sutSubstitutions));

            return invokedMethodsOfMocks;
        }

        private static Func<Fields, bool> DontHaveSetups(SutInfo suitableSut, IReadOnlyDictionary<string, ITypeSymbol> sutSubstitutions)
        {
            return syntax => !IsExistInSetups(syntax, suitableSut.InjectedFields, sutSubstitutions);
        }

        private static bool IsExistInSetups(Fields fields, IEnumerable<TreeNode<DependencyField>> injectedFields, IReadOnlyDictionary<string, ITypeSymbol> sutSubstitutions)
        {
            return injectedFields.Any(x => x.FindTreeNodes(y => y.Parent != null
                                                                && IsMemberEquals(fields.MethodOrPropertySymbol, sutSubstitutions, y.Data.SetupIdentifierNode?.Name)
                                                                && fields.FieldsToSetup.Any(z => z.Field.Any(w => y.Parent.Data.Field.Declaration.Variables.Any(t => t.Identifier.Text == w)))).Any());
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
            return suitableSut.InjectedFields.Find(IsCorrespondingField(semanticModel, symbol, sutSubstitutions))
                .Select(z => new FieldsSetups
                {
                    Field = z.Data.Field.Declaration.Variables.Select(f => f.Identifier.ValueText),
                    Substitutions = (z.Data.Field.Declaration.Type as GenericNameSyntax)?.TypeArgumentList
                        .Arguments
                        .Select(y => GetSubstitutions(semanticModel, y))
                        .SelectMany(s => s)
                        .ToDictionary(s => s.Key, s => s.Value),
                    SutSubstitutions = sutSubstitutions
                }).ToArray();
        }

        private static Func<TreeNode<DependencyField>, bool> IsCorrespondingField(SemanticModel semanticModel, 
            ISymbol symbol,
            IReadOnlyDictionary<string, ITypeSymbol> sutSubstitutions)
        {
            return z => (z.Data.Field.Declaration.Type as GenericNameSyntax)?.TypeArgumentList.Arguments.Any(y => IsCorrespondingType(semanticModel, y, symbol, sutSubstitutions, z)) ?? false;
        }

        private static bool IsCorrespondingType(SemanticModel semanticModel, 
            ExpressionSyntax y, 
            ISymbol x, 
            IReadOnlyDictionary<string, ITypeSymbol> sutSubstitutions, 
            TreeNode<DependencyField> treeNode)
        {
            var symbol = semanticModel.GetSymbolInfo(y).Symbol;

            var methodSymbol = x as IMethodSymbol;
            var propertySymbol = x as IPropertySymbol;

            if (methodSymbol == null && propertySymbol == null)
                return false;

            var ct = methodSymbol != null ? methodSymbol.ReceiverType : propertySymbol.ContainingType;
            var symbolDefinitionsReplacement = TestSemanticHelper.GetReplacedDefinitions(sutSubstitutions, ct);
            
            var ctName = ct.GetSimpleTypeName();

            return (symbol.GetSimpleTypeName() == ctName
                    || symbolDefinitionsReplacement.Select(z => z.Result).Contains(symbol.GetSimpleTypeName()))
                   && !treeNode.FindChildTreeNodes(z => IsCorrespondingField(semanticModel, x, sutSubstitutions)(z)).Any();
        }

        private static bool IsMemberEquals(ISymbol methodOrPropertySymbol, IReadOnlyDictionary<string, ITypeSymbol> sutSubstitutions, SimpleNameSyntax name)
        {
            return methodOrPropertySymbol.Name == name.Identifier.Text;
            // && ((methodOrPropertySymbol as ConstructedMethodSymbol)?.TypeArguments.Contains);
        }

        private static Dictionary<string, ITypeSymbol> GetSubstitutions(SemanticModel semanticModel, ExpressionSyntax y)
        {
            var model = y.GetModelFromExpression(semanticModel);
            var symbol = model.GetSymbolInfo(y).Symbol;
            return TestSemanticHelper.GetSubstitutions(symbol);
        }
    }
}