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

            if(symbol == null) return new Fields[0];

            var refType = symbol.ContainingType;

            var suitableSut = refType.GetSuitableSut(suts);

            if (suitableSut == null) return new Fields[0];

            var sutSubstitutions = TestSemanticHelper.GetSubstitutions(refType);

            if (suitableSut.SemanticModel == null)
                return new Fields[0];

            var suitableSutSymbol = suitableSut.GetSuitableSutSymbol(symbol);
            var sutFirstLocation = suitableSutSymbol.Locations.First();
            var node = sutFirstLocation.GetMemberNode();

            var allNodes = node.DescendantNodesAndSelf().Where(x => !x.Span.IsEmpty).ToList();

            var allSyntax = new List<ExpressionSyntax>();

            var count = int.MaxValue;
            //var maxDepth = 3;

            while (count != allSyntax.Count 
                //&& maxDepth > 0
                )
            {
                count = allSyntax.Count;
                //maxDepth--;

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

            invokedMethodsOfMocks = invokedMethodsOfMocks.Where(DontHaveSetups(suitableSut));

            return invokedMethodsOfMocks;
        }

        private static Func<Fields, bool> DontHaveSetups(SutInfo suitableSut)
        {
            return syntax => !IsExistInSetups(syntax, suitableSut.InjectedFields);
        }

        private static bool IsExistInSetups(Fields fields, IEnumerable<TreeNode<DependencyField>> injectedFields)
        {
            return injectedFields.Any(x => x.FindTreeNodes(y => y.Parent != null
                                                                && IsMemberEquals(fields.MethodOrPropertySymbol, y.Data.SetupIdentifierNode?.Name)
                                                                && fields.FieldsToSetup.Any(z => z.Field.Any(w => y.Parent.Data.Field.Declaration.Variables.Any(t => t.Identifier.Text == w)))).Any());
        }

        private static IEnumerable<SyntaxNode> GetReferencedNodes(SyntaxNode node, SutInfo sutInfo)
        {
            var symbolModel = node.GetModelFromNode(sutInfo.SemanticModel.Compilation);

            if (symbolModel == null)
                return new SyntaxNode[0];

            var symbol = symbolModel.GetSymbolInfo(node).Symbol;

            if (symbol == null || symbol.Locations.All(x => !x.IsInSource))
                return new SyntaxNode[0];

            var sutType = sutInfo.SymbolInfo.Symbol as INamedTypeSymbol;

            //contstraint to get nodes for current sut type or base type of it
            if (sutType != null
                && !(Equals(symbol, sutType) 
                     ||  symbol.ContainingType == sutType 
                     || symbol.ContainingType?.ConstructedFrom == sutType.ConstructedFrom)
                && !(Equals(symbol, sutType.BaseType) 
                     || symbol.ContainingType == sutType.BaseType 
                     || symbol.ContainingType?.ConstructedFrom == sutType.BaseType?.ConstructedFrom)
                )
            {
                return new SyntaxNode[0];
            }

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
            return z => (z.Data.Field?.Declaration.Type as GenericNameSyntax)?.TypeArgumentList.Arguments.Any(y => IsCorrespondingType(semanticModel, y, symbol, sutSubstitutions)) ?? false;
        }

        private static bool IsCorrespondingType(SemanticModel semanticModel, 
            ExpressionSyntax y, 
            ISymbol x, 
            IReadOnlyDictionary<string, ITypeSymbol> sutSubstitutions)
        {
            var symbol = semanticModel.GetSymbolInfo(y).Symbol;

            return IsCorrespondingType(symbol, x, sutSubstitutions);
        }

        private static bool IsCorrespondingType(ISymbol symbol,
            ISymbol x,
            IReadOnlyDictionary<string, ITypeSymbol> sutSubstitutions)
        {
            var methodSymbol = x as IMethodSymbol;
            var propertySymbol = x as IPropertySymbol;

            if (methodSymbol == null && propertySymbol == null)
                return false;

            var ct = methodSymbol != null ? methodSymbol.ReceiverType : propertySymbol.ContainingType;
            var symbolDefinitionsReplacement = TestSemanticHelper.GetReplacedDefinitions(sutSubstitutions, ct);

            var ctName = ct.GetSimpleTypeName();

            return (symbol.GetSimpleTypeName() == ctName
                    || symbolDefinitionsReplacement.Select(z => z.Result).Contains(symbol.GetSimpleTypeName()))
                   //&& !treeNode.FindChildTreeNodes(z => IsCorrespondingField(semanticModel, x, sutSubstitutions)(z)).Any()
                   || (symbol as INamedTypeSymbol)?.AllInterfaces.Any(z => IsCorrespondingType(z, x, sutSubstitutions)) == true;
        }

        private static bool IsMemberEquals(ISymbol methodOrPropertySymbol, SimpleNameSyntax name)
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