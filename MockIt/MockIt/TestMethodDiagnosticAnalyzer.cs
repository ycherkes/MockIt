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
using System.Linq;
using System.Text.RegularExpressions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using MockIt.ThirdParty;

namespace MockIt
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class TestMethodDiagnosticAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "MockItMethod";
        internal const string Category = "Usage";
        
        internal static readonly LocalizableString Title = new LocalizableResourceString(nameof(Resources.AnalyzerTitle), Resources.ResourceManager, typeof (Resources));

        internal static readonly LocalizableString MessageFormat = new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager, typeof (Resources));

        internal static readonly LocalizableString Description = new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager, typeof (Resources));

        internal static DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, true, Description);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSemanticModelAction(AnalyzeSemantic);
        }

        private static void AnalyzeSemantic(SemanticModelAnalysisContext semanticContext)
        {
            var testSemanticModel = semanticContext.SemanticModel;
            var methodDecls = TestSemanticHelper.GetTestMethods(testSemanticModel);

            var methods = TestSemanticHelper.GetMethodsToConfigureMocks(methodDecls);

            var properties = TestSemanticHelper.GetPropertiesToConfigureMocks(methodDecls, methods);

            var expressions = methods.Concat(properties).ToArray();

            if (!expressions.Any())
                return;

            var testInitMethodDecl = TestSemanticHelper.GetTestInitializeMethod(testSemanticModel);

            if (testInitMethodDecl == null) return;

            var declaredFields = testInitMethodDecl.Parent.ChildNodes().OfType<FieldDeclarationSyntax>().ToArray();

            var suts = testInitMethodDecl.GetSuts(testSemanticModel, declaredFields);

            foreach (var expression in expressions)
            {
                var symbol = testSemanticModel.GetSymbolInfo(expression).Symbol;

                if(symbol == null)
                    continue;

                var refType = symbol.OriginalDefinition.ContainingType;

                var suitableSut = refType.GetSuitableSut(suts);

                if (suitableSut == null)
                    continue;

                var suitableSutSymbol = suitableSut.GetSuitableSutSymbol(symbol);
                var sutFirstLocation = suitableSutSymbol.Locations.First();
                var sutSemanticModel = TestSemanticHelper.GetSutSemanticModel(testSemanticModel, suitableSutSymbol, sutFirstLocation);
                
                if(sutSemanticModel == null)
                    continue;

                var node = sutFirstLocation.GetMemberNode();

                var allNodes = new[] {node};

                var descendants = allNodes.SelectMany(nod => nod.DescendantNodes()).ToArray();

                var allSymbols = new List<IMethodSymbol>();

                var count = int.MaxValue;

                while (count != allSymbols.Count)
                {
                    count = allSymbols.Count;

                    var propertyGetSetSymbols = descendants.OfType<MemberAccessExpressionSyntax>()
                        .SelectMany(x =>
                        {
                            var sm = x.GetModelFromNode(new[] {sutSemanticModel, testSemanticModel});
                            var symb = sm?.GetSymbolInfo(x).Symbol as IPropertySymbol;
                            return symb != null
                                ? new[] {symb.GetMethod, symb.SetMethod}
                                : Enumerable.Empty<IMethodSymbol>();
                        });

                    var methodInvokationSymbols = descendants.OfType<InvocationExpressionSyntax>()
                        .Select(x =>  (IMethodSymbol)x.GetModelFromNode(new[] { sutSemanticModel, testSemanticModel })?.GetSymbolInfo(x.Expression).Symbol);

                    allSymbols.AddRange(propertyGetSetSymbols.Concat(methodInvokationSymbols));

                    allSymbols = allSymbols.Where(x => x != null).Distinct().ToList();

                    descendants = allSymbols.SelectMany(nod => nod.DeclaringSyntaxReferences.SelectMany(x => x.GetSyntax().DescendantNodes())).ToArray();
                }

                var fieldsToSetup = GetFieldsToSetup(testSemanticModel, allSymbols, suitableSut);

                if(IsNotExpressionNeedsToMock(fieldsToSetup, expression))
                    continue;

                semanticContext.ReportDiagnostic(Diagnostic.Create(Rule, expression.Parent.GetLocation()));
            }
        }

        private static IReadOnlyCollection<string> GetFieldsToSetup(SemanticModel semanticModel, IEnumerable<IMethodSymbol> methodSymbols, SutInfo suitableSut)
        {
            return methodSymbols.SelectMany(x =>  suitableSut.InjectedFields.Find(IsCorrespondingField(semanticModel, x))
                                                             .SelectMany(z => z.Data.Field.Declaration.Variables.Select(f => f.Identifier.ValueText))
                                                             .ToArray())
                                .ToArray();
        }

        private static bool IsNotExpressionNeedsToMock(IReadOnlyCollection<string> mocksInvokations, SyntaxNode expression)
        {
            return mocksInvokations.Count == 0
                    || expression.Parents(n => n is BlockSyntax)
                                  ?.DescendantNodes()
                                  .OfType<InvocationExpressionSyntax>()
                                  .Select(x => x.ToString())
                                  .Any(x => mocksInvokations.Any(e => Regex.IsMatch(x, e + @"\s*\.Setup"))) == true;
        }

        private static Func<TreeNode<DependencyField>, bool> IsCorrespondingField(SemanticModel semanticModel, IMethodSymbol x)
        {
            return z => (z.Data.Field.Declaration.Type as GenericNameSyntax)?.TypeArgumentList
                                                                  .Arguments
                                                                  .Any(a => IsCorrespondingType(semanticModel, x, a, z)) ?? false;
        }

        //todo: replace corresponding by type to corresponding by chain of calls
        private static bool IsCorrespondingType(SemanticModel semanticModel, IMethodSymbol x, TypeSyntax typeSyntax, TreeNode<DependencyField> field)
        {
            var fieldTree = typeSyntax.SyntaxTree;
            var fieldSemanticModel = fieldTree.GetModelFromSyntaxTree(semanticModel.Compilation);
            var symbolInfo = fieldSemanticModel.GetSymbolInfo(typeSyntax);

            return (symbolInfo.Symbol.ToString() == x.ReceiverType.ToString() 
                        || (symbolInfo.Symbol as INamedTypeSymbol)?.ConstructedFrom.ToString() == x.ReceiverType.ToString()
                        || (symbolInfo.Symbol as INamedTypeSymbol)?.ConstructedFrom.ToString() == (x.ReceiverType as INamedTypeSymbol)?.ConstructedFrom.ToString()) 
                        && !field.FindChildTreeNodes(y => IsCorrespondingField(semanticModel, x)(y)).Any()
                        ||
                   (symbolInfo.Symbol.ToString() == x.ReturnType.ToString()
                        || (symbolInfo.Symbol as INamedTypeSymbol)?.ConstructedFrom.ToString() == x.ReturnType.ToString()
                        || (symbolInfo.Symbol as INamedTypeSymbol)?.ConstructedFrom.ToString() == (x.ReturnType as INamedTypeSymbol)?.ConstructedFrom.ToString());
        }
    }
}