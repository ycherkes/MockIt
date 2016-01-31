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
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

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

        private static void AnalyzeSemantic(SemanticModelAnalysisContext obj)
        {
            var methodDecls = TestSemanticHelper.GetTestMethods(obj.SemanticModel);

            var methods = TestSemanticHelper.GetMethodsToConfigureMocks(methodDecls);

            var properties = TestSemanticHelper.GetPropertiesToConfigureMocks(methodDecls, methods);

            var expressions = methods.Concat(properties).ToArray();

            if (!expressions.Any())
                return;

            var testInitMethodDecl = TestSemanticHelper.GetTestInitializeMethod(obj.SemanticModel);

            if (testInitMethodDecl == null)
                return;

            var declaredFields = testInitMethodDecl.Parent.ChildNodes().OfType<FieldDeclarationSyntax>();

            var suts = testInitMethodDecl.DescendantNodes()
                .OfType<ObjectCreationExpressionSyntax>()
                .Select(x => new
                {
                    SymbolInfo = obj.SemanticModel.GetSymbolInfo(x.Type),
                    DeclaredFields = declaredFields.Where(z => x.ArgumentList.Arguments.Any(y => y.Expression.GetText().ToString() == z.Declaration.Variables.FirstOrDefault()?.Identifier.Text + ".Object")).ToArray()
                })
                .Where(x => x.DeclaredFields.Any())
                .ToArray();

            foreach (var expression in expressions)
            {
                var symbol = obj.SemanticModel.GetSymbolInfo(expression).Symbol;

                if(symbol == null)
                    continue;

                var refType = symbol.OriginalDefinition.ContainingType;

                var suitableSut =
                    suts.FirstOrDefault(
                        x =>
                            x.SymbolInfo.Symbol is INamedTypeSymbol &&
                            ((INamedTypeSymbol) x.SymbolInfo.Symbol == refType ||
                             ((INamedTypeSymbol) x.SymbolInfo.Symbol).ConstructedFrom == refType) ||
                            ((INamedTypeSymbol) x.SymbolInfo.Symbol).AllInterfaces.Any(
                                y => y == refType || y.ConstructedFrom == refType));

                if (suitableSut == null)
                    continue;

                var suitableSutMember = ((INamedTypeSymbol)suitableSut.SymbolInfo.Symbol).FindImplementationForInterfaceMember(symbol);

                //for parameterized generic method
                if (suitableSutMember == null)
                {
                    var s1 = symbol as IMethodSymbol;
                    if(s1?.ConstructedFrom != null)
                        suitableSutMember =
                        ((INamedTypeSymbol) suitableSut.SymbolInfo.Symbol).FindImplementationForInterfaceMember(s1.ConstructedFrom);
                }

                if (suitableSutMember == null) suitableSutMember = symbol;

                var sourceTree = suitableSutMember.Locations.First().SourceTree;
                var treeRoot = sourceTree.GetRoot();
                var position = suitableSutMember.Locations.First().SourceSpan.Start;
                var node = treeRoot.FindToken(position).Parent.FirstAncestorOrSelf<MethodDeclarationSyntax>() as MemberDeclarationSyntax ?? treeRoot.FindToken(position).Parent.FirstAncestorOrSelf<PropertyDeclarationSyntax>();

                var compilation = symbol.ContainingAssembly.Name == obj.SemanticModel.Compilation.Assembly.Name
                            ? obj.SemanticModel.Compilation
                            : obj.SemanticModel.Compilation.ExternalReferences
                                .OfType<CompilationReference>()
                                .Select(x => x.Compilation)
                                .FirstOrDefault(x => x.Assembly.Name == symbol.ContainingAssembly.Name);
                var model = compilation.GetSemanticModel(sourceTree);


                var invokedMethodsOfMocks = node.DescendantNodes().OfType<MemberAccessExpressionSyntax>().SelectMany(
                    x =>
                    {
                        var symb = model.GetSymbolInfo(x).Symbol as IPropertySymbol;
                        return symb != null ? new[] {symb.GetMethod, symb.SetMethod} : Enumerable.Empty<IMethodSymbol>();
                    }).Concat( node.DescendantNodes()
                    .OfType<InvocationExpressionSyntax>()
                    .Select(x => (IMethodSymbol)model.GetSymbolInfo(x.Expression).Symbol))
                    .Where(x => x != null)
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
                                                                                      .Any(y => obj.SemanticModel.GetSymbolInfo(y).Symbol.ToString() == x.ReceiverType.ToString() || (obj.SemanticModel.GetSymbolInfo(y).Symbol as INamedTypeSymbol)?.ConstructedFrom.ToString() == x.ReceiverType.ToString()) 
                                                                                     ?? false)
                                        .SelectMany(z => z.Declaration.Variables.Select(f => f.Identifier.ValueText))
                                        .ToArray()
                            })
                    .Where(x => x.FieldsToSetup.Any())
                    .ToArray();
                


                if (invokedMethodsOfMocks.Length == 0 || Parents(expression, n => n is BlockSyntax)?.DescendantNodes()
                                                               .OfType<InvocationExpressionSyntax>()
                                                               .Select(x => x.ToString())
                                                               .Any(x => invokedMethodsOfMocks.SelectMany(y => y.FieldsToSetup)
                                                                                              .Any(e => x.StartsWith(e + ".Setup"))) == true)
                {
                    continue;
                }

                obj.ReportDiagnostic(Diagnostic.Create(Rule, expression.Parent.GetLocation()));
            }
        }

        public static SyntaxNode Parents(SyntaxNode node, Func<SyntaxNode, bool> criteria)
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
    }
}