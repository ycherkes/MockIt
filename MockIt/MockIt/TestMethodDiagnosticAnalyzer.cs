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

        private static void AnalyzeSemantic(SemanticModelAnalysisContext semanticContext)
        {
            var semanticModel = semanticContext.SemanticModel;
            var methodDecls = TestSemanticHelper.GetTestMethods(semanticModel);

            var methods = TestSemanticHelper.GetMethodsToConfigureMocks(methodDecls);

            var properties = TestSemanticHelper.GetPropertiesToConfigureMocks(methodDecls, methods);

            var expressions = methods.Concat(properties).ToArray();

            if (!expressions.Any())
                return;

            var testInitMethodDecl = TestSemanticHelper.GetTestInitializeMethod(semanticModel);

            if (testInitMethodDecl == null) return;

            var declaredFields = testInitMethodDecl.Parent.ChildNodes().OfType<FieldDeclarationSyntax>();

            var suts = testInitMethodDecl.GetSuts(semanticModel, declaredFields);

            foreach (var expression in expressions)
            {
                var symbol = semanticModel.GetSymbolInfo(expression).Symbol;

                if(symbol == null)
                    continue;

                var refType = symbol.OriginalDefinition.ContainingType;

                var suitableSut = refType.GetSuitableSut(suts);

                if (suitableSut == null)
                    continue;

                var suitableSutMember = suitableSut.GetSuitableSutMember(symbol);

                var sourceTree = suitableSutMember.Locations.First().SourceTree;
                var treeRoot = sourceTree.GetRoot();
                var position = suitableSutMember.Locations.First().SourceSpan.Start;
                var parentNode = treeRoot.FindToken(position).Parent;
                var node = parentNode.FirstAncestorOrSelf<MethodDeclarationSyntax>() as MemberDeclarationSyntax 
                            ?? parentNode.FirstAncestorOrSelf<PropertyDeclarationSyntax>();

                var compilation = TestSemanticHelper.GetCompilation(suitableSutMember, semanticModel);

                if(compilation == null) return;

                var model = compilation.GetSemanticModel(sourceTree);

                var descendants = node.DescendantNodes().ToArray();

                var propertyGetSetSymbols = descendants.OfType<MemberAccessExpressionSyntax>()
                                                       .SelectMany(x =>
                                                       {
                                                           var symb = model.GetSymbolInfo(x).Symbol as IPropertySymbol;
                                                           return symb != null 
                                                                    ? new[] { symb.GetMethod, symb.SetMethod } 
                                                                    : Enumerable.Empty<IMethodSymbol>();
                                                       });

                var methodInvokationSymbols = descendants.OfType<InvocationExpressionSyntax>()
                                                         .Select(x => (IMethodSymbol)model.GetSymbolInfo(x.Expression).Symbol);

                var allSymbols = propertyGetSetSymbols.Concat(methodInvokationSymbols);

                var fieldsToSetup = GetFieldsToSetup(semanticModel, allSymbols, suitableSut);

                if(IsNotExpresisonNeedsToMock(fieldsToSetup, expression))
                    continue;

                semanticContext.ReportDiagnostic(Diagnostic.Create(Rule, expression.Parent.GetLocation()));
            }
        }

        private static IEnumerable<string> GetFieldsToSetup(SemanticModel semanticModel, IEnumerable<IMethodSymbol> methodSymbols, SutInfo suitableSut)
        {
            return methodSymbols.SelectMany(x =>  suitableSut.DeclaredFields
                                                             .Where(IsCorrespondingField(semanticModel, x))
                                                             .SelectMany(z => z.Declaration.Variables.Select(f => f.Identifier.ValueText))
                                                             .ToArray())
                                .ToArray();
        }

        private static bool IsNotExpresisonNeedsToMock(IEnumerable<string> mocksInvokations, SyntaxNode expression)
        {
            return expression.Parents(n => n is BlockSyntax)
                             ?.DescendantNodes()
                             .OfType<InvocationExpressionSyntax>()
                             .Select(x => x.ToString())
                             .Any(x => mocksInvokations.Any(e => x.StartsWith(e + ".Setup"))) == true;
        }

        private static Func<FieldDeclarationSyntax, bool> IsCorrespondingField(SemanticModel semanticModel, IMethodSymbol x)
        {
            return z => (z.Declaration.Type as GenericNameSyntax)?.TypeArgumentList
                                                                  .Arguments
                                                                  .Any(y => semanticModel.GetSymbolInfo(y).Symbol.ToString() == x.ReceiverType.ToString() 
                                                                            || (semanticModel.GetSymbolInfo(y).Symbol as INamedTypeSymbol)?.ConstructedFrom.ToString() == x.ReceiverType.ToString()) 
                    ?? false;
        }
    }
}