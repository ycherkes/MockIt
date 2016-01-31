using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace MockIt
{
    public static class TestSemanticHelper
    {
        public static MethodDeclarationSyntax GetTestInitializeMethod(SemanticModel semanticModel)
        {
            var methodDecl = semanticModel
                .SyntaxTree
                .GetRoot()
                .DescendantNodes()
                .OfType<MethodDeclarationSyntax>()
                .FirstOrDefault(x => x.AttributeLists
                    .Any(y => y.Attributes
                        .Any(z => new[] {"SetUp", "TestInitialize"}.Contains(((IdentifierNameSyntax) z.Name).Identifier.Text))));

            return methodDecl;
        }

        public static IEnumerable<MethodDeclarationSyntax> GetTestMethod(SemanticModel semanticModel)
        {
            var methodDecl = semanticModel
                .SyntaxTree
                .GetRoot()
                .DescendantNodes()
                .OfType<MethodDeclarationSyntax>()
                .Where(x => x.AttributeLists
                    .Any(y => y.Attributes
                        .Any(z => new[] { "Test", "TestMethod" }.Contains(((IdentifierNameSyntax)z.Name).Identifier.Text))));
            return methodDecl;
        }

        public static IEnumerable<MemberAccessExpressionSyntax> GetPropertiesToConfigureMocks(SyntaxNode node,
            IEnumerable<ExpressionSyntax> methods)
        {
            var properties = node.DescendantNodes()
                .OfType<MemberAccessExpressionSyntax>()
                .Where(expr => !expr.DescendantNodes(methods.Contains).Any())
                .ToArray();

            return properties;
        }

        public static ExpressionSyntax[] GetMethodsToConfigureMocks(SyntaxNode node)
        {
            var methods = node.DescendantNodes()
                .OfType<InvocationExpressionSyntax>()
                .Select(expr => expr.Expression).ToArray();

            return methods;
        }
    }
}