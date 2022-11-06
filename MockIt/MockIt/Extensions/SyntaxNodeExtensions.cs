using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace MockIt.Extensions
{
    internal static class SyntaxNodeExtensions
    {
        public static bool IsLeftSideOfAssignExpression(this SyntaxNode node)
        {
            return node.IsParentKind(SyntaxKind.SimpleAssignmentExpression) && ((AssignmentExpressionSyntax)node.Parent)?.Left == node;
        }

        private static bool IsParentKind(this SyntaxNode node, SyntaxKind kind)
        {
            return node?.Parent?.IsKind(kind) == true;
        }
    }
}
