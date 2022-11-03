using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace MockIt
{
    internal static class SyntaxNodeExtensions
    {
        public static bool IsLeftSideOfAssignExpression(this SyntaxNode node)
        {
            return node.IsParentKind(SyntaxKind.SimpleAssignmentExpression) &&
                ((AssignmentExpressionSyntax)node.Parent)?.Left == node;
        }

        private static bool IsParentKind(this SyntaxNode node, SyntaxKind kind)
        {
            return node != null && node.Parent.IsKind(kind);
        }

        public static bool IsKind(this SyntaxNode node, SyntaxKind kind)
        {
            return node?.RawKind == (int)kind;
        }
    }
}
