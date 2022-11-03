using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Linq;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace MockIt
{
    internal static class SyntaxHelper
    {
        public static AssignmentExpressionSyntax SimpleAssignment(this ExpressionSyntax leftSide, ExpressionSyntax rightSide)
        {
            return AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                                        leftSide,
                                        rightSide);
        }

        public static MemberAccessExpressionSyntax Member(this ExpressionSyntax owner, string memberName)
        {
            return MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, owner, IdentifierName(memberName));
        }

        public static InvocationExpressionSyntax Invoke(this ExpressionSyntax owner, string memberName)
        {
            return InvocationExpression(owner.Member(memberName));
        }

        public static InvocationExpressionSyntax InvokeGeneric(this ExpressionSyntax owner, string genericName, params TypeSyntax[] typeArguments)
        {
            var generic = GenericName(Identifier(genericName)).WithTypeArgumentList(TypeArgumentList(SeparatedList(typeArguments)));

            return InvocationExpression(MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, owner, generic));
        }

        public static InvocationExpressionSyntax WithArguments(this InvocationExpressionSyntax owner, params ExpressionSyntax[] arguments)
        {
            return owner.WithArgumentList(ArgumentList(SeparatedList(arguments?.Select(Argument) ?? Array.Empty<ArgumentSyntax>())));
        }

        public static InvocationExpressionSyntax WithArgument(this InvocationExpressionSyntax owner, ExpressionSyntax argument)
        {
            return owner.WithArgumentList(ArgumentList(SingletonSeparatedList(Argument(argument))));
        }

        public static LambdaExpressionSyntax SimpleLambdaExpression(string identifier)
        {
            return SyntaxFactory.SimpleLambdaExpression(Parameter(Identifier(identifier)));
        }

        public static LambdaExpressionSyntax ParenthesizedLambdaExpression(IEnumerable<string> identifiers)
        {
            return SyntaxFactory.ParenthesizedLambdaExpression()
                                .WithParameterList(ParameterList(SeparatedList(identifiers.Select(i => Parameter(Identifier(i))))));
        }

        public static ExpressionSyntax EqualsDefaultExpression(string identifier)
        {
            return BinaryExpression(SyntaxKind.EqualsExpression,
                                    IdentifierName(identifier),
                                    LiteralExpression(SyntaxKind.DefaultLiteralExpression,
                                                      Token(SyntaxKind.DefaultKeyword)));
        }

        public static TypeSyntax GetTypeSyntax(string typeIdentifier)
        {
            return ParseTypeName(typeIdentifier);
        }
    }
}
