using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Linq;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace MockIt.Syntax
{
    internal static class SyntaxHelper
    {
        public static AssignmentExpressionSyntax SimpleAssignTo(this ExpressionSyntax leftSide, ExpressionSyntax rightSide)
        {
            return AssignmentExpression(SyntaxKind.SimpleAssignmentExpression,
                                        leftSide,
                                        rightSide);
        }

        public static MemberAccessExpressionSyntax MemberAccess(this ExpressionSyntax owner, string memberName)
        {
            return MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, owner, IdentifierName(memberName));
        }

        public static InvocationExpressionSyntax Invoke(this ExpressionSyntax owner, string memberName)
        {
            return InvocationExpression(owner.MemberAccess(memberName));
        }

        public static InvocationExpressionSyntax InvokeGeneric(this ExpressionSyntax owner, string genericName, params TypeSyntax[] typeArguments)
        {
            var generic = Identifier(genericName).AsGeneric().WithTypeArgumentList(TypeArgumentList(SeparatedList(typeArguments)));

            return InvocationExpression(MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, owner, generic));
        }

        public static GenericNameSyntax AsGeneric(this SyntaxToken identifier)
        {
            return GenericName(identifier);
        }

        public static VariableDeclaratorSyntax AsVariableDeclarator(this SyntaxToken identifier)
        {
            return VariableDeclarator(identifier);
        }

        public static VariableDeclarationSyntax WithVariable(this VariableDeclarationSyntax declaration, VariableDeclaratorSyntax variable)
        {
            return declaration.WithVariables(SingletonSeparatedList(variable));
        }

        public static LocalDeclarationStatementSyntax AsLocalDeclarationStatement(this VariableDeclarationSyntax declaration)
        {
            return LocalDeclarationStatement(declaration);
        }

        public static ArgumentListSyntax AsArgumentList(this IEnumerable<SyntaxNodeOrToken> arguments)
        {
            return ArgumentList(SeparatedList<ArgumentSyntax>(arguments));
        }

        public static FieldDeclarationSyntax AsFieldDeclaration(this VariableDeclarationSyntax declaration)
        {
            return FieldDeclaration(declaration);
        }

        public static ArgumentSyntax AsArgument(this ExpressionSyntax owner)
        {
            return Argument(owner);
        }

        public static ObjectCreationExpressionSyntax AsObjectCreationExpression(this TypeSyntax typeSyntax)
        {
            return ObjectCreationExpression(typeSyntax);
        }
        public static ObjectCreationExpressionSyntax AsObjectCreationExpressionWithoutArguments(this TypeSyntax typeSyntax)
        {
            return ObjectCreationExpression(typeSyntax).WithArgumentList(ArgumentList());
        }

        public static ExpressionStatementSyntax AsExpressionStatement(this ExpressionSyntax expressionSyntax)
        {
            return ExpressionStatement(expressionSyntax);
        }
        public static VariableDeclarationSyntax AsVariableDeclaration(this TypeSyntax typeSyntax)
        {
            return VariableDeclaration(typeSyntax);
        }

        public static InvocationExpressionSyntax WithArguments(this InvocationExpressionSyntax owner, params ExpressionSyntax[] arguments)
        {
            return owner.WithArgumentList(ArgumentList(SeparatedList(arguments?.Select(Argument) ?? Array.Empty<ArgumentSyntax>())));
        }


        public static GenericNameSyntax WithTypeArguments(this GenericNameSyntax owner, params string[] typeArguments)
        {
            return owner.WithTypeArgumentList(TypeArgumentList(SeparatedList(typeArguments?.Select(GetTypeSyntax) ?? Array.Empty<TypeSyntax>())));
        }

        public static VariableDeclarationSyntax WithVariable(this VariableDeclarationSyntax owner, string variableName)
        {
            return owner.WithVariables(SingletonSeparatedList(VariableDeclarator(Identifier(variableName))));
        }

        public static GenericNameSyntax WithTypeArgument(this GenericNameSyntax owner, string typeArgument)
        {
            return owner.WithTypeArgumentList(TypeArgumentList(SingletonSeparatedList(GetTypeSyntax(typeArgument))));
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
        public static ExpressionSyntax WithCarriageReturnTrailingTrivia(this ExpressionSyntax expression)
        {
            return expression.WithTrailingTrivia(TriviaList(CarriageReturnLineFeed));
        }

        public static TypeSyntax GetTypeSyntax(string typeIdentifier)
        {
            return ParseTypeName(typeIdentifier);
        }

        public static TypeSyntax VarTypeSyntax()
        {
            return IdentifierName(
                        Identifier(
                            TriviaList(),
                            SyntaxKind.VarKeyword,
                            "var",
                            "var",
                            TriviaList()));
        }
    }
}
