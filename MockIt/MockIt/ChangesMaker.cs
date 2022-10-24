using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using static System.String;

namespace MockIt
{
    public static class ChangesMaker
    {
        private static IEnumerable<ExpressionStatementSyntax> MakeVerifiers(IEnumerable<Fields> invokedMethodsOfMocks)
        {
            var verifiers = invokedMethodsOfMocks.SelectMany(x => x.FieldsToSetup
                                                                   .SelectMany(y => y.Field))
                                                 .Distinct()
                                                 .Select(x => SyntaxFactory.ExpressionStatement(SyntaxFactory.ParseExpression(x + ".VerifyAll()")))
                                                 .ToArray();

            return verifiers;
        }

        private static ExpressionStatementSyntax[] MakeSetups(IEnumerable<Fields> invokedMethodsOfMocks,
            bool withCallBack)
        {
            var setups = invokedMethodsOfMocks.SelectMany(x => x.FieldsToSetup
                .SelectMany(y => y.Field.Select(f => GetSetups(f, x, y, withCallBack))))
                .SelectMany(x => x)
                .Distinct()
                .Select(x => SyntaxFactory.ExpressionStatement(SyntaxFactory.ParseExpression(x)))
                .ToArray();

            return setups;
        }

        //private static ExpressionSyntax GetSetups1(string f, Fields x, FieldsSetups y)
        //{
        //    if (x.MethodOrPropertySymbol is IMethodSymbol methodSymbol)
        //    {
        //        //todo: determine the generic replacements correctly by semantic model from sut

        //        var returnType = GetReplacedType(methodSymbol.ReturnType, y.Substitutions, y.SutSubstitutions);
        //        bool isTaskReturnType = returnType == "Task";
        //        bool isTaskResultReturnType = !isTaskReturnType && returnType.StartsWith("Task<");
        //        var returnTaskTypeArgument = isTaskResultReturnType ? GetReplacedType(((INamedTypeSymbol)methodSymbol.ReturnType).TypeArguments.First(), y.Substitutions, y.SutSubstitutions) : "";

        //        //return new[]
        //        //{

        //        //};

        //        //return new[]
        //        //{
        //        //    f + ".#ToReplace#(x => x." +
        //        //    GetMethodName(methodSymbol, y.Substitutions, y.SutSubstitutions) + "(" +
        //        //    Join(", ", methodSymbol.Parameters.Select(z => "It.Is<" +
        //        //                                                          GetReplacedType(z.Type, y.Substitutions,
        //        //                                                              y.SutSubstitutions) +
        //        //                                                          ">(" + z.Name + " => " + z.Name +
        //        //                                                          " == default(" +
        //        //                                                          GetReplacedType(z.Type, y.Substitutions,
        //        //                                                              y.SutSubstitutions) +
        //        //                                                          "))")) + "))" +
        //        //    (methodSymbol.ReturnType.ToDisplayString() != "void" //todo: to right determine generic return type
        //        //        ? ".Returns(default(" + returnType + "))"
        //        //        : "")
        //        //};
        //        var expr = SyntaxFactory.InvocationExpression(
        //            SyntaxFactory.MemberAccessExpression(
        //                SyntaxKind.SimpleMemberAccessExpression,
        //                SyntaxFactory.InvocationExpression(
        //                    SyntaxFactory.MemberAccessExpression(
        //                        SyntaxKind.SimpleMemberAccessExpression,
        //                        SyntaxFactory.InvocationExpression(
        //                            SyntaxFactory.MemberAccessExpression(
        //                                SyntaxKind.SimpleMemberAccessExpression,
        //                                SyntaxFactory.IdentifierName("subServiceMock"),
        //                                SyntaxFactory.IdentifierName("Setup")))
        //                        .WithArgumentList(
        //                            SyntaxFactory.ArgumentList(
        //                                SyntaxFactory.SingletonSeparatedList(
        //                                    SyntaxFactory.Argument(
        //                                        SyntaxFactory.SimpleLambdaExpression(
        //                                            SyntaxFactory.Parameter(
        //                                                SyntaxFactory.Identifier("x")))
        //                                        .WithExpressionBody(
        //                                            SyntaxFactory.InvocationExpression(
        //                                                SyntaxFactory.MemberAccessExpression(
        //                                                    SyntaxKind.SimpleMemberAccessExpression,
        //                                                    SyntaxFactory.IdentifierName("x"),
        //                                                    SyntaxFactory.IdentifierName("DoSubSomething")))
        //                                            .WithArgumentList(
        //                                                SyntaxFactory.ArgumentList(
        //                                                    SyntaxFactory.SingletonSeparatedList(
        //                                                        SyntaxFactory.Argument(
        //                                                            SyntaxFactory.InvocationExpression(
        //                                                                SyntaxFactory.MemberAccessExpression(
        //                                                                    SyntaxKind.SimpleMemberAccessExpression,
        //                                                                    SyntaxFactory.IdentifierName("It"),
        //                                                                    SyntaxFactory.GenericName(
        //                                                                        SyntaxFactory.Identifier("IsAny"))
        //                                                                    .WithTypeArgumentList(
        //                                                                        SyntaxFactory.TypeArgumentList(
        //                                                                            SyntaxFactory.SingletonSeparatedList<TypeSyntax>(
        //                                                                                SyntaxFactory.PredefinedType(
        //                                                                                    SyntaxFactory.Token(SyntaxKind.IntKeyword)))))))))))))))),
        //                        SyntaxFactory.GenericName(
        //                            SyntaxFactory.Identifier("Callback"))
        //                        .WithTypeArgumentList(
        //                            SyntaxFactory.TypeArgumentList(
        //                               SyntaxFactory.SingletonSeparatedList<TypeSyntax>(
        //                                   SyntaxFactory.PredefinedType(
        //                                       SyntaxFactory.Token(SyntaxKind.IntKeyword)))))))
        //                .WithArgumentList(
        //                    SyntaxFactory.ArgumentList(
        //                        SyntaxFactory.SingletonSeparatedList(
        //                            SyntaxFactory.Argument(
        //                                SyntaxFactory.SimpleLambdaExpression(
        //                                   SyntaxFactory.Parameter(
        //                                        SyntaxFactory.Identifier("doInt")))
        //                                .WithBlock(
        //                                    SyntaxFactory.Block()))))),
        //                SyntaxFactory.IdentifierName("ReturnsAsync")))
        //        .WithArgumentList(
        //            SyntaxFactory.ArgumentList(
        //                SyntaxFactory.SingletonSeparatedList(
        //                    SyntaxFactory.Argument(
        //                        SyntaxFactory.DefaultExpression(
        //                            SyntaxFactory.PredefinedType(
        //                                SyntaxFactory.Token(SyntaxKind.BoolKeyword)))))));
        //    }
        //}

        private static IEnumerable<string> GetSetups(string f, Fields x, FieldsSetups y, bool withCallBack)
        {
            if (x.MethodOrPropertySymbol is IMethodSymbol methodSymbol)
            {
                //todo: determine the generic replacements correctly by semantic model from sut

                var returnType = GetReplacedType(methodSymbol.ReturnType, y.Substitutions, y.SutSubstitutions);
                bool isTaskReturnType = returnType == "Task";
                bool isTaskResultReturnType = !isTaskReturnType && returnType.StartsWith("Task<");
                var returnTaskTypeArgument = isTaskResultReturnType ? GetReplacedType(((INamedTypeSymbol)methodSymbol.ReturnType).TypeArguments.First(), y.Substitutions, y.SutSubstitutions) : "";

                if (withCallBack)
                {

                    return new[]
                    {
                        f + ".Setup(x => x." +
                        GetMethodName(methodSymbol, y.Substitutions, y.SutSubstitutions) + "(" +
                        Join(", ", methodSymbol.Parameters.Select(z => "It.IsAny<" +
                                                                              GetReplacedType(z.Type, y.Substitutions,
                                                                                  y.SutSubstitutions) +
                                                                              ">()")) + "))" +
                        (methodSymbol.Parameters.Length > 0 ? ".Callback<" + Join(", ", methodSymbol.Parameters.Select(z => GetReplacedType(z.Type, y.Substitutions, y.SutSubstitutions))) + ">(" + (methodSymbol.Parameters.Length > 1 ? "(" : "") +
                        Join(", ", methodSymbol.Parameters.Select(z => z.Name)) + (methodSymbol.Parameters.Length > 1 ? ")" : "") + " => {})" : "") +
                        (methodSymbol.ReturnType.ToDisplayString() != "void" //todo: to right determine generic return type
                            ? isTaskResultReturnType ? ".ReturnsAsync(default(" + returnTaskTypeArgument + "))" : isTaskReturnType ? ".Returns(Task.CompletedTask)" : ".Returns(default(" + returnType + "))"
                            : "")
                        };
                }

                return new[]
                {
                    f + ".Setup(x => x." +
                    GetMethodName(methodSymbol, y.Substitutions, y.SutSubstitutions) + "(" +
                    Join(", ", methodSymbol.Parameters.Select(z => "It.Is<" +
                                                                   GetReplacedType(z.Type, y.Substitutions,
                                                                       y.SutSubstitutions) +
                                                                   ">(" + z.Name + " => " + z.Name +
                                                                   " == default(" +
                                                                   GetReplacedType(z.Type, y.Substitutions,
                                                                       y.SutSubstitutions) +
                                                                   "))")) + "))" +
                    (methodSymbol.ReturnType.ToDisplayString() != "void" //todo: to right determine generic return type
                        ? isTaskResultReturnType ? ".ReturnsAsync(default(" + returnTaskTypeArgument + "))" : isTaskReturnType ? ".Returns(Task.CompletedTask)" : ".Returns(default(" + returnType + "))"
                        : "")
                };
            }

            var propertySymbol = (IPropertySymbol)x.MethodOrPropertySymbol;

            var expressions = new List<string>();

            if (!propertySymbol.IsWriteOnly && !x.Expression.IsLeftSideOfAssignExpression())
            {
                var getExpression = f + ".SetupGet(x => x." + propertySymbol.Name + ").Returns(default(" +
                                    GetReplacedType(propertySymbol.Type, y.Substitutions, y.SutSubstitutions) + "))";

                expressions.Add(getExpression);
            }

            if (propertySymbol.IsReadOnly || !x.Expression.IsLeftSideOfAssignExpression()) return expressions;

            var setExpression = f + ".SetupSet(x => x." + propertySymbol.Name + " = default(" +
                                GetReplacedType(propertySymbol.Type, y.Substitutions, y.SutSubstitutions) + "))";

            expressions.Add(setExpression);

            return expressions;
        }

        private static string GetReplacedType(ITypeSymbol typeSymbol, Dictionary<string, ITypeSymbol> substitutions,
            IReadOnlyDictionary<string, ITypeSymbol> sutSubstitutions)
        {
            var type = GetSimpleTypeName(substitutions, sutSubstitutions, typeSymbol);

            if (typeSymbol.ToDisplayString() == "void" || (typeSymbol as INamedTypeSymbol)?.IsGenericType != true)
                return type;

            var symbolDefinitionsReplacement = TestSemanticHelper.GetReplacedDefinitions(sutSubstitutions, typeSymbol);

            if (symbolDefinitionsReplacement.Any())
            {
                type = (symbolDefinitionsReplacement.FirstOrDefault(z => z.IsReplaced) ?? symbolDefinitionsReplacement.First()).Result;
            }

            return type;
        }

        private static string GetMethodName(IMethodSymbol methodSymbol,
            IReadOnlyDictionary<string, ITypeSymbol> substitutions,
            IReadOnlyDictionary<string, ITypeSymbol> sutSubstitutions)
        {
            if (!methodSymbol.IsGenericMethod)
                return methodSymbol.Name;

            return methodSymbol.Name + "<" + Join(", ",
                                                    methodSymbol.TypeArguments.Any()
                                                        ? methodSymbol.TypeArguments.Select(x => GetSimpleTypeName(substitutions, sutSubstitutions, x))
                                                        //todo - determine is that necessary or could be removed
                                                        : methodSymbol.TypeParameters.Select(x => GetSimpleTypeName(substitutions, sutSubstitutions, x)))
                  + ">";
        }

        private static string GetSimpleTypeName(
            IReadOnlyDictionary<string, ITypeSymbol> substitutions,
            IReadOnlyDictionary<string, ITypeSymbol> sutSubstitutions,
            ITypeSymbol typeSymbol)
        {
            ITypeSymbol substitution;
            ITypeSymbol sutSubstitution;

            return (substitutions.TryGetValue(typeSymbol.ToString(), out substitution)
                ? substitution
                : sutSubstitutions.TryGetValue(typeSymbol.ToString(), out sutSubstitution)
                    ? sutSubstitution
                    : typeSymbol).GetSimpleTypeName();
        }

        public static void ApplyChanges(SyntaxNode invokationSyntax,
            SyntaxEditor editor,
            IReadOnlyCollection<Fields> invokedMethodsOfMocks, bool withCallBack)
        {
            var setups = MakeSetups(invokedMethodsOfMocks, withCallBack);
            var verifiers = MakeVerifiers(invokedMethodsOfMocks);

            editor.InsertBefore(invokationSyntax,
                setups.Select(
                    (x, i) =>
                        setups.Length - 1 == i
                            ? x.WithLeadingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.ElasticMarker))
                                .WithTrailingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.CarriageReturnLineFeed,
                                    SyntaxFactory.CarriageReturnLineFeed))
                            : x.WithLeadingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.ElasticMarker))));


            //todo will try to understand how to add a new line before
            editor.InsertAfter(invokationSyntax, verifiers.Select(x => x.WithLeadingTrivia(SyntaxFactory.ElasticMarker)));
        }

        public static ConstructorInjections[] MakeConstructorInjections(this IEnumerable<ConstructorParameters> constructorParameters)
        {
            var changes = constructorParameters.Select(x => new ConstructorInjections
            {
                NewField = SyntaxFactory.FieldDeclaration(
                    SyntaxFactory.VariableDeclaration(
                        SyntaxFactory.GenericName(
                            SyntaxFactory.Identifier("Mock"))
                            .WithTypeArgumentList(
                                SyntaxFactory.TypeArgumentList(
                                    SyntaxFactory.SingletonSeparatedList<TypeSyntax>(
                                        SyntaxFactory.IdentifierName(x.TypeName)))))
                        .WithVariables(
                            SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.VariableDeclarator(
                                    SyntaxFactory.Identifier("_" + "mock" + FirstCharToUpper(x.ArgumentName))))))
                    .WithModifiers(
                        SyntaxFactory.TokenList(
                            SyntaxFactory.Token(SyntaxKind.PrivateKeyword),
                            SyntaxFactory.Token(SyntaxKind.ReadOnlyKeyword))),
                NewExpression = SyntaxFactory.ExpressionStatement(
                    SyntaxFactory.AssignmentExpression(
                        SyntaxKind.SimpleAssignmentExpression,
                        SyntaxFactory.IdentifierName("_" + "mock" + FirstCharToUpper(x.ArgumentName)),
                        SyntaxFactory.ObjectCreationExpression(
                            SyntaxFactory.GenericName(
                                SyntaxFactory.Identifier("Mock"))
                                .WithTypeArgumentList(
                                    SyntaxFactory.TypeArgumentList(
                                        SyntaxFactory.SingletonSeparatedList<TypeSyntax>(
                                            SyntaxFactory.IdentifierName(x.TypeName)))))
                            .WithArgumentList(SyntaxFactory.ArgumentList()))),
                CreationArgument = SyntaxFactory.Argument(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName("_" + "mock" + FirstCharToUpper(x.ArgumentName)),
                        SyntaxFactory.IdentifierName(@"Object")))
            }).ToArray();
            return changes;
        }

        private static string FirstCharToUpper(string input)
        {
            return char.ToUpper(input[0]) + input.Substring(1);
        }

        public static async Task<Document> ApplyConstructorInjections(Document document,
            SyntaxNode creation,
            CancellationToken cancellationToken,
            IReadOnlyCollection<ConstructorInjections> changes,
            ObjectCreationExpressionSyntax creationExpressionSyntax)
        {
            var arguments = (from change in changes
                             from comma in new[] { (SyntaxNodeOrToken)SyntaxFactory.Token(SyntaxKind.CommaToken) }
                             select new { change.CreationArgument, comma })
                .SelectMany(x => new[] { x.CreationArgument, x.comma })
                .Take(changes.Count * 2 - 1);

            var editor = await DocumentEditor.CreateAsync(document, cancellationToken).ConfigureAwait(false);

            editor.InsertBefore(creation.Parent.Parent, changes.Select(x => x.NewField));
            editor.InsertBefore(creation,
                changes.Select(
                    (x, i) =>
                        changes.Count - 1 == i
                            ? x.NewExpression.WithTrailingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.CarriageReturnLineFeed,
                                SyntaxFactory.CarriageReturnLineFeed))
                            : x.NewExpression));
            editor.ReplaceNode(creationExpressionSyntax.ArgumentList,
                SyntaxFactory.ArgumentList(SyntaxFactory.SeparatedList<ArgumentSyntax>(arguments)));

            return editor.GetChangedDocument();
        }
    }
}