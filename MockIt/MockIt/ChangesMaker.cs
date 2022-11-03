using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.Formatting;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace MockIt
{
    public static class ChangesMaker
    {
        private static IEnumerable<ExpressionStatementSyntax> GetVerifiers(IEnumerable<Fields> invokedMethodsOfMocks)
        {
            var verifiers = invokedMethodsOfMocks.SelectMany(x => x.FieldsToSetup.SelectMany(y => y.Field))
                                                 .Distinct()
                                                 .Select(x => SyntaxFactory.ExpressionStatement(SyntaxFactory.IdentifierName(x).Invoke("VerifyAll")))
                                                 .ToArray();

            return verifiers;
        }

        private static ExpressionStatementSyntax[] GetSetups(IEnumerable<Fields> invokedMethodsOfMocks,
            bool withCallBack)
        {
            var setups = invokedMethodsOfMocks.SelectMany(x => x.FieldsToSetup
                .SelectMany(y => y.Field.Select(f => GetSetups(f, x, y, withCallBack))))
                .SelectMany(x => x)
                //.DistinctBy(x => x.ToFullString())
                .DistinctBy(x => x, (st, nd) => SyntaxFactory.AreEquivalent(st, nd, false))
                .Select(SyntaxFactory.ExpressionStatement)
                .ToArray();

            return setups;
        }

        private static IReadOnlyCollection<ExpressionSyntax> GetSetups(string identifier, Fields fields, FieldsSetups fieldsSetups, bool withCallBack)
        {
            if (fields.MethodOrPropertySymbol is IMethodSymbol methodSymbol)
            {
                //todo: determine the generic replacements correctly by semantic model from sut

                var returnType = GetReplacedType(methodSymbol.ReturnType, fieldsSetups.Substitutions, fieldsSetups.SutSubstitutions);
                bool isTaskReturnType = returnType == "Task";
                bool isTaskResultReturnType = !isTaskReturnType && returnType.StartsWith("Task<");
                var returnTaskTypeArgument = isTaskResultReturnType ? GetReplacedType(((INamedTypeSymbol)methodSymbol.ReturnType).TypeArguments.First(), fieldsSetups.Substitutions, fieldsSetups.SutSubstitutions) : "";

                var setupMethod = GetMethod(methodSymbol, fieldsSetups.Substitutions, fieldsSetups.SutSubstitutions);

                var lambdaArgument = SyntaxFactory.IdentifierName("x");

                var setupBody = setupMethod.typeArguments.Length == 0
                    ? lambdaArgument.Invoke(setupMethod.name)
                    : lambdaArgument.InvokeGeneric(setupMethod.name, setupMethod.typeArguments.Select(z => SyntaxHelper.GetTypeSyntax(GetReplacedType(z, fieldsSetups.Substitutions, fieldsSetups.SutSubstitutions))).ToArray());

                var itIdentifier = SyntaxFactory.IdentifierName("It");

                var setupArguments = withCallBack
                    ? methodSymbol.Parameters.Select(z => (ExpressionSyntax)itIdentifier.InvokeGeneric("IsAny", SyntaxHelper.GetTypeSyntax(GetReplacedType(z.Type, fieldsSetups.Substitutions, fieldsSetups.SutSubstitutions))))
                                             .ToArray()
                    : methodSymbol.Parameters.Select(z => (ExpressionSyntax)itIdentifier.InvokeGeneric("Is", SyntaxHelper.GetTypeSyntax(GetReplacedType(z.Type, fieldsSetups.Substitutions, fieldsSetups.SutSubstitutions)))
                                                                                        .WithArguments(SyntaxHelper.SimpleLambdaExpression(z.Name)
                                                                                                                       .WithExpressionBody(SyntaxHelper.EqualsDefaultExpression(z.Name))))
                                             .ToArray();

                var setupLambda = SyntaxHelper.SimpleLambdaExpression("x")
                    .WithExpressionBody(setupBody.WithArguments(setupArguments));

                var expression = SyntaxFactory.IdentifierName(identifier)
                    .Invoke("Setup")
                    .WithArgument(setupLambda);

                if (withCallBack && methodSymbol.Parameters.Length > 0)
                {
                    var lambda = (methodSymbol.Parameters.Length == 1
                        ? SyntaxHelper.SimpleLambdaExpression(methodSymbol.Parameters[0].Name)
                        : SyntaxHelper.ParenthesizedLambdaExpression(methodSymbol.Parameters.Select(lp => lp.Name)))
                        .WithBlock(SyntaxFactory.Block());

                    expression = expression.WithTrailingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.CarriageReturnLineFeed))
                        .InvokeGeneric("Callback", methodSymbol.Parameters.Select(z => SyntaxHelper.GetTypeSyntax(GetReplacedType(z.Type, fieldsSetups.Substitutions, fieldsSetups.SutSubstitutions))).ToArray())
                        .WithArgument(lambda);
                }

                if (methodSymbol.ReturnsVoid)
                {
                    return new ExpressionSyntax[] { expression };
                }

                if (isTaskResultReturnType)
                {
                    expression = expression.WithTrailingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.CarriageReturnLineFeed)).Invoke("ReturnsAsync")
                                           .WithArgument(SyntaxFactory.DefaultExpression(SyntaxHelper.GetTypeSyntax(returnTaskTypeArgument)));
                }
                else if (isTaskReturnType)
                {
                    // ".Returns(Task.CompletedTask)" 
                    expression = expression.WithTrailingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.CarriageReturnLineFeed)).Invoke("Returns")
                                           .WithArgument(SyntaxFactory.IdentifierName("Task").Member("CompletedTask"));
                }
                else
                {
                    // ".Returns(default(" + returnType + "))"
                    expression = expression.WithTrailingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.CarriageReturnLineFeed)).Invoke("Returns")
                                           .WithArgument(SyntaxFactory.DefaultExpression(SyntaxHelper.GetTypeSyntax(returnType)));
                }

                return new ExpressionSyntax[] { expression };
            }

            var propertySymbol = (IPropertySymbol)fields.MethodOrPropertySymbol;

            var expressions = new List<ExpressionSyntax>();
            var defaultType = SyntaxFactory.DefaultExpression(SyntaxHelper.GetTypeSyntax(GetReplacedType(propertySymbol.Type, fieldsSetups.Substitutions, fieldsSetups.SutSubstitutions)));

            if (!propertySymbol.IsWriteOnly && !fields.Expression.IsLeftSideOfAssignExpression())
            {
                var setupBody = SyntaxFactory.IdentifierName("x").Member(propertySymbol.Name);

                var setupLambda = SyntaxHelper.SimpleLambdaExpression("x")
                    .WithExpressionBody(setupBody);

                var getExpression = SyntaxFactory.IdentifierName(identifier)
                    .Invoke("SetupGet")
                    .WithArgument(setupLambda)
                    .WithTrailingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.CarriageReturnLineFeed))
                    .Invoke("Returns")
                    .WithArgument(defaultType);

                expressions.Add(getExpression);
            }

            if (propertySymbol.IsReadOnly || !fields.Expression.IsLeftSideOfAssignExpression()) return expressions;

            var setupSetBody = SyntaxFactory.IdentifierName("x")
                .Member(identifier)
                .SimpleAssignment(defaultType);

            var setupSetLambda = SyntaxHelper.SimpleLambdaExpression("x")
                .WithExpressionBody(setupSetBody);

            var setExpression = SyntaxFactory.IdentifierName(identifier)
                    .Invoke("SetupSet")
                    .WithArgument(setupSetLambda);

            expressions.Add(setExpression);

            return expressions;
        }

        private static string GetReplacedType(ITypeSymbol typeSymbol,
            Dictionary<string, ITypeSymbol> substitutions,
            Dictionary<string, ITypeSymbol> sutSubstitutions)
        {
            var type = GetActualType(substitutions, sutSubstitutions, typeSymbol).GetSimpleTypeName();

            if (typeSymbol.SpecialType == SpecialType.System_Void || (typeSymbol as INamedTypeSymbol)?.IsGenericType != true)
                return type;

            var symbolDefinitionsReplacement = TestSemanticHelper.GetReplacedDefinitions(sutSubstitutions, typeSymbol);

            if (symbolDefinitionsReplacement.Count > 0)
            {
                type = (symbolDefinitionsReplacement.FirstOrDefault(z => z.IsReplaced) ?? symbolDefinitionsReplacement.First()).Result;
            }

            return type;
        }

        private static (string name, ITypeSymbol[] typeArguments) GetMethod(IMethodSymbol methodSymbol,
            IReadOnlyDictionary<string, ITypeSymbol> substitutions,
            IReadOnlyDictionary<string, ITypeSymbol> sutSubstitutions)
        {
            if (!methodSymbol.IsGenericMethod)
                return (methodSymbol.Name, Array.Empty<ITypeSymbol>());

            return (methodSymbol.Name, methodSymbol.TypeArguments.Select(x => GetActualType(substitutions, sutSubstitutions, x)).ToArray());
        }

        private static ITypeSymbol GetActualType(
            IReadOnlyDictionary<string, ITypeSymbol> substitutions,
            IReadOnlyDictionary<string, ITypeSymbol> sutSubstitutions,
            ITypeSymbol typeSymbol)
        {

            var typeName = typeSymbol.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);

            return substitutions.TryGetValue(typeName, out var substitution)
                ? substitution
                : sutSubstitutions.TryGetValue(typeName, out var sutSubstitution)
                    ? sutSubstitution
                    : typeSymbol;
        }

        public static void ApplyChanges(SyntaxNode invocationSyntax,
            SyntaxEditor editor,
            IReadOnlyCollection<Fields> invokedMethodsOfMocks, bool withCallBack)
        {
            var setups = GetSetups(invokedMethodsOfMocks, withCallBack);
            var verifiers = GetVerifiers(invokedMethodsOfMocks);

            foreach (var setup in setups.Reverse())
            {
                editor.InsertBefore(invocationSyntax,
                    setup.WithTrailingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.CarriageReturnLineFeed, SyntaxFactory.CarriageReturnLineFeed))
                         .WithAdditionalAnnotations(Formatter.Annotation));
            }

            foreach (var verifier in verifiers)
            {
                editor.InsertAfter(invocationSyntax,
                    verifier.WithLeadingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.CarriageReturnLineFeed))
                            .WithAdditionalAnnotations(Formatter.Annotation));
            }
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