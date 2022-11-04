using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.Formatting;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
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

                var (name, typeArguments) = GetMethod(methodSymbol, fieldsSetups.Substitutions, fieldsSetups.SutSubstitutions);

                var lambdaArgument = SyntaxFactory.IdentifierName("x");

                var setupBody = typeArguments.Length == 0
                    ? lambdaArgument.Invoke(name)
                    : lambdaArgument.InvokeGeneric(name, typeArguments.Select(z => SyntaxHelper.GetTypeSyntax(GetReplacedType(z, fieldsSetups.Substitutions, fieldsSetups.SutSubstitutions))).ToArray());

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
                    expression = expression.WithTrailingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.CarriageReturnLineFeed))
                                           .Invoke("ReturnsAsync")
                                           .WithArgument(SyntaxFactory.DefaultExpression(SyntaxHelper.GetTypeSyntax(returnTaskTypeArgument)));
                }
                else if (isTaskReturnType)
                {
                    // ".Returns(Task.CompletedTask)" 
                    expression = expression.WithTrailingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.CarriageReturnLineFeed))
                                           .Invoke("Returns")
                                           .WithArgument(SyntaxFactory.IdentifierName("Task")
                                                                      .MemberAccess("CompletedTask"));
                }
                else
                {
                    // ".Returns(default(" + returnType + "))"
                    expression = expression.WithTrailingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.CarriageReturnLineFeed))
                                           .Invoke("Returns")
                                           .WithArgument(SyntaxFactory.DefaultExpression(SyntaxHelper.GetTypeSyntax(returnType)));
                }

                return new ExpressionSyntax[] { expression };
            }

            var propertySymbol = (IPropertySymbol)fields.MethodOrPropertySymbol;

            var expressions = new List<ExpressionSyntax>();
            var defaultType = SyntaxFactory.DefaultExpression(SyntaxHelper.GetTypeSyntax(GetReplacedType(propertySymbol.Type, fieldsSetups.Substitutions, fieldsSetups.SutSubstitutions)));

            if (!propertySymbol.IsWriteOnly && !fields.Expression.IsLeftSideOfAssignExpression())
            {
                var setupBody = SyntaxFactory.IdentifierName("x").MemberAccess(propertySymbol.Name);

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

            if (propertySymbol.IsReadOnly || !fields.Expression.IsLeftSideOfAssignExpression())
                return expressions;

            var setupSetBody = SyntaxFactory.IdentifierName("x")
                                            .MemberAccess(identifier)
                                            .SimpleAssignTo(defaultType);

            var setupSetLambda = SyntaxHelper.SimpleLambdaExpression("x")
                                             .WithExpressionBody(setupSetBody);

            var setExpression = SyntaxFactory.IdentifierName(identifier)
                                             .Invoke("SetupSet")
                                             .WithArgument(setupSetLambda);

            expressions.Add(setExpression);

            return expressions;
        }

        private static string GetReplacedType(ITypeSymbol typeSymbol,
            IImmutableDictionary<string, ITypeSymbol> substitutions,
            IImmutableDictionary<string, ITypeSymbol> sutSubstitutions)
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
            IImmutableDictionary<string, ITypeSymbol> substitutions,
            IImmutableDictionary<string, ITypeSymbol> sutSubstitutions)
        {
            if (!methodSymbol.IsGenericMethod)
                return (methodSymbol.Name, Array.Empty<ITypeSymbol>());

            return (methodSymbol.Name, methodSymbol.TypeArguments.Select(x => GetActualType(substitutions, sutSubstitutions, x)).ToArray());
        }

        private static ITypeSymbol GetActualType(
            IImmutableDictionary<string, ITypeSymbol> substitutions,
            IImmutableDictionary<string, ITypeSymbol> sutSubstitutions,
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

            editor.InsertBefore(invocationSyntax, setups.Select(x => x.WithTrailingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.CarriageReturnLineFeed, SyntaxFactory.CarriageReturnLineFeed))
                         .WithAdditionalAnnotations(Formatter.Annotation)));

            editor.InsertAfter(invocationSyntax, verifiers.Select(x => x.WithLeadingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.CarriageReturnLineFeed))
                            .WithAdditionalAnnotations(Formatter.Annotation)));
        }

        public static ConstructorInjections[] MakeConstructorInjections(
            this IEnumerable<ConstructorParameters> constructorParameters, SutCreationContextType creationContext)
        {
            if (creationContext != SutCreationContextType.Method)
            {
                SyntaxTokenList modifiers = SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PrivateKeyword));

                if (creationContext == SutCreationContextType.Constructor)
                {
                    modifiers = modifiers.Add(SyntaxFactory.Token(SyntaxKind.ReadOnlyKeyword));
                }

                return constructorParameters.Select(x => new ConstructorInjections
                {
                    NewField = SyntaxFactory.Identifier("Mock")
                                            .AsGeneric()
                                            .WithTypeArgument(x.TypeName)
                                            .AsVariableDeclaration()
                                            .WithVariable("_" + "mock" + FirstCharToUpper(x.ArgumentName))
                                            .AsFieldDeclaration()
                                            .WithModifiers(modifiers),

                    NewExpression = SyntaxFactory.IdentifierName("_" + "mock" + FirstCharToUpper(x.ArgumentName))
                                                 .SimpleAssignTo(SyntaxFactory.Identifier("Mock")
                                                                              .AsGeneric()
                                                                              .WithTypeArgument(x.TypeName)
                                                                              .AsObjectCreationExpressionWithoutArguments())
                                                 .AsExpressionStatement(),

                    CreationArgument = SyntaxFactory.IdentifierName("_" + "mock" + FirstCharToUpper(x.ArgumentName))
                                                    .MemberAccess("Object")
                                                    .AsArgument()
                }).ToArray();
            }

            return constructorParameters.Select(x => new ConstructorInjections
            {

                NewExpression = SyntaxHelper.VarTypeSyntax()
                                    .AsVariableDeclaration()
                                    .WithVariable(SyntaxFactory.Identifier("mock" + FirstCharToUpper(x.ArgumentName))
                                                               .AsVariableDeclarator()
                                                               .WithInitializer(
                                                                   SyntaxFactory.EqualsValueClause(
                                                                       SyntaxFactory.Identifier("Mock")
                                                                                    .AsGeneric()
                                                                                    .WithTypeArgument(x.TypeName)
                                                                                    .AsObjectCreationExpressionWithoutArguments())))
                                    .AsLocalDeclarationStatement(),

                CreationArgument = SyntaxFactory.IdentifierName("mock" + FirstCharToUpper(x.ArgumentName))
                                                .MemberAccess("Object")
                                                .AsArgument()
            }).ToArray();
        }

        private static string FirstCharToUpper(string input)
        {
            return char.ToUpper(input[0]) + input.Substring(1);
        }

        public static async Task<Document> ApplyConstructorInjections(Document document,
            SyntaxNode creation,
            CancellationToken cancellationToken,
            IReadOnlyCollection<ConstructorInjections> changes,
            ObjectCreationExpressionSyntax creationExpressionSyntax,
            SutCreationContextType sutCreationContextType)
        {
            var editor = await DocumentEditor.CreateAsync(document, cancellationToken).ConfigureAwait(false);

            var arguments = (from change in changes
                             from comma in new[] { (SyntaxNodeOrToken)SyntaxFactory.Token(SyntaxKind.CommaToken) }
                             select new { change.CreationArgument, comma })
                .SelectMany(x => new[] { x.CreationArgument, x.comma })
                .Take(changes.Count * 2 - 1);

            if (sutCreationContextType != SutCreationContextType.Method)
            {
                editor.InsertBefore(creation.Parent.Parent, changes.Select(x => x.NewField));
            }

            editor.InsertBefore(creation, changes.Select(x => x.NewExpression.WithAdditionalAnnotations(Formatter.Annotation)));
            editor.ReplaceNode(creationExpressionSyntax.ArgumentList, arguments.AsArgumentList());

            return editor.GetChangedDocument();
        }
    }
}