using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;
using static System.String;

namespace MockIt
{
    public class ChangesMaker
    {
        private static IEnumerable<ExpressionStatementSyntax> MakeVerifiers(IEnumerable<Fields> invokedMethodsOfMocks)
        {
            var verifiers = invokedMethodsOfMocks.SelectMany(x => x.FieldsToSetup.SelectMany(y => y.Field))
                .Distinct()
                .Select(x => SyntaxFactory.ExpressionStatement(SyntaxFactory.ParseExpression(x + ".VerifyAll()")))
                .ToArray();
            return verifiers;
        }

        private static ExpressionStatementSyntax[] MakeSetups(IEnumerable<Fields> invokedMethodsOfMocks)
        {
            var setups = invokedMethodsOfMocks.SelectMany(x => x.FieldsToSetup
                .SelectMany(y => y.Field.Select(f => GetSetups(f, x, y))))
                .SelectMany(x =>
                {
                    var collections = x as string[] ?? x.ToArray();
                    return collections;
                })
                .Distinct()
                .Select(x => SyntaxFactory.ExpressionStatement(
                    SyntaxFactory.ParseExpression(
                        x.Replace("#ToReplace#", "Setup")
                            .Replace("#ToReplaceGet#", "SetupGet")
                            .Replace("#ToReplaceSet#", "SetupSet"))))
                .ToArray();

            return setups;
        }

        private static IEnumerable<string> GetSetups(string f, Fields x, FieldsSetups y)
        {
            var methodSymbol = x.MethodOrPropertySymbol as IMethodSymbol;

            if (methodSymbol != null)
            {
                ////todo: determine the generic replacements correctly by semantic model from sut
                var returnType = GetSimpleTypeName(y.Substitutions, y.SutSubstitutions, methodSymbol.ReturnType);
                if (methodSymbol.ReturnType.ToDisplayString() != "void")
                {

                    if (methodSymbol.ReturnType.IsAbstract && methodSymbol.IsGenericMethod)
                    {
                        var symbolDefinitionsReplacement = TestSemanticHelper.GetReplacedDefinitions(
                            y.SutSubstitutions, methodSymbol.ReturnType);

                        var definitionsReplacement = symbolDefinitionsReplacement as string[] ?? symbolDefinitionsReplacement.ToArray();
                        if (definitionsReplacement.Any())
                        {
                            returnType = definitionsReplacement.First();
                        }

                    }


                }

                return new[]
                {
                    f + ".#ToReplace#(x => x." +
                    GetMethodName(methodSymbol, y.Substitutions, y.SutSubstitutions) + "(" +
                    Join(", ", methodSymbol.Parameters.Select(z => "It.Is<" +
                                                                          GetSimpleTypeName(y.Substitutions,
                                                                              y.SutSubstitutions, z.Type) +
                                                                          ">(" + z.Name + " => " + z.Name +
                                                                          " == default(" +
                                                                          GetSimpleTypeName(y.Substitutions,
                                                                              y.SutSubstitutions, z.Type) +
                                                                          "))")) + "))" +
                    (methodSymbol.ReturnType.ToDisplayString() != "void" //todo: to rigth determine generic return type
                        ? ".Returns(default(" + returnType + "))"
                        : "")
                };
            }

            var propertySymbol = (IPropertySymbol)x.MethodOrPropertySymbol;

            var expressions = new List<string>();

            if (!propertySymbol.IsWriteOnly && !x.Expression.IsLeftSideOfAssignExpression())
            {
                var getExpression = f + ".#ToReplaceGet#(x => x." + propertySymbol.Name + ").Returns(default(" +
                                    GetSimpleTypeName(y.Substitutions, y.SutSubstitutions, propertySymbol.Type) + "))";

                expressions.Add(getExpression);
            }

            if (propertySymbol.IsReadOnly || !x.Expression.IsLeftSideOfAssignExpression()) return expressions;

            var setExpression = f + ".#ToReplaceSet#(x => x." + propertySymbol.Name + " = default(" +
                                GetSimpleTypeName(y.Substitutions, y.SutSubstitutions, propertySymbol.Type) + "))";

            expressions.Add(setExpression);

            return expressions;
        }

        private static string GetMethodName(IMethodSymbol methodSymbol, 
            IReadOnlyDictionary<string, ITypeSymbol> substitutions,
            IReadOnlyDictionary<string, ITypeSymbol> sutSubstitutions)
        {
            if(!methodSymbol.IsGenericMethod)
                return methodSymbol.Name;

            return methodSymbol.Name + "<" + Join(", ", methodSymbol.TypeParameters.Select(x => GetSimpleTypeName(substitutions, sutSubstitutions, x))) + ">";
        }

        private static string GetSimpleTypeName(
            IReadOnlyDictionary<string, ITypeSymbol> substitutions, 
            IReadOnlyDictionary<string, ITypeSymbol> sutSubstitutions, 
            ITypeSymbol typeSymbol)
        {
            ITypeSymbol substitution;
            ITypeSymbol sutSubstitution;

            return TestSemanticHelper.GetSimpleTypeName(
                substitutions.TryGetValue(typeSymbol.ToString(), out substitution)
                    ? substitution
                    : sutSubstitutions.TryGetValue(typeSymbol.ToString(), out sutSubstitution)
                        ? sutSubstitution
                        : typeSymbol);
        }

        public static void ApplyChanges(SyntaxNode invokationSyntax, 
            SyntaxEditor editor,
            IReadOnlyCollection<Fields> invokedMethodsOfMocks)
        {
            var setups = MakeSetups(invokedMethodsOfMocks);
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
    }
}