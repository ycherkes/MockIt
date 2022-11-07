using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.Formatting;
using MockIt.Model;
using MockIt.Syntax;
using System.Collections.Generic;
using System.Linq;

namespace MockIt
{
    public static class SyntaxEditorExtensions
    {
        public static void ApplyConstructorInjections(this SyntaxEditor editor,
            SyntaxNode objectCreationNode,
            IReadOnlyCollection<ConstructorInjections> constructorInjections,
            ObjectCreationExpressionSyntax creationExpressionSyntax,
            SutCreationContextType sutCreationContextType)
        {
            var arguments = constructorInjections.Select(change => change.CreationArgument);

            // Insert generated field declarations
            if (sutCreationContextType != SutCreationContextType.Method && objectCreationNode.Parent?.Parent != null)
            {
                editor.InsertBefore(objectCreationNode.Parent.Parent, constructorInjections.Select(x => x.NewField));
            }

            editor.InsertBefore(objectCreationNode, constructorInjections.Select(x => x.NewExpression.WithAdditionalAnnotations(Formatter.Annotation)));
            editor.ReplaceNode(creationExpressionSyntax.ArgumentList, arguments.AsArgumentList());
        }

        public static void ApplyMethodCodeFixChanges(this SyntaxNode invocationSyntax,
            SyntaxEditor editor,
            IReadOnlyCollection<FieldOrLocalVariables> invokedMethodsOfMocks, bool withCallBack)
        {
            var setups = MockSyntaxGenerator.GetSetups(invokedMethodsOfMocks, withCallBack);
            var verifiers = MockSyntaxGenerator.GetVerifiers(invokedMethodsOfMocks);

            editor.InsertBefore(invocationSyntax, setups.Select(x => x.WithTrailingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.CarriageReturnLineFeed, SyntaxFactory.CarriageReturnLineFeed))
                                                                      .WithAdditionalAnnotations(Formatter.Annotation)));

            editor.InsertAfter(invocationSyntax, verifiers.Select(x => x.WithLeadingTrivia(SyntaxFactory.TriviaList(SyntaxFactory.CarriageReturnLineFeed))
                                                                        .WithAdditionalAnnotations(Formatter.Annotation)));
        }
    }
}