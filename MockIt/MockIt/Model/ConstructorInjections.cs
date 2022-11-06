namespace MockIt.Model
{
    public class ConstructorInjections
    {
        public FieldDeclarationSyntax NewField { get; set; }
        public SyntaxNode NewExpression { get; set; }
        public ArgumentSyntax CreationArgument { get; set; }
    }
}