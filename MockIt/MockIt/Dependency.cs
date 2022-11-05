using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Linq;

namespace MockIt
{
    public class Dependency : IEquatable<Dependency>
    {
        public VariableDeclarationSyntax FieldOrLocalVariable { get; set; }
        public bool IsInjectedFromConstructor { get; set; }
        public ExpressionStatementSyntax SetupExpression { get; set; }
        public MemberAccessExpressionSyntax SetupIdentifierNode { get; set; }

        public GenericNameSyntax GetVariableOrFieldType()
        {
            return FieldOrLocalVariable.Type as GenericNameSyntax
                    ?? FieldOrLocalVariable.DescendantNodes().OfType<GenericNameSyntax>().FirstOrDefault();
        }

        public bool Equals(Dependency other)
        {
            if (ReferenceEquals(null, other)) return false;
            if (ReferenceEquals(this, other)) return true;
            return Equals(FieldOrLocalVariable, other.FieldOrLocalVariable) && IsInjectedFromConstructor == other.IsInjectedFromConstructor && Equals(SetupExpression, other.SetupExpression) && Equals(SetupIdentifierNode, other.SetupIdentifierNode);
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            if (obj.GetType() != this.GetType()) return false;
            return Equals((Dependency)obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = (FieldOrLocalVariable != null ? FieldOrLocalVariable.GetHashCode() : 0);
                hashCode = (hashCode * 397) ^ IsInjectedFromConstructor.GetHashCode();
                hashCode = (hashCode * 397) ^ (SetupExpression != null ? SetupExpression.GetHashCode() : 0);
                hashCode = (hashCode * 397) ^ (SetupIdentifierNode != null ? SetupIdentifierNode.GetHashCode() : 0);
                return hashCode;
            }
        }
    }
}