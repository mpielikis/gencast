using System;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp;

namespace Netmf.Plus.Tests
{
    static class Helpers
    {
        public static SemanticModel CreateSemanticModel(SyntaxTree tree)
        {
            MetadataReference mscorlib = MetadataReference.CreateFromFile("mscorlib");

            Compilation compilation = CSharpCompilation.Create("HelloWorld")
            .AddReferences(mscorlib)
            .AddSyntaxTrees(tree);

            return compilation.GetSemanticModel(tree);
        }

        public static CastExpressionSyntax CastTo(this ExpressionSyntax expression, ITypeSymbol type)
        {
            return SyntaxFactory.CastExpression(
                type: SyntaxFactory.ParseTypeName(type.ToDisplayString()),
                expression: expression.ParenthesizeIfNeeded());
        }

        public static ExpressionSyntax ParenthesizeIfNeeded(this ExpressionSyntax expression)
        {
            if (expression is BinaryExpressionSyntax ||
                expression is ConditionalExpressionSyntax ||
                expression is ParenthesizedLambdaExpressionSyntax ||
                expression is SimpleLambdaExpressionSyntax)
            {
                return expression.Parenthesize();
            }

            return expression;
        }

        public static ExpressionSyntax Parenthesize(this ExpressionSyntax expression)
        {
            return SyntaxFactory.ParenthesizedExpression(expression: expression);
        }
    }
}
