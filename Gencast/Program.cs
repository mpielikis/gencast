using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Linq;

namespace ConsoleApplication1
{
    static class Program
    {
        static void Main(string[] args)
        {
            string source = 
                @"using System; 

                 namespace HelloWorld 
                 { 
                     class A<T>
                     {
                         public T A { get; set; }
                     }

                     class Program
                     { 
                         static void Main(string[] args) 
                         { 
                             var a = new A<string>();

                             a.A = ""Hello, World!""

                             Console.WriteLine(a.A); 
                         } 
                     } 
                 }";

            var result = 
                @"using System; 

                namespace HelloWorld 
                { 
                    class A
                    {
                        public object A { get; set; }
                    }

                    class Program
                    { 
                        static void Main(string[] args) 
                        { 
                            var a = new A();

                            a.A = ""Hello, World!""

                            Console.WriteLine((string)a.A); 
                        } 
                    } 
                }";

            // 1. Parsing the source tree

            SyntaxTree tree = CSharpSyntaxTree.ParseText(source);

            // 2. Compile parsed tree

            MetadataReference mscorlib = MetadataReference.CreateFromFile("mscorlib");

            Compilation compilation = CSharpCompilation.Create("HelloWorld")
                .AddReferences(mscorlib)
                .AddSyntaxTrees(tree);
            
            // 3. Take the Semantic model of compiled tree

            var semanticModel = compilation.GetSemanticModel(tree);

            // 4. Check each class declaration of parsed source tree

            foreach (var clsDecl in tree.GetRoot()
                .DescendantNodes()
                .OfType<ClassDeclarationSyntax>())
                {                    
                    var info = semanticModel.GetDeclaredSymbol(clsDecl);

                    // If class is generic

                    if (info.IsGenericType)
                    {
                        // Get pointer for new class

                        ClassDeclarationSyntax newCls = clsDecl;

                        // For each class property

                        foreach (var prop in clsDecl.Members.OfType<BasePropertyDeclarationSyntax>())
                        {
                            var s = (IPropertySymbol)semanticModel.GetDeclaredSymbol(prop);

                            // Find the first member access via an argument in a source tree

                            var inv = tree.GetRoot().DescendantNodes().OfType<MemberAccessExpressionSyntax>().Where(x => x.Parent is ArgumentSyntax).First();

                            SymbolInfo e = semanticModel.GetSymbolInfo(inv);
                            ISymbol invokedSymbol = e.Symbol;
                                
                            // If this argument is the searched property

                            if (e.Symbol.OriginalDefinition == s)
                            {
                                var propertySymbol = (IPropertySymbol)invokedSymbol;

                                // Make a cast of it
                                var castEx = Program.CastTo(inv, propertySymbol.Type);

                                Console.WriteLine("hi");
                            }

                            // Change the generic property to object property

                            if (info.TypeParameters.Any(x => x == s.Type))
                            {
                                var objectType = compilation.GetTypeByMetadataName("System.Object");

                                var typeSynax = SyntaxFactory.IdentifierName(objectType.ToDisplayString()).WithTrailingTrivia(prop.Type.GetTrailingTrivia());

                                var newProp = prop.ReplaceNode(prop.Type, typeSynax);

                                newCls = newCls.ReplaceNode(prop, newProp);

                            }
                        }

                        // Remove generic <> thing after the class

                        var a = newCls.ChildNodes().First();
                        newCls = newCls.RemoveNode(a, SyntaxRemoveOptions.KeepExteriorTrivia);
                    }

                
                }               

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

    //public class XRewriter : SyntaxRewriter
    //{
        
    //}
}
