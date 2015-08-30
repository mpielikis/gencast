using System;
using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Netmf.Plus.Tests
{
    [TestFixture]
    public class ReferenceCasterTests
    {
        [Test]
        public void A()
        {
            string source =
                @"using System; 

                 namespace HelloWorld 
                 { 
                     class A<T>
                     {
                         public T A { get; set; }
                         public T B { get; set; }
                     }

                     class Program
                     { 
                         static void Main(string[] args) 
                         { 
                             var a = new A<string>();

                             a.A = ""Hello, World!""

                             Console.WriteLine(a.A); 
                             Console.WriteLine(a.A); 
                             Console.WriteLine(a.B); 
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

            var tree = CSharpSyntaxTree.ParseText(source);
            MetadataReference mscorlib = MetadataReference.CreateFromFile("mscorlib");
            Compilation compilation = CSharpCompilation.Create("HelloWorld")
                .AddReferences(mscorlib)
                .AddSyntaxTrees(tree);

            // 3. Take the Semantic model of compiled tree

            var semanticModel = compilation.GetSemanticModel(tree);

            // Find abstract classes

            var objectType = compilation.GetTypeByMetadataName("System.Object");

            var genericClassFinder = new GenericClassFinder(tree, semanticModel);
            var classDeclarations = genericClassFinder.Get().ToArray();

            SyntaxNode newTree = tree.GetRoot();
            // Find reference to it's members

            // properties symbols
            var propertiesToCast = classDeclarations
                .SelectMany(x => x.Members)
                .OfType<BasePropertyDeclarationSyntax>()
                .Select(x => (IPropertySymbol)semanticModel.GetDeclaredSymbol(x));

            var memberCasts = GetCastedReferences(newTree, semanticModel, propertiesToCast).ToArray();
            var genericChanges = ChangeGenericClasses(newTree, semanticModel, objectType).ToArray();

            var allChanges = memberCasts.Concat(genericChanges).ToDictionary(x => x.Key, x => x.Value);

            var newTree2 = newTree.ReplaceNodes(allChanges.Keys, (x, y) => allChanges[x]);

        }

        private static IEnumerable<KeyValuePair<SyntaxNode, SyntaxNode>> GetCastedReferences(SyntaxNode tree, SemanticModel semanticModel, IEnumerable<IPropertySymbol> properties)
        {
            foreach (var node in tree.ChildNodes().OfType<MemberAccessExpressionSyntax>().Where(x => x.Parent is ArgumentSyntax))
            {
                ISymbol invokedSymbol = semanticModel.GetSymbolInfo(node).Symbol;

                if (properties.Any(x => x == invokedSymbol.OriginalDefinition))
                {
                    var castEx = Helpers.CastTo(node, ((IPropertySymbol)invokedSymbol).Type);
                    yield return new KeyValuePair<SyntaxNode, SyntaxNode>(node, castEx);
                }
            }
        }

        private static IEnumerable<KeyValuePair<SyntaxNode, SyntaxNode>> ChangeGenericClasses(SyntaxNode tree, SemanticModel semanticModel, INamedTypeSymbol symbol)
        {
            foreach (var node in tree.ChildNodes().OfType<ClassDeclarationSyntax>())
            {
                var info = semanticModel.GetDeclaredSymbol(node);

                // If class is generic

                if (info.IsGenericType)
                {
                    foreach (var changedPart in GetCleanedGenericClass(tree, semanticModel, info.TypeParameters.ToArray(), symbol))
                        yield return changedPart;
                }
            }
        }

        private static IEnumerable<KeyValuePair<SyntaxNode, SyntaxNode>> GetCleanedGenericClass(SyntaxNode tree, SemanticModel semanticModel, IEnumerable<ITypeParameterSymbol> typeParameters, INamedTypeSymbol symbol)
        {
            foreach (var node in tree.ChildNodes().OfType<PropertyDeclarationSyntax>())
            {
                var s = semanticModel.GetDeclaredSymbol(node);

                if (typeParameters.Any(x => x == s.Type))
                {
                    var typeSynax = SyntaxFactory.IdentifierName(symbol.ToDisplayString())
                                .WithTrailingTrivia(node.Type.GetTrailingTrivia());

                    yield return new KeyValuePair<SyntaxNode, SyntaxNode>(node.Type, typeSynax);
                }
            }
            
        }
    }
  
    public class MemberReferenceCaster : CSharpSyntaxRewriter
    {
        private readonly SemanticModel semanticModel;
        private readonly IEnumerable<IPropertySymbol> properties;

        public MemberReferenceCaster(IEnumerable<IPropertySymbol> properties, SemanticModel semanticModel)
        {
            this.properties = properties;
            this.semanticModel = semanticModel;
        }

        public override SyntaxNode VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
        {
            if (node.Parent is ArgumentSyntax)
            {
                ISymbol invokedSymbol = semanticModel.GetSymbolInfo(node).Symbol;

                if (properties.Any(x => x == invokedSymbol.OriginalDefinition))
                {
                    var castEx = Helpers.CastTo(node, ((IPropertySymbol)invokedSymbol).Type);
                    return castEx;
                }
            }

            return node;
        }
    }

    public class ClassMembersRewriter : CSharpSyntaxRewriter
    {
        private readonly SemanticModel semanticModel;
        private readonly IEnumerable<ITypeParameterSymbol> typeParameters;

        private readonly INamedTypeSymbol symbol;

        public ClassMembersRewriter(SemanticModel semanticModel, IEnumerable<ITypeParameterSymbol> typeParameters, INamedTypeSymbol symbol)
        {
            this.symbol = symbol;
            this.typeParameters = typeParameters;
            this.semanticModel = semanticModel;
        }

        public override SyntaxNode VisitPropertyDeclaration(PropertyDeclarationSyntax node)
        {
            var s = semanticModel.GetDeclaredSymbol(node);

            if (typeParameters.Any(x => x == s.Type))
            {
                var typeSynax = SyntaxFactory.IdentifierName(symbol.ToDisplayString())
                            .WithTrailingTrivia(node.Type.GetTrailingTrivia());

                return node.ReplaceNode(node.Type, typeSynax);
            }

            return node;
        }
    }
    
    public class GenericMembersToObjectRewriter : CSharpSyntaxRewriter
    {
        private readonly SemanticModel semanticModel;

        private readonly INamedTypeSymbol symbol;

        public GenericMembersToObjectRewriter(SemanticModel semanticModel, INamedTypeSymbol symbol)
        {
            this.symbol = symbol;
            this.semanticModel = semanticModel;
        }

        public override SyntaxNode VisitClassDeclaration(ClassDeclarationSyntax node)
        {
            var info = semanticModel.GetDeclaredSymbol(node);

            // If class is generic

            if (info.IsGenericType)
            {
                var classMemberRewriter = new ClassMembersRewriter(
                    semanticModel, 
                    info.TypeParameters.ToArray(), 
                    symbol);

                return classMemberRewriter.Visit(node);

                //var a = newCls.ChildNodes().First();
                //newCls = newCls.RemoveNode(a, SyntaxRemoveOptions.KeepExteriorTrivia);
                //return newCls;
            }

            return node;
        }
    }
}
