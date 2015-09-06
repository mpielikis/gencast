using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using NUnit.Framework;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Gencast.Tests
{
    [TestFixture]
    class GenericClassFinderTests
    {
        [Test]
        public void A()
        {
            string source =
                @"using System; 

                 namespace HelloWorld 
                 { 
                     class A<T>
                     { }

                     class B
                     { } 

                     class C<T>
                     { }
                 }";

            var tree = CSharpSyntaxTree.ParseText(source);
            var semanticModel = Helpers.CreateSemanticModel(tree);

            var genericClassFinder = new GenericClassFinder(tree, semanticModel);

            var classDeclarations = genericClassFinder.Get().ToArray();

            Assert.That(classDeclarations.Count(), Is.EqualTo(2));
            //Assert.That(classDeclarations.Single(x => x.))
        }
    }
  
    public class GenericClassFinder
    {
        private readonly SyntaxTree tree;
        private readonly SemanticModel semanticModel;

        public GenericClassFinder(SyntaxTree tree, SemanticModel semanticModel)
        {
            this.semanticModel = semanticModel;
            this.tree = tree;
        }

        public IEnumerable<ClassDeclarationSyntax> Get()
        {
            foreach (var clsDecl in tree.GetRoot()
                .DescendantNodes()
                .OfType<ClassDeclarationSyntax>())
            {
                var info = semanticModel.GetDeclaredSymbol(clsDecl);

                if (info.IsGenericType)
                    yield return clsDecl;
            }
        }
    }
}
