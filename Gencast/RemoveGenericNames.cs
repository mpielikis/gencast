using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ConsoleApplication1
{
    public class RemoveGenericNames : CSharpSyntaxRewriter
    {
        public override SyntaxNode VisitGenericName(GenericNameSyntax node)
        {
            var typeArgument = node.ChildNodes().OfType<TypeArgumentListSyntax>();

            if (typeArgument.Any())
                return SyntaxFactory.IdentifierName(node.Identifier);
            
            return node;
        }
    }

    public class RemoveGenericClass : CSharpSyntaxRewriter
    {
        public override SyntaxNode VisitClassDeclaration(ClassDeclarationSyntax node)
        {
            var typeList = node.ChildNodes().OfType<TypeParameterListSyntax>();

            if (typeList.Any())
                return node.RemoveNodes(typeList, SyntaxRemoveOptions.KeepEndOfLine);

            return node;
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
