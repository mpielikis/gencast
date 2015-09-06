using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Gencast.Tests
{
    public class Generator
    {
        public static async Task<Project> MakeRetro(Project prj)
        {
            var compilation = await prj.GetCompilationAsync();
            var objectType = compilation.GetTypeByMetadataName("System.Object");

            // GetPropertiesToCast

            var changedDocuments = MakeChanges(compilation, objectType);

            Project nPrj = prj;

            foreach (var newDoc in changedDocuments)
            {
                var docId = nPrj.GetDocumentId(newDoc.Item1);

                var doc = nPrj.GetDocument(docId);
                nPrj = nPrj.RemoveDocument(docId);

                var nd = nPrj.AddDocument(doc.Name, newDoc.Item2, doc.Folders, doc.FilePath);
                nPrj = nd.Project;
            }

            return nPrj;
        }

        private static IEnumerable<Tuple<SyntaxTree, SyntaxNode>> MakeChanges(Compilation compilation, INamedTypeSymbol objectType)
        {
            var changedNodes = new List<Tuple<SyntaxTree, SyntaxNode>>();

            var changes = GetChanges(compilation, objectType);

            foreach (var treeGroup in changes.GroupBy(x => x.Key.SyntaxTree))
            {
                var root = treeGroup.Key.GetRoot();

                var treeAfterReplace = root.ReplaceNodes(treeGroup.Select(x => x.Key), (x, y) => changes[x]);

                var clean = new RemoveGenericNames().Visit(new RemoveGenericClass().Visit(treeAfterReplace));

                yield return new Tuple<SyntaxTree, SyntaxNode>(treeGroup.Key, clean);
            }

        }

        private static IDictionary<SyntaxNode, SyntaxNode> GetChanges(Compilation compilation, INamedTypeSymbol objectType)
        {
            var syntaxNodes = new Dictionary<SyntaxNode, SyntaxNode>();

            var genericProperties = new List<IPropertySymbol>();

            // FIND
            // 1. Generic properties symbols
            // =============================

            foreach (var tree in compilation.SyntaxTrees)
            {
                var semanticModel = compilation.GetSemanticModel(tree);

                // Find generic classes
                var genericClassFinder = new GenericClassFinder(tree, semanticModel);
                var classDeclarations = genericClassFinder.Get().ToArray();

                SyntaxNode newTree = tree.GetRoot();
                // Find reference to it's members

                // properties symbols
                var propertiesToCast = classDeclarations
                    .SelectMany(x => x.Members)
                    .OfType<BasePropertyDeclarationSyntax>()
                    .Select(x => (IPropertySymbol)semanticModel.GetDeclaredSymbol(x));

                genericProperties.AddRange(propertiesToCast);
            }

            // CHANGE
            // 1. Generic properties references with cast
            // 2. Generic properties to Object properties
            // ======

            foreach (var tree in compilation.SyntaxTrees)
            {
                var root = tree.GetRoot();
                var semanticModel = compilation.GetSemanticModel(tree);

                var castReferences = FindReferencesAndCast(root, semanticModel, genericProperties);
                var retroProperties = GenericPropertiesToObject(root, semanticModel, objectType);

                var allChanges = castReferences.Concat(retroProperties);

                foreach (var ch in allChanges)
                    syntaxNodes.Add(ch.Key, ch.Value);
            }

            return syntaxNodes;
        }

        private static IEnumerable<KeyValuePair<SyntaxNode, SyntaxNode>> FindReferencesAndCast(SyntaxNode tree, SemanticModel semanticModel, IEnumerable<IPropertySymbol> properties)
        {
            foreach (var node in tree.DescendantNodes().OfType<MemberAccessExpressionSyntax>().Where(x => x.Parent is ArgumentSyntax))
            {
                ISymbol invokedSymbol = semanticModel.GetSymbolInfo(node).Symbol;

                if (properties.Any(x => x == invokedSymbol.OriginalDefinition))
                {
                    var castEx = Helpers.CastTo(node, ((IPropertySymbol)invokedSymbol).Type);
                    yield return new KeyValuePair<SyntaxNode, SyntaxNode>(node, castEx);
                }
            }
        }

        private static IEnumerable<KeyValuePair<SyntaxNode, SyntaxNode>> GenericPropertiesToObject(SyntaxNode tree, SemanticModel semanticModel, INamedTypeSymbol symbol)
        {
            foreach (var node in tree.DescendantNodes().OfType<ClassDeclarationSyntax>())
            {
                var info = semanticModel.GetDeclaredSymbol(node);

                // If class is generic

                if (info.IsGenericType)
                {
                    foreach (var changedPart in GetCleanedGenericClass(node, semanticModel, info.TypeParameters.ToArray(), symbol))
                        yield return changedPart;
                }
            }
        }

        private static IEnumerable<KeyValuePair<SyntaxNode, SyntaxNode>> GetCleanedGenericClass(SyntaxNode tree, SemanticModel semanticModel, IEnumerable<ITypeParameterSymbol> typeParameters, INamedTypeSymbol symbol)
        {
            foreach (var node in tree.DescendantNodes().OfType<PropertyDeclarationSyntax>())
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
}
