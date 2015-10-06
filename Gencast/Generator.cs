using ConsoleApplication1;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Diagnostics;
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
            
            // GetPropertiesToCast

            var changedDocuments = MakeChanges(compilation);

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

        public static IEnumerable<Tuple<SyntaxTree, SyntaxNode>> MakeChanges(Compilation compilation)
        {
            var objectType = compilation.GetTypeByMetadataName("System.Object");
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

                var castReferences = new Dictionary<SyntaxNode, SyntaxNode>();

                var catedRoot = CastResursive(root, semanticModel, genericProperties, castReferences);
                 
                var retroProperties = GenericPropertiesToObject(root, semanticModel, objectType);

                var allChanges = castReferences.Concat(retroProperties);

                foreach (var ch in allChanges)
                    syntaxNodes.Add(ch.Key, ch.Value);
            }

            return syntaxNodes;
        }

        //private static IEnumerable<KeyValuePair<SyntaxNode, SyntaxNode>> FindReferencesAndCast(SyntaxNode tree, SemanticModel semanticModel, IEnumerable<IPropertySymbol> properties)
        //{
        //    foreach (var node in tree.ChildNodes().OfType<MemberAccessExpressionSyntax>().Where(x => !(x.Parent is AssignmentExpressionSyntax)))
        //    {
        //        ISymbol invokedSymbol = semanticModel.GetSymbolInfo(node).Symbol;

        //        if (properties.Any(x => x == invokedSymbol.OriginalDefinition))
        //        {
        //            Debug.Print(node.ToString());
        //            Debug.Print("<" + node.Parent.ToString() + ">");

        //            //var castEx = Helpers.CastTo(node, ((IPropertySymbol)invokedSymbol).Type);

        //            var castEx = CastResursive(node, semanticModel, properties, ((IPropertySymbol)invokedSymbol).Type);

        //            yield return new KeyValuePair<SyntaxNode, SyntaxNode>(node, castEx);
        //        }
        //    }
        //}

        private static SyntaxNode CastResursive(SyntaxNode tree, SemanticModel semanticModel, IEnumerable<IPropertySymbol> properties, Dictionary<SyntaxNode, SyntaxNode> castChanges)
        {
            var change = new Dictionary<SyntaxNode, SyntaxNode>();

            foreach (var node in tree.ChildNodes())
            {
                // .OfType<MemberAccessExpressionSyntax>().Where(x => !(x.Parent is AssignmentExpressionSyntax))

                ITypeSymbol ts = null;

                if ((node is MemberAccessExpressionSyntax) && !(node.Parent is AssignmentExpressionSyntax))
                {
                    ISymbol invokedSymbol = semanticModel.GetSymbolInfo(node).Symbol;

                    // if is generic property

                    if (properties.Any(x => x == invokedSymbol.OriginalDefinition))
                    {
                        ts = ((IPropertySymbol)invokedSymbol).Type;                        
                    }

                }

                // recurse for changed node
                // do cast

                var casted = CastResursive(node, semanticModel, properties, castChanges);

                if (ts != null)
                {
                    Debug.Print("cast " + node.ToString() + " to " + ts);
                    casted = Helpers.CastTo((ExpressionSyntax)casted, ts);

                    if (node.Parent is MemberAccessExpressionSyntax)
                        casted = ((ExpressionSyntax)casted).Parenthesize();

                    castChanges.Add(node, casted);
                }

                // add for replace
                if (node != casted)
                    change.Add(node, casted);
            }

            if (change.Any())
                tree = tree.ReplaceNodes(change.Keys, (x, y) => change[x]);

            return tree;
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
