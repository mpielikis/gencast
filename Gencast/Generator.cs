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

            // get changes for retro properties
            var changes = GetRetroProperties(compilation, objectType);

            // apply changes and remove generics by syntax tree
            foreach (var treeGroup in changes.GroupBy(x => x.Key.SyntaxTree))
            {
                var root = treeGroup.Key.GetRoot();

                var treeAfterReplace = root.ReplaceNodes(treeGroup.Select(x => x.Key), (x, y) => changes[x]);

                var clean = new RemoveGenericNames().Visit(
                    new RemoveGenericClass().Visit(treeAfterReplace));

                yield return new Tuple<SyntaxTree, SyntaxNode>(treeGroup.Key, clean);
            }

            // get changes for retro methods
            var methodChanges = GetRetroMethods(compilation, objectType);

            // apply changes and remove generics by syntax tree
            foreach (var treeGroup in methodChanges.GroupBy(x => x.Key.SyntaxTree))
            {
                var root = treeGroup.Key.GetRoot();

                var treeAfterReplace = root.ReplaceNodes(treeGroup.Select(x => x.Key), (x, y) => methodChanges[x]);

                var clean = new RemoveGenericNames().Visit(
                    new RemoveGenericClass().Visit(treeAfterReplace));

                yield return new Tuple<SyntaxTree, SyntaxNode>(treeGroup.Key, clean);
            }

        }

        private static IDictionary<SyntaxNode, SyntaxNode> GetRetroMethods(Compilation compilation, INamedTypeSymbol objectType)
        {
            var syntaxNodes = new Dictionary<SyntaxNode, SyntaxNode>();

            // FIND
            // 1. Generic properties symbols
            // =============================

            // NOTE -
            var genericMethodsList = new List<IMethodSymbol>();

            foreach (var tree in compilation.SyntaxTrees)
            {
                var semanticModel = compilation.GetSemanticModel(tree);

                // Find generic classes
                var genericClassFinder = new GenericClassFinder(tree, semanticModel);
                var classDeclarations = genericClassFinder.Get().ToArray();

                SyntaxNode newTree = tree.GetRoot();
                // Find reference to it's members

                // methods symbols
                var methodsToCast = classDeclarations
                    .SelectMany(x => x.Members)
                    .OfType<BaseMethodDeclarationSyntax>()
                    .Select(x => (IMethodSymbol)semanticModel.GetDeclaredSymbol(x));

                genericMethodsList.AddRange(methodsToCast);
            }

            var genericMethods = new HashSet<ISymbol>(genericMethodsList);

             //CHANGE
             //1. Generic properties references with cast
             //2. Generic properties to Object properties
             //======

            foreach (var tree in compilation.SyntaxTrees)
            {
                var root = tree.GetRoot();

                // NOTE semantic model is taken for the second time
                var semanticModel = compilation.GetSemanticModel(tree);

                // get cast changes
                var castReferences = new Dictionary<SyntaxNode, SyntaxNode>();
                CastResursiveMethod(root, semanticModel, genericMethods, castReferences);
                
                // get generic properties to object properties
                var retroProperties = GenericPropertiesToObject(root, semanticModel, objectType);

                var allChanges = castReferences.Concat(retroProperties);

                foreach (var ch in allChanges)
                    syntaxNodes.Add(ch.Key, ch.Value);
            }

            return syntaxNodes;
        }

        private static IDictionary<SyntaxNode, SyntaxNode> GetRetroProperties(Compilation compilation, INamedTypeSymbol objectType)
        {
            var syntaxNodes = new Dictionary<SyntaxNode, SyntaxNode>();

            // FIND
            // 1. Generic properties symbols
            // =============================

            var genericProperties = new List<IPropertySymbol>();

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

                // get cast changes
                var castReferences = new Dictionary<SyntaxNode, SyntaxNode>();
                CastResursive(root, semanticModel, genericProperties, castReferences);
                 
                // get generic properties to object properties
                var retroProperties = GenericPropertiesToObject(root, semanticModel, objectType);

                var allChanges = castReferences.Concat(retroProperties);

                foreach (var ch in allChanges)
                    syntaxNodes.Add(ch.Key, ch.Value);
            }

            return syntaxNodes;
        }

        private static SyntaxNode CastResursiveMethod(SyntaxNode tree, SemanticModel semanticModel, HashSet<ISymbol> methods, Dictionary<SyntaxNode, SyntaxNode> castChanges)
        {
            var change = new Dictionary<SyntaxNode, SyntaxNode>();

            foreach (var node in tree.ChildNodes())
            {
                ITypeSymbol ts = null;
                
                // if invocation -> ITypeSymbol
                // -------------------------
                if (node is InvocationExpressionSyntax)
                {
                    ISymbol invokedSymbol = semanticModel.GetSymbolInfo(node).Symbol;

                    // if is generic property
                    if (methods.Contains(invokedSymbol.OriginalDefinition))
                    {
                        ts = ((IMethodSymbol)invokedSymbol).ReturnType;                        
                    }
                }

                // recurse for changed node
                var casted = CastResursiveMethod(node, semanticModel, methods, castChanges);

                if (ts != null)
                {
                    // do cast
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

        private static SyntaxNode CastResursive(SyntaxNode tree, SemanticModel semanticModel, IEnumerable<IPropertySymbol> properties, Dictionary<SyntaxNode, SyntaxNode> castChanges)
        {
            var change = new Dictionary<SyntaxNode, SyntaxNode>();

            foreach (var node in tree.ChildNodes())
            {
                ITypeSymbol ts = null;

                // invocation
                // ----------
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
                var casted = CastResursive(node, semanticModel, properties, castChanges);

                if (ts != null)
                {
                    // do cast
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

            foreach (var node in tree.DescendantNodes().OfType<MethodDeclarationSyntax>())
            {
                var s = (IMethodSymbol)semanticModel.GetDeclaredSymbol(node);

                if (typeParameters.Any(x => x == s.ReturnType))
                {
                    var typeSynax = SyntaxFactory.IdentifierName(symbol.ToDisplayString())
                                .WithTrailingTrivia(node.ReturnType.GetTrailingTrivia());

                    yield return new KeyValuePair<SyntaxNode, SyntaxNode>(node.ReturnType, typeSynax);
                }

                foreach (var p in node.ParameterList.Parameters)
                {
                    var ps = semanticModel.GetDeclaredSymbol(p);

                    if (typeParameters.Any(x => x == ps.Type))
                    {
                        var typeSynax = SyntaxFactory.IdentifierName(symbol.ToDisplayString())
                                .WithTrailingTrivia(p.Type.GetTrailingTrivia());

                        yield return new KeyValuePair<SyntaxNode, SyntaxNode>(p.Type, typeSynax);
                    }
                }
            }
        }
    }
}
