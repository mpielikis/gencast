using System;
using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.IO;
using Microsoft.CodeAnalysis.MSBuild;
using System.Threading.Tasks;

namespace Gencast.Tests
{
    public class ReferenceCasterTests
    {
        [Test]
        public void A()
        {
            string source =@"
                 using System; 

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

                             a.A = ""Hello, World!"";
                             string s = a.A;

                             Console.WriteLine(  a.A);
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
                         public object B { get; set; }
                     }

                     class Program
                     { 
                         static void Main(string[] args) 
                         { 
                             var a = new A();

                             a.A = ""Hello, World!"";
                             string s = (string)a.A;

                             Console.WriteLine(  (string)a.A);
                             Console.WriteLine((string)a.B);
                         } 
                     } 
                 }";

            var compilation = Compilation(source);

            var change = Generator.MakeChanges(compilation).Single();

            Assert.AreEqual(result, change.Item2.ToString());
        }

        [Test]
        public void B()
        {
            string source = @"
                 using System; 

                 namespace HelloWorld 
                 { 
                     class A<T>
                     {
                         public T A { get; set; }
                         public T B { get; set; }
                     }

                     class B<T>
                     {
                         public T A { get; set; }
                         public T B { get; set; }
                     }

                     class Program
                     { 
                         static void Main(string[] args) 
                         { 
                             var a = new A<B<string>>();

                             a.A = new B<string>();
                             a.A.A = ""Hello, World!"";

                             Console.WriteLine(a.A.A);
                             Console.WriteLine(a.B.B);
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
                         public object B { get; set; }
                     }

                     class B
                     {
                         public object A { get; set; }
                         public object B { get; set; }
                     }

                     class Program
                     { 
                         static void Main(string[] args) 
                         { 
                             var a = new A();

                             a.A = new B();
                             ((HelloWorld.B)a.A).A = ""Hello, World!"";

                             Console.WriteLine((string)((HelloWorld.B)a.A).A);
                             Console.WriteLine((string)((HelloWorld.B)a.B).B);
                         } 
                     } 
                 }";

            var compilation = Compilation(source);

            var change = Generator.MakeChanges(compilation).Single();

            Assert.AreEqual(result, change.Item2.ToString());
        }

        [Test]
        public void C()
        {
            string source = @"using System;

                class A<T>
                {
                    public T A { get; set; }

                    static void Main() 
                    {
                        var a = new A<A<string>>();

                        a.A = new A<string>();
                        a.A.A = ""Hello, World!"";

                        Console.WriteLine(a.A.A);
                    } 
                }";

            var result = @"using System;

                class A
                {
                    public object A { get; set; }

                    static void Main() 
                    {
                        var a = new A();

                        a.A = new A();
                        ((A)a.A).A = ""Hello, World!"";

                        Console.WriteLine((string)((A)a.A).A);
                    } 
                }";

            var compilation = Compilation(source);

            var change = Generator.MakeChanges(compilation).Single();

            var changestr = change.Item2.ToString();

            Assert.AreEqual(result, changestr);
        }

        private static CSharpCompilation Compilation(string source)
        {
            var assemblyPath = Path.GetDirectoryName(typeof(object).Assembly.Location);
            var tree = CSharpSyntaxTree.ParseText(source);

            var compilation = CSharpCompilation.Create("HelloWorld")
                .AddReferences(MetadataReference.CreateFromFile(Path.Combine(assemblyPath, "mscorlib.dll")))
                .AddSyntaxTrees(tree);

            return compilation;
        }

        [Test]
        public void FullTest()
        {
            var assemblyPath = Path.GetDirectoryName(typeof(object).Assembly.Location);

            var ws = MSBuildWorkspace.Create();

            var open = ws.OpenSolutionAsync(@"sample\Solution1\Solution1.sln");
            open.Wait();
            var solution = open.Result;

            var prj = solution.Projects.Single(x => x.Name == "Project1");

            var retro = Generator.MakeRetro(prj);

            retro.Wait();

            var retroProject = retro.Result;

            if (retroProject != prj)
                ws.TryApplyChanges(retroProject.Solution);
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
