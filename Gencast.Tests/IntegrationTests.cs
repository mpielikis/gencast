﻿using Microsoft.CodeAnalysis.MSBuild;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Gencast.Tests
{
    public class IntegrationTests
    {
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
}
