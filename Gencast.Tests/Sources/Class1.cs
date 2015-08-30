using System;
using System.Linq;

namespace Netmf.Plus.Tests.Sources
{
    class Class1a
    {
        public T A<T>(T a)
        {
            return a;
        }

        public void Use()
        {
            int a = 0;

            int b = A(a) + 1;
        }
    }

    class Class1b
    {
        public object A(object a)
        {
            return a;
        }

        public void Use()
        {
            int a = 0;

            int b = ((int)A(a)) + 1;
        }
    }

    class Class2a<T>
    {
        
    }

    class Class2b
    {
        
    }
}
