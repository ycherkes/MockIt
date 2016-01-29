using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ConsoleApplication2
{
    public interface IClass3<T>
    {
        T Foo2(T a);

        T Foo { get; set; }
    }
}
