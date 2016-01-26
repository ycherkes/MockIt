using System;

namespace ConsoleApplication2
{
    public interface IClass1<T, T1>
    {
        Tuple<T, T1> Foo(T a, T1 b);
        Tuple<T, T1> PropertyFoo { get;}
    }
}