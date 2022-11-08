using System;

namespace DemoClassLibrary
{
    public interface IClass1<T, T1>
    {
        Tuple<T, T1> Foo(T a, T1 b);
        Tuple<T, T1> FooFromFactory(T a, T1 b);
        Tuple<T, T1> PropertyFoo { get;}
        T3 Foo<T3>(T3 ab, T c);
    }
}