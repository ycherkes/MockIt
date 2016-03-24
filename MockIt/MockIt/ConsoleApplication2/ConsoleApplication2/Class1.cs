using System;


namespace ConsoleApplication2
{
    public class Class1<T, T1> : IClass1<T, T1>
    {
        readonly IClass2<T> _class2;
        readonly IClass3<T1> _class3;
        private readonly IFactoryClass _factoryClass;

        public Class1(IClass2<T> class2, IClass3<T1> class3, IFactoryClass factoryClass)
        {
            _class2 = class2;
            _class3 = class3;
            _factoryClass = factoryClass;
        }        

        public Tuple<T, T1> PropertyFoo
        {
            get
            {
                var firstRes = _class3.Foo;
                var secondRes = _class2.Foo;
                return new Tuple<T, T1>(secondRes, firstRes);
            }
            set
            {
                _class3.Foo = value.Item2;
                _class2.Foo = value.Item1;
            }
        }

        public T3 Foo<T3>(T3 ab, T c)
        {
            _class2.Foo2(c);
            return ab;
        }

        public Tuple<T, T1> Foo(T a, T1 b)
        {
            _class3.Foo2(b);
            var firstRes = _class3.Foo;
            var secondRes = _class2.Foo;
            return new Tuple<T, T1>(secondRes, firstRes);
        }

        public Tuple<T, T1> FooFromFactory(T a, T1 b)
        {
            var class2 = _factoryClass.GetClass2<T>();
            var class3 = _factoryClass.GetClass3<T1>();
            class3.Foo2(b);
            var firstRes = class3.Foo;
            var secondRes = class2.Foo;
            return new Tuple<T, T1>(secondRes, firstRes);
        }
    }
}
