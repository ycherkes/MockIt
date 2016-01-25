using System;


namespace ConsoleApplication2
{
    public class Class1<T, T1> : IClass1<T, T1>
    {
        IClass2<T> class2;
        IClass2<T1> class3;

        public Class1(IClass2<T> class2, IClass2<T1> class3)
        {
            this.class2 = class2;
            this.class3 = class3;
        }

        public Tuple<T, T1> PropertyFoo
        {
            get
            {
                var firstRes = class3.Foo;
                var secondRes = class2.Foo;
                return new Tuple<T, T1>(secondRes, firstRes);
            }
        }

        public Tuple<T, T1> Foo(T a, T1 b)
        {
            class3.Foo2(b);
            var firstRes = class3.Foo;
            var secondRes = class2.Foo;
            return new Tuple<T, T1>(secondRes, firstRes);
        }        
    }
}
