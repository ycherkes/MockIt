﻿using System;


namespace ConsoleApplication2
{
    public class Class1<T, T1> : IClass1<T, T1>
    {
        IClass2<T> class2;
        IClass3<T1> class3;

        public Class1(IClass2<T> class2, IClass3<T1> class3)
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
            set
            {
                class3.Foo = value.Item2;
                class2.Foo = value.Item1;
            }
        }

        public T3 Foo<T3>(T3 ab, T c)
        {
            class2.Foo2(c);
            return ab;
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
