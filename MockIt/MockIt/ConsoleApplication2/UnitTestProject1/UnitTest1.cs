using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using ConsoleApplication2;
using Moq;

namespace UnitTestProject1
{
    [TestClass]
    public class UnitTest1
    {
        private Class1<List<string>, Dictionary<int, string>> _sut;
        private Mock<IClass2<List<string>>> class2Mock;
        private Mock<IClass3<Dictionary<int, string>>> class3Mock;
        private Mock<IFactoryClass> factoryClassMock;

        [TestInitialize]
        public void Init()
        {
            class2Mock = new Mock<IClass2<List<string>>>();
            class3Mock = new Mock<IClass3<Dictionary<int, string>>>();
            factoryClassMock = new Mock<IFactoryClass>();

            _sut = new Class1<List<string>, Dictionary<int, string>>(class2Mock.Object, class3Mock.Object, factoryClassMock.Object);
        }

        [TestMethod]
        public void TestMethod()
        {
            var prop = _sut.PropertyFoo;
        }

        [TestMethod]
        public void TestProp()
        {
            var res = _sut.Foo(new List<string>(), new Dictionary<int, string>());
        }

        [TestMethod]
        public void TestParameterizedGenericMethod()
        {
            var res = _sut.Foo(long.MaxValue, new List<string>());
        }

        [TestMethod]
        public void TestFactoryUsingMethod()
        {
            //todo: wrong return type for factory.GetClass3
            var res = _sut.FooFromFactory(new List<string>(), new Dictionary<int, string>());
        }
    }
}
