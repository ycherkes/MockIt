using DemoClassLibrary;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;

namespace UnitTestProject1
{
    [TestClass]
    public class UnitTest1
    {
        private Class1<List<string>, Dictionary<int, string>> _sut;

        [TestInitialize]
        public void Init()
        {
            _sut = new Class1<List<string>, Dictionary<int, string>>();
        }

        private void SimpleNonTestMethod()
        {
            var sut = new Class1<List<string>, Dictionary<int, string>>();
        }

        [TestMethod]
        private void SimpleTestMethod()
        {
            var sut = new Class1<List<string>, Dictionary<int, string>>();

            var prop = sut.PropertyFoo;
        }

        [TestMethod]
        public void TestMethod()
        {
            var sut = new Class1<List<string>, Dictionary<int, string>>();

            var prop = sut.PropertyFoo;
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
            var res = _sut.FooFromFactory(new List<string>(), new Dictionary<int, string>());
        }
    }
}
