using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using ConsoleApplication2;
using Moq;

namespace UnitTestProject1
{
    [TestClass]
    public class UnitTest1
    {
        private IClass1<int, bool> sut;
        private Mock<IClass2<int>> class2Mock;
        private Mock<IClass2<bool>> class3Mock;

        [TestInitialize]
        public void Init()
        {
            class2Mock = new Mock<IClass2<int>>();
            class3Mock = new Mock<IClass2<bool>>();
            sut = new Class1<int, bool>(class2Mock.Object, class3Mock.Object);
        }

        [TestMethod]
        public void TestMethod()
        {
            var prop = sut.PropertyFoo;
        }

        [TestMethod]
        public void TestProp()
        {
            sut.Foo(5, true);
        }
    }
}
