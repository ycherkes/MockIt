using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using ConsoleApplication2;
using Moq;
using NUnit.Framework;

namespace UnitTestProject1
{
    [TestClass]
    public class UnitTest1
    {
        private IClass1<int, bool> sut;
        private Mock<IClass2<int>> class2Mock;
        private Mock<IClass2<bool>> class3Mock;

        [SetUp]
        public void Init()
        {
            class2Mock = new Mock<IClass2<int>>();
            class3Mock = new Mock<IClass2<bool>>();
            sut = new Class1<int, bool>(class2Mock.Object, class3Mock.Object);
        }

        [Test]
        public void TestMethod()
        {
            class3Mock.SetupGet(x => x.Foo).Returns(default(bool));
            class3Mock.SetupSet(x => x.Foo = default(bool));
            class2Mock.SetupGet(x => x.Foo).Returns(default(int));
            class2Mock.SetupSet(x => x.Foo = default(int));
            var prop = sut.PropertyFoo;
            class3Mock.VerifyAll();
            class2Mock.VerifyAll();
        }

        [Test]
        public void TestProp()
        {
            class3Mock.Setup(x => x.Foo2(It.Is<bool>(a => a == default(bool)))).Returns(default(bool));
            class3Mock.SetupGet(x => x.Foo).Returns(default(bool));
            class3Mock.SetupSet(x => x.Foo = default(bool));
            class2Mock.SetupGet(x => x.Foo).Returns(default(int));
            class2Mock.SetupSet(x => x.Foo = default(int));
            sut.Foo(5, true);
            class3Mock.VerifyAll();
            class2Mock.VerifyAll();
        }
    }
}
