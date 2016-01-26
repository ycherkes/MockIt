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

        [SetUp]
        public void Init()
        {
            sut = new Class1<int, bool>();
        }

        [Test]
        public void TestMethod()
        {
            var prop = sut.PropertyFoo;
        }

        [Test]
        public void TestProp()
        {
            sut.Foo(5, true);
        }
    }
}
