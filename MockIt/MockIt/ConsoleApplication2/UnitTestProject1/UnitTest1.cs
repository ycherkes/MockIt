using System;
using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using ConsoleApplication2;
using Moq;
using NUnit.Framework;

namespace UnitTestProject1
{
    [TestClass]
    public class UnitTest1
    {
        private IClass1<List<string>, Dictionary<int, string>> sut;

        [SetUp]
        public void Init()
        {
            sut = new Class1<List<string>, Dictionary<int, string>>();
        }

        [Test]
        public void TestMethod()
        {
            var prop = sut.PropertyFoo;
        }

        [Test]
        public void TestProp()
        {
            var res = sut.Foo(new List<string>(), new Dictionary<int, string>());
        }

        [TestMethod]
        public void TestParameterizedGenericMethod()
        {
            var res = sut.Foo(long.MaxValue, new List<string>());
        }
    }
}
