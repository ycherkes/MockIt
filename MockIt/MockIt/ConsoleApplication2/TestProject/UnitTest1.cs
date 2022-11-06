using DemoClassLibrary;
using Xunit;

namespace TestProject
{
    public class UnitTest1
    {
        private readonly Class1<List<string>, Dictionary<int, string>> _sut;

        public UnitTest1()
        {
            _sut = new Class1<List<string>, Dictionary<int, string>>();
        }

        private Class1<List<string>, Dictionary<int, string>> CreateSut()
        {
            var sut = new Class1<List<string>, Dictionary<int, string>>();

            return sut;
        }

        [Fact]
        public void TestMethod()
        {
            var sut = new Class1<List<string>, Dictionary<int, string>>();

            var prop = sut.PropertyFoo;
        }

        [Fact]
        public void TestProp()
        {
            var res = _sut.Foo(new List<string>(), new Dictionary<int, string>());
        }

        [Fact]
        public void TestParameterizedGenericMethod()
        {
            var res = _sut.Foo(long.MaxValue, new List<string>());
        }

        [Fact]
        public void TestFactoryUsingMethod()
        {
            var res = _sut.FooFromFactory(new List<string>(), new Dictionary<int, string>());
        }
    }
}