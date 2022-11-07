using DemoClassLibrary;
using Moq;
using Xunit;

namespace TestProject
{
    public class UnitTest1
    {
        private readonly Class1<List<string>, Dictionary<int, string>> _sut;
        private readonly Mock<IClass2<List<string>>> _class2Mock;
        private readonly Mock<IClass3<Dictionary<int, string>>> _class3Mock;
        private readonly Mock<IFactoryClass> _factoryClassMock;

        public UnitTest1()
        {
            _class2Mock = new Mock<IClass2<List<string>>>();
            _class3Mock = new Mock<IClass3<Dictionary<int, string>>>();
            _factoryClassMock = new Mock<IFactoryClass>();
            _sut = new Class1<List<string>, Dictionary<int, string>>(_class2Mock.Object, _class3Mock.Object, _factoryClassMock.Object);
        }

        [Fact]
        public void TestProp()
        {
            _class3Mock.Setup(x => x.Foo2(It.IsAny<Dictionary<int, string>>()))
                .Callback<Dictionary<int, string>>(a => { })
                .Returns(default(Dictionary<int, string>));
            _class3Mock.SetupGet(x => x.Foo)
                .Returns(default(Dictionary<int, string>));
            _class2Mock.SetupGet(x => x.Foo)
                .Returns(default(List<string>));

            var res = _sut.Foo(new List<string>(), new Dictionary<int, string>());

            _class3Mock.VerifyAll();
            _class2Mock.VerifyAll();
        }

        [Fact]
        public void TestParameterizedGenericMethod()
        {
            _class2Mock.Setup(x => x.Foo2(It.Is<List<string>>(a => a == default)))
                .Returns(default(List<string>));

            var res = _sut.Foo(long.MaxValue, new List<string>());

            _class2Mock.VerifyAll();
        }

        [Fact]
        public void TestFactoryUsingMethod()
        {
            _factoryClassMock.Setup(x => x.GetClass2<Dictionary<int, string>>())
                .Returns(default(IClass2<Dictionary<int, string>>));
            _factoryClassMock.Setup(x => x.GetClass3<List<string>>())
                .Returns(default(IClass3<List<string>>));

            var res = _sut.FooFromFactory(new List<string>(), new Dictionary<int, string>());

            _factoryClassMock.VerifyAll();
        }

        private Class1<List<string>, Dictionary<int, string>> CreateSut()
        {
            var class2Mock = new Mock<IClass2<List<string>>>();
            var class3Mock = new Mock<IClass3<Dictionary<int, string>>>();
            var factoryClassMock = new Mock<IFactoryClass>();
            var sut = new Class1<List<string>, Dictionary<int, string>>(class2Mock.Object, class3Mock.Object, factoryClassMock.Object);

            return sut;
        }

        [Fact]
        public void TestMethod()
        {
            var class2Mock = new Mock<IClass2<List<string>>>();
            var class3Mock = new Mock<IClass3<Dictionary<int, string>>>();
            var factoryClassMock = new Mock<IFactoryClass>();
            var sut = new Class1<List<string>, Dictionary<int, string>>(class2Mock.Object, class3Mock.Object, factoryClassMock.Object);
            class3Mock.SetupGet(x => x.Foo)
                .Returns(default(Dictionary<int, string>));
            class2Mock.SetupGet(x => x.Foo)
                .Returns(default(List<string>));


            var prop = sut.PropertyFoo;

            class3Mock.VerifyAll();
            class2Mock.VerifyAll();
        }
    }
}