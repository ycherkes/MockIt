using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using MockIt.Test.Helpers;
using CodeFixVerifier = MockIt.Test.Verifiers.CodeFixVerifier;

namespace MockIt.Test
{
    [TestClass]
    public class TestMethodUnitTests : CodeFixVerifier
    {

        //No diagnostics expected to show up
        [TestMethod]
        public void TestNoDiagnostics()
        {
            var test = @"";

            VerifyCSharpDiagnostic(test);
        }

        //Diagnostic and CodeFix both triggered and checked for
        [TestMethod]
        public void TestSetupWithoutReturns()
        {
            var test = @"
            using Microsoft.VisualStudio.TestTools.UnitTesting;
            using TestMockProjectTest;
            
            namespace TestMockUnitTests
            {
                [TestClass]
                public class UnitTest2
                {
                    private IService sut;
                    private Mock<ISubService> subServiceMock;

        [TestInitialize]
                    public void Init()
                    {
            subServiceMock = new Mock<ISubService>();
            sut = new Service(subServiceMock.Object);
                    }
            
                    [TestMethod]
                    public void TestMethod1()
                    {
                        sut.DoSomething(2);
                    }
                }
            }
            
            namespace TestMockProjectTest
            {
            
                public interface IService
                {
                    void DoSomething(int doInt);
                }
            
                public interface ISubService
                {
                    void DoSubSomething(int doInt);
                }
            
                public class Service : IService
                {
                    private readonly ISubService _subService;
            
                    public Service(ISubService subService)
                    {
                        _subService = subService;
                    }
            
                    public void DoSomething(int doInt)
                    {
                        _subService.DoSubSomething(doInt);
                    }
                }
            }";
            var expected = new DiagnosticResult
            {
                Id = TestMethodDiagnosticAnalyzer.DiagnosticId,
                Message = $"Can be mocked",
                Severity = DiagnosticSeverity.Warning,
                Locations =
                    new[] {
                            new DiagnosticResultLocation("Test0.cs", 23, 25)
                        }
            };

            VerifyCSharpDiagnostic(test, expected);

            var fixtest = @"
            using Microsoft.VisualStudio.TestTools.UnitTesting;
            using TestMockProjectTest;
            
            namespace TestMockUnitTests
            {
                [TestClass]
                public class UnitTest2
                {
                    private IService sut;
                    private Mock<ISubService> subServiceMock;

        [TestInitialize]
                    public void Init()
                    {
            subServiceMock = new Mock<ISubService>();
            sut = new Service(subServiceMock.Object);
                    }
            
                    [TestMethod]
                    public void TestMethod1()
                    {
            subServiceMock.Setup(x => x.DoSubSomething(It.Is<int>(doInt => doInt == default(int))));

            sut.DoSomething(2);
            subServiceMock.VerifyAll();
        }
                }
            }
            
            namespace TestMockProjectTest
            {
            
                public interface IService
                {
                    void DoSomething(int doInt);
                }
            
                public interface ISubService
                {
                    void DoSubSomething(int doInt);
                }
            
                public class Service : IService
                {
                    private readonly ISubService _subService;
            
                    public Service(ISubService subService)
                    {
                        _subService = subService;
                    }
            
                    public void DoSomething(int doInt)
                    {
                        _subService.DoSubSomething(doInt);
                    }
                }
            }";
            VerifyCSharpFix(test, fixtest);
        }


        [TestMethod]
        public void TestSetupWithReturns()
        {
            var test = @"
            using Microsoft.VisualStudio.TestTools.UnitTesting;
            using TestMockProjectTest;
            
            namespace TestMockUnitTests
            {
                [TestClass]
                public class UnitTest2
                {
                    private IService sut;
                    private Mock<ISubService> subServiceMock;

        [TestInitialize]
                    public void Init()
                    {
            subServiceMock = new Mock<ISubService>();
            sut = new Service(subServiceMock.Object);
                    }
            
                    [TestMethod]
                    public void TestMethod1()
                    {
                        sut.DoSomething(2);
                    }
                }
            }
            
            namespace TestMockProjectTest
            {
            
                public interface IService
                {
                    bool DoSomething(int doInt);
                }
            
                public interface ISubService
                {
                    bool DoSubSomething(int doInt);
                }
            
                public class Service : IService
                {
                    private readonly ISubService _subService;
            
                    public Service(ISubService subService)
                    {
                        _subService = subService;
                    }
            
                    public bool DoSomething(int doInt)
                    {
                        return _subService.DoSubSomething(doInt);
                    }
                }
            }";

            var fixtest = @"
            using Microsoft.VisualStudio.TestTools.UnitTesting;
            using TestMockProjectTest;
            
            namespace TestMockUnitTests
            {
                [TestClass]
                public class UnitTest2
                {
                    private IService sut;
                    private Mock<ISubService> subServiceMock;

        [TestInitialize]
                    public void Init()
                    {
            subServiceMock = new Mock<ISubService>();
            sut = new Service(subServiceMock.Object);
                    }
            
                    [TestMethod]
                    public void TestMethod1()
                    {
            subServiceMock.Setup(x => x.DoSubSomething(It.Is<int>(doInt => doInt == default(int)))).Returns(default(bool));

            sut.DoSomething(2);
            subServiceMock.VerifyAll();
        }
                }
            }
            
            namespace TestMockProjectTest
            {
            
                public interface IService
                {
                    bool DoSomething(int doInt);
                }
            
                public interface ISubService
                {
                    bool DoSubSomething(int doInt);
                }
            
                public class Service : IService
                {
                    private readonly ISubService _subService;
            
                    public Service(ISubService subService)
                    {
                        _subService = subService;
                    }
            
                    public bool DoSomething(int doInt)
                    {
                        return _subService.DoSubSomething(doInt);
                    }
                }
            }";
            VerifyCSharpFix(test, fixtest);
        }

        protected override CodeFixProvider GetCSharpCodeFixProvider()
        {
            return new TestMethodCodeFixProvider();
        }

        protected override DiagnosticAnalyzer GetCSharpDiagnosticAnalyzer()
        {
            return new TestMethodDiagnosticAnalyzer();
        }
    }
}