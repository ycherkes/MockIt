using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using MockIt.Test.Helpers;
using System;
using System.Diagnostics;
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
                Message = "Can be mocked",
                Severity = DiagnosticSeverity.Warning,
                Locations = new[] { new DiagnosticResultLocation("Test0.cs", 23, 25) }
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
            subServiceMock.Setup(x => x.DoSubSomething(It.IsAny<int>()))
            .Callback<int>(doInt => { });

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
        public void TestSetupWithoutReturnsNoDiagnosticsInCodeBlock()
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
                         subServiceMock.Setup(x => x.DoSubSomething(It.IsAny<int>()))
                                       .Callback<int>(doInt => { });
              
                         { 
                              sut.DoSomething(2); 
                         }

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
            };";

            VerifyCSharpDiagnostic(test, Array.Empty<DiagnosticResult>());
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
                    private IService _sut;
                    private Mock<ISubService> _subServiceMock;

                    [TestInitialize]
                    public void Init()
                    {
                         _subServiceMock = new Mock<ISubService>();
                         _sut = new Service(_subServiceMock.Object);
                    }
            
                    [TestMethod]
                    public void TestMethod1()
                    {
                        _sut.DoSomething(2);
                    }

                    [TestMethod]
                    public void TestMethod2()
                    {
                        var subServiceMock = new Mock<ISubService>();
                        var sut = new Service(subServiceMock.Object);
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
                    Task<bool> DoSubSomething(int doInt);
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
                    private IService _sut;
                    private Mock<ISubService> _subServiceMock;

                    [TestInitialize]
                    public void Init()
                    {
                         _subServiceMock = new Mock<ISubService>();
                         _sut = new Service(_subServiceMock.Object);
                    }
            
                    [TestMethod]
                    public void TestMethod1()
                    {
            _subServiceMock.Setup(x => x.DoSubSomething(It.IsAny<int>()))
            .Callback<int>(doInt => { })
            .ReturnsAsync(default(bool));

            _sut.DoSomething(2);

            _subServiceMock.VerifyAll();
        }

                    [TestMethod]
                    public void TestMethod2()
                    {
                        var subServiceMock = new Mock<ISubService>();
                        var sut = new Service(subServiceMock.Object);
            subServiceMock.Setup(x => x.DoSubSomething(It.IsAny<int>()))
            .Callback<int>(doInt => { })
            .ReturnsAsync(default(bool));

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
                    Task<bool> DoSubSomething(int doInt);
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

        [TestMethod]
        public void TestCreateMockChainOfCalls()
        {
            var test = @"
            using Microsoft.VisualStudio.TestTools.UnitTesting;
            using TestMockProjectTest;
            
            namespace TestMockUnitTests
            {
                [TestClass]
                public class UnitTest2
                {
                    private IService _sut;
                    private Mock<ISubService> _subServiceMock;
            
                    [TestInitialize]
                    public void Init()
                    {
                        _subServiceMock = new Mock<ISubService>();
                        _subServiceMock.Setup(s => s.DoSubSomething(It.IsAny<int>())).Returns(_subServiceMock.Object);
                        _sut = new Service(_subServiceMock.Object);
                    }
            
                    [TestMethod]
                    public void TestMethod1()
                    {
                        var result = _sut.DoSomething(2);
                    }
                }
            }
            
            namespace TestMockProjectTest
            {
            
                public interface IService
                {
                    IService DoSomething(int doInt);
                }
            
                public interface ISubService
                {
                    ISubService DoSubSomething(int doInt);
                }
            
                public class Service : IService
                {
                    private readonly ISubService _subService;
            
                    public Service(ISubService subService)
                    {
                        _subService = subService;
                    }
            
                    public ISubService DoSomething(int doInt)
                    {
                        return _subService.DoSubSomething(doInt);
                    }
                }
            }";
            VerifyCSharpDiagnostic(test, Array.Empty<DiagnosticResult>());
        }

        [DebuggerStepThrough]
        protected override CodeFixProvider GetCSharpCodeFixProvider()
        {
            return new TestMethodCodeFixProvider();
        }

        [DebuggerStepThrough]
        protected override DiagnosticAnalyzer GetCSharpDiagnosticAnalyzer()
        {
            return new TestMethodDiagnosticAnalyzer();
        }
    }
}