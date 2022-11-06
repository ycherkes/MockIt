using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using MockIt.Test.Helpers;
using MockIt.Test.Verifiers;
using System.Diagnostics;

namespace MockIt.Test
{
    [TestClass]
    public class TestInitializeUnitTests : CodeFixVerifier
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
        public void TestCreateMock()
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
            
                    [TestInitialize]
                    public void Init()
                    {
                        _sut = new Service();
                    }
            
                    [TestMethod]
                    public void TestMethod1()
                    {
                        _sut.DoSomething(2);
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
                Id = TestInitializeDiagnosticAnalyzer.DiagnosticId,
                Message = "Can be mocked",
                Severity = DiagnosticSeverity.Info,
                Locations =
                    new[] {
                            new DiagnosticResultLocation("Test0.cs", 15, 25)
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
                    private IService _sut;
        private Mock<ISubService> _mockSubService;

        [TestInitialize]
                    public void Init()
                    {
            _mockSubService = new Mock<ISubService>();
            _sut = new Service(_mockSubService.Object);
                    }
            
                    [TestMethod]
                    public void TestMethod1()
                    {
                        _sut.DoSomething(2);
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

        //Diagnostic and CodeFix both triggered and checked for
        [TestMethod]
        public void TestMethodCreateMock()
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

                    [TestInitialize]
                    public void TestInitialize()
                    {
                        _sut = new Service();
                    }

                    [TestMethod]
                    public void TestMethod1()
                    {
                        var sut = new Service();
                        _sut.DoSomething(2);
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
            var expected1 = new DiagnosticResult
            {
                Id = TestInitializeDiagnosticAnalyzer.DiagnosticId,
                Message = "Can be mocked",
                Severity = DiagnosticSeverity.Info,
                Locations =
                    new[] {
                            new DiagnosticResultLocation("Test0.cs", 15, 25)
                        }
            };

            var expected2 = new DiagnosticResult
            {
                Id = TestInitializeDiagnosticAnalyzer.DiagnosticId,
                Message = "Can be mocked",
                Severity = DiagnosticSeverity.Info,
                Locations =
                    new[] {
                            new DiagnosticResultLocation("Test0.cs", 21, 33)
                        }
            };

            VerifyCSharpDiagnostic(test, expected1, expected2);

            var fixtest = @"
            using Microsoft.VisualStudio.TestTools.UnitTesting;
            using TestMockProjectTest;
            
            namespace TestMockUnitTests
            {
                [TestClass]
                public class UnitTest2
                {
                    private IService _sut;
        private Mock<ISubService> _mockSubService;

        [TestInitialize]
                    public void TestInitialize()
                    {
            _mockSubService = new Mock<ISubService>();
            _sut = new Service(_mockSubService.Object);
                    }

                    [TestMethod]
                    public void TestMethod1()
                    {
            var mockSubService = new Mock<ISubService>();
            var sut = new Service(mockSubService.Object);
                        _sut.DoSomething(2);
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

        //test to mock the generics 
        [TestMethod]
        public void TestCreateMockOfGenericService()
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
            
                    [TestInitialize]
                    public void Init()
                    {
                        sut = new Service();
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
            
                public interface ISubService<T>
                {
                    void DoSubSomething(int doInt);
                }
            
                public class Service : IService
                {
                    private readonly ISubService<string> _subService;
            
                    public Service(ISubService<string> subService)
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
                Id = TestInitializeDiagnosticAnalyzer.DiagnosticId,
                Message = "Can be mocked",
                Severity = DiagnosticSeverity.Info,
                Locations =
                    new[] {
                            new DiagnosticResultLocation("Test0.cs", 15, 25)
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
        private Mock<ISubService<string>> _mockSubService;

        [TestInitialize]
                    public void Init()
                    {
            _mockSubService = new Mock<ISubService<string>>();
            sut = new Service(_mockSubService.Object);
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
            
                public interface ISubService<T>
                {
                    void DoSubSomething(int doInt);
                }
            
                public class Service : IService
                {
                    private readonly ISubService<string> _subService;
            
                    public Service(ISubService<string> subService)
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
        public void TestCreateMockxUnit()
        {
            var test = @"
            using Microsoft.VisualStudio.TestTools.UnitTesting;
            using TestMockProjectTest;
            
            namespace TestMockUnitTests
            {
                public class UnitTest2
                {
                    private IService sut;
            
                    public UnitTest2()
                    {
                        sut = new Service();
                    }
            
                    [Fact]
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
                Id = TestInitializeDiagnosticAnalyzer.DiagnosticId,
                Message = "Can be mocked",
                Severity = DiagnosticSeverity.Info,
                Locations =
                    new[] {
                            new DiagnosticResultLocation("Test0.cs", 13, 25)
                        }
            };

            VerifyCSharpDiagnostic(test, expected);

            var fixtest = @"
            using Microsoft.VisualStudio.TestTools.UnitTesting;
            using TestMockProjectTest;
            
            namespace TestMockUnitTests
            {
                public class UnitTest2
                {
                    private IService sut;
        private Mock<ISubService> subServiceMock;

        public UnitTest2()
                    {
            subServiceMock = new Mock<ISubService>();

            sut = new Service(subServiceMock.Object);
                    }
            
                    [Fact]
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
            VerifyCSharpFix(test, fixtest);
        }

        [DebuggerStepThrough]
        protected override CodeFixProvider GetCSharpCodeFixProvider()
        {
            return new TestInitializeCodeFixProvider();
        }

        [DebuggerStepThrough]
        protected override DiagnosticAnalyzer GetCSharpDiagnosticAnalyzer()
        {
            return new TestInitializeDiagnosticAnalyzer();
        }
    }
}