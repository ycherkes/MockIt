# MockIt
A Visual Studio extension to automate the mocks creating and configuring in the unit tests

It helps you to automate mocks creating in your unit tests.

Similar tasks can be solved by using Moq.AutoMocker but if you like to do it manually this tool helps you.

How to use it:

In the **TestInitialize** (**SetUp** for NUnit) marked method create the instance of your Service under the test (sut).

After that you can see code analysis error and the next suggestions (lightbulbs):
	to create a constructor without parameters
	to make mock
	
Choose "**Make mock**" - it will create the mocks required for your service and put them into constructor.

Create **TestMethod** (**Test** for NUnit) marked method and write here a call of any methods of your sut.

If sut's method uses any of injected instances Visual Studio shows your code analysis info.

Choose "**Make mock**" - it will setup the mock's methods and calls VerifyAll for each of them.

After that change all **default(type)** expressions to values you want.

Using a hotkeys: **F8** (Go to nex error-warning-info) or **F12** with Resharper -> **Ctrl + .** (Show potential fixes - Smart Tags  menu) or **Alt + Enter** with Resharper -> **m** (select "Make mock" in the menu) -> **Enter**
 
Release History:

 * v 1.0.0.10 Fixed mocks detection when mocked classes/interfaces are declared in different assembly than sut
 * v 1.0.0.17 Partial support the chains of mocks
 * v 1.0.0.19 Using statement support
 * v 1.0.0.21 Bugs fixing
 * v 1.0.0.22 xUnit.Net added to supportable test frameworks
 * v 1.0.0.23 Visual Studio 2017 compatibility, bugs fixing

Plans:

 * Add support for factories via delegates and Func<Tresult>
 * Strict injected vars tracking (currently only by type of var)
 * Add support for Moq.AutoMocker
 
![alt tag](https://raw.githubusercontent.com/ycherkes/MockIt/master/MockIto.gif)

Sample for the factories mocking and using statement:

![alt tag](https://raw.githubusercontent.com/ycherkes/MockIt/master/MockItFactories.gif)