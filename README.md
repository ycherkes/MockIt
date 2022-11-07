# MockIt
A Visual Studio extension which provides a Diagnostic Analyzer and CodeFix Provider to automate mocks creating and configuring.

It helps you to automate mocks creating in your unit tests.

How to use it:

In the **TestInitialize** (**SetUp** for NUnit) marked method create the instance of your Service under the test (sut).

After that you can see code analysis error and the next suggestions (lightbulbs):
	to create a constructor without parameters
	to fill with mocks
	
Choose "**Fill with mocks**" - it will create the mocks required for your service and put them into constructor.

Create **TestMethod** (**Test** for NUnit) marked method and write here a call of any methods of your sut.

If sut's method uses any of injected instances Visual Studio shows your code analysis info.

Choose "**Setup mocks with callbacks**" or "**Setup mocks**" - it will setup the mock's methods and calls VerifyAll for each of them.

After that change all **default(type)** expressions to values you want.

![alt tag](https://raw.githubusercontent.com/ycherkes/MockIt/master/Mockit.gif)