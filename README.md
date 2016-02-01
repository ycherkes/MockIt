# MockIt
A simple Visual Studio 2015 extension to automate of creating and configuring mocks in the unit tests

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

Plans: 

simple events support

![alt tag](https://raw.githubusercontent.com/ycherkes/MockIt/master/MockIto.gif)
