# MockIt
A simple Visual Studio 2015 extension to automate of creating and configuring mocks in the unit tests

It helps you to automate mocks creating in your unit tests.

How to use it:

In the TestInitialize or SetUp marked method create the instance of your Service under test (sut).

After that you can see code analysis error and the next suggestions (bulb):

to create a constructor without parameters
to make mock
 Choose "Make mock" - it will create the mocks required for your service and put them into constructor.

 Create TestMethod or Test marked method and write here a call of any methods of your sut.

 If sut's method uses any of injected instances Visual Studio shows your code analysis info.

 Choose "Make mock" - it will setup the mock's methods and calls VerifyAll for each of them.

It currently supports properties partially (setups generates for both get and set methods without analysis uses it or no)

![alt tag](https://raw.githubusercontent.com/ycherkes/MockIt/master/MockIto.gif)
