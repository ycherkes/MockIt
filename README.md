# [![Made in Ukraine](https://img.shields.io/badge/made_in-ukraine-ffd700.svg?labelColor=0057b7&style=for-the-badge)](https://stand-with-ukraine.pp.ua) [Stand with the people of Ukraine: How to Help](https://stand-with-ukraine.pp.ua)

<img src="https://yevhencherkes.gallerycdn.vsassets.io/extensions/yevhencherkes/mockit/2.0.0.0/1667833072887/Microsoft.VisualStudio.Services.Icons.Default" width="100" height="100" />

# MockIt

[![marketplace](https://img.shields.io/visual-studio-marketplace/v/YevhenCherkes.MockIt.svg?label=Marketplace&style=for-the-badge)](https://marketplace.visualstudio.com/items?itemName=YevhenCherkes.MockIt)
[![marketplace downloads](https://img.shields.io/visual-studio-marketplace/d/YevhenCherkes.MockIt?label=MarketPlace%20Downloads&style=for-the-badge)](https://marketplace.visualstudio.com/items?itemName=YevhenCherkes.MockIt)
[![nuget downloads](https://img.shields.io/nuget/dt/MockIt?label=NuGet%20Downloads&style=for-the-badge)](https://www.nuget.org/packages/MockIt)

[![License: Apache 2.0](https://img.shields.io/github/license/ycherkes/MockIt?style=for-the-badge)](https://github.com/ycherkes/MockIt/blob/master/LICENSE.txt)

A Diagnostic Analyzer and CodeFix Provider aimed to automate mocks creating and configuring.

You can use it as a [Visual Studio Extension](https://marketplace.visualstudio.com/items?itemName=YevhenCherkes.MockIt) or install as a [NuGet package](https://www.nuget.org/packages/MockIt).

For Visual Studio Code you should install a [NuGet package](https://www.nuget.org/packages/MockIt) and enable [Roslyn Analyzers](https://www.strathweb.com/2019/04/roslyn-analyzers-in-code-fixes-in-omnisharp-and-vs-code/), see [Example project](https://github.com/ycherkes/MockIt/tree/master/MockIt/Example).

Additionally, you can [configure variable and field name templates](https://github.com/ycherkes/MockIt/blob/master/MockIt/Example/.editorconfig#L4).

**Quick introduction**:

In the **Constructor** or **TestInitialize** (**SetUp** for NUnit) marked method create the instance of your **Service Under the Test** (SUT).

After that you can see code analysis error and the next suggestions (lightbulbs):
 - to create a constructor without parameters
 - to fill with mocks
	
Choose "**Fill with mocks**" - it will create the mocks required for your service and put them into constructor.

Create **TestMethod** (**Test** for NUnit) marked method and write here a call of any methods of your SUT.

If SUT's method uses any of injected instances Visual Studio shows you code analysis info.

Choose "**Setup mocks with callbacks**" or "**Setup mocks**" - it will setup the mock's methods and calls VerifyAll for each of them.

After that change all **default(type)** expressions to values you want.

![alt tag](https://raw.githubusercontent.com/ycherkes/MockIt/master/Mockit.gif)

This tool has been working well for my own personal needs, but outside that its future depends on your feedback. Feel free to [open an issue](https://github.com/ycherkes/MockIt/issues).

[![PayPal](https://img.shields.io/badge/Donate-PayPal-ffd700.svg?labelColor=0057b7&style=for-the-badge)](https://www.paypal.com/donate/?business=KXGF7CMW8Y8WJ&no_recurring=0&item_name=Help+MockIt+library+become+better.&currency_code=USD)

Any donations during this time will be directed to local charities at my own discretion.

**Privacy Notice:** No personal data is collected at all.
