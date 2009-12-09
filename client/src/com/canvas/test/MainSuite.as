package com.canvas.test
{
	import com.canvas.test.dataIO.TestBox;
	import com.canvas.test.dataIO.TestLine;
	import com.canvas.test.dataIO.TestTileListenerManager;
	
	[Suite]
	[RunWith("org.flexunit.runners.Suite")]
	public class MainSuite
	{
		public var test1:TestBox;
		public var test2:TestTileListenerManager;
		public var test3:TestLine;
	}
}