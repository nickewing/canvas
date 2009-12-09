package com.canvas.test.dataIO {

import com.canvas.dataIO.Box;
import flexunit.framework.Assert;

public class TestBox {
	private var box:Box;
	
	[Before]
	public function runBeforeEach():void {
		box = new Box(-100, -100, 100, 100);
	}
	
	[Test]
	public function testShift():void {
		box.shift(5, 5);
		// all coordinate values should be 5 now
		Assert.assertEquals(box.x, -95);
		Assert.assertEquals(box.y, -95);
		Assert.assertEquals(box.x1, 105);
		Assert.assertEquals(box.y1, 105);
	}
	
	[Test]
	public function testProperties():void {
		box.x  = box.y  = -10;
		box.x1 = box.y1 = 10;
		
		Assert.assertEquals(box.x, -10);
		Assert.assertEquals(box.y, -10);
		Assert.assertEquals(box.x1, 10);
		Assert.assertEquals(box.y1, 10);
		Assert.assertEquals(box.centerX, 0);
		Assert.assertEquals(box.centerY, 0);
	}
	
	[Test]
	public function testSerialize():void {
		Assert.assertEquals("-100,-100,100,100", box.serialize());
	}
}

}