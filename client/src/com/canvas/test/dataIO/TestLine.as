package com.canvas.test.dataIO {

import com.canvas.dataIO.Line;
import flexunit.framework.Assert;

public class TestLine {
	protected var line:Line;
	
	[Before]
	public function beforeEachRun():void {
		line = new Line();
	}
	
	[Test]
	public function testCreate():void {
		Assert.assertEquals(0, line.pointCount);
		Assert.assertEquals(0, line.paintColor);
		Assert.assertEquals(1, line.brushSize);
	}
	
	[Test]
	public function testAddPoint():void {
		line.addPoint(0,0);
		Assert.assertEquals(1, line.pointCount);
		Assert.assertEquals(0, line.points[line.points.length - 1]);
		Assert.assertEquals(0, line.points[line.points.length - 2]);
		
		line.addPoint(100.0,100.0);
		Assert.assertEquals(2, line.pointCount);
		Assert.assertEquals(100.0, line.points[line.points.length - 1]);
		Assert.assertEquals(100.0, line.points[line.points.length - 2]);
	}
	
	[Test]
	public function testSerialize():void {
		line.addPoint(-10, 0);
		line.addPoint(100.1, 50.99);
		
		line.brushSize = 20;
		line.paintColor = 10039704;
		
		Assert.assertEquals("-10,0,100.1,50.99/10039704/20", line.serialize());
	}
}

}