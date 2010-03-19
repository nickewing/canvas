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
	
	[Test]
	public function testUnserializeLineArray():void {
		var str:String = "-10,0,100.1,50.99/10039704/20;10,40,50.1,50.99/0/1";
		var arr:Array  = Line.unserializeLineArray(str);
		
		var line1:Line = arr[0] as Line;
		var line2:Line = arr[1] as Line;
		
		Assert.assertEquals(10039704, line1.paintColor);
		Assert.assertEquals(0,        line2.paintColor);
		Assert.assertEquals(20,       line1.brushSize);
		Assert.assertEquals(1,        line2.brushSize);
		
		Assert.assertEquals(-10,      line1.points[0]);
		Assert.assertEquals(0,        line1.points[1]);
		Assert.assertEquals(100.1,    line1.points[2]);
		Assert.assertEquals(50.99,    line1.points[3]);
		
		Assert.assertEquals(10,	      line2.points[0]);
		Assert.assertEquals(40,		  line2.points[1]);
		Assert.assertEquals(50.1,     line2.points[2]);
		Assert.assertEquals(50.99,    line2.points[3]);
	}
}

}