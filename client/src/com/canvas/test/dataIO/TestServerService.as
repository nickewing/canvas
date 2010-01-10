package com.canvas.test.dataIO {

import com.canvas.dataIO.ServerService;

import flexunit.framework.Assert;
	
public class TestServerService {
	protected var service:ServerService;
	
	[Before]
	public function runBeforeEach():void {
		service = new ServerService("test");
	}
	
	[Test]
	public function testProperties():void {
		Assert.assertEquals(service.url, "http://canvas.nickewing.net:8000/test");
		Assert.assertEquals(service.resultFormat, "text");
		Assert.assertEquals(service.method, "POST");
	}
}

}