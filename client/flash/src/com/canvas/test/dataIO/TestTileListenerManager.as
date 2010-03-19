package com.canvas.test.dataIO {

import com.canvas.dataIO.TileListenerManager;
import flexunit.framework.Assert;

/**
 * Effectively tests TileListenerManager and TileListener
 */
public class TestTileListenerManager {
	protected var manager:TileListenerManager;
	
	[Before]
	public function runBeforeEach():void {
		manager = TileListenerManager.instance;
	}
	
	[Test]
	public function testProperties():void {
		manager.enabled = true;
		Assert.assertEquals(manager.enabled, true);
	}
	
	[Test]
	public function testListenerRegistration():void {
		var listener:ListenerStub = new ListenerStub();
		
		manager.registerTileListener(listener);
		manager.unregisterTileListener(listener);
	}
	
	[Test]
	public function testReset():void {
		manager.reset();
	}
}

}