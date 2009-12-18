package com.canvas.test.dataIO {

import com.canvas.dataIO.Box;
import com.canvas.dataIO.Line;
import com.canvas.dataIO.TileListener;

public class ListenerStub implements TileListener {
	public function ListenerStub() {
	}
	
	public function handleLine(l:Line):void {
	}
	
	public function doneHandlingLines():void {
	}
	
	public function get box():Box {
		return new Box();
	}
	
	public function get lastUpdate():Number {
		return 0;
	}
	
	public function set lastUpdate(v:Number):void {
	}
}

}