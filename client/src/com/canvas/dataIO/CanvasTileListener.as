package com.canvas.dataIO {

import com.canvas.gui.components.CanvasTile;

public class CanvasTileListener implements TileListener {
	
	//---------------------------------------------------------------------
	//
	//  Variables
	//
	//---------------------------------------------------------------------
	
	/**
	 * Time of last update from server
	 */
	protected var tile:CanvasTile;
	
	/**
	 * Time of last update from server
	 */
	protected var _lastUpdate:Number;
	
	//---------------------------------------------------------------------
	//
	//  Constructor
	//
	//---------------------------------------------------------------------
	
	public function CanvasTileListener(initTile:CanvasTile) {
		_lastUpdate = 0;
		tile = initTile;
	}
	
	//---------------------------------------------------------------------
	//
	//  Properties
	//
	//---------------------------------------------------------------------
	
	/**
	 * Get CanvasTile box
	 */
	public function get box():Box {
		return tile.box;
	}
	
	/**
	 * Get last update from server
	 */
	public function get lastUpdate():Number {
		return _lastUpdate;
	}
	
	/**
	 * Set time of last update recieved from server
	 */
	public function set lastUpdate(v:Number):void {
		_lastUpdate = v;
	}
	
	//---------------------------------------------------------------------
	//
	//  Methods
	//
	//---------------------------------------------------------------------
	
	
	
	/**
	 * Handle new lines fetched from 
	 */
	public function handleLine(line:Line):void {
		for (var i:Number = 2; i < line.points.length; i += 2) {
			tile.drawLine(
				line.points[i - 2] - tile.box.x,
				line.points[i - 1] - tile.box.y,
				line.points[i]     - tile.box.x,
				line.points[i + 1] - tile.box.y,
				line.paintColor,
				line.brushSize
			);
		}
	}
	
	/**
	 * Done receiving lines
	 */
	public function doneHandlingLines():void {
		tile.shaded = false;
	}
}

}