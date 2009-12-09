package com.canvas.dataIO {

import com.canvas.gui.CanvasTile;

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
	protected var _lastUpdate:Date;
	
	//---------------------------------------------------------------------
	//
	//  Constructor
	//
	//---------------------------------------------------------------------
	
	public function CanvasTileListener(initTile:CanvasTile) {
		_lastUpdate = new Date();
		_lastUpdate.setTime(0);
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
	public function get lastUpdate():Date {
		return _lastUpdate;
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
}

}