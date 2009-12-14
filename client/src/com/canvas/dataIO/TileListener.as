package com.canvas.dataIO {

public interface TileListener {
	//---------------------------------------------------------------------
	//
	//  Properties
	//
	//---------------------------------------------------------------------
	
	/**
	 * Get box to listen for
	 */
	function get box():Box;
	
	/**
	 * Get time of last update received from server
	 */
	function get lastUpdate():Number;
	
	/**
	 * Set time of last update recieved from server
	 */
	function set lastUpdate(v:Number):void;
	
	//---------------------------------------------------------------------
	//
	//  Methods
	//
	//---------------------------------------------------------------------
	
	/**
	 * Handle new lines fetched from 
	 */
	function handleLine(lines:Line):void
		
	/**
	 * Done receiving lines
	 */
	function doneHandlingLines():void
}

}