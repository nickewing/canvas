package com.canvas.dataIO {

import mx.rpc.events.FaultEvent;
import mx.rpc.events.ResultEvent;

public class TileListenerManager {
	
	//---------------------------------------------------------------------
	//
	//  Variables
	//
	//---------------------------------------------------------------------
	
	/**
	 * List of current listeners
	 */
	protected var listeners:Array = [];
	
	/**
	 * Service for requesting lines from the server
	 */
	protected var updateService:ServerSessionService = new ServerSessionService("update");
	
	/**
	 * Whether or not tile manager will send requests to the server
	 */
	protected var _enabled:Boolean = false;
	
	//---------------------------------------------------------------------
	//
	//  Class Variables
	//
	//---------------------------------------------------------------------

	/**
	 * Singleton instance
	 */
	protected static var _instance:TileListenerManager;
	
	//---------------------------------------------------------------------
	//
	//  Constructor
	//
	//---------------------------------------------------------------------
	
	public function TileListenerManager() {
		// setup event handlers for the service
		updateService.addEventListener(ResultEvent.RESULT, handleUpdateResponse);
		updateService.addEventListener(FaultEvent.FAULT, handleUpdateFault);
	}
	
	//---------------------------------------------------------------------
	//
	//  Class Properties
	//
	//---------------------------------------------------------------------
	
	/**
	 * Return the singleton instance, creating it if needed
	 */
	public static function get instance():TileListenerManager {
		if (_instance == null)
			_instance = new TileListenerManager();
		return _instance;
	}
	
	//---------------------------------------------------------------------
	//
	//  Properties
	//
	//---------------------------------------------------------------------
	
	public function set enabled(v:Boolean):void {
		_enabled = v;
		if (v)
			reset();
	}
	public function get enabled():Boolean { return _enabled; }
	
	//---------------------------------------------------------------------
	//
	//  Methods
	//
	//---------------------------------------------------------------------
	
	/**
	 * Add a listener
	 */
	public function registerTileListener(listener:TileListener):void {
		if (listeners.indexOf(listener) == -1)
			listeners.push(listener);
	}
	
	/**
	 * Remove a listener
	 */
	public function unregisterTileListener(listener:TileListener):void {
		var pos:Number = listeners.indexOf(listener);
		if (pos == -1)
			return;
		listeners.splice(pos, 1);
	}
	
	/**
	 * Reset the manager's connection to the server
	 */
	public function reset():void {
		updateService.cancel();
		makeRequest();
	}
	
	/**
	 * Update listeners with response lines
	 */
	protected function updateListeners(lines:Array):void {
		for (var i:Number = 0; i < lines.length; i++) {
			for (var j:Number = 0; j < listeners.length; j++) {
				var line:Line = lines[i] as Line;
				var listener:TileListener = listeners[j] as TileListener;
				if (line.box.intersects(listener.box))
					listener.handleLine(line);
			}
		}
	}
	
	/**
	 * Open a request to the server
	 */
	protected function makeRequest():void {
		if (!_enabled) return;
		
		updateService.send({
			t: makeTileRequestString()
		});
	}
	
	/**
	 * Make a request string of the tiles in the format accepted by the server
	 */
	protected function makeTileRequestString():String {
		var tilesStrings:Array = [];
		
		for (var i:Number = 0; i < listeners.length; i++) {
			var listener:TileListener = listeners[i] as TileListener
			tilesStrings.push(
				listener.box.serialize() + "/" +
				listener.lastUpdate.getTime().toString()
			);
		}
		
		return tilesStrings.join(";");
	}

	//---------------------------------------------------------------------
	//
	//  Methods: Event handling
	//
	//---------------------------------------------------------------------
	
	/**
	 * Handle update response from server
	 */
	protected function handleUpdateResponse(e:ResultEvent):void {
		makeRequest();
		
		var resultParts:Array = e.result.toString().split(" ");
		switch (resultParts[0]) {
		case "OK":
			trace("Got some lines");
			//trace("Got lines: " + resultParts[1]);
			updateListeners(Line.unserializeLineArray(resultParts[1]));
			break;
		case "TIMEOUT":
			trace("timeout");
			break;
		}
	}
	
	/**
	 * Handle update fault
	 */
	protected function handleUpdateFault(e:FaultEvent):void {
		makeRequest();
		trace("update failure");
	}
}

}