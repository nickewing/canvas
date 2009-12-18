package com.canvas.dataIO {

import flash.events.EventDispatcher;

import mx.rpc.events.FaultEvent;
import mx.rpc.events.ResultEvent;

/**
 * Holds a list of TileListeners and constantly polls server for new line
 * data.  Upon receiving new line data, updates are sent to the listeners
 * 
 * See in SRS:	3.1.1.7 Request Manager
 * 				3.2.10 Requesting New Drawings from the Server
 */

public class TileListenerManager extends EventDispatcher {
	
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
		// only allow one request at a time, cancel upon replacing request
		updateService.concurrency = "last";
		
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
		if (v) {
			//trace("TLM reset by enable");
			reset();
		}
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
		updateService.disconnect();
		makeRequest();
	}
	
	/**
	 * Update listeners with response lines
	 */
	protected function updateListeners(lastUpdate:Number, lines:Array):void {
		for (var j:Number = 0; j < listeners.length; j++) {
			(listeners[j] as TileListener).lastUpdate = lastUpdate;
		}
		
		for (var i:Number = 0; i < lines.length; i++) {
			for (j = 0; j < listeners.length; j++) {
				var line:Line = lines[i] as Line;
				var listener:TileListener = listeners[j] as TileListener;
				if (line.box.intersects(listener.box)) {
					listener.handleLine(line);
				}
			}
		}
		
		for (j = 0; j < listeners.length; j++) {
			(listeners[j] as TileListener).doneHandlingLines();
		}
	}
	
	/**
	 * Open a request to the server
	 */
	protected function makeRequest():void {
		if (!_enabled) return;
		
		var tileStr:String = makeTileRequestString();
		
		if (!tileStr.length) return;
		
		//trace("new update request");
		updateService.send({
			t: tileStr
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
				listener.lastUpdate.toString()
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
		
		dispatchEvent(e);
		
		var resultParts:Array = e.result.toString().split(" ");
		switch (resultParts[0]) {
		case "OK":
			//trace("Got lines (at " + resultParts[1] + "): " + resultParts[2]);
			updateListeners(resultParts[1], Line.unserializeLineArray(resultParts[2]));
			break;
		case "TIMEOUT":
			//trace("timeout");
			break;
		}
	}
	
	/**
	 * Handle update fault
	 */
	protected function handleUpdateFault(e:FaultEvent):void {
		dispatchEvent(e);
		trace("update failure");
	}
}

}