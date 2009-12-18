package com.canvas.dataIO {

import flash.events.Event;
import flash.events.EventDispatcher;

import mx.rpc.events.FaultEvent;
import mx.rpc.events.ResultEvent;

/**
 * Manages client session
 * 
 * See in SRS:	3.1.1.7 Request Manager
 * 				3.2.1 Joining the System
 * 				3.2.2 Leaving the System
 */

public class SessionManager extends EventDispatcher {
	//---------------------------------------------------------------------
	//
	//  Variables
	//
	//---------------------------------------------------------------------
	
	/**
	 * Whether session manager has joined the server yet
	 */
	protected var _joined:Boolean = false;
	
	/**
	 * List of current listeners
	 */
	protected var _sid:String;
	
	/**
	 * Service for joining the server
	 */
	protected var joinService:ServerService = new ServerService("join");
	
	//---------------------------------------------------------------------
	//
	//  Class Variables
	//
	//---------------------------------------------------------------------
	
	/**
	 * Eager singleton instance
	 */
	protected static var _instance:SessionManager;
	
	//---------------------------------------------------------------------
	//
	//  Constructor
	//
	//---------------------------------------------------------------------
	
	public function SessionManager() {
		joinService.addEventListener(ResultEvent.RESULT, handleJoinResponse);
		joinService.addEventListener(FaultEvent.FAULT, handleJoinFault);
	}
	
	//---------------------------------------------------------------------
	//
	//  Class Methods
	//
	//---------------------------------------------------------------------
	
	/**
	 * Return the singleton instance, creating it if needed
	 */
	public static function get instance():SessionManager {
		if (_instance == null)
			_instance = new SessionManager();
		return _instance;
	}
	
	/**
	 * Start the manager
	 */
	public static function start():void {
		instance;
	}
	
	//---------------------------------------------------------------------
	//
	//  Properties
	//
	//---------------------------------------------------------------------
	
	public function get sid():String { return _sid; }
	
	public function get joined():Boolean { return _joined; }
	
	//---------------------------------------------------------------------
	//
	//  Methods
	//
	//---------------------------------------------------------------------
	
	public function join():void {
		sendJoinRequest();
	}
	
	/**
	 * Send request to join to server
	 */
	protected function sendJoinRequest():void {
		joinService.send();
	}
	
	//---------------------------------------------------------------------
	//
	//  Methods: Event handling
	//
	//---------------------------------------------------------------------
	
	/**
	 * Handle join response from server
	 */
	protected function handleJoinResponse(e:ResultEvent):void {
		_sid = (e.result as String).split(" ")[1];
		_joined = true;
		
		dispatchEvent(new Event(Event.CONNECT));
		
		trace("joined and got SID: " + _sid);
	}
	
	/**
	 * Handle join fault
	 */
	protected function handleJoinFault(e:FaultEvent):void {
		dispatchEvent(e);
		
		trace("failed to join");
	}
}

}