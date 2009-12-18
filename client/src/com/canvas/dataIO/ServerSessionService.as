package com.canvas.dataIO {

import mx.rpc.AsyncToken;

/**
 * Wrapper for all sever requests after connected to a session.
 * Automatically includes session id in requests.
 */
public class ServerSessionService extends ServerService {
	//---------------------------------------------------------------------
	//
	//  Constructor
	//
	//---------------------------------------------------------------------
	
	public function ServerSessionService(name:String, rootURL:String=null, destination:String=null) {
		super(name, rootURL, destination);
	}
	
	//---------------------------------------------------------------------
	//
	//  Properties
	//
	//---------------------------------------------------------------------
	
	public function get ready():Boolean { return !!SessionManager.instance.joined; };
	
	//---------------------------------------------------------------------
	//
	//  Methods
	//
	//---------------------------------------------------------------------
	
	/**
	 * Override send method to add SID to every request
	 */
	override public function send(parameters:Object = null):AsyncToken {
		if (ready)
			parameters.sid = SessionManager.instance.sid;
		else
			throw "Session not connected.";
		return super.send(parameters);
	}
}

}