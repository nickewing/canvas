package com.canvas.dataIO {
	
import mx.rpc.http.HTTPService;
import mx.core.FlexGlobals;

/**
 * Wrapper for all server requests 
 */

public class ServerService extends HTTPService {
	//---------------------------------------------------------------------
	//
	//  Constants
	//
	//---------------------------------------------------------------------
	
	/**
	 * Default server's host name
	 */
	public const DEFAULT_HOST:String = "canvas.nickewing.net:8000";
	
	//---------------------------------------------------------------------
	//
	//  Constructor
	//
	//---------------------------------------------------------------------
	
	public function ServerService(name:String, rootURL:String = null, destination:String = null) {
		super(rootURL, destination);
		
		url          = "http://" + server_host + "/" + name;
		resultFormat = "text";
		method       = "POST";
	}
	
	//---------------------------------------------------------------------
	//
	//  Properties
	//
	//---------------------------------------------------------------------
	
	private function get server_host():String {
		return FlexGlobals.topLevelApplication.parameters.server ?
				FlexGlobals.topLevelApplication.parameters.server :
				DEFAULT_HOST;
	}
}

}