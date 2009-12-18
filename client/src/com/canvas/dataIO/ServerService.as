package com.canvas.dataIO {
	
import mx.rpc.http.HTTPService;

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
	 * Server's host name
	 */
	public const SERVER_HOST:String = "canvas.nickewing.net";
	
	/**
	 * Port of server
	 */
	public const SERVER_PORT:Number = 80;
	
	//---------------------------------------------------------------------
	//
	//  Constructor
	//
	//---------------------------------------------------------------------
	
	public function ServerService(name:String, rootURL:String = null, destination:String = null) {
		super(rootURL, destination);
		
		url          = "http://" + SERVER_HOST + ":" + SERVER_PORT + "/" + name;
		resultFormat = "text";
		method       = "POST";
	}
}

}