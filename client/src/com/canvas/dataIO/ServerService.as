package com.canvas.dataIO {
	
import mx.rpc.http.HTTPService;

public class ServerService extends HTTPService {
	//---------------------------------------------------------------------
	//
	//  Constants
	//
	//---------------------------------------------------------------------
	
	/**
	 * Server's host name
	 */
	protected const SERVER_HOST:String = "localhost";
	
	/**
	 * Port of server
	 */
	protected const SERVER_PORT:Number = 8000;
	
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