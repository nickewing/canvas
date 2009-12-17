package com.canvas.dataIO {

import flash.events.TimerEvent;
import flash.utils.Timer;

import mx.rpc.events.FaultEvent;
	
public class SendLineManager {
	//---------------------------------------------------------------------
	//
	//  Instance constants
	//
	//---------------------------------------------------------------------
	
	/**
	 * Interval time to cut line whilst drawing
	 */
	protected const LINE_CUT_INTERVAL:Number = 300;
	
	//---------------------------------------------------------------------
	//
	//  Variables
	//
	//---------------------------------------------------------------------
	
	/**
	 * Current line being drawn
	 */
	protected var _currentLine:Line = new Line();
	
	/**
	 * Send line service
	 */
	protected var sendLineService:ServerSessionService = new ServerSessionService("send_line");
	
	/**
	 * Line cut interval
	 */
	protected var lineCutTimer:Timer;
	
	//---------------------------------------------------------------------
	//
	//  Class Variables
	//
	//---------------------------------------------------------------------
	
	/**
	 * Singleton instance
	 */
	protected static var _instance:SendLineManager;
	
	//---------------------------------------------------------------------
	//
	//  Constructor
	//
	//---------------------------------------------------------------------
	
	public function SendLineManager() {
		lineCutTimer = new Timer(LINE_CUT_INTERVAL, 0);
		
		lineCutTimer.addEventListener(TimerEvent.TIMER, lineCut);
		
		sendLineService.addEventListener(FaultEvent.FAULT, sendLineFault);
	}
	
	//---------------------------------------------------------------------
	//
	//  Class Methods
	//
	//---------------------------------------------------------------------
	
	/**
	 * Return the singleton instance, creating it if needed
	 */
	public static function get instance():SendLineManager {
		if (_instance == null)
			_instance = new SendLineManager();
		return _instance;
	}
	
	//---------------------------------------------------------------------
	//
	//  Properties
	//
	//---------------------------------------------------------------------
	
	public function get currentLine():Line { return _currentLine; }
	
	//---------------------------------------------------------------------
	//
	//  Methods
	//
	//---------------------------------------------------------------------
	
	/**
	 * Send the serialized current line to the server
	 */
	protected function sendLine():void {
		//trace("sent line");
		sendLineService.send({
			l: _currentLine.serialize()
		});
	}
	
	/**
	 * Add a point to the lines points
	 */
	public function addPoint(x:Number, y:Number):void {
		if (!_currentLine.pointCount) {
			lineCutTimer.reset();
			lineCutTimer.start();
		}
		
		_currentLine.addPoint(x, y);
	}
	
	/**
	 * End the current line and send it to server
	 */
	public function endLine():void {
		lineCutTimer.stop();
		
		if (!_currentLine.pointCount)
			return;
		
		sendLine();
		var oldLine:Line = _currentLine;
		_currentLine = new Line();
		_currentLine.brushSize = oldLine.brushSize;
		_currentLine.paintColor = oldLine.paintColor;
	}
	
	//---------------------------------------------------------------------
	//
	//  Methods: Event handlers
	//
	//---------------------------------------------------------------------
	
	/**
	 * End line when timer fires
	 */
	protected function lineCut(e:TimerEvent):void {
		endLine();
	}
	
	/**
	 * Error sending line to server.
	 */
	protected function sendLineFault(e:FaultEvent):void {
		trace("error sending line");
	}
}

}