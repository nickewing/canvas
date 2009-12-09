package com.canvas.gui {

import com.canvas.dataIO.Box;
import com.canvas.dataIO.CanvasTileListener;
import com.canvas.dataIO.TileListenerManager;

import flash.display.CapsStyle;
import flash.display.JointStyle;
import flash.display.LineScaleMode;

import mx.containers.Canvas;
import mx.controls.Label;
import mx.events.FlexEvent;

/**
 * Tile in the canvas
 */
public class CanvasTile extends Canvas {
	//---------------------------------------------------------------------
	//
	//  Class constants
	//
	//---------------------------------------------------------------------
	
	public static var SIZE:Number = 500.0;
	
	//---------------------------------------------------------------------
	//
	//  Variables
	//
	//---------------------------------------------------------------------
	
	/**
	 * Listener for updates from server
	 */
	protected var listener:CanvasTileListener;
	
	/**
	 * Whether tile is currently active. Enables and disables tile listener
	 */
	protected var _active:Boolean;
	
	/**
	 * The box in the tile represented by the tile
	 */
	protected var _box:Box;
	
	/**
	 * Tile coodinate label
	 */
	protected var coordLabel:Label = new Label();
	/**
	 * Tile drawing canvas
	 */
	protected var canvas:Canvas = new Canvas();
	
	//---------------------------------------------------------------------
	//
	//  Constructor
	//
	//---------------------------------------------------------------------
	
	public function CanvasTile() {
		super();
		
		listener = new CanvasTileListener(this);
		
		active = false;
		verticalScrollPolicy = horizontalScrollPolicy = "off";
		cacheAsBitmap = true;
		
		setStyle("backgroundColor",	"#FFFFFF");
		
		addChild(coordLabel);
		
		addEventListener(FlexEvent.CREATION_COMPLETE, setupDrawingCanvas);
	}
	
	//---------------------------------------------------------------------
	//
	//  Properties
	//
	//---------------------------------------------------------------------
	
	public function set text(v:String):void { coordLabel.text = v; }
	
	public function get box():Box { return _box }
	public function set box(v:Box):void { _box = v; }
	
	public function set active(v:Boolean):void {
		if (v == _active) return;
		
		_active = visible = enabled = v;
		
		var manager:TileListenerManager = TileListenerManager.instance;
		if (v)
			manager.registerTileListener(listener);
		else
			manager.unregisterTileListener(listener);
	}
	
	public function get active():Boolean {
		return _active;
	}
	
	//---------------------------------------------------------------------
	//
	//  Methods: Event handling
	//
	//---------------------------------------------------------------------
	
	/**
	 * Setup the drawing canvas on the tile
	 */
	protected function setupDrawingCanvas(e:FlexEvent):void {
		canvas.x = 0;
		canvas.y = 0;
		canvas.width = width;
		canvas.height = height;
		canvas.setStyle("borderColor",		"#333333");
		canvas.setStyle("borderStyle", 	"solid");
		canvas.setStyle("borderVisible",	"true");
		addChild(canvas);
	}
	
	//---------------------------------------------------------------------
	//
	//  Methods: Drawing
	//
	//---------------------------------------------------------------------
	
	/**
	 * Draw a line on the tile from (x,y) to (x1,y1)
	 */
	public function drawLine(x:Number, y:Number, x1:Number, y1:Number, color:uint, size:Number):void {
		canvas.graphics.lineStyle(
			size,
			color,
			1, // alpha
			false, // pixel hinting
			LineScaleMode.NORMAL, // line scale mode
			CapsStyle.ROUND,
			JointStyle.ROUND
		);
		canvas.graphics.moveTo(x, y);
		canvas.graphics.lineTo(x1, y1);
	}
}

}