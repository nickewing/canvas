package com.canvas.gui.components {

import com.canvas.dataIO.Box;
import com.canvas.dataIO.CanvasTileListener;
import com.canvas.dataIO.TileListenerManager;

import flash.display.CapsStyle;
import flash.display.JointStyle;
import flash.display.LineScaleMode;

import mx.containers.Canvas;
import mx.core.UIComponent;
import mx.events.FlexEvent;

import spark.components.HGroup;
import spark.components.Label;
import spark.components.VGroup;

/**
 * Tile in the canvas
 * 
 * Part of SRS requirement 3.1.1.1 Drawing Canvas
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
	/**
	 * Shade overlay for disabled state
	 */
	protected var shade:Canvas = new Canvas();
	
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
		
		addEventListener(FlexEvent.INITIALIZE, setupDrawingCanvas);
	}
	
	//---------------------------------------------------------------------
	//
	//  Properties
	//
	//---------------------------------------------------------------------
	
	public function get box():Box { return _box }
	public function set box(v:Box):void {
		_box = v;
		coordLabel.text = (box.x / CanvasTile.SIZE) + ", " + (-box.y / CanvasTile.SIZE);
	}
	
	public function set active(v:Boolean):void {
		if (v == _active) return;
		
		_active = visible = enabled = v;
		
		var manager:TileListenerManager = TileListenerManager.instance;
		if (v) {
			manager.registerTileListener(listener);
		} else {
			manager.unregisterTileListener(listener);
			shaded = true;
		}
	}
	
	public function get active():Boolean {
		return _active;
	}
	
	public function set shaded(v:Boolean):void {
		shade.visible = v;
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
		var canvasMask:UIComponent = new UIComponent();
		
		coordLabel.setStyle("paddingTop", 5);
		coordLabel.setStyle("paddingLeft", 5);
		
		shade.x      = canvas.x      = 0;
		shade.y      = canvas.y      = 0;
		shade.width  = canvas.width  = width;
		shade.height = canvas.height = height;
		
		// draw masking area for canvas
		canvasMask.graphics.beginFill(0xFFFFFF, 1);
		canvasMask.graphics.drawRect(-1, -1, width, height);
		canvasMask.graphics.endFill();
		
		canvas.setStyle("borderColor",	"#333333");
		canvas.setStyle("borderStyle", 	"solid");
		canvas.setStyle("borderVisible","true");
		canvas.verticalScrollPolicy = canvas.horizontalScrollPolicy = "off";
		canvas.addChild(canvasMask);
		canvas.mask = canvasMask;
		addChild(canvas);
		
		shade.setStyle("backgroundColor", "0x000000");
		shade.setStyle("backgroundAlpha", "0.2");
		
		var shadeLabel:Label = new Label();
		shadeLabel.text = "Loading...";
		
		// center in vertical middle
		var shadeGroup1:HGroup = new HGroup;
		shadeGroup1.verticalAlign = "middle";
		shadeGroup1.height = height;
		shadeGroup1.addElement(shadeLabel);
		
		// center in horizontal center
		var shadeGroup:VGroup = new VGroup;
		shadeGroup.horizontalAlign = "center";
		shadeGroup.width  = width;
		shadeGroup.addElement(shadeGroup1);
		
		shade.addChild(shadeGroup);
		
		addChild(shade);
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