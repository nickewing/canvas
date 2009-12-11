package com.canvas.gui {

import com.canvas.dataIO.Box;
import com.canvas.dataIO.SendLineManager;
import com.canvas.dataIO.TileListenerManager;

import flash.events.Event;
import flash.events.MouseEvent;

import mx.containers.Canvas;
import mx.events.FlexEvent;

/**
 * Loads and displays tiles, allows for panning of and drawing on tiles
 */
public class CanvasView extends Canvas {
	
	//---------------------------------------------------------------------
	//
	//  Class constants
	//
	//---------------------------------------------------------------------
	
	/**
	 * Drawing mode, allows for drawing on canvas
	 */
	public static const MODE_DRAWING:Number = 1;
	/**
	 * Panning mode, allows for panning of the canvas
	 */
	public static const MODE_PANNING:Number = 2;
	
	//---------------------------------------------------------------------
	//
	//  Variables
	//
	//---------------------------------------------------------------------
	
	/**
	 * Bounding box of canvas currently in view
	 */
	protected var box:Box = new Box();
	
	/**
	 * Service to manage and send drawn lines
	 */
	protected var sendLineService:SendLineManager = new SendLineManager();
	
	/**
	 * Interaction mode
	 */
	protected var _mode:Number = MODE_DRAWING;
	/**
	 * Whether mouse is interacting w/ pane for drawing or panning
	 */
	protected var mouseInteracting:Boolean;
	/**
	 * Whether mouseLastX and mouseLastY are valid for this interaction
	 */
	protected var mouseMoved:Boolean;
	/**
	 * Last position of mouse during interaction
	 */
	protected var mouseLastX:Number;
	/**
	 * Last position of mouse during interaction
	 */
	protected var mouseLastY:Number;
	
	/**
	 * All tiles, cached and active
	 */
	protected var tiles:Object = new Object();
	/**
	 * Current active tiles
	 */
	protected var currentTiles:Array = new Array();
	
	/**
	 * Drawing line color
	 */
	protected var _drawingColor:uint  = 0;
	/**
	 * Drawing line size
	 */
	protected var _drawingSize:Number = 2;
	
	/**
	 * Has been resized once
	 */
	protected var hasBeenResized:Boolean;
	
	//---------------------------------------------------------------------
	//
	//  Constructor
	//
	//---------------------------------------------------------------------
	
	public function CanvasView() {
		super();
		
		verticalScrollPolicy = horizontalScrollPolicy = "off";
		
		setStyle("backgroundColor",	"#FFFFFF");
		
		addEventListener(MouseEvent.MOUSE_DOWN, mouseInteractionBegan);
		addEventListener(MouseEvent.MOUSE_MOVE, mouseMove);
		addEventListener(MouseEvent.MOUSE_UP,   mouseInteractionEnded);
		addEventListener(MouseEvent.ROLL_OUT,   mouseInteractionEnded);
		
		addEventListener(Event.RESIZE, resize);
	}
	
	//---------------------------------------------------------------------
	//
	//  Properties
	//
	//---------------------------------------------------------------------
	
	/**
	 * Interaction mode
	 */
	public function get mode():Number { return _mode; }
	public function set mode(v:Number):void { _mode = v; }
	
	/**
	 * Drawing line color
	 */
	public function get drawingColor():uint { return _drawingColor; }
	
	public function set drawingColor(v:uint):void {
		_drawingColor = v;
		sendLineService.currentLine.paintColor = v;
	}
	
	/**
	 * Drawing line size
	 */
	public function get drawingSize():Number { return _drawingSize; }
	
	public function set drawingSize(v:Number):void {
		_drawingSize = v;
		sendLineService.currentLine.brushSize = v;
	}
	
	//--------------------------------------------------------------------------
	//
	//  Methods: Event Handling
	//
	//--------------------------------------------------------------------------
	
	/**
	 * Handle pane resize
	 */
	protected function resize(e:Event):void {
		box.x1 = box.x + width;
		box.y1 = box.y + height;
		trace("resize");
		
		if (!hasBeenResized) {
			setLocation(0, 0);
			hasBeenResized = true;
		}
		
		buildTiles();
	}

	/**
	 * Handle events causing mouse interaction to begin
	 */
	protected function mouseInteractionBegan(e:MouseEvent):void {
		mouseInteracting  = true;
		mouseMoved = false;
		mouseLastX = mouseLastY = 0.0;
	}
	
	/**
	 * Handle events causing mouse interaction to end
	 */
	protected function mouseInteractionEnded(e:MouseEvent):void {
		sendLineService.endLine();
		mouseInteracting = false;
	}
	
	/**
	 * Handle a mouse move if there is a mouse interaction happening
	 */
	protected function mouseMove(e:MouseEvent):void {
		if (!mouseInteracting) return;
		
		if (mouseMoved) {
			if (_mode == MODE_PANNING) {
				var dx:Number = mouseLastX - mouseX;
				var dy:Number = mouseLastY - mouseY;
				
				box.shift(dx, dy);
				buildTiles();
			} else if (_mode == MODE_DRAWING) {
				var t1:CanvasTile = tileAtPoint(mouseLastX, mouseLastY);
				var t2:CanvasTile = tileAtPoint(mouseX, mouseY);
				
				if (t1 == t2) { // Line is only on one tile
					// draw segment on tile
					t1.drawLine(
						mouseLastX - t1.x,
						mouseLastY - t1.y,
						mouseX - t1.x,
						mouseY - t1.y,
						_drawingColor,
						_drawingSize
					);
					
					// add segment or point to send line service
					if (!sendLineService.currentLine.pointCount)
						sendLineService.addPoint(mouseLastX + box.x, mouseLastY + box.y);
					sendLineService.addPoint(mouseX + box.x, mouseY + box.y);
					
				} else { // Line crosses tiles, stop drawing
					mouseInteracting = false;
					return;
				}
			}
		} else {
			mouseMoved = true;
		}
		
		mouseLastX = mouseX;
		mouseLastY = mouseY;
	}
	
	///--------------------------------------------------------------------------
	//
	//  Methods: Tiles
	//
	//--------------------------------------------------------------------------
	
	/**
	 * Fetch the tile surrounding an (x,y) point
	 */
	protected function tileAtPoint(x:Number, y:Number):CanvasTile {
		var x0:Number = Math.floor((box.x + x) / CanvasTile.SIZE) * CanvasTile.SIZE;
		var y0:Number = Math.floor((box.y + y) / CanvasTile.SIZE) * CanvasTile.SIZE;
		return getTile(x0, y0);
	}
	
	/**
	 * Fetch a tile by its bottom left corner coordinate
	 */
	protected function getTile(x:Number, y:Number):CanvasTile {
		if (!tiles[x]) tiles[x] = new Object();
		
		if (!tiles[x][y]) {
			var tile:CanvasTile = new CanvasTile();
			tile.box = new Box(x, y, x + CanvasTile.SIZE, y + CanvasTile.SIZE);
			tile.text = (x / CanvasTile.SIZE) + ", " + (y / CanvasTile.SIZE);
			tile.width = tile.height = CanvasTile.SIZE;
			addChild(tile);
			tiles[x][y] = tile;
		}
		
		return tiles[x][y];
	}
	
	/**
	 * Position the tile relative to the viewing box on the stage
	 */
	protected function positionTile(x:Number, y:Number, tile:CanvasTile):void {
		tile.x = x - box.x;
		tile.y = y - box.y;
	}
	
	/**
	 * Show and activate tile
	 */
	protected function activateTile(x:Number, y:Number):CanvasTile {
		var tile:CanvasTile = getTile(x, y);					
		positionTile(x, y, tile);
		tile.active = true;
		return tile;
	}
	
	/**
	 * Calculate tiles in viewing box and activate them, deactivate old tiles
	 */
	protected function buildTiles():void {
		var x0:Number  = Math.floor(box.x / CanvasTile.SIZE) * CanvasTile.SIZE;
		var y0:Number  = Math.floor(box.y / CanvasTile.SIZE) * CanvasTile.SIZE;
		var xf:Number  = Math.ceil(box.x1 / CanvasTile.SIZE) * CanvasTile.SIZE;
		var yf:Number  = Math.ceil(box.y1 / CanvasTile.SIZE) * CanvasTile.SIZE;
		var x:Number   = x0;
		var y:Number   = y0;
		
		var newTiles:Array = new Array();
		
		/* calculate all tiles intersecting pane 
			NOTE: all tiles are stored from their bottom left corner
			coordinate because it simplifies the math 
		*/
		while (x < xf) {
			y = y0;
			while (y < yf) {
				newTiles.push(activateTile(x, y));
				y += CanvasTile.SIZE;
			}
			x += CanvasTile.SIZE;
		}
		
		// Deactive old tiles in current tiles
		for (var i:int = 0; i < currentTiles.length; i++) {
			if (newTiles.indexOf(currentTiles[i]) == -1) {
				currentTiles[i].active = false;
			}
		}
		
		if (!tileArrayEqual(currentTiles, newTiles)) {
			// Reset the connection with the server since tiles were updated
			trace("TLM reset by CanvasView 2");
			TileListenerManager.instance.reset();
		}
		
		currentTiles = newTiles;
	}
	
	protected function setLocation(x:Number, y:Number):void {
		box.x  = (CanvasTile.SIZE * x) - (width - CanvasTile.SIZE) / 2;
		box.y  = (CanvasTile.SIZE * y) - (height - CanvasTile.SIZE) / 2;
		box.x1 = (CanvasTile.SIZE * x) + width - (width - CanvasTile.SIZE) / 2;
		box.y1 = (CanvasTile.SIZE * y) + height - (height - CanvasTile.SIZE) / 2;
	}
	
	/**
	 * Jump to tile by its bottom left coordinate
	 */
	public function jumpToTile(x:Number, y:Number):void {
		setLocation(x, y);
		
		buildTiles();
	}
	
	/**
	 * Test whether two tile arrays are equal based on their elements
	 */
	protected function tileArrayEqual(a:Array, b:Array):Boolean {
		if (b == a)
			return true;
		var i:Number = b.length;
		if (i != a.length)
			return false;
		while(i--)
			if (b[i] != a[i])
				return false;
		return true;
	}
}

}