package com.canvas.dataIO {

/**
 * Represents a box in planar space
 */
public class Box {
	
	//---------------------------------------------------------------------
	//
	//  Variables
	//
	//---------------------------------------------------------------------
	
	protected var _x:Number;
	protected var _y:Number;
	protected var _x1:Number;
	protected var _y1:Number;
	
	//---------------------------------------------------------------------
	//
	//  Constructor
	//
	//---------------------------------------------------------------------
	
	public function Box(x0:Number = 0.0, y0:Number = 0.0, x10:Number = 0.0, y10:Number = 0.0) {
		_x  = x0;
		_y  = y0;
		_x1 = x10;
		_y1 = y10;
	}
	
	//---------------------------------------------------------------------
	//
	//  Properties
	//
	//---------------------------------------------------------------------
	
	public function set x(v:Number):void  { _x = v; }
	public function set y(v:Number):void  { _y = v; }
	public function set x1(v:Number):void { _x1 = v; }
	public function set y1(v:Number):void { _y1 = v; }
	
	public function get x():Number  { return _x; }
	public function get y():Number  { return _y; }
	public function get x1():Number { return _x1; }
	public function get y1():Number { return _y1; }
	
	public function get centerX():Number { return (_x + _x1) / 2.0; }
	public function get centerY():Number { return (_y + _y1) / 2.0; }
	
	public function get width():Number  { return Math.abs(_x - _x1); }
	public function get height():Number { return Math.abs(_y - _y1); }
	
	//---------------------------------------------------------------------
	//
	//  Methods
	//
	//---------------------------------------------------------------------
	
	/**
	 * Shift the box coordinates by the given x and y
	 */
	public function shift(x:Number, y:Number):void {
		_x  += x;
		_x1 += x;
		_y  += y;
		_y1 += y;
	}
	
	/**
	 * Return string of box cordinates in form of #.#,#.#,#.#,#.#
	 */
	public function serialize():String {
		return [_x, _y, _x1, _y1].join(",");
	}
	
	public function hasPoint(x:Number, y:Number):Boolean {
		return (_x <= x) && (x <= _x1) && (_y <= y) && (y <= _y1);
	}
	
	public function intersects(b2:Box):Boolean {
		return 		b2.hasPoint(_x,  _y)
				||	b2.hasPoint(_x1, _y1)
				||	b2.hasPoint(_x,  _y1)
				||	b2.hasPoint(_x1, _y)
				||	hasPoint(b2.x,  b2.y)
				||	hasPoint(b2.x1, b2.y1)
				||	hasPoint(b2.x,  b2.y1)
				||	hasPoint(b2.x1, b2.y);
	}
}

}