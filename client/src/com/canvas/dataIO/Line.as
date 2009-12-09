package com.canvas.dataIO {

public class Line {
	//---------------------------------------------------------------------
	//
	//  Variables
	//
	//---------------------------------------------------------------------
	
	protected var _paintColor:uint;
	protected var _brushSize:Number;
	
	/**
	 * Array of points type Number
	 */
	protected var _points:Array;
	
	/**
	 * Bounding box for line
	 */
	protected var _box:Box;
	
	//---------------------------------------------------------------------
	//
	//  Constructor
	//
	//---------------------------------------------------------------------
	
	public function Line(iPoints:Array = null, iBrushSize:Number = 1, iPaintColor:uint = 0) {
		_points     = iPoints ? iPoints : [];
		_brushSize  = iBrushSize;
		_paintColor = iPaintColor;
	}
	
	//---------------------------------------------------------------------
	//
	//  Properties
	//
	//---------------------------------------------------------------------
	
	public function set paintColor(v:uint):void { _paintColor = v; }
	public function get paintColor():uint { return _paintColor; }
	
	public function set brushSize(v:Number):void { _brushSize = v; }
	public function get brushSize():Number { return _brushSize; }
	
	public function get points():Array { return _points; }
	
	public function get pointCount():Number { return points.length / 2; }
	
	public function get box():Box {
		if (!_box) {
			var minX:Number = _points[0],
				minY:Number = _points[1],
				maxX:Number = _points[0],
				maxY:Number = _points[1];
			
			for (var i:Number = 0; i < _points.length; i += 2) {
				minX = Math.min(minX, _points[i]);
				minY = Math.min(minY, _points[i + 1]);
				maxX = Math.max(maxX, _points[i]);
				maxY = Math.max(maxY, _points[i + 1]);
			}
			_box = new Box(minX, minY, maxX, maxY);
		}
		
		return _box;
	}
	
	//---------------------------------------------------------------------
	//
	//  Class Methods
	//
	//---------------------------------------------------------------------
	
	public static function unserializeLineArray(str:String):Array {
		var lines:Array = [];
		var lineStrings:Array = str.split(";");
		for (var i:Number = 0; i < lineStrings.length; i++) {
			var lineParts:Array = str.split("/");
			var points:Array = lineParts[0].split(",");
			
			for (var j:Number = 0; j < points.length; j++) {
				points[j] = parseFloat(points[j]);
			}
			
			var color:Number = parseInt(lineParts[1]);
			var size:Number = parseInt(lineParts[2]);
			
			lines.push(new Line(points, size, color));
		}
		
		return lines;
	}
	
	//---------------------------------------------------------------------
	//
	//  Methods
	//
	//---------------------------------------------------------------------
	
	/**
	 * Add a point to the lines points
	 */
	public function addPoint(x:Number, y:Number):void {
		_points.push(x);
		_points.push(y);
		_box = null;
	}
	
	/**
	 * Return the line in the format #,#,#,#,#,#/#####/#####
	 */
	public function serialize():String {
		return [_points.join(","), paintColor.toString(), brushSize.toString()].join("/");
	}
}

}