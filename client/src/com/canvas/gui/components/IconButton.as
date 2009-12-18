package com.canvas.gui.components {

import spark.components.Button;
import spark.primitives.BitmapImage;

/**
 * Extends a Spark button to have an icon
 */
public class IconButton extends Button {
	//--------------------------------------------------------------------------
	//
	//    Constructor
	//
	//--------------------------------------------------------------------------
	
	public function IconButton()
	{
		super();
	}
	
	//--------------------------------------------------------------------------
	//
	//    Properties
	//
	//--------------------------------------------------------------------------
	
	/**
	 *  Internal storage for the icon property.
	 */
	private var _icon:Class;
	
	
	[Bindable]
	
	/**
	 *  
	 */
	public function get icon():Class { return _icon; }
	
	public function set icon(val:Class):void {
		_icon = val;
		
		if (iconElement != null)
			iconElement.source = _icon;
	}
	
	//--------------------------------------------------------------------------
	//
	//  Skin Parts
	//
	//--------------------------------------------------------------------------
	
	[SkinPart("false")]
	public var iconElement:BitmapImage;
	
	
	//--------------------------------------------------------------------------
	//
	//  Overridden methods
	//
	//--------------------------------------------------------------------------
	
	/**
	 * 
	 */
	override protected function partAdded(partName:String, instance:Object):void {
		super.partAdded(partName, instance);
		
		if (icon !== null && instance == iconElement)
			iconElement.source = icon;
	}
}

}