import mx.managers.PopUpManager;

protected var _canvasView:CanvasView;

public function get canvasView():CanvasView { return _canvasView; }
public function set canvasView(v:CanvasView):void { _canvasView = v; }

protected function closeWindow():void {
	PopUpManager.removePopUp(this);
}

protected function constrainJumpCoord(str:String):Number {
	var v:Number = parseInt(str);
	if (isNaN(v)) v = 0;
	return v;
}

protected function jump():void {
	var jumpX:Number = constrainJumpCoord(jumpXInput.text);
	var jumpY:Number = constrainJumpCoord(jumpYInput.text);
	
	_canvasView.jumpToTile(jumpX, jumpY);
	
	closeWindow();
}