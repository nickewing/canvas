import com.canvas.dataIO.SessionManager;
import com.canvas.dataIO.TileListenerManager;
import flash.events.Event;
import mx.containers.TitleWindow;
import mx.managers.PopUpManager;
import spark.events.IndexChangeEvent;

/**
 * Minimum drawing line size allowed
 */
protected const MIN_LINE_SIZE:Number = 1;
/**
 * Maximum drawing line size allowed
 */
protected const MAX_LINE_SIZE:Number = 50;
/**
 * Initial drawing line size
 */
protected const INIT_LINE_SIZE:Number = 3;


protected function init(e:Event):void {
	SessionManager.instance.join();
	SessionManager.instance.addEventListener(Event.CONNECT, sessionJoined);
}

protected function sessionJoined(e:Event):void {
	trace("session joined handler");
	TileListenerManager.instance.enabled = true;
}

/**
 * Handle line size input text change
 */
protected function lineSizeTextChange(e:Event):void {
	var val:Number = parseInt(lineSizeInput.text);
	
	if (val >= MIN_LINE_SIZE && val <= MAX_LINE_SIZE) {
		lineSize.value = val;
	} else if (val > MAX_LINE_SIZE) {
		lineSize.value = MAX_LINE_SIZE;
	} else {
		lineSize.value = MIN_LINE_SIZE;
	}
	
	lineSizeInput.text = lineSize.value.toString();
}

/**
 * Change mode based on ToggleButtonBar
 */
protected function changeMode(e:IndexChangeEvent):void {
	switch (e.newIndex) {
	case -1:
		break;
	case 0:
		canvasView.mode = CanvasView.MODE_PANNING;
		break;
	case 1:
		canvasView.mode = CanvasView.MODE_DRAWING;
		break
	}
}

/**
 * Open jump window
 */
protected function showJumpWindow():void {
	var window:TitleWindow = PopUpManager.createPopUp(this, JumpWindow, true) as TitleWindow;
	(window as JumpWindow).canvasView = canvasView;
	PopUpManager.centerPopUp(window);
}