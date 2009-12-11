import com.canvas.dataIO.TileListenerManager;
import flash.events.Event;
import mx.rpc.events.FaultEvent;
import mx.containers.TitleWindow;
import mx.managers.PopUpManager;
import spark.events.IndexChangeEvent;

//---------------------------------------------------------------------
//
//  Constants
//
//---------------------------------------------------------------------

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

//---------------------------------------------------------------------
//
//  Methods: Event handling
//
//---------------------------------------------------------------------

/**
 * Setup on initialization
 */
protected function init(e:Event):void {
	showSessionWindow();
	
	TileListenerManager.instance.addEventListener(FaultEvent.FAULT, function (e:Event):void {
		showReconnectWindow();
	});
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

//---------------------------------------------------------------------
//
//  Methods
//
//---------------------------------------------------------------------

/**
 * Open the session window
 */
protected function showSessionWindow():void {
	var window:TitleWindow = PopUpManager.createPopUp(this, SessionWindow, true) as TitleWindow;
	PopUpManager.centerPopUp(window);
}

/**
 * Open reconnect window
 */
protected function showReconnectWindow():void {
	var window:TitleWindow = PopUpManager.createPopUp(this, ReconnectWindow, true) as ReconnectWindow;
	PopUpManager.centerPopUp(window);
}

/**
 * Open jump window
 */
protected function showJumpWindow():void {
	var window:TitleWindow = PopUpManager.createPopUp(this, JumpWindow, true) as TitleWindow;
	(window as JumpWindow).canvasView = canvasView;
	PopUpManager.centerPopUp(window);
}