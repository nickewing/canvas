



var SCREEN_WIDTH = window.innerWidth;
var SCREEN_HEIGHT = window.innerHeight;

var canvas, context;

var mouseX = 0, mouseY = 0, oldMouseX = 0, oldMouseY = 0;
var isMouseDown = false;

// var points;
// var count = 0;

var brushSize = 1;
var drawColor = "rgba(99, 0, 0, 1)";

init();

function init() {
  
  canvas = document.createElement("canvas");
  canvas.width = 600;
  canvas.height = 500;
  canvas.style.cursor = 'crosshair';
  $('#container').append(canvas);

  context = canvas.getContext("2d");
  context.fillStyle = "rgb(250, 250, 250)";
  context.fillRect(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT);
  context.lineWidth = 1;

  $('#save').click(onSave);
  $('#clear').click(onClear);

  // points = new Array();

  canvas.addEventListener('mousedown', onMouseDown, false);
  canvas.addEventListener('mouseup', onMouseUp, false);
  canvas.addEventListener('mousemove', onMouseMove, false);

  window.addEventListener('mouseout', onMouseUp, false);

  document.onmousedown = function() {return false;} // avoid text selection
}

function onMouseDown(e) {
  isMouseDown = true;

  oldMouseX = mouseX;
  oldMouseY = mouseY;

  return false;
}

function onMouseUp(e) {
  isMouseDown = false;

  return false;
}

function onMouseMove(e) {
  if (!e) var e = window.event;
  mouseX = e.clientX;
  mouseY = e.clientY;

  draw();
}

function startStroke() {
  context.moveTo(mouseX, mouseY);
}

function draw() {
  if (isMouseDown) {
    //points.push([mouseX, mouseY]);
    
    context.lineCap = 'round';
    context.lineWidth = brushSize;
    context.strokeStyle = drawColor;
    context.beginPath();
    context.moveTo(oldMouseX, oldMouseY);
    context.lineTo(mouseX, mouseY);
    context.stroke();

    oldMouseX = mouseX;
    oldMouseY = mouseY;
    
    // context.lineWidth = 1;
    // for (var i = 0; i < points.length; i++) {
    //   var dx = points[i][0] - points[count][0];
    //   var dy = points[i][1] - points[count][1];
    //   var d = dx * dx + dy * dy;
    // 
    //   if (d < 2500 && Math.random() > 0.9) {
    //     context.lineWidth = 1;
    //     context.strokeStyle = "rgba(0, 0, 0, 0.1)";
    //     context.beginPath();
    //     context.moveTo(points[count][0], points[count][1]);
    //     context.lineTo(points[i][0], points[i][1]);
    //     context.stroke();
    //   }
    // }
    
    // count ++;
  }
  
  
}

function onSave() {
  window.open(canvas.toDataURL(), 'outputwindow');
}

function onClear() {
  // points = new Array();
  // count = 0;

  context.fillStyle = "rgb(250, 250, 250)";
  context.fillRect(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT);
}