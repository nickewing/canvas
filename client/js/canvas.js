// TODO: check tiles are actually copied by Update
// TODO: make session a singleton
// TODO: Line cutting while drawing seems to be missing a segment when sent
//       see tile 102,100 for example

"use strict";

function arraysAreEqual(a, b) {
  if (b == a)
    return true;
  var i = b.length;
  if (i != a.length)
    return false;
  while(i--)
    if (b[i] != a[i])
      return false;
  return true;
}


var Canvas = {
  tileSize:   500,
  host:       'http://' + window.location.host + '/'
}

/**
 * @constructor
 */
function UnclosableMessageDialog(initTitle, initText) {
  this.text   = $('<p> /');
  this.dialog = $('<div />');
  
  this.dialog.dialog({
    'autoOpen':     false,
    'resizable':    false,
    'modal':        true,
    'height':       '100px',
    'dialogClass':  'unclosable'
  });
  
  this.setText(initTitle);
  this.setTitle(initText); 
}

$.extend(UnclosableMessageDialog.prototype, {
  setText:    function(newText) {this.text.text(newText);},
  setTitle:   function(newTitle) {this.dialog.attr('title', newTitle);},
  open:       function() {this.dialog.dialog('open');},
  close:      function() {this.dialog.dialog('close');},
  setButtons: function(buttons) {this.dialog.dialog({buttons: buttons});}
})

/**
 * @constructor
 */
function ReconnectDialog() {
  this.error  = 'There was an error while attempting to talk to the server';
  this.dialog = new UnclosableMessageDialog('Connection Error', this.error);
}

ReconnectDialog.getInstance = function() {
  this.instance = new this();
  return this.instance;
}

$.extend(ReconnectDialog.prototype, {
  retryButtonVisible: function(val) {
    if (val) this.dialog.setButtons({'Retry': this.retryButtonClicked});
    else     this.dialog.setButtons({});
  },
  retryButtonClicked: function() {
    this.retryButtonVisible(false);
    this.dialog.setText('Retrying connection to server...');
    this.retryCallback();
  },
  promptRetry: function(callback) {
    this.dialog.setText(error);
    this.retryCallback = callback;
    this.retryButtonVisible(false);
    this.dialog.dialog('open');
    setTimeout(function() {
      this.retryButtonVisible(true);
    }, 1000);
  },
  close: function() {
    this.dialog.close();
  }
});

/**
 * @constructor
 */
function CanvasServerSession(readyCallback, failureCallback) {
  this.readyCallback = readyCallback;
  this.failureCallback = failureCallback;
  this.join();
}

$.extend(CanvasServerSession.prototype, {
  serverRequest: function(action, data, success, failure) {
    data.sid = this.sid;
    return $.ajax({
      url:      Canvas.host + action,
      data:     data,
      type:     'POST',
      success:  success,
      error:    failure
    });
  },
  serializeLine: function(points, color, size) {
    return [points.join(','), color, size].join('/');
  },
  joinSuccessful: function(data) {
    var resp = data.split(' ');
    if (resp[0] == 'OK') {
      this.sid = resp[1];
      //console.log(sid);
      this.readyCallback();
    } else {
      this.failureCallback();
    }
  },
  join: function() {
    this.serverRequest(
      'join',
      {},
      $.hitch(this, this.joinSuccessful),
      $.hitch(this, this.failureCallback)
    );
  },
  getUpdate: function(tiles, success, failure) {
    var t        = Canvas.tileSize,
        tileStrs = [];
    
    for (x in tiles) {
      for (y in tiles[x]) {
        tileStrs.push([
          x, y, parseInt(x) + t, parseInt(y) + t
        ].join(',') + '/' + tiles[x][y]);
      }
    }
    
    if (tileStrs.length < 1) return;
    
    return this.serverRequest(
      'update',
      {t: tileStrs.join(';')},
      success,
      failure
    );
  },
  sendLine: function(points, color, size, success, failure) {
    var lineStr = this.serializeLine(points, color, size);
    this.serverRequest('send_line', {l: lineStr}, success, failure);
  },
  retryJoin: function() {
    this.join();
  }
});

/**
 * @constructor
 */
function CanvasUpdate(serverConn, tiles, success, failure) {
  // make a full copy of the tiles given
  this.tiles     = $.extend(true, {}, tiles);
  this.onSuccess = success;
  this.onFailure = failure;
  
  this.request = serverConn.getUpdate(
    this.tiles,
    $.hitch(this, this.handleUpdates),
    $.hitch(this, this.handleFailure)
  );
}

$.extend(CanvasUpdate.prototype, {
  unserializeLineArray: function(str) {
    var lines       = [],
        lineStrings = str.split(';');

    for (var i = 0, il = lineStrings.length; i < il; i++) {
      var lineParts = lineStrings[i].split('/'),
          points    = lineParts[0].split(','),
          color     = lineParts[1],
          size      = parseInt(lineParts[2]);
      
      for (var j = 0, jl = points.length; j < jl; j++)
        points[j] = parseFloat(points[j]);

      lines.push([points, color, size]);
    }
    
    return lines;
  },
  cancel: function() {
    this.request.abort();
  },
  getLines: function() {
    if (!this.lines) this.lines = this.unserializeLineArray(this.lineStr);
    return this.lines;
  },
  handleUpdates: function(data) {
    var resp = data.split(' ');
    this.status = resp[0];
    
    if (resp[0] == 'OK') {
      this.time    = resp[1]
      this.lineStr = resp[2];
    }
    
    this.onSuccess(this);
  },
  handleFailure: function() {
    this.onFailure(this);
  }
});

/**
 * @constructor
 */
function CanvasUpdateManager(serverConn, callback) {
  this.serverConn    = serverConn,
  this.linesCallback = callback;
}

$.extend(CanvasUpdateManager.prototype, {
  setTiles: function(tiles) {this.newUpdate(tiles);},
  
  newUpdate: function(tiles) {
    if (this.currentUpdate) this.currentUpdate.cancel();
    this.currentUpdate = new CanvasUpdate(
      this.serverConn,
      tiles,
      $.hitch(this, this.updateSuccess),
      $.hitch(this, this.updateFailure)
    );
  },
  
  updateSuccess: function(update) {
    if (update.status == 'OK' || update.status == 'TIMEOUT') { 
      this.newUpdate(update.tiles);
      
      if (update.status == 'OK')
        this.linesCallback(update);
    } else {
      //console.log('Update failed: ' + update.status);
    }
  },
  
  updateFailure: function(update) {
    if (update.status == 'CANCELLED') return;
    console.log('Update failure.');
  }
});

/**
 * @constructor
 */
function Tile(x, y) {
  var t = Canvas.tileSize,
      imgURL = ['/tiles/', x, '/', y, '.jpg'].join(''),
      imgSource = new Image();
  
  this.canvas = $(['<canvas width="', t, '" height="', t, '" />'].join(''));
  
  this.shade = $('<div><span>Loading...</span></div>');
  
  this.canvasContext = this.canvas[0].getContext('2d');
  
  this.node = $('<div class="canvasTile" />').
                width(t).
                height(t).
                hide().
                append(this.canvas).
                append(['<span>', (x / t), ',',
                        (-y / t), '</span>'].join('')).
                append(shade);
  
  function onBackgroundLoaded() {
    this.backgroundLoaded = true;
    this.drawPendingLines();
  }
  
  imgSource.src = imgURL;
  imgSource.onload = $.hitch(this, function() {
    this.canvasContext.drawImage(imgSource, 0, 0);
    $.hitch(this, onBackgroundLoaded)();
  });
  imgSource.onerror = $.hitch(this, function() {
    $.hitch(this, onBackgroundLoaded)();
  });
  
  this.x = x;
  this.y = y;
  this.lastUpdate = 0;
  this.pendingLines = [];
}

$.extend(Tile.prototype, {
  getScreenOffset: function() {
    if (!this.screenOffset)
      this.screenOffset = this.node.offset();
    return this.screenOffset;
  },
  
  getScreenX: function() {
    return this.getScreenOffset().left;
  },
  
  getScreenY: function() {
    return this.getScreenOffset().top;
  },
  
  position: function(x, y) {
    var o = {left: parseInt(x), top: parseInt(y)};
    this.node.css(o);
    this.screenOffset = o;
  },
  
  getBox: function() {
    if (!this.box) {
      var t = Canvas.tileSize;
      this.box = {x: this.x, y: this.y, x1: this.x + t, y1: this.y + t};
    }
    return this.box;
  },
  
  drawLineToCanvas: function(x, y, x1, y1, color, size) {
    var ctx = this.canvasContext;
    
    ctx.lineCap     = 'round';
    ctx.lineWidth   = size;
    ctx.strokeStyle = '#' + color;
    
    ctx.beginPath();
    ctx.moveTo(x, y);
    ctx.lineTo(x1, y1);
    ctx.stroke();
  },
  
  drawLine: function(x, y, x1, y1, color, size) {
    if (this.backgroundLoaded)
      this.drawLineToCanvas(x, y, x1, y1, color, size);
    else
      this.pendingLines.push([x, y, x1, y1, color, size]);
  },
  
  shaded: function(v) {
    if(v) this.shade.show();
    else  this.shade.hide();
  },
  
  drawPendingLines: function() {
    for (var i = 0, l = this.pendingLines.length; i < l; i++)
      this.drawLineToCanvas.apply(this, this.pendingLines[i]);
    this.pendingLines = [];
  },
  
  getImageURL: function() {
    return this.canvas[0].toDataURL();
  }
});

/**
 * @constructor
 */
function SendLineManager(serverConn) {
  this.serverConn = serverConn;
  this.hasLine    = false;
  this.linePoints = [];
}

$.extend(SendLineManager.prototype, {
  setLineColor: function(v) {
    this.lineColor = v;
  },
  
  setLineSize: function(v) {
    this.lineSize = v;
  },
  
  addPoint: function(x, y) {
    if (!this.linePoints.length) {
      if (this.lineCutTimouet) clearTimeout(this.lineCutTimouet);
      this.lineCutTimouet = setTimeout($.hitch(this, this.endLine), 300);
    }
    
    this.hasLine = true;
    this.linePoints.push(x);
    this.linePoints.push(y);
  },
  
  endLine: function() {
    this.sendLine();
    if (this.lineCutTimouet) clearTimeout(this.lineCutTimouet);
    this.linePoints = [];
  },
  
  clearLine: function() {
    this.hasLine = false;
  },
  
  sendLine: function() {
    if (!this.linePoints.length) return;
    this.serverConn.sendLine(this.linePoints, this.lineColor, this.lineSize);
  }
});

/**
 * @constructor
 */
function CanvasArea(container) {
  var area,
      drawingColor     = 'rgba(90,0,0,1)',
      drawingSize      = 3,
      box              = {x: 0, y: 0, x1: 0, y1: 0},
      
      currentTiles     = [],
      allTiles         = {},
      activeTiles      = {},
      
      mode             = 'drawing',
      mouseInteracting = false,
      mouseLastX,
      mouseLastY,
      
      hasBeenResized   = false,
      
      serverConn,
      updateManager,
      sendLineManager,
      
      sessionDialog;
  
  function shiftBox(x, y) {
    box.x  += x;
    box.y  += y;
    box.x1 += x;
    box.y1 += y;
  }
  
  function setLocation(x, y) {
    box = {x: x, y: y, x1: x + area.width(), y1: y + area.height()};
  }
  
  function setTileLocation(x, y) {
    var t  = Canvas.tileSize,
        w  = area.width(),
        h  = area.height(),
        tx = t * x,
        ty = t * y;
    
    box.x  = tx - (w - t) / 2;
    box.y  = ty - (h - t) / 2;
    box.x1 = tx + w - (w - t) / 2;
    box.y1 = ty + h - (h - t) / 2;
  }
  
  
  
  function tileAtPoint(x, y) {
    var t = Canvas.tileSize;
    return getTile(
      Math.floor((box.x + x) / t) * t,
      Math.floor((box.y + y) / t) * t
    );
  }
  
  function getTile(x, y) {
    if (!allTiles[x]) allTiles[x] = {};
    
    if (!allTiles[x][y]) {
      var tile = new Tile(x, y);
      area.append(tile.node);
      allTiles[x][y] = tile;
    }
    
    return allTiles[x][y];
  }
  
  function activateTile(tile) {
    var x = tile.x, y = tile.y;
    
    tile.node.show();
    tile.position(tile.x - box.x, tile.y - box.y);
    
    if (!activeTiles[x]) activeTiles[x] = {};
    activeTiles[x][y] = tile.lastUpdate;
  }
  
  function deactivateTile(tile) {
    var x = tile.x, y = tile.y;
    
    tile.node.hide();
    tile.shaded(true);
    
    // Maybe should use = null here instead,
    // but then iteration must be considered
    delete activeTiles[x][y];
  }
  
  function buildTiles() {
    var t        = Canvas.tileSize,
        x0       = Math.floor(box.x / t) * t,
        y0       = Math.floor(box.y / t) * t,
        xf       = Math.ceil(box.x1 / t) * t,
        yf       = Math.ceil(box.y1 / t) * t,
        x        = x0,
        y        = y0,
        newTiles = [];
    
    /* calculate all tiles intersecting pane 
      NOTE: all tiles are stored from their bottom left corner
      coordinate because it simplifies the math 
    */
    while (x < xf) {
      y = y0;
      while (y < yf) {
        var tile = getTile(x, y);
        activateTile(tile);
        newTiles.push(tile);
        y += t;
      }
      x += t;
    }
    
    // Deactive old tiles in current tiles
    for (var i = 0, l = currentTiles.length; i < l; i++) {
      if (newTiles.indexOf(currentTiles[i]) == -1) {
        deactivateTile(currentTiles[i]);
      }
    }
    
    if (!arraysAreEqual(currentTiles, newTiles)) {
      // Reset the connection with the server since tiles were updated
      if (updateManager) updateManager.setTiles(activeTiles);
    }
    
    currentTiles = newTiles;
  }
  
  function mouseInteractionBegan(e) {
    mouseInteracting  = true;
    mouseLastX = e.clientX;
    mouseLastY = e.clientY;
  }
  
  function mouseInteractionEnded(e) {
    if (!sendLineManager.hasLine && mode == 'drawing') {
      // draw point if no movement occurred
      var t1 = tileAtPoint(mouseLastX, mouseLastY);
      
      sendLineManager.addPoint(mouseLastX + box.x, mouseLastY + box.y);
      sendLineManager.addPoint(mouseLastX + box.x + .1, mouseLastY + box.y);
      
      t1.drawLine(
        mouseLastX - t1.getScreenX(),
        mouseLastY - t1.getScreenY(),
        mouseLastX - t1.getScreenX() + .1,
        mouseLastY - t1.getScreenY(),
        drawingColor,
        drawingSize
      );
    }
    
    sendLineManager.endLine();
    sendLineManager.clearLine();
    mouseInteracting = false;
  }
  
  function mouseMove(e) {
    if (!mouseInteracting) return;
    
    mouseX = e.clientX;
    mouseY = e.clientY;
    
    if (mode == 'panning') {
      var dx = mouseLastX - mouseX,
          dy = mouseLastY - mouseY;
      
      shiftBox(dx, dy);
      buildTiles();
    } else if (mode == 'drawing') {
      var t1 = tileAtPoint(mouseLastX, mouseLastY),
          t2 = tileAtPoint(mouseX, mouseY);
      
      if (t1 == t2) { // Line is only on one tile
        
        // draw segment on tile
        t1.drawLine(
          mouseLastX - t1.getScreenX(),
          mouseLastY - t1.getScreenY(),
          mouseX     - t1.getScreenX(),
          mouseY     - t1.getScreenY(),
          drawingColor,
          drawingSize
        );
        
        // add segment or point to send line service
        if (!sendLineManager.hasLine)
          sendLineManager.addPoint(mouseLastX + box.x, mouseLastY + box.y);
        sendLineManager.addPoint(mouseX + box.x, mouseY + box.y);
        
      } else { // Line crosses tiles, stop drawing
        mouseInteracting = false;
        return;
      }
    }
    
    mouseLastX = mouseX;
    mouseLastY = mouseY;
  }
  
  function handleUpdate(update) {
    var t     = Canvas.tileSize,
        tiles = update.tiles,
        lines = update.getLines();
    
    for (var x in tiles) {
      for (var y in tiles[x]) {
        var tile = getTile(x, y);
        tile.lastUpdate = update.time;
        tile.shaded(false);
      }
    }
    
    for (var i = 0, il = lines.length; i < il; i++) {
      var line    = lines[i],
          points  = line[0];
      
      for (var j = 2, jl = lines[i][0].length; j < jl; j += 2) {
        // NOTE: This assumes lines do not cross tiles!
        var x       = points[j - 2],
            y       = points[j - 1],
            x1      = points[j],
            y1      = points[j + 1],
            tileX   = parseInt(Math.floor(x / t) * t),
            tileY   = parseInt(Math.floor(y / t) * t),
            tile    = getTile(tileX, tileY),
            tileBox = tile.getBox();
        
        tile.drawLine(
          x  - tileBox.x,
          y  - tileBox.y,
          x1 - tileBox.x,
          y1 - tileBox.y,
          line[1],
          line[2]
        );
      }
    }
  }
  
  function successfullyJoined() {
    sessionDialog.close();
    ReconnectDialog.getInstance().close();
    
    updateManager = new CanvasUpdateManager(serverConn, handleUpdate);
    updateManager.setTiles(activeTiles);
  }
  
  function failedToJoin() {
    sessionDialog.close();
    ReconnectDialog.getInstance().promptRetry(retryJoin);
  }
  
  function retryJoin() {
    serverConn.retryJoin();
  }
  
  this.setMode = function(newMode) {
    mode = newMode;
    if (mode == 'drawing') {
      area.css({cursor: 'crosshair'});
    } else if (mode == 'panning') {
      area.css({cursor: 'move'});
    }
  }
  
  this.resize = function() {
    if (container.width() < 0 || container.height() < 0) return;
    
    if (!hasBeenResized) {
      setTileLocation(0, 0);
      hasBeenResized = true;
    }
    
    box.x1 = box.x + container.width();
    box.y1 = box.y + container.height();
    
    area.
      width(container.width()).
      height(container.height());
    
    buildTiles();
  };
  
  this.setPaintColor = function(color) {
    drawingColor = color;
    sendLineManager.setLineColor(color);
  };
  
  this.setBrushSize = function(size) {
    drawingSize = size;
    sendLineManager.setLineSize(size);
  };
  
  this.jumpToTile = function(x, y) {
    setTileLocation(x, y);
    buildTiles();
  };
  
  this.setLocation = function(x, y) {
    setLocation(x, y);
    buildTiles();
  };
  
  this.getTiles = function() {
    return allTiles;
  };
  
  this.getTileImageURL = function(x ,y) {
    return getTile(x, y).getImageURL();
  }
  
  // initialization
  
  sessionDialog = new UnclosableMessageDialog(
    'Connecting',
    'Please wait, you are being connected...'
  );
  
  serverConn = new CanvasServerSession(successfullyJoined, failedToJoin);
  
  sendLineManager = new SendLineManager(serverConn);
  
  area = $('<div class="canvasArea" />').appendTo(container);
  
  area.mousedown(mouseInteractionBegan);
  area.mousemove(mouseMove);
  area.mouseup(mouseInteractionEnded);
  area.mouseout(function(e) {
    if (e.relatedTarget != document.body) return
    mouseInteractionEnded(e);
  });
  
  // prevent text selection
  container[0].onmousedown = function() {return false;}
  
  this.setMode('panning');
  buildTiles();
}



function hasCanvas() {
  return !!document.createElement('canvas').getContext;
}

function loadApplication() {
  var defaultColor    = '000000',
      defaultSize     = 3,
      canvasContainer = $('#canvasAreaContainer'),
      toolbar         = $('#toolbar'),
      mode,
      lastMode,
      modeNodes       = {
        panning: {
          radio: $('#panningMode'),
          button: $("label[for='panningMode']")
        },
        drawing: {
          radio: $('#drawingMode'),
          button: $("label[for='drawingMode']")
        }
      },
      cavasArea;
  
  function setMode(newMode) {
    mode = newMode;
    modeNodes[newMode].button.click();
    modeNodes[newMode].radio.attr('checked', 'checked');
    canvasArea.setMode(newMode);
  }
  
  canvasArea = new CanvasArea(canvasContainer);
  canvasArea.setPaintColor(defaultColor);
  canvasArea.setBrushSize(defaultSize);
  
  window.canvasArea = canvasArea;
  
  setMode('panning');
  
  $('#modeSelector').buttonset();
  
  function jumpToTile() {
    var m = 999999999999,
        x = parseInt($('#jumpX').val()),
        y = parseInt($('#jumpY').val())
    
    if (isNaN(x) || x > m || x < -m) x = 0;
    if (isNaN(y) || y > m || y < -m) y = 0;
    
    canvasArea.jumpToTile(x, -y);
  }
  
  $('#jumpDialog').dialog({
    resizable: false,
    modal:     true,
    autoOpen:  false,
    buttons: {
      Go: function() {
        jumpToTile();
        $(this).dialog('close');
      },
      Cancel: function() {
        $(this).dialog('close');
      }
    }
  });
  
  function jumpAndCloseDialog() {
    jumpToTile();
    $('#jumpDialog').dialog('close');
  }
  
  $('#jumpDialog form').submit(function(e) {
    jumpAndCloseDialog();
    return false;
  });
  
  $('#jumpButton').button({
    icons: {primary: 'ui-icon-radio-off'}
  }).click(function(e) {
    $('#jumpDialog').dialog('open');
    $('#jumpX').focus();
    $('#jumpX')[0].select();
  });
  
  $("#jumpDialog input[type='text']").keypress(function(e) {
    if (e.keyCode == 13) jumpAndCloseDialog();
  })

  $('#modeSelector input').
    change(function(e) {
      setMode($('#modeSelector input:checked').val());
    });
  
  $('#brushSize').slider({
    values: [1],
    min: 1,
    max: 50,
    change: function(e, ui) {
      canvasArea.setBrushSize(ui.value);
      setMode('drawing');
    }
  });
  
  $(window).keydown(function(e) {
    lastMode = mode;
    if (e.keyCode == 32 && mode == 'drawing') {
      setMode('panning');
    }
  }).keyup(function(e) {
    if (e.keyCode == 32) {
      setMode(lastMode);
    }
  });
  
  function resizeUI() {
    canvasContainer.height(window.innerHeight - toolbar.height());
    canvasArea.resize();
  }
  resizeUI();
  $(window).resize(resizeUI);
  
  $('#colorSwatch').css({backgroundColor: '#' + defaultColor});
  
  $('#colorSelector').ColorPicker({
    color: defaultColor,
    onChange: function (hsb, hex, rgb) {
      $('#colorSwatch').css('backgroundColor', '#' + hex);
      canvasArea.setPaintColor(hex);
      setMode('drawing');
    }
  });
}

