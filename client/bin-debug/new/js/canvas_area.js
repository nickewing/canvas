// TODO: have server send line box
// TODO: check tiles are actually copied by Update

//var TEST_IMAGE = 'http://www.yourjigsaw.org.uk/Commercial/fullsize/jigsaw500.jpg';
var TEST_IMAGE = '',
    TILE_SIZE  = 500;

function CanvasServerSession(readyCallback, failureCallback) {
  var HOST = 'http://localhost:8000/',
      sid;
  
  function serverRequest(action, data, success, failure) {
    data.sid = sid;
    return $.ajax({
      url:      HOST + action,
      data:     data,
      type:     'POST',
      success:  success,
      error:    failure
    });
  }
  
  serverRequest('join', {}, function(data) {
    var resp = data.split(' ');
    if (resp[0] == 'OK') {
      sid = resp[1];
      //console.log(sid);
      readyCallback();
    } else {
      failureCallback();
    }
  }, failureCallback);
  
  function serializeLine(points, color, size) {
    return [points.join(','), color, size].join('/');
  }
  
  return {
    getUpdate: function(tiles, success, failure) {
      var tileStrs = [];
      
      for(x in tiles) {
        for (y in tiles[x]) {
          tileStrs.push([
            x, y, parseInt(x) + TILE_SIZE, parseInt(y) + TILE_SIZE
          ].join(',') + '/' + tiles[x][y]);
        }
      }
      
      if (tileStrs.length < 1) return;
      
      return serverRequest('update', {t: tileStrs.join(';')}, success, failure);
    },
    sendLine: function(points, color, size, success, failure) {
      var lineStr = serializeLine(points, color, size);
      serverRequest('send_line', {l: lineStr}, success, failure);
    }
  }
}


function CanvasUpdateManager(serverConn, linesCallback) {
  var currentUpdate;
  
  function Update(tiles, success, failure) {
    // make a full copy of the tiles given
    var tiles = $.extend(true, {}, tiles),
        request,
        lineStr,
        lines,
        status,
        time,
        interface = {
          cancel: function() {
            request.abort();
          },
          getLines: function() {
            if (!lines) lines = unserializeLineArray(lineStr);
            return lines;
          },
          getTime: function() {return time;},
          getStatus: function() {return status;},
          getTiles: function() {return tiles;}
        };
    
    function lineBox(points) {
      var minX = points[0],
          minY = points[1],
          maxX = points[0],
          maxY = points[1];

      for (var i = 0; i < points.length; i += 2) {
        minX = Math.min(minX, points[i]);
        minY = Math.min(minY, points[i + 1]);
        maxX = Math.max(maxX, points[i]);
        maxY = Math.max(maxY, points[i + 1]);
      }

      return {x: minX, y: minY, x1: maxX, y1: maxY};
    }

    function unserializeLineArray(str) {
      var lines       = [],
          lineStrings = str.split(';');

      for (var i = 0; i < lineStrings.length; i++) {
        var lineParts = lineStrings[i].split('/'),
            points    = lineParts[0].split(','),
            color     = lineParts[1],
            size      = parseInt(lineParts[2]);

        for (var j = 0; j < points.length; j++)
          points[j] = parseFloat(points[j]);

        lines.push([points, color, size, lineBox(points)]);
      }

      return lines;
    }
    
    function handleUpdates(data) {
      var resp = data.split(' ');
      status = resp[0];
      
      switch (resp[0]) {
      case 'OK':
        time = resp[1]
        lineStr = resp[2];
        break;
      }
      
      success(interface);
    }

    function handleFailure() {
      failure(interface);
    }
    
    request = serverConn.getUpdate(tiles, handleUpdates, handleFailure);
    
    return interface;
  }
  
  function newUpdate(tiles) {
    if (currentUpdate) currentUpdate.cancel();
    currentUpdate = new Update(tiles, updateSuccess, updateFailure);
  }
  
  function updateSuccess(update) {
    if (update.getStatus() == 'OK' || update.getStatus() == 'TIMEOUT') { 
      newUpdate(update.getTiles());
      
      if (update.getStatus() == 'OK')
        linesCallback(update);
    } else {
      //console.log('Update failed: ' + update.getStatus());
    }
  }
  
  function updateFailure() {
    //console.log('Update failure.');
  }
  
  return {
    setTiles: function(tiles) {newUpdate(tiles);}
  }
}

function CanvasArea(container) {
  
  var box              = {x: 0, y: 0, x1: 0, y1: 0},
      currentTiles     = [],
      allTiles         = {},
      mode             = 'drawing',
      mouseInteracting = false,
      mouseLastX,
      mouseLastY,
      drawingColor     = 'rgba(90,0,0,1)',
      drawingSize      = 3,
      area,
      totalLinePoints  = 0,
      linePoints       = [],
      lineCutTimouet,
      hasBeenResized   = false,
      serverConn,
      activeTiles      = {},
      updateManager;
  
  function shiftBox(x, y) {
    box.x  += x;
    box.y  += y;
    box.x1 += x;
    box.y1 += y;
  }
  
  function newTile(x, y, imageURL) {
    var imageSource = new Image(),
        
        shade = $('<div><span>Loading...</span></div>'),
        
        image = $('<img />').hide(),
        
        canvas = $(['<canvas width="', TILE_SIZE,
                    '" height="', TILE_SIZE, '" />'].join('')),
        
        node = $('<div class="canvasTile" />').
                  width(TILE_SIZE).
                  height(TILE_SIZE).
                  hide().
                  append(image).
                  append(['<span>', (x / TILE_SIZE), ',',
                          (-y / TILE_SIZE), '</span>'].join('')).
                  append(canvas).
                  append(shade);
    
    imageSource.src = imageURL;
    $(imageSource).load(function(e) {
      image.attr({src: imageURL});
      image.show();
    });
    
    return {
      node: node,
      shade: shade,
      x: x,
      y: y,
      lastUpdate: 0,
      canvas: canvas,
      canvasContext: canvas[0].getContext('2d')
    }
  }
  
  function setLocation(x, y) {
    box = {x: x, y: y, x1: x + area.width(), y1: y + area.height()};
  }
  
  function setTileLocation(x, y) {
    box.x  = (TILE_SIZE * x) - (area.width() - TILE_SIZE) / 2;
    box.y  = (TILE_SIZE * y) - (area.height() - TILE_SIZE) / 2;
    box.x1 = (TILE_SIZE * x) + area.width() - (area.width() - TILE_SIZE) / 2;
    box.y1 = (TILE_SIZE * y) + area.height() - (area.height() - TILE_SIZE) / 2;
  }
  
  function positionTile(x, y, tile) {
    tile.node.css({left: x - box.x, top: y - box.y});
  }
  
  function tileAtPoint(x, y) {
    return getTile(
      Math.floor((box.x + x) / TILE_SIZE) * TILE_SIZE,
      Math.floor((box.y + y) / TILE_SIZE) * TILE_SIZE
    );
  }
  
  function getTile(x, y) {
    if (!allTiles[x]) allTiles[x] = {};
    
    if (!allTiles[x][y]) {
      var tile = newTile(x, y, TEST_IMAGE);
      area.append(tile.node);
      allTiles[x][y] = tile;
    }
    
    return allTiles[x][y];
  }
  
  function getTileScreenX(tile) {
    return tile.node.offset().left;
  }
  
  function getTileScreenY(tile) {
    return tile.node.offset().top;
  }
  
  function getTileBox(tile) {
    if (!tile.box) {
      var x = tile.x, y = tile.y;
      tile.box = {x: x, y: y, x1: x + TILE_SIZE, y1: y + TILE_SIZE};
    }
    
    return tile.box;
  }
  
  function activateTile(tile) {
    var x = tile.x, y = tile.y;
    
    tile.node.show();
    positionTile(x, y, tile);
    
    if (!activeTiles[x]) activeTiles[x] = {};
    activeTiles[x][y] = tile.lastUpdate;
  }
  
  function deactivateTile(tile) {
    var x = tile.x, y = tile.y;
    
    tile.node.hide();
    tile.shade.show();
    
    delete activeTiles[x][y];
  }
  
  function tileArrayEqual(a, b) {
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
  
  function buildTiles() {
    var x0       = Math.floor(box.x / TILE_SIZE) * TILE_SIZE,
        y0       = Math.floor(box.y / TILE_SIZE) * TILE_SIZE,
        xf       = Math.ceil(box.x1 / TILE_SIZE) * TILE_SIZE,
        yf       = Math.ceil(box.y1 / TILE_SIZE) * TILE_SIZE,
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
        y += TILE_SIZE;
      }
      x += TILE_SIZE;
    }
    
    // Deactive old tiles in current tiles
    for (var i = 0; i < currentTiles.length; i++) {
      if (newTiles.indexOf(currentTiles[i]) == -1) {
        deactivateTile(currentTiles[i]);
      }
    }
    
    if (!tileArrayEqual(currentTiles, newTiles)) {
      // Reset the connection with the server since tiles were updated
      if (updateManager) updateManager.setTiles(activeTiles);
    }
    
    currentTiles = newTiles;
  }
  
  function resize() {
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
  }
  
  function mouseInteractionBegan(e) {
    mouseInteracting  = true;
    mouseLastX = e.clientX;
    mouseLastY = e.clientY;
  }
  
  function mouseInteractionEnded(e) {
    if (!totalLinePoints && mode == 'drawing') {
      // draw point if no movement occurred
      var t1 = tileAtPoint(mouseLastX, mouseLastY);
      
      addLinePoint(mouseLastX + box.x, mouseLastY + box.y);
      addLinePoint(mouseLastX + box.x + .1, mouseLastY + box.y);
      
      drawTileLine(
        t1,
        mouseLastX - getTileScreenX(t1),
        mouseLastY - getTileScreenY(t1),
        mouseLastX - getTileScreenX(t1) + .1,
        mouseLastY - getTileScreenY(t1),
        drawingColor,
        drawingSize
      );
    }
    
    endLine();
    clearLine();
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
        drawTileLine(
          t1,
          mouseLastX - getTileScreenX(t1),
          mouseLastY - getTileScreenY(t1),
          mouseX     - getTileScreenX(t1),
          mouseY     - getTileScreenY(t1),
          drawingColor,
          drawingSize
        );
        
        // add segment or point to send line service
        if (!linePoints.pointCount)
          addLinePoint(mouseLastX + box.x, mouseLastY + box.y);
        addLinePoint(mouseX + box.x, mouseY + box.y);
        
      } else { // Line crosses tiles, stop drawing
        mouseInteracting = false;
        return;
      }
    }
    
    mouseLastX = mouseX;
    mouseLastY = mouseY;
  }
  
  function addLinePoint(x, y) {
    if (!linePoints.length) {
      if (lineCutTimouet) clearTimeout(lineCutTimouet);
      lineCutTimouet = setTimeout(endLine, 300);
    }
    
    ++totalLinePoints;
    linePoints.push(x);
    linePoints.push(y);
  }
  
  function endLine() {
    sendLine();
    if (lineCutTimouet) clearTimeout(lineCutTimouet);
    linePoints = [];
  }
  
  function clearLine() {
    totalLinePoints = 0;
  }
  
  function sendLine() {
    if (!linePoints.length) return;
    serverConn.sendLine(linePoints, drawingColor, drawingSize);
    //console.log(linePoints.join(','));
  }
  
  function drawTileLine(tile, x, y, x1, y1, color, size) {
    var context = tile.canvasContext;
    
    context.lineCap     = 'round';
    context.lineWidth   = size;
    context.strokeStyle = '#' + color;
    
    context.beginPath();
    context.moveTo(x, y);
    context.lineTo(x1, y1);
    context.stroke();
  }
  
  function setMode(newMode) {
    mode = newMode;
    if (mode == 'drawing') {
      area.css({cursor: 'crosshair'});
    } else if (mode == 'panning') {
      area.css({cursor: 'move'});
    }
  }
  
  function serverConnected() {
    //console.log('server connected');
    updateManager = new CanvasUpdateManager(serverConn, handleUpdate);
    updateManager.setTiles(activeTiles);
  }
  
  function connectionFailed() {
    //console.log('failed to connect to server');
  }
  
  function handleUpdate(update) {
    
    // for (var x in activeTiles) {
    //   for (var y in activeTiles[x]) {
    //     var tile = getTile(x, y);
    //     tile.lastUpdate = lastUpdate;
    //     tile.shade.hide();
    //   }
    // }
    
    var tiles = update.getTiles(),
        lines = update.getLines();
    
    for (var x in tiles) {
      for (var y in tiles[x]) {
        var tile = getTile(x, y);
        tile.lastUpdate = update.getTime();
        tile.shade.hide();
      }
    }
    
    for (var i = 0; i < lines.length; i++) {
      
      for (var j = 2; j < lines[i][0].length; j += 2) {
        // NOTE: This assumes lines do not cross tiles!
        
        var x       = lines[i][0][j - 2],
            y       = lines[i][0][j - 1],
            x1      = lines[i][0][j],
            y1      = lines[i][0][j + 1],
            tileX   = parseInt(Math.floor(x / TILE_SIZE) * TILE_SIZE),
            tileY   = parseInt(Math.floor(y / TILE_SIZE) * TILE_SIZE),
            tile    = getTile(tileX, tileY),
            tileBox = getTileBox(tile);
        
        drawTileLine(
          tile,
          x  - tileBox.x,
          y  - tileBox.y,
          x1 - tileBox.x,
          y1 - tileBox.y,
          lines[i][1],
          lines[i][2]
        );
      }
      
      
    }
  }
  
  function initialize() {
    serverConn = new CanvasServerSession(serverConnected, connectionFailed);
    
    area = $('<div class="canvasArea" />').appendTo(container);
    
    area.mousedown(mouseInteractionBegan);
    area.mousemove(mouseMove);
    area.mouseup(mouseInteractionEnded);
    area.mouseout(function(e) {
      if (e.relatedTarget != document.body) return
      mouseInteractionEnded();
    });
    
    area
      .bind('mousewheel', function(event, delta) {
        console.log(event);
        console.log(delta);
        var dir = delta > 0 ? 'Up' : 'Down',
            vel = Math.abs(delta);
        $(this).text(dir + ' at a velocity of ' + vel);
        return false;
      });
    
  
    // prevent text selection
    container[0].onmousedown = function() {return false;}
    
    setMode('panning');
    buildTiles();
  }
  
  initialize();
  return {
    setPaintColor: function(color) {drawingColor = color;},
    setBrushSize: function(size) {drawingSize = size;},
    jumpToTile: function(x, y) {
      setTileLocation(x, y);
      buildTiles();
    },
    setLocation: function(x, y) {
      setLocation(x, y);
      buildTiles();
    },
    resize: resize,
    setMode: setMode,
    getTiles: function() {return allTiles;}
  };
}



var canvasArea;

function hasCanvas() {
  return !!document.createElement('canvas').getContext;
}

function loadApplication() {
  var defaultColor    = '000000',
      defaultSize     = 3,
      canvasContainer = $('#canvasAreaOuterContainer'),
      toolbar         = $('#toolbar'),
      modeNodes       = {
        panning: {
          radio: $('#panningMode'),
          button: $("label[for='panningMode']")
        },
        drawing: {
          radio: $('#drawingMode'),
          button: $("label[for='drawingMode']")
        },
      };
  
  function setMode(mode) {
    modeNodes[mode].button.click();
    modeNodes[mode].radio.attr('checked', 'checked');
    canvasArea.setMode(mode);
  }
  
  canvasArea = new CanvasArea(canvasContainer);
  canvasArea.setPaintColor(defaultColor);
  canvasArea.setBrushSize(defaultSize);
  
  setMode('panning');
  
  $('#modeSelector').buttonset();
  
  function jumpToTile() {
    var x = parseInt($('#jumpX').val()),
        y = parseInt($('#jumpY').val())
    
    if (isNaN(x) || x > 999999999999 || x < 0) x = 0;
    if (isNaN(y) || y > 999999999999 || y < 0) y = 0;
    
    canvasArea.jumpToTile(x, -y);
  }
  
  $('#jumpDialog').dialog({
    resizable: false,
    modal: true,
    autoOpen: false,
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
  
  $('#jumpDialog form').submit(function(e) {
    jumpToTile();
    $('#jumpDialog').dialog('close');
    return false;
  });
  
  $('#jumpButton').button({
    icons: {primary: 'ui-icon-radio-off'}
  }).click(function(e) {
    $('#jumpDialog').dialog('open');
    $('#jumpX').focus();
  });

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
  
  function resizeUI(e) {
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