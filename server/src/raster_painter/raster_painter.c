/*
 * Author: Nick Ewing <nick@nickewing.net>
 * Copyright 2009 Nick Ewing.
 * 
 * Paints vector lines to a raster image using ImageMagick API
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "raster_painter.h"

void RPLineFree(RPLine *line) {
  free(line->points);
  free(line);
}

/*
  Draw a line using a RPCanvas
*/
int RPDrawLine(RPCanvas *canvas, RPLine* line) {
  unsigned int  i;
  char          colorBuffer[8];
  
  if (line->pointCount % 2 != 0 || line->pointCount < 0)
    return 0;
  
  sprintf(colorBuffer, "#%06x", line->color);
  
  DrawSetStrokeWidth(canvas->dWand, line->size);
  PixelSetColor(canvas->cWand, colorBuffer);
  DrawSetStrokeColor(canvas->dWand, canvas->cWand);
  
  for (i = 0; i <= line->pointCount - 4; i += 2)
    DrawLine(canvas->dWand, line->points[i], line->points[i + 1],
             line->points[i + 2], line->points[i + 3]);
  
  return 1;
}

/*
  Setup a RPCanvas
*/
RPCanvas* RPNewCanvas(char *filename, unsigned int width,
                      unsigned int height) {
  RPCanvas          *canvas;
  MagickBooleanType status;
  
  /* Setup drawing tools */
  MagickWandGenesis();
  canvas           = malloc(sizeof(RPCanvas));
  canvas->mWand    = NewMagickWand();
  canvas->dWand    = NewDrawingWand();
  canvas->cWand    = NewPixelWand();
  canvas->filename = strdup(filename);
  
  /* Read image */
  status = MagickReadImage(canvas->mWand, filename);
  
  if (status == MagickFalse) {
    /* Make image */
    PixelSetColor(canvas->cWand, "white");
    MagickNewImage(canvas->mWand, width, height, canvas->cWand);
  }
  
  /* Setup stroke */
  DrawSetStrokeAntialias(canvas->dWand, MagickTrue);
  DrawSetStrokeLineCap(canvas->dWand, RoundCap);
  DrawSetStrokeLineJoin(canvas->dWand, RoundJoin);
  
  return canvas;
}

/*
  Draw 
*/
int RPDrawCanvas(RPCanvas *canvas) {
  MagickBooleanType status;
  
  /* Set compression */
  MagickSetCompression(canvas->mWand, JPEGCompression);
  MagickSetImageCompressionQuality(canvas->mWand, 90);
  
  /* Write image */
  MagickDrawImage(canvas->mWand, canvas->dWand);
  status = MagickWriteImages(canvas->mWand, canvas->filename, MagickTrue);
  
  return status != MagickFalse;
}

void RPFreeCanvas(RPCanvas *canvas) {
  canvas->cWand = DestroyPixelWand(canvas->cWand);
  canvas->mWand = DestroyMagickWand(canvas->mWand);
  canvas->dWand = DestroyDrawingWand(canvas->dWand);
  MagickWandTerminus();
  
  free(canvas->filename);
  free(canvas);
}