/*
 * Author: Nick Ewing <nick@nickewing.net>
 * Copyright 2009 Nick Ewing.
 * 
 * Paints vector lines to a raster image using ImageMagick API
 */

#include <wand/MagickWand.h>

typedef struct {
  MagickWand    *mWand;
  DrawingWand   *dWand;
  PixelWand     *cWand;
  char          *filename;
} RPCanvas;

typedef struct {
  unsigned int  size;
  unsigned int  color;
  float         *points;
  unsigned int  pointCount;
} RPLine;

void RPLineFree(RPLine*);

int RPDrawLine(RPCanvas *canvas, RPLine* line);
RPCanvas* RPNewCanvas(char *filename, unsigned int width, unsigned int height);
int RPDrawCanvas(RPCanvas *canvas);
void RPFreeCanvas(RPCanvas *canvas);