#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "raster_painter.h"

#define ThrowWandException(wand) \
  { \
    char *description; \
    ExceptionType severity; \
    description = MagickGetException(wand,&severity); \
    fprintf(stderr,"%s %s %lu %s\n", GetMagickModule(), description); \
    description = (char *) MagickRelinquishMemory(description); \
    exit(-1); \
  }

void freeLine(Line *line) {
  free(line->points);
  free(line);
}

// void freeLineSet(LineSet *lineSet) {
//   unsigned int i;
//   for (i = 0; i < lineSet->count; i++)
//     freeLine(lineSet->lines[i]);
//   free(lineSet->lines);
//   free(lineSet);
// }

void drawLine(RasterPainter *painter, Line* line) {
  unsigned int i;
  char colorBuffer[8];
  
  assert(line->pointCount % 2 == 0 && line->pointCount > 0);
  
  sprintf(colorBuffer, "#%06x", line->color);
  
  DrawSetStrokeWidth(painter->dWand, line->size);
  PixelSetColor(painter->cWand, colorBuffer);
  DrawSetStrokeColor(painter->dWand, painter->cWand);
  
  for (i = 0; i <= line->pointCount - 4; i += 2) {
    DrawLine(painter->dWand, line->points[i], line->points[i + 1],
             line->points[i + 2], line->points[i + 3]);
  }
}

// void drawLineSet(RasterPainter *painter, LineSet *lineSet) {
//   unsigned int i;
//   for (i = 0; i < lineSet->count; i++)
//     drawLine(painter, lineSet->lines[i]);
// }




RasterPainter* painterStart(char *filename, unsigned int width,
                            unsigned int height) {
  RasterPainter *painter;
  MagickBooleanType status;
  
  // Setup drawing tools
  MagickWandGenesis();
  painter           = malloc(sizeof(RasterPainter));
  painter->mWand    = NewMagickWand();
  painter->dWand    = NewDrawingWand();
  painter->cWand    = NewPixelWand();
  painter->filename = malloc(sizeof(char) * strlen(filename));
  
  strcpy(painter->filename, filename);
  
  // Read image
  status = MagickReadImage(painter->mWand, filename);
  
  if (status == MagickFalse) {
    // Make image
    PixelSetColor(painter->cWand, "white");
    MagickNewImage(painter->mWand, width, height, painter->cWand);
  }
  
  // Setup stroke
  DrawSetStrokeAntialias(painter->dWand, MagickTrue);
  DrawSetStrokeLineCap(painter->dWand, RoundCap);
  DrawSetStrokeLineJoin(painter->dWand, RoundJoin);
  
  return painter;
}

void painterFinish(RasterPainter *painter) {
  MagickBooleanType status;
  
  // Set compression
  MagickSetCompression(painter->mWand, BZipCompression);
  MagickSetCompressionQuality(painter->mWand, 4);
  
  // Write image
  MagickDrawImage(painter->mWand, painter->dWand);
  status = MagickWriteImages(painter->mWand, painter->filename, MagickTrue);
  
  if (status == MagickFalse)
    ThrowWandException(painter->mWand);
  
  // Cleanup
  painter->cWand = DestroyPixelWand(painter->cWand);
  painter->mWand = DestroyMagickWand(painter->mWand);
  painter->dWand = DestroyDrawingWand(painter->dWand);
  MagickWandTerminus();
  
  free(painter->filename);
  free(painter);
}