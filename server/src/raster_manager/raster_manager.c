#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "raster_manager.h"

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

void freeLineSet(LineSet *lineSet) {
  unsigned int i;
  for (i = 0; i < lineSet->count; i++)
    freeLine(lineSet->lines[i]);
  free(lineSet->lines);
  free(lineSet);
}

void drawLine(MagickWand* m_wand, DrawingWand* d_wand, PixelWand* c_wand,
              Line* line) {
  unsigned int i;
  char colorBuffer[8];
  
  assert(line->pointCount % 2 == 0 && line->pointCount > 0);
  
  sprintf(colorBuffer, "#%06x", line->color);
  
  DrawSetStrokeWidth(d_wand, line->size);
  PixelSetColor(c_wand, colorBuffer);
  DrawSetStrokeColor(d_wand, c_wand);
  
  for (i = 0; i <= line->pointCount / 2 - 1; i += 2) {
    DrawLine(d_wand, line->points[i], line->points[i + 1],
             line->points[i + 2], line->points[i + 3]);
  }
}

void drawLineSet(MagickWand* m_wand, DrawingWand* d_wand, PixelWand* c_wand,
                 LineSet *lineSet) {
  unsigned int i;
  for (i = 0; i < lineSet->count; i++)
    drawLine(m_wand, d_wand, c_wand, lineSet->lines[i]);
}

int drawLinesToImage(char *filename, unsigned int width, unsigned int height,
                     LineSet *lineSet) {
  MagickBooleanType status;
  
  // Setup drawing tools
  MagickWandGenesis();
  MagickWand*   m_wand = NewMagickWand();
  DrawingWand*  d_wand = NewDrawingWand();
  PixelWand*    c_wand = NewPixelWand();
  
  // Read image
  status = MagickReadImage(m_wand, filename);
  
  if (status == MagickFalse) {
    // Make image
    PixelSetColor(c_wand, "white");
    MagickNewImage(m_wand, width, height, c_wand);
  }
  
  // Setup stroke
  DrawSetStrokeAntialias(d_wand, MagickTrue);
  DrawSetStrokeLineCap(d_wand, RoundCap);
  DrawSetStrokeLineJoin(d_wand, RoundJoin);
  
  // Draw lines
  drawLineSet(m_wand, d_wand, c_wand, lineSet);
  
  // Set compression
  MagickSetCompression(m_wand, BZipCompression);
  MagickSetCompressionQuality(m_wand, 4);
  
  // Write image
  MagickDrawImage(m_wand, d_wand);
  status = MagickWriteImages(m_wand, filename, MagickTrue);
  
  if (status == MagickFalse)
    ThrowWandException(m_wand);
  
  // Cleanup
  c_wand = DestroyPixelWand(c_wand);
  m_wand = DestroyMagickWand(m_wand);
  d_wand = DestroyDrawingWand(d_wand);
  MagickWandTerminus();
  
  return 1;
}


