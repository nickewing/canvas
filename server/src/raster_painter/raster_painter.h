#include <wand/MagickWand.h>

typedef struct {
  MagickWand    *mWand;
  DrawingWand   *dWand;
  PixelWand     *cWand;
  char          *filename;
} RasterPainter;

typedef struct {
  unsigned int  size;
  unsigned int  color;
  float         *points;
  unsigned int  pointCount;
} Line;

void freeLine(Line*);

void drawLine(RasterPainter *painter, Line* line);

RasterPainter* painterStart(char *filename, unsigned int width,
                            unsigned int height);
void painterFinish(RasterPainter *painter);