#include <wand/MagickWand.h>

typedef struct {
  unsigned int  size;
  unsigned int  color;
  float         *points;
  unsigned int  pointCount;
} Line;

typedef struct {
  Line          **lines;
  unsigned int  count;
} LineSet;

void freeLine(Line*);
void freeLineSet(LineSet*);

void drawLine(MagickWand*, DrawingWand*, PixelWand*, Line*);
void drawLineSet(MagickWand*, DrawingWand*, PixelWand*, LineSet*);

int drawLinesToImage(char* filename, unsigned int width, unsigned int height,
                     LineSet *);