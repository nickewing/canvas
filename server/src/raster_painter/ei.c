#include <stdlib.h>
#include <string.h>
#include "erl_interface.h"
#include "ei.h"
#include "erl_comm.h"
#include "raster_painter.h"

typedef struct {
  RasterPainter *painter;
} State;

float etermNumToFloat(ETERM* term) {
  if (ERL_IS_FLOAT(term)) {
    return ERL_FLOAT_VALUE(term);
  } else if (ERL_IS_INTEGER(term)) {
    return (float) ERL_INT_VALUE(term);
  } else {
    return 0.0;
  }
}

Line* etermToLine(ETERM *term) {
  ETERM         *list,
                *head,
                *tail,
                *eSize,
                *eColor,
                *ePoints;
  Line          *line;
  unsigned int  ePointsLen,
                i;
  
  if (!ERL_IS_TUPLE(term))
    return NULL;
  
  eSize   = erl_element(1, term);
  eColor  = erl_element(2, term);
  ePoints = erl_element(3, term);
  
  if (!ERL_IS_LIST(ePoints))
    return NULL;
  
  ePointsLen = erl_length(ePoints);
  
  if (ePointsLen < 1)
    return NULL;
  
  line              = malloc(sizeof(Line));
  line->size        = ERL_INT_VALUE(eSize);
  line->color       = ERL_INT_VALUE(eColor);
  line->pointCount  = ePointsLen;
  line->points      = malloc(sizeof(int) * ePointsLen);
  
  list = ePoints;
  for (i = 0; i < ePointsLen; i++) {
    head = erl_hd(list);
    line->points[i] = etermNumToFloat(head);
    erl_free_term(head);
    tail = erl_tl(list);
    erl_free_term(list);
    list = tail;
  }
  
  if (list != ePoints)
    erl_free_term(list);
  
  erl_free_term(eSize);
  erl_free_term(eColor);
  erl_free_term(ePoints);
  
  return line;
}

ETERM* startDrawingReq(ETERM *eTuple, State *state) {
  ETERM   *eFilename,
          *eHeight,
          *eWidth;
  char    *filename;
  char    result[30] = "unknown_error";
  
  
  eFilename = erl_element(2, eTuple);
  eWidth    = erl_element(3, eTuple);
  eHeight   = erl_element(4, eTuple);
  
  if (!ERL_IS_LIST(eFilename)) {
    strcpy(result, "invalid_filename");
  } else if(!ERL_IS_INTEGER(eWidth)) {
    strcpy(result, "invalid_width");
  } else if (!ERL_IS_INTEGER(eHeight)) {
    strcpy(result, "invalid_height");
  } else {
    filename  = erl_iolist_to_string(eFilename);
    // if (state->painter) {
    //   painterFinish(state->painter);
    // }
    
    state->painter = painterStart(filename, ERL_INT_VALUE(eWidth),
                                  ERL_INT_VALUE(eHeight));
    free(filename);
  }
  
  erl_free_term(eFilename);
  erl_free_term(eHeight);
  erl_free_term(eWidth);
  
  return erl_mk_atom(result);
}

ETERM* drawLineReq(ETERM *eTuple, State *state) {
  ETERM *eLine;
  Line  *line;
  
  eLine = erl_element(2, eTuple);
  
  if (!state->painter)
    return erl_mk_atom("not_started");
  
  if ((line = etermToLine(eLine))) {
    drawLine(state->painter, line);
    return erl_mk_atom("ok");
  } else {
    return erl_mk_atom("invalid_line");
  }
}

ETERM* stopDrawingReq(ETERM *eTuple, State *state) {
  if (state->painter) {
    painterFinish(state->painter);
    return erl_mk_atom("ok");
  } else {
    return erl_mk_atom("not_started");
  }
}

ETERM* identity(ETERM *eTuple) {
  return erl_copy_term(eTuple);
}

ETERM* routeRequest(byte *buf, State *state) {
  ETERM   *eTuple,
          *eCmd,
          *eResult;
  char    *cmd;
  
  eTuple  = erl_decode(buf);
  eCmd    = erl_element(1, eTuple);
  cmd     = ERL_ATOM_PTR(eCmd);
  if (strcmp(cmd, "start_drawing") == 0) {
    eResult = startDrawingReq(eTuple, state);
  } else if (strcmp(cmd, "draw_line") == 0) {
    eResult = drawLineReq(eTuple, state);
  } else if (strcmp(cmd, "stop_drawing") == 0) {
    eResult = stopDrawingReq(eTuple, state);
  } else if (strcmp(cmd, "identity") == 0) {
    eResult = identity(eTuple);
  } else {
    eResult = erl_mk_atom("invalid_request");
  }
  
  erl_free_compound(eTuple);
  erl_free_term(eCmd);
  
  return eResult;
}

int main() {
  ETERM   *eResult;
  byte    *buf;
  State   *state;
  
  state = malloc(sizeof(State));
  
  erl_init(NULL, 0);
  
  while (read_cmd(&buf) > 0) {
    eResult = routeRequest(buf, state);
    erl_encode(eResult, buf);
    write_cmd(buf, erl_term_len(eResult));
    
    erl_free_term(eResult);
  }
  
  return 0;
}
