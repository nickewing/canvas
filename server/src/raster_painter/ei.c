/*
 * Author: Nick Ewing <nick@nickewing.net>
 * Copyright 2009 Nick Ewing.
 * 
 * Erlang interface for raster_painter
 */

#include <stdlib.h>
#include <string.h>
#include "erl_interface.h"
#include "ei.h"
#include "erl_comm.h"
#include "raster_painter.h"

typedef struct {
  RPCanvas *canvas;
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

RPLine* etermToRPLine(ETERM *term) {
  ETERM         *list,
                *head,
                *tail,
                *size,
                *color,
                *points;
  RPLine        *line;
  unsigned int  pointsLen,
                i;
  
  if (!ERL_IS_TUPLE(term))
    return NULL;
  
  size   = erl_element(1, term);
  color  = erl_element(2, term);
  points = erl_element(3, term);
  
  if (!ERL_IS_LIST(points))
    return NULL;
  
  pointsLen = erl_length(points);
  
  if (pointsLen < 1)
    return NULL;
  
  line              = malloc(sizeof(RPLine));
  line->size        = ERL_INT_VALUE(size);
  line->color       = ERL_INT_VALUE(color);
  line->pointCount  = pointsLen;
  line->points      = malloc(sizeof(int) * pointsLen);
  
  list = points;
  for (i = 0; i < pointsLen; i++) {
    head = erl_hd(list);
    line->points[i] = etermNumToFloat(head);
    erl_free_term(head);
    tail = erl_tl(list);
    erl_free_term(list);
    list = tail;
  }
  
  if (list != points)
    erl_free_term(list);
  
  erl_free_term(size);
  erl_free_term(color);
  erl_free_term(points);
  
  return line;
}

ETERM* eiStartDrawingReq(ETERM *cmdTuple, State *state) {
  ETERM   *eFilename,
          *height,
          *width;
  char    *filename;
  char    result[30] = "unknown_error";
  
  if (!state || !state->canvas) {
    strcpy(result, "already_started");
  } else {
    eFilename = erl_element(2, cmdTuple);
    width     = erl_element(3, cmdTuple);
    height    = erl_element(4, cmdTuple);
    
    if (!ERL_IS_LIST(eFilename)) {
      strcpy(result, "invalid_filename");
    } else if(!ERL_IS_INTEGER(width)) {
      strcpy(result, "invalid_width");
    } else if (!ERL_IS_INTEGER(height)) {
      strcpy(result, "invalid_height");
    } else {
      filename  = erl_iolist_to_string(eFilename);
    
      state->canvas = RPNewCanvas(filename, ERL_INT_VALUE(width),
                                  ERL_INT_VALUE(height));
      free(filename);
    }
    
    erl_free_term(eFilename);
    erl_free_term(height);
    erl_free_term(width);
  }
  
  return erl_mk_atom(result);
}

ETERM* eiDrawLineReq(ETERM *cmdTuple, State *state) {
  ETERM   *eLine;
  RPLine  *line;
  
  eLine = erl_element(2, cmdTuple);
  
  if (!state->canvas)
    return erl_mk_atom("not_started");
  
  if ((line = etermToRPLine(eLine))) {
    if (RPDrawLine(state->canvas, line))
      return erl_mk_atom("ok");
    else
      return erl_mk_atom("error");
  } else {
    return erl_mk_atom("invalid_line");
  }
}

ETERM* eiStopDrawingReq(ETERM *cmdTuple, State *state) {
  int res;
  
  if (state->canvas) {
    res = RPDrawCanvas(state->canvas);
    RPFreeCanvas(state->canvas);
    return erl_mk_atom(res ? "ok" : "error");
  } else {
    return erl_mk_atom("not_started");
  }
}

ETERM* eiRouteReq(byte *buf, State *state) {
  ETERM   *cmdTuple,
          *eCmd,
          *result;
  char    *cmd;
  
  cmdTuple  = erl_decode(buf);
  eCmd    = erl_element(1, cmdTuple);
  cmd     = ERL_ATOM_PTR(eCmd);
  if (strcmp(cmd, "start_drawing") == 0) {
    result = eiStartDrawingReq(cmdTuple, state);
  } else if (strcmp(cmd, "draw_line") == 0) {
    result = eiDrawLineReq(cmdTuple, state);
  } else if (strcmp(cmd, "stop_drawing") == 0) {
    result = eiStopDrawingReq(cmdTuple, state);
  } else {
    result = erl_mk_atom("invalid_request");
  }
  
  erl_free_compound(cmdTuple);
  erl_free_term(eCmd);
  
  return result;
}

int main() {
  ETERM   *result;
  byte    *buf;
  State   *state;
  
  state = malloc(sizeof(State));
  
  erl_init(NULL, 0);
  
  while (erlCommRead(&buf) > 0) {
    result = eiRouteReq(buf, state);
    erl_encode(result, buf);
    erlCommWrite(buf, erl_term_len(result));
    erl_free_term(result);
  }
  
  if (state->canvas)
    RPFreeCanvas(state->canvas);
  free(state);
  
  return 0;
}
