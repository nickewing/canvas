#include <stdlib.h>
#include <string.h>
#include "erl_interface.h"
#include "ei.h"
#include "erl_comm.h"
#include "raster_manager.h"

#define COMM_BUFFER_SIZE 2048

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

LineSet* etermToLineSet(ETERM *term) {
  ETERM         *list,
                *head,
                *tail;
  LineSet       *ls;
  byte          failed = 0;
  Line          *line;
  unsigned int  listLen,
                i;
  
  if (!ERL_IS_LIST(term))
    return NULL;
  
  listLen = erl_length(term);
  
  if (listLen < 1)
    return NULL;
  
  ls        = malloc(sizeof(LineSet));
  ls->count = listLen;
  ls->lines = malloc(sizeof(Line) * listLen);
  
  list = term;
  for (i = 0; i < listLen && !failed; i++) {
    head = erl_hd(list);
    line = etermToLine(head);
    erl_free_term(head);
    
    if (line) {
      ls->lines[i] = line;
      tail = erl_tl(list);
      erl_free_term(list);
      list = tail;
    } else {
      failed = 1;
    }
  }
  
  if (list != term)
    erl_free_term(list);
  
  if (ls->count && !failed)
    return ls;
  
  freeLineSet(ls);
  return NULL;
}

ETERM* drawLinesReq(ETERM *eTuple) {
  ETERM   *eFilename,
          *eHeight,
          *eWidth,
          *eLineSet;
  LineSet *lineSet;
  char    *filename;
  char    result[30] = "unknown_error";
  
  eFilename = erl_element(2, eTuple);
  eWidth    = erl_element(3, eTuple);
  eHeight   = erl_element(4, eTuple);
  eLineSet  = erl_element(5, eTuple);
  
  if (!ERL_IS_LIST(eFilename)) {
    strcpy(result, "invalid_filename");
  } else if (!ERL_IS_LIST(eLineSet)) {
    strcpy(result, "invalid_lines");
  } else if(!ERL_IS_INTEGER(eWidth)) {
    strcpy(result, "invalid_width");
  } else if (!ERL_IS_INTEGER(eHeight)) {
    strcpy(result, "invalid_height");
  } else {
    filename  = erl_iolist_to_string(eFilename);
    
    if ((lineSet = etermToLineSet(eLineSet))) {
      if (drawLinesToImage(filename, ERL_INT_VALUE(eWidth),
                           ERL_INT_VALUE(eHeight), lineSet)) {
        strcpy(result, "ok");
      } else {
        strcpy(result, "drawing_failed");
      }
      freeLineSet(lineSet);
    } else {
      strcpy(result, "line_parsing_failed");
    }
    
    free(filename);
  }
  
  erl_free_term(eFilename);
  erl_free_term(eHeight);
  erl_free_term(eWidth);
  erl_free_term(eLineSet);
  
  return erl_mk_atom(result);
}

ETERM* routeRequest(byte* buf) {
  ETERM   *eTuple,
          *eCmd,
          *eResult;
  char    *cmd;
  
  eTuple  = erl_decode(buf);
  eCmd    = erl_element(1, eTuple);
  cmd     = ERL_ATOM_PTR(eCmd);
  
  if (strcmp(cmd, "draw_lines") == 0) {
    eResult = drawLinesReq(eTuple);
  } else {
    eResult = erl_mk_atom("invalid_request");
  }
  
  erl_free_compound(eTuple);
  erl_free_term(eCmd);
  
  return eResult;
}

int main() {
  ETERM   *eResult;
  byte    buf[COMM_BUFFER_SIZE];
  
  erl_init(NULL, 0);
  
  while (read_cmd(buf) > 0) {
    eResult = routeRequest(buf);
    erl_encode(eResult, buf);
    write_cmd(buf, erl_term_len(eResult));
    
    erl_free_term(eResult);
  }
  
  return 0;
}
