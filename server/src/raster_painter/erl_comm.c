/*
 * Author: Nick Ewing <nick@nickewing.net>
 * Copyright 2009 Nick Ewing.
 * 
 * General functions for reading and writing terms from Erlang
 */

#include <unistd.h>
#include <stdlib.h>
#include "erl_comm.h"

int erlCommRead(byte **buf) {
  int len;
  byte cBuf[2];
  if (erlCommReadExact(cBuf, 2) != 2)
    return -1;
  len = (cBuf[0] << 8) | cBuf[1];
  *buf = malloc(sizeof(byte) * len);
  return erlCommReadExact(*buf, len);
}

int erlCommWrite(byte *buf, int len) {
  byte li;
  li = (len >> 8) & 0xff;
  erlCommWriteExact(&li, 1);
  li = len & 0xff;
  erlCommWriteExact(&li, 1);
  return erlCommWriteExact(buf, len);
}

int erlCommReadExact(byte *buf, int len) {
  int i, got=0;
  do {
    if ((i = read(0, buf + got, len - got)) <= 0)
      return i;
    got += i;
  } while (got<len);
  return len;
}

int erlCommWriteExact(byte *buf, int len) {
  int i, wrote = 0;
  do {
    if ((i = write(1, buf + wrote, len - wrote)) <= 0)
      return i;
    wrote += i;
  } while (wrote<len);
  return len;
}