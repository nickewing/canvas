/*
 * Author: Nick Ewing <nick@nickewing.net>
 * Copyright 2009 Nick Ewing.
 * 
 * General functions for reading and writing terms from Erlang
 */

typedef unsigned char byte;

int erlCommRead(byte **buf);
int erlCommWrite(byte *buf, int len);
int erlCommReadExact(byte *buf, int len);
int erlCommWriteExact(byte *buf, int len);