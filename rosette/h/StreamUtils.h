/* Mode: -*- C++ -*- */
/* @BC
 *		                Copyright (c) 1993
 *	    by Microelectronics and Computer Technology Corporation (MCC)
 *				All Rights Reserved
 *
 *	Permission to use, copy, modify, and distribute this software and its
 *	documentation for any purpose and without fee is hereby granted,
 *	provided that this notice be retained unaltered, and that the name of
 *	MCC and its shareholders and participants shall not be used in
 *	advertising or publicity pertaining to distribution of the software
 *	without specific written prior permission.
 *
 *	THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 *	IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 *	WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

/*
 * $Header$
 *
 * $Log$
 *
 @EC */

#if !defined(_StreamUtils_h)
#define _StreamUtils_h 1

#ifdef __GNUG__
#pragma interface
#endif

#include "rosette.h"

#include <ctype.h>

inline int isoctal(char c) { return (isdigit(c) && (c < '8')); }

typedef int (*errfn)(FILE*);

extern int readError(FILE*, const char*, ...);
extern int eofError(FILE*);

extern char getEscapedChar(FILE*);
extern char getSafeChar(FILE*, char = '\\');

extern int putEscapedChar(char, FILE*);
extern int putSafeChar(char, FILE*, char = '\\');

extern char* safeStrcpy(char*, char*, char = '\\');
extern int safeStrlen(char*, char = '\\');

#endif
