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
 @EC */

#ifdef __GNUG__
#pragma implementation
#endif

#include "misc.h"


#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <errno.h>

extern "C" {
void
suicide(const char* fmt ...)
{
    va_list args;
    int status;

    va_start(args, fmt);
    fputs("*** fatal error: ", stderr);
    status = vfprintf(stderr, fmt, args);
    va_end(args);
    putc('\n', stderr);
    exit(1);
}


void
warning(const char* fmt ...)
{
    va_list args;
    int status;

    va_start(args, fmt);
    fputs("*** warning: ", stderr);
    status = vfprintf(stderr, fmt, args);
    va_end(args);
    putc('\n', stderr);
}


void
warningx(const char* fmt ...)
{
    va_list args;
    int status;

    va_start(args, fmt);
    fputs("***warning: ", stderr);
    status = vfprintf(stderr, fmt, args);
    va_end(args);
}

};

void
printLeading (FILE* f, int cnt, char c)
{
    while (cnt--)
	putc(c, f);
}


const char*
numberSuffix (int n)
{
    switch (n) {
      case 1:
	return "st";
      case 2:
	return "nd";
      case 3:
	return "rd";
      default:
	return "th";
    }
}


const char*
plural (int n)
{
    return (n == 1 ? "" : "s");
}


const char*
properPrep (char* s)
{
    char c = *s;
    if (c != 0) {
	if (isupper(c))
	    c = tolower(c);

	switch (c) {
	  case 'a':
	  case 'e':
	  case 'i':
	  case 'o':
	  case 'u':
	    return "an";

	  default:
	    return "a";
	}
    }
    return "";
}
#ifdef __GNUC__
extern "C" {
extern const char* const sys_errlist[];
extern int sys_nerr;
};
#endif

const char*
sys_errmsg ()
{
    return (errno < sys_nerr) ? sys_errlist[errno] : "unknown system error";
}
