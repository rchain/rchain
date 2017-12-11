/* Mode: -*- C++ -*- */
/* @BC
 *		                Copyright (c) 1993
 *	    by Microelectronics and Computer Technology Corporation (MCC)
 *                                      and
 *		                Copyright (c) 1996
 *	                      by Rosette WebWorks Inc.
 *				All Rights Reserved
 *
 *	Permission to use, copy, modify, and distribute this software and its
 *	documentation for any purpose and without fee is hereby granted,
 *	provided that this notice be retained unaltered, and that the name of
 *	RWI or MCC and its shareholders and participants shall not be used in
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

#if !defined(__ROSETTE__)
#define __ROSETTE__
#include ARCH_INC

#if !defined(DO_UNGETC)
#define DO_UNGETC            \
    int c = getc(&_iob[fd]); \
    ungetc(c, &_iob[fd]);
#endif

#define HAS_BOOL

#include <assert.h>
#include <stdio.h>

#ifndef name2
#define name2(x, y) x##y
#endif

#ifndef name3
#define name3(x, y, z) x##y##z
#endif

#ifdef __GNUG__
/* #define NEW(loc) new{loc} */
#ifndef MIPS_SGI_SYSV
typedef void (*SIG_PF)(int);
#endif

#else
#define NEW(loc) new (loc)
#endif

#ifdef LITTLE_END
#define ORDER2(a, b) b a
#define ORDER3(a, b, c) c b a
#define ORDER4(a, b, c, d) d c b a
#define ORDER5(a, b, c, d, e) e d c b a
#else
#define ORDER2(a, b) a b
#define ORDER3(a, b, c) a b c
#define ORDER4(a, b, c, d) a b c d
#define ORDER5(a, b, c, d, e) a b c d e
#endif

#endif

#ifndef RBL_WOULDBLOCK
#define RBL_WOULDBLOCK (errno == EWOULDBLOCK)
#endif

#ifndef EMPTY
#define EMPTY
#endif
