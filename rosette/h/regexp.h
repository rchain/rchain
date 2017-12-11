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
 * $Header: /mcc/project/carnot/root/master/pub-ess/h/regexp.h,v 1.1.1.1
 1993/02/12 01:25:02 tomlic Exp $
 *
 * $Log: regexp.h,v $
 * Revision 1.1.1.1  1993/02/12  01:25:02  tomlic
 * pub release of rosette
 *
 @EC */

/*
 * Definitions etc. for regexp(3) routines.
 *
 * Caveat:  this is V8 regexp(3) [actually, a reimplementation thereof],
 * not the System V one.
 */


#ifndef _regexp_h
#define _regexp_h 1

#define NSUBEXP 10
typedef struct regexp {
    char *startp[NSUBEXP];
    char *endp[NSUBEXP];
    char regstart;   /* Internal use only. */
    char reganch;    /* Internal use only. */
    char *regmust;   /* Internal use only. */
    int regmlen;     /* Internal use only. */
    char program[1]; /* Unwarranted chumminess with compiler. */
} regexp;

regexp *GS_regcomp(char *);
int GS_regexec(register regexp *, register char *);

#endif
