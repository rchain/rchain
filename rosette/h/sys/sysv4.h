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
 * $Header: /mcc/project/carnot/root/master/pub-ess/h/sys/sysv4.h,v 1.1.1.1
 1993/02/12 01:25:04 tomlic Exp $
 *
 * $Log: sysv4.h,v $
 * Revision 1.1.1.1  1993/02/12  01:25:04  tomlic
 * pub release of rosette
 *
 @EC */

#define SYSV4

#include <stdio.h>
extern char *core_end;

/* 386 specific !! */
#define LITTLE_END(x) x
#define DYNLOAD "Dynl_sysv4.c"
#define UNEXEC "unexelf.c"


/* #include <stdlib.h> */
#define valloc(n) memalign(getpagesize(), n)

#ifdef __cplusplus
extern "C" {
int mysigmask(int);
int setbuffer(FILE *, char *, int);
void *memalign(size_t, size_t);
int getpagesize(void);
int sigblock(int);
int sigsetmask(int);
int select(int, void *, void *, void *, void *);

int accept(int, void *, int *);
int bind(int, void *, int);
int connect(int, const void *, int);
int getpeername(int, void *, int *);
int getsockname(int, void *, int *);
int getsockopt(int, int, int, char *, int *);
int listen(int, int);
int recv(int, char *, int, int);
int recvfrom(int, char *, int, int, void *, int *);
int send(int, const char *, int, int);
int sendto(int, const char *, int, int, const void *, int);
int setsockopt(int, int, int, const char *, int);
int socket(int, int, int);
int recvmsg(int, void *, int);
int sendmsg(int, void *, int);
int socketpair(int, int, int, int *);

void bcopy(void *, void *, int);
void bzero(void *, int);
};
#endif
typedef struct exhdmap exec;

/* from unistd.h :  that file conflicts with sysent.h */
/* Symbolic constants for the "access" routine: */
#define R_OK 4 /* Test for Read permission */


typedef unsigned long LONG_ARG;
typedef unsigned long INT_ARG;
typedef unsigned long SHORT_ARG;
typedef unsigned long CHAR_ARG;
typedef float FLOAT_ARG;
typedef double DOUBLE_ARG;
typedef void *PTR_ARG;

/* from
   /usr/ucbinclude/sys/signal.h:#define	sigmask(n)		((unsigned
   long)1 << ((n) - 1))
*/
#define sigmask(n) ((unsigned long)1 << ((n)-1))

#ifdef cplusplus
#include "<sys/byteorder.h>"
#endif

#ifndef _SYS_BYTEORDER_H
#define _SYS_BYTEORDER_H
#endif

#define ARCH_DEFINE                 \
    Define("arch", SYMBOL("sun3")); \
    Define("machine", SYMBOL("mc68020"));
