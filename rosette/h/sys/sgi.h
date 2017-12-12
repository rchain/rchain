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
 * $Header: /mcc/project/carnot/root/master/pub-ess/h/sys/sgi.h,v 1.1.1.1
 1993/02/12 01:25:04 tomlic Exp $
 *
 * $Log: sgi.h,v $
 * Revision 1.1.1.1  1993/02/12  01:25:04  tomlic
 * pub release of rosette
 *
 @EC */

#ifndef CONFIG_INCLUDED
#define CONFIG_INCLUDED

#define ARCH "sgi"
#define MACHINE "sgi-mips"
#define OS "irix"
#define MIPS_SGI_SYSV 1
#define HANDLE_POLL_WITH_IO 1
#define REINSTALL_SIGNALS 1


#include <stdio.h>
#include <stropts.h>
#include <sys/ioctl.h>


/*typedef void (*SIG_PF) (int);*/

/* flag to use with fcntl to give non blocking io
   the ~ of it should give blocking io
 */
/* see /usr/include/sys/fcntl.h */
#define FCNTL_NONBLOCK (FNONBLK | FASYNC)

/* You must supply something to cause the current process
   to receive the io signal when input is available on fd.
   return is < 0 on failure.
*/

#define SET_SIGNAL_IO_DESIRED(result) \
    result = (desiredState ? fcntl(fd, F_SETOWN, getpid()) : 0)

#define SET_SIGNAL_POLL_DESIRED(result) \
    result = ioctl(fd, I_SETSIG, (desiredState ? S_INPUT | S_HIPRI : S_HIPRI))


#ifdef __cplusplus
extern "C" {
int setpgrp();
int getpid();
/*int execve(const char*, const char**, char**);*/
int getpagesize();
int strcasecmp(const char *, const char *);
int getdtablesize();
void *sbrk(char *);
void *valloc(unsigned int);
int setsid();
int ioctl(int fildes, int request, ...);
void bzero(void *, int);
}

#endif
#define vfork fork

#define UNEXEC "unexmips.c"

#define DYNLOAD "Dynl_hpux.c"


#define HAVE_GETWD
#define HAVE_GETRUSAGE
#define HAVE_GETDTABLESIZE

/* char *core_end; */

#define DATA_START 0x10000000
#define TEXT_START 0x00400000

#endif

/* the dec nlist is broken */
/*
#define NEED_NLIST
*/

#ifdef IN_UNEXEC
static int mark_x();
int getpagesize();
#include <fcntl.h>
#include <scnhdr.h>
#undef _LIT8
#endif

#define RBL_WOULDBLOCK ((errno == EWOULDBLOCK) || (errno == EAGAIN))

#define WANT_LIBC_H
