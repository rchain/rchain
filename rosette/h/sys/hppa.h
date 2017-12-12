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
 * $Header: /mcc/project/carnot/root/master/pub-ess/h/sys/hppa.h,v 1.1.1.1
 1993/02/12 01:25:03 tomlic Exp $
 *
 * $Log: hppa.h,v $
 * Revision 1.1.1.1  1993/02/12  01:25:03  tomlic
 * pub release of rosette
 *
 @EC */

#include <stdio.h>
#include <sys/ioctl.h>

#define OS "hpux"
#define MACHINE "hp"
#define ARCH "hppa"

#define HPUX
extern char *core_end;
/* a dummy */
#define SYS_EXEC <a.out.h>
#define exec struct header
#define DYNLOAD "Dynl_hpux.c"

/* flag to use with fcntl to give non blocking io
   the ~ of it should give blocking io
 */

#define FCNTL_NONBLOCK O_NONBLOCK

/* You must supply something to cause the current process
   to receive the io signal when input is available on fd
*/

#define SET_SIGNAL_IO_DESIRED(result)              \
    do {                                           \
        int ssid_flag = -getpid();                 \
        result = ioctl(fd, SIOCSPGRP, &ssid_flag); \
    } while (0)

#define UNEXEC "unexhp9k800.c"

#ifdef c_plusplus
typedef void (*SIG_PF)(int);
extern "C" {
int set_fd_async(int, int);
int getpagesize(void);
void *valloc(unsigned int);
int getdtablesize();
void bzero(char *, int);
void bcopy(const char *, char *, int);
};
#endif

#ifdef IN_SOCKETSUPP
#include <unistd.h>
#endif


/* the include
   /usr/include/sys/time.h:     extern int select(size_t, int *, int *, int *,
   const struct timeval *);
   does not match the fd_set * ..
*/
#define SELECT(a, b, c, d, e) \
    select(a, (int *)(void *)b, (int *)(void *)c, (int *)(void *)d, e)

#define WANT_LIBC_H
#define NO_SYSENT_H
/*
  #if defined(IN_BASE_SUPP) || defined(IN_CONSOLE)
#include <unistd.h>
#endif
*/

/* because of need for alignment on 8 byte boundary of doubles */
#define USE_SHORT_FLOAT

#define _STRING(stringify) #stringify

#define NO_SETPRIORITY

#define RBL_WOULDBLOCK ((errno == EWOULDBLOCK) || (errno == EAGAIN))

#define REINSTALL_SIGNALS

#define DBEGIN 0x40000000
#define END_SMALL_ADDR 0xa00000
#define MAP_BACK_ADDRESS
