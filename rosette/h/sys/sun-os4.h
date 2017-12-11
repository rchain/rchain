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
 * $Header: /mcc/project/carnot/root/master/pub-ess/h/sys/sun-os4.h,v 1.1.1.1
 1993/02/12 01:25:04 tomlic Exp $
 *
 * $Log: sun-os4.h,v $
 * Revision 1.1.1.1  1993/02/12  01:25:04  tomlic
 * pub release of rosette
 *
 @EC */

/* flag to use with fcntl to give non blocking io
   the ~ of it should give blocking io
 */
#define FCNTL_NONBLOCK (FNDELAY | FASYNC)

/* You must supply something to cause the current process
   to receive the io signal when input is available on fd.
   return is < 0 on failure.
*/
#define SET_SIGNAL_IO_DESIRED(result) \
    result = (desiredState ? fcntl(fd, F_SETOWN, getpid()) : 0)

#define OS "os4"

#ifndef __GNUG__
extern "C" {
int setpgrp();
int getpid();
int execve(const char*, const char**, char**);
int setsid();
}
#endif
