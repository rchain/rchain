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

#define ARCH "x86"
#define MACHINE "x86"
#define OS "linux"
#define LINUX

#define GCC27X
#define NO_SYSENT_H

#define DO_UNGETC

/* because of need for alignment on 8 byte boundary of doubles */
#define USE_SHORT_FLOAT

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
