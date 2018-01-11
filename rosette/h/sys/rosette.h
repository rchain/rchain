/* Mode: -*- C++ -*- */
// vim: set ai ts=4 sw=4 expandtab
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

#if !defined(__ROSETTE__)
#define __ROSETTE__

#include "signal.h"

#include <cassert>
#include <stdio.h>

// TODO(leaf): CMake doesn't trivially offer a way to figure this out.
// However, I can't tell that it's being used for anything. So, for now
// set this to something reasonable and hope to ignore it until we can
// get rid of it.
#define ARCH "x86_64"
#define MACHINE "x86_64"
#define OS "linux"
#define LINUX


#ifndef name2
#define name2(x, y) x##y
#endif

#ifndef name3
#define name3(x, y, z) x##y##z
#endif

#define NEW(loc) new (loc)

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

#endif
