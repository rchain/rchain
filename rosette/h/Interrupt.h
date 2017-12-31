/* Mode: -*- C++ -*- */
// vim: set ai ts=4 sw=4 expandtab
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

#if !defined(_RBL_Interrupt_h)
#define _RBL_Interrupt_h

#include "rosette.h"
#include <signal.h>


enum VM_EVENT { VM_READ_EVENT, VM_WRITE_EVENT, VM_EXCEPTION_EVENT };


extern int interruptPending;
extern int arithmeticException;

extern void handleInterrupts();
extern void ignoreInterrupts();

typedef void IO_HANDLER(VM_EVENT, int, void*);

extern void RosetteSignalHandler(int);

extern IO_HANDLER RosetteIoHandler;

#endif
