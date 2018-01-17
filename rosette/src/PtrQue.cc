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

#include "PtrQue.h"

static const int DefaultPtrQueSize = 32;

PtrQue::PtrQue() : ResizeablePtrArray(DefaultPtrQueSize) { init(); }
PtrQue::PtrQue(int sz) : ResizeablePtrArray(sz) { init(); }
void PtrQue::resize() { resize(2 * size); }


void PtrQue::resize(int newsize) {
    int offset = head - array;
    ResizeablePtrArray::resize(newsize);
    head = array + offset;
    limit = array + size;
}


void PtrQue::compact() {
    void** end = head;
    void** ptr = array;

    for (void** current = array; current < end; current++) {
        if (*current != 0) {
            *ptr++ = *current;
        }
    }

    head = ptr;
}
