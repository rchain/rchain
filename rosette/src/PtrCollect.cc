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

#include "PtrCollect.h"

static const int DefaultPtrCollectionSize = 32;

PtrCollection::PtrCollection() : ResizeablePtrArray(DefaultPtrCollectionSize) {
    init();
}


PtrCollection::PtrCollection(int sz) : ResizeablePtrArray(sz) { init(); }


void PtrCollection::resize() { resize(2 * size); }


void PtrCollection::resize(int newsize) {
    int offset = next - array;
    ResizeablePtrArray::resize(newsize);
    next = array + offset;
    limit = array + size;
}


void PtrCollection::compact() {
    void** end = next;
    void** ptr = array;

    for (void** current = array; current < end; current++) {
        if (*current != 0) {
            *ptr++ = *current;
        }
    }

    next = ptr;
}


PtrCollectionTrav::PtrCollectionTrav(PtrCollection* collection) {
    pc = collection;
    current = -1;
    advance();
}


void PtrCollectionTrav::advance() {
    int end = pc->next - pc->array;
    void** p = &pc->array[current];
    while (++current < end && *++p == 0) {
        continue;
    }
}
