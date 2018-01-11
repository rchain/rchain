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

#if !defined(_PtrCollection_h)
#define _PtrCollection_h

#include "rosette.h"
#include "ResizeAry.h"

class PtrCollection : public ResizeablePtrArray {
   protected:
    void** next;
    void** limit;

    friend class PtrCollectionTrav;

   public:
    PtrCollection();
    PtrCollection(int);

    void resize();
    void resize(int);
    void compact();
    void init() {
        next = array;
        limit = array + size;
    }

    int empty() { return next == array; }


    void add(void* p) {
        if (next >= limit) {
            resize();
        }

        *next++ = p;
    }

    void del(int n) { next -= n; }
};


class PtrCollectionTrav {
   protected:
    /*
     * We use array indexing here, rather than pointer chasing, to
     * protect ourselves in case the PtrCollection grows and moves while
     * this PtrCollectionTrav still exists.
     */

    int current;
    PtrCollection* pc;

   public:
    PtrCollectionTrav(PtrCollection*);

    int valid() { return &pc->array[current] < pc->next; }
    void advance();
    void*& get() { return pc->array[current]; }

    operator void*() { return valid() ? this : 0; }
};


#endif
