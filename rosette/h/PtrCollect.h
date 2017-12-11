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

#if !defined(_PtrCollection_h)
#define _PtrCollection_h

#ifdef __GNUG__
#pragma interface
#endif

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

    void init();
    void resize();
    void resize(int);
    int empty();
    void add(void*);
    void del(int = 1);
    void compact();
};


inline void PtrCollection::init() {
    next = array;
    limit = array + size;
}

inline int PtrCollection::empty() { return next == array; }


inline void PtrCollection::add(void* p) {
    if (next >= limit)
        resize();
    *next++ = p;
}


inline void PtrCollection::del(int n) { next -= n; }


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

    int valid();
    void advance();
    void*& get();

    operator void*();
};


inline int PtrCollectionTrav::valid() { return &pc->array[current] < pc->next; }


inline PtrCollectionTrav::operator void*() { return valid() ? this : 0; }


inline void*& PtrCollectionTrav::get() { return pc->array[current]; }

#endif
