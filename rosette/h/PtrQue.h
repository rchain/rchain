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

#if !defined(_PtrQue_h)
#define _PtrQue_h

#ifdef __GNUG__
#pragma interface
#endif

#include "rosette.h"

#include "ResizeablePtrArray.h"

class PtrQue : public ResizeablePtrArray {
   protected:
    void** head;
    void** limit;

   public:
    PtrQue();
    PtrQue(int);

    void init();
    void resize();
    void resize(int);
    int empty();
    void add(void*);
    void del(int = 1);
    void compact();
};


inline void PtrQue::init() {
    head = array;
    limit = array + size;
}

inline int PtrQue::empty() { return head == array; }


inline void PtrQue::add(void* p) {
    if (head >= limit)
        resize();
    *head++ = p;
}


inline void PtrQue::del(int n) { head -= n; }

#endif
