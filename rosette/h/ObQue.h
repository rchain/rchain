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

#if !defined(_RBL_ObQue_h)
#define _RBL_ObQue_h

#ifdef __GNUG__
#pragma interface
#endif

#include "rosette.h"

#include "Ob.h"

class ObQue {
   protected:
    Ob** array;
    Ob** head;
    Ob** tail;
    Ob** limit;
    int size;
    int last_added;

   public:
    ObQue();
    ObQue(int);
    ~ObQue();

    void init();
    void reset();
    void resize();
    void resize(int);
    int empty();
    void enq(Ob*);
    void push(Ob*);
    Ob* deq();

    int traversePtrs(PSOb__PSOb);
    int traversePtrs(SI__PSOb);
    void traversePtrs(V__PSOb);

    Ob*& operator[](int);
};

inline Ob*& ObQue::operator[](int n) { return array[n]; }

inline void ObQue::init() {
    last_added = 0;
    head = array;
    tail = array;
    limit = array + size;
}

inline int ObQue::empty() { return (head == tail && !last_added); }

inline void ObQue::enq(Ob* o) {
    if ((head == tail && last_added) || (tail == array && head == limit))
        resize();
    if (tail == array)
        tail = limit;

    last_added = 1;
    (*--tail) = o;
}

inline Ob* ObQue::deq() {
    Ob* result;
    if (empty()) {
        last_added = 0;
        result = (Ob*)0;
    }

    if (head == array)
        head = limit;

    last_added = 0;
    --head;
    result = *head;
    return result;
}

#endif
