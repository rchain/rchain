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

#if !defined(_RBL_Queue_h)
#define _RBL_Queue_h

#include "rosette.h"
#include "Ob.h"

class Queue : public Ob {
    STD_DECLS(Queue);

   protected:
    Queue(Tuple*);
    Queue(int, Ob*, Ob*, Tuple*);

   public:
    Ob* qHead;
    Ob* qTail;
    Ob* nElems;
    Tuple* elems;

    static Queue* create();
    virtual Ob* cloneTo(Ob* meta, Ob* parent);

    int depth() { return FIXVAL(nElems); }
    bool isEmpty() { return nElems == FIXNUM(0); }

    void enqueue(Ob*);
    Ob* dequeue();
    void reset();

    Ob* indexedSize();
    Ob* nth(int);
    Ob* setNth(int, Ob*);
    Ob* subObject(int, int);

    Ob* patternDequeue(Tuple*);
    Ob* patternRead(Tuple*);
    Ob* dequeueNth(int);
};


class MboxQueue : public Queue {
    STD_DECLS(MboxQueue);

   protected:
    MboxQueue(Tuple*);

   public:
    static MboxQueue* create();
    virtual Ob* maybeDequeue(Ob*);
};

#endif
