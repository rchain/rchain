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

#include "Queue.h"
#include "Prim.h"
#include "Tuple.h"
#include "BuiltinClass.h"

BUILTIN_CLASS(Queue) {
    OB_FIELD("head", Queue, qHead);
    OB_FIELD("tail", Queue, qTail);
    OB_FIELD("nelems", Queue, nElems);
    OB_FIELD("elems", Queue, elems);
}


static const int DefaultQueueSize = 4;


Queue::Queue(Tuple* elems)
    : Ob(sizeof(Queue), CLASS_META(Queue), CLASS_SBO(Queue)),
      qHead(FIXNUM(0)),
      qTail(FIXNUM(0)),
      nElems(FIXNUM(0)),
      elems(elems) {
    Queue::updateCnt();
}


Queue::Queue(int sz, Ob* meta, Ob* parent, Tuple* elems)
    : Ob(sz, meta, parent),
      qHead(FIXNUM(0)),
      qTail(FIXNUM(0)),
      nElems(FIXNUM(0)),
      elems(elems) {}


Queue* Queue::create() {
    Tuple* elems = Tuple::create(DefaultQueueSize, NIV);
    void* loc = PALLOC1(sizeof(Queue), elems);
    return new (loc) Queue(elems);
}


Ob* Queue::cloneTo(Ob* new_meta, Ob* new_parent) {
    PROTECT_THIS(Queue);
    Queue* new_queue = (Queue*)SELF->Ob::cloneTo(new_meta, new_parent);
    PROTECT(new_queue);
    Tuple* new_elems = (Tuple*)SELF->elems->clone();
    ASSIGN(new_queue, elems, new_elems);
    return new_queue;
}


void Queue::enqueue(Ob* val) {
    int qh = FIXVAL(qHead);
    int qt = FIXVAL(qTail);
    int qsize = FIXVAL(nElems);
    int qcapacity = elems->numberOfElements();

    if (qsize < qcapacity) {
        qTail = FIXNUM((qt + 1) % qcapacity);
        FIXNUM_INC(nElems);
        ASSIGN(elems, elem(qt), val);
    } else {
        PROTECT_THIS(Queue);
        PROTECT(val);
        Tuple* new_elems = Tuple::create(2 * qcapacity, NIV);
        int i = 0;
        for (; i < qsize; (i++, qh = (qh + 1) % qcapacity)) {
            new_elems->elem(i) = SELF->elems->elem(qh);
        }

        new_elems->elem(qsize) = val;
        SELF->qHead = FIXNUM(0);
        SELF->qTail = FIXNUM(qsize + 1);
        FIXNUM_INC(SELF->nElems);
        ASSIGN(SELF, elems, new_elems);
    }
}


Ob* Queue::dequeue() {
    int qh = FIXVAL(qHead);
    int qcapacity = elems->numberOfElements();
    Ob* val = elems->elem(qh);
    elems->elem(qh) = NIV;
    qHead = FIXNUM((qh + 1) % qcapacity);
    FIXNUM_DEC(nElems);
    return val;
}


void Queue::reset() {
    qHead = FIXNUM(0);
    qTail = FIXNUM(0);
    nElems = FIXNUM(0);
    for (int i = elems->numberOfElements(); i--;) {
        elems->elem(i) = NIV;
    }
}


Ob* Queue::indexedSize() { return nElems; }


Ob* Queue::nth(int n) {
    int index = (FIXVAL(qHead) + n) % elems->numberOfElements();
    return elems->elem(index);
}


Ob* Queue::setNth(int n, Ob* val) {
    int index = (FIXVAL(qHead) + n) % elems->numberOfElements();
    ASSIGN(elems, elem(index), val);
    return this;
}


Ob* Queue::subObject(int start, int size) {
    PROTECT_THIS(Queue);
    Queue* new_queue = Queue::create();
    Tuple::create(SELF->elems->numberOfElements(), NIV);

    while (size--) {
        new_queue->enqueue(SELF->nth(start++));
    }

    return new_queue;
}

Ob* Queue::patternDequeue(Tuple* pat) {
    int qcapacity = elems->numberOfElements();
    int qsize = FIXVAL(nElems);
    int qh = FIXVAL(qHead);
    int qt = FIXVAL(qTail);

    while (qsize--) {
        Ob* msg = elems->elem(qh);
        if ((IS_A(msg, Tuple)) && (pat->matches((Tuple*)msg))) {
            while (qsize--) {
                int new_qh = (qh + 1) % qcapacity;
                elems->elem(qh) = elems->elem(new_qh);
                qh = new_qh;
            }

            elems->elem(qh) = NIV;
            qTail = FIXNUM(qt == 0 ? qcapacity - 1 : qt - 1);
            FIXNUM_DEC(nElems);
            return msg;
        }

        qh = (qh + 1) % qcapacity;
    }

    return ABSENT;
}

Ob* Queue::patternRead(Tuple* pat) {
    int qcapacity = elems->numberOfElements();
    int qsize = FIXVAL(nElems);
    int qh = FIXVAL(qHead);

    while (qsize--) {
        Ob* msg = elems->elem(qh);
        if ((IS_A(msg, Tuple)) && (pat->matches((Tuple*)msg))) {
            return msg;
        }

        qh = (qh + 1) % qcapacity;
    }

    return ABSENT;
}
Ob* Queue::dequeueNth(int n) {
    int qcapacity = elems->numberOfElements();
    int qsize = FIXVAL(nElems);

    if ((n < 0) || (n >= qsize)) {
        return ABSENT;
    }

    int qh = (FIXVAL(qHead) + n) % qcapacity;
    int qt = FIXVAL(qTail);

    qsize = qsize - (n + 1);

    Ob* msg = elems->elem(qh);
    while (qsize--) {
        int new_qh = (qh + 1) % qcapacity;
        elems->elem(qh) = elems->elem(new_qh);
        qh = new_qh;
    }

    elems->elem(qh) = NIV;
    qTail = FIXNUM(qt == 0 ? qcapacity - 1 : qt - 1);
    FIXNUM_DEC(nElems);
    return msg;
}


BUILTIN_CLASS(MboxQueue) {
    OB_FIELD("head", MboxQueue, qHead);
    OB_FIELD("tail", MboxQueue, qTail);
    OB_FIELD("nelems", MboxQueue, nElems);
    OB_FIELD("elems", MboxQueue, elems);
}


MboxQueue::MboxQueue(Tuple* elems)
    : Queue(sizeof(MboxQueue), CLASS_META(MboxQueue), CLASS_SBO(MboxQueue),
            elems) {
    MboxQueue::updateCnt();
}


MboxQueue* MboxQueue::create() {
    Tuple* elems = Tuple::create(DefaultQueueSize, NIV);
    void* loc = PALLOC1(sizeof(MboxQueue), elems);
    return new (loc) MboxQueue(elems);
}


Ob* MboxQueue::maybeDequeue(Ob* enabledSet) {
    int qcapacity = elems->numberOfElements();
    int qsize = FIXVAL(nElems);
    int qh = FIXVAL(qHead);
    int qt = FIXVAL(qTail);
    Ob* es = BASE(enabledSet);

    while (qsize--) {
        Ctxt* msg = (Ctxt*)elems->elem(qh);
        if (es->accepts(msg)) {
            while (qsize--) {
                int new_qh = (qh + 1) % qcapacity;
                elems->elem(qh) = elems->elem(new_qh);
                qh = new_qh;
            }

            elems->elem(qh) = NIV;
            qTail = FIXNUM(qt == 0 ? qcapacity - 1 : qt - 1);
            FIXNUM_DEC(nElems);
            return msg;
        }

        qh = (qh + 1) % qcapacity;
    }

    return INVALID;
}


DEF("queue-new", queueNew, 0, 0) { return Queue::create(); }


DEF("queue-depth", queueDepth, 1, 1) {
    CHECK(0, Queue, queue);
    return queue->nElems;
}


DEF("queue-empty?", queueIsEmpty, 1, 1) {
    CHECK(0, Queue, queue);
    return RBLBOOL(queue->isEmpty());
}


DEF("queue-enqueue", queueEnqueue, 2, 2) {
    CHECK(0, Queue, queue);
    PROTECT(queue);
    queue->enqueue(ARG(1));
    return queue;
}


DEF("queue-dequeue", queueDequeue, 1, 1) {
    CHECK(0, Queue, queue);
    return (queue->isEmpty() ? PRIM_ERROR("queue is empty") : queue->dequeue());
}

DEF("queue-read", queueRead, 1, 1) {
    CHECK(0, Queue, queue);
    return (queue->isEmpty() ? PRIM_ERROR("queue is empty")
                             : queue->elems->elem(FIXVAL(queue->qHead)));
}

DEF("queue-pat-dequeue", queuePDequeue, 2, 2) {
    CHECK(0, Queue, queue);
    CHECK(1, Tuple, pat);
    if (queue->isEmpty()) {
        return ABSENT;
    } else if (pat == NIL) {
        return queue->dequeue();
    } else {
        return queue->patternDequeue(pat);
    }
}

DEF("queue-pat-read", queuePRead, 2, 2) {
    CHECK(0, Queue, queue);
    CHECK(1, Tuple, pat);
    if (queue->isEmpty()) {
        return ABSENT;
    } else if (pat == NIL) {
        return queue->elems->elem(FIXVAL(queue->qHead));
    } else {
        return queue->patternRead(pat);
    }
}

DEF("queue-read-nth", queueReadNth, 2, 2) {
    CHECK(0, Queue, queue);
    CHECK_FIXNUM(1, n);
    int qsize = FIXVAL(queue->nElems);

    if ((n < 0) || (n >= qsize)) {
        return ABSENT;
    }

    int i = (FIXVAL(queue->qHead) + n) % queue->elems->numberOfElements();

    return queue->elems->elem(i);
}

DEF("queue-dequeue-nth", queueDequeueNth, 2, 2) {
    CHECK(0, Queue, queue);
    CHECK_FIXNUM(1, n);

    return queue->dequeueNth(n);
}

DEF("queue-reset", queueReset, 1, 1) {
    CHECK(0, Queue, queue);
    queue->reset();
    return queue;
}
