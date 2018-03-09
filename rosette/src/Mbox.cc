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

#include "Mbox.h"
#include "Ctxt.h"
#include "Queue.h"
#include "Tuple.h"
#include "BuiltinClass.h"

#include <assert.h>


Ob* emptyMbox = INVALID;
Ob* lockedMbox = INVALID;


BUILTIN_CLASS(EmptyMbox) {}


EmptyMbox::EmptyMbox()
    : Ob(sizeof(EmptyMbox), CLASS_META(EmptyMbox), CLASS_SBO(EmptyMbox)) {
    EmptyMbox::updateCnt();
}


EmptyMbox* EmptyMbox::create() { return gc_new<EmptyMbox>(); }


Ob* EmptyMbox::cloneTo(Ob*, Ob*) { return this; }


Ob* EmptyMbox::receiveMsg(MboxOb* client, Ctxt* task) {
    ASSIGN(client, mbox, lockedMbox);
    client->schedule(task);
    return NIV;
}


Ob* EmptyMbox::nextMsg(MboxOb* client, Ob* new_enabledSet) {
    warning("%s::nextMsg invoked", typestring());
    if (new_enabledSet != NIL) {
        PROTECT(client);
        QueueMbox* new_mbox = QueueMbox::create(new_enabledSet);
        new_mbox->unlock();
        ASSIGN(client, mbox, new_mbox);
    }
    return NIV;
}


BUILTIN_CLASS(LockedMbox) {}


LockedMbox::LockedMbox()
    : Ob(sizeof(LockedMbox), CLASS_META(LockedMbox), CLASS_SBO(LockedMbox)) {
    LockedMbox::updateCnt();
}


LockedMbox* LockedMbox::create() { return gc_new<LockedMbox>(); }


Ob* LockedMbox::cloneTo(Ob*, Ob*) { return this; }


Ob* LockedMbox::receiveMsg(MboxOb* client, Ctxt* task) {
    PROTECT_THIS(LockedMbox);
    PROTECT(client);
    PROTECT(task);

    QueueMbox* new_mbox = QueueMbox::create(NIL);
    assert(task != NIV);
    new_mbox->enqueue(task);
    ASSIGN(client, mbox, new_mbox);

    return NIV;
}


Ob* LockedMbox::nextMsg(MboxOb* client, Ob* new_enabledSet) {
    if (new_enabledSet == NIL) {
        ASSIGN(client, mbox, emptyMbox);
    } else {
        PROTECT(client);
        QueueMbox* new_mbox = QueueMbox::create(new_enabledSet);
        new_mbox->unlock();
        ASSIGN(client, mbox, new_mbox);
    }
    return NIV;
}


BUILTIN_CLASS(QueueMbox) {
    OB_FIELD("locked", QueueMbox, lockVal);
    OB_FIELD("enabled-set", QueueMbox, enabledSet);
    OB_FIELD("queue", QueueMbox, queue);
}


QueueMbox::QueueMbox(Ob* enabledSet, MboxQueue* queue)
    : Ob(sizeof(QueueMbox), CLASS_META(QueueMbox), CLASS_SBO(QueueMbox)) {
    this->lockVal = RBLTRUE;
    this->enabledSet = enabledSet;
    this->queue = queue;
    QueueMbox::updateCnt();
}


QueueMbox* QueueMbox::create(Ob* enabledSet) {
    PROTECT(enabledSet);
    MboxQueue* queue = MboxQueue::create();
    return gc_new<QueueMbox>(enabledSet, queue);
}


Ob* QueueMbox::cloneTo(Ob*, Ob*) { return emptyMbox; }


Ob* QueueMbox::receiveMsg(MboxOb* client, Ctxt* task) {
    if (isLocked() || !enabledSet->accepts(task)) {
        queue->enqueue(task);
    } else {
        if (enabledSet == NIL) {
            warning("NIL enabled-set on unlocked %s", typestring());
        }

        lock();
        client->schedule(task);
    }

    return NIV;
}


Ob* QueueMbox::nextMsg(MboxOb* client, Ob* new_enabledSet) {
    if (!isLocked()) {
        warning("%s::nextMsg received on unlocked mbox", typestring());
        return NIV;
    }

    Ctxt* task = (Ctxt*)queue->maybeDequeue(new_enabledSet);

    if (task == INVALID) {
        /*
         * Either there is no acceptable msg, or the queue was empty to
         * begin with.  In both cases we want to leave the mbox unlocked,
         * either by reverting to the (unique) emptyMbox if the
         * new_enabledSet is NIL, or by resetting lockVal if
         * new_enabledSet is non-NIL.
         */

        if (queue->isEmpty() && new_enabledSet == NIL) {
            ASSIGN(client, mbox, emptyMbox);
        } else {
            ASSIGN(this, enabledSet, new_enabledSet);
            unlock();
        }
    } else {
        /*
         * The mbox is presumably locked at this point, and it should
         * remain so, either by reverting to the (unique) lockedMbox or
         * by keeping lockVal set.
         */

        if (queue->isEmpty() && new_enabledSet == NIL) {
            ASSIGN(client, mbox, lockedMbox);
        } else {
            ASSIGN(this, enabledSet, new_enabledSet);
        }

        client->schedule(task);
    }

    return NIV;
}


void QueueMbox::enqueue(Ob* val) { queue->enqueue(val); }


Ob* QueueMbox::dequeue() { return queue->dequeue(); }
