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

#if !defined(_RBL_Mbox_h)
#define _RBL_Mbox

#include "rosette.h"
#include "Ob.h"

class EmptyMbox : public Ob {
    STD_DECLS(EmptyMbox);

   public:
    EmptyMbox();

    static EmptyMbox* create();
    virtual Ob* cloneTo(Ob*, Ob*);
    virtual Ob* receiveMsg(MboxOb*, Ctxt*);
    virtual Ob* nextMsg(MboxOb*, Ob*);
};


class LockedMbox : public Ob {
    STD_DECLS(LockedMbox);

   public:
    LockedMbox();

    static LockedMbox* create();
    virtual Ob* cloneTo(Ob*, Ob*);
    virtual Ob* receiveMsg(MboxOb*, Ctxt*);
    virtual Ob* nextMsg(MboxOb*, Ob*);
};


class QueueMbox : public Ob {
    STD_DECLS(QueueMbox);

   public:
    QueueMbox(Ob*, MboxQueue*);

    Ob* lockVal;
    Ob* enabledSet;
    MboxQueue* queue;

    static QueueMbox* create(Ob*);

    virtual Ob* cloneTo(Ob*, Ob*);
    virtual Ob* receiveMsg(MboxOb*, Ctxt*);
    virtual Ob* nextMsg(MboxOb*, Ob*);

    bool isLocked() { return BOOLVAL(lockVal); }
    void lock() { lockVal = RBLTRUE; }
    void unlock() { lockVal = RBLFALSE; }
    void enqueue(Ob*);
    Ob* dequeue();
};


extern Ob* emptyMbox;
extern Ob* lockedMbox;

#endif
