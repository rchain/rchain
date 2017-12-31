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

#if !defined(_RBL_ObStk_h)
#define _RBL_ObStk_h

#include "rosette.h"
#include "PtrCollect.h"
#include "Ob.h"

class ObStk : public PtrCollection {
   public:
    ObStk() : PtrCollection() {}
    ObStk(int sz) : PtrCollection(sz) {}

    void reset();
    void push(Ob* o) { PtrCollection::add((void*)o); }
    Ob* pop() { return (Ob*)(*--next); }
    Ob*& top(int n) {
        Ob** p = (Ob**)(next - n);
        return *p;
    }

    int traversePtrs(PSOb__PSOb);
    int traversePtrs(SI__PSOb);
    void traversePtrs(V__PSOb);

    void scavenge();
    void mark();
    void check();
};

#endif
