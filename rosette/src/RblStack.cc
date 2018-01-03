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

#include "RblStack.h"
#include "Ctxt.h"
#include "Prim.h"
#include "Tuple.h"
#include "BuiltinClass.h"


BUILTIN_CLASS(RblStack) {
    OB_FIELD("depth", RblStack, nElems);
    OB_FIELD("elems", RblStack, elems);
}


static const int DefaultStackSize = 8;


RblStack::RblStack(Tuple* elems)
    : Ob(sizeof(RblStack), CLASS_META(RblStack), CLASS_SBO(RblStack)),
      nElems(FIXNUM(0)),
      elems(elems) {
    RblStack::updateCnt();
}


RblStack* RblStack::create() {
    Tuple* elems = Tuple::create(DefaultStackSize, NIV);
    void* loc = PALLOC1(sizeof(RblStack), elems);
    return new (loc) RblStack(elems);
}


Ob* RblStack::cloneTo(Ob* new_meta, Ob* new_parent) {
    PROTECT_THIS(RblStack);
    RblStack* new_stack = (RblStack*)SELF->Ob::cloneTo(new_meta, new_parent);
    PROTECT(new_stack);
    Tuple* new_elems = (Tuple*)SELF->elems->clone();
    ASSIGN(new_stack, elems, new_elems);
    return new_stack;
}


void RblStack::push(Ob* val) {
    int N = FIXVAL(nElems);

    if (N == elems->numberOfElements()) {
        PROTECT_THIS(RblStack);
        PROTECT(val);
        Tuple* new_elems = Tuple::create(2 * N, elems, 0, N, NIV);
        new_elems->elem(N) = val;
        FIXNUM_INC(SELF->nElems);
        ASSIGN(SELF, elems, new_elems);
    } else {
        FIXNUM_INC(nElems);
        ASSIGN(elems, elem(N), val);
    }
}


Ob* RblStack::pop() {
    FIXNUM_DEC(nElems);
    int n = FIXVAL(nElems);
    Ob* result = elems->elem(n);
    elems->elem(n) = NIV;

    int extent = elems->numberOfElements();
    if (n < extent / 4 && extent > DefaultStackSize) {
        PROTECT_THIS(RblStack);
        PROTECT(result);
        Tuple* new_elems = elems->makeSlice(0, extent / 2);
        ASSIGN(SELF, elems, new_elems);
    }

    return result;
}


Ob* RblStack::top() { return elems->elem(depth() - 1); }


void RblStack::reset() {
    nElems = FIXNUM(0);
    for (int i = elems->numberOfElements(); i--;) {
        elems->elem(i) = NIV;
    }
}


Ob* RblStack::indexedSize() { return nElems; }
Ob* RblStack::nth(int n) { return elems->elem(n); }
Ob* RblStack::setNth(int n, Ob* val) {
    ASSIGN(elems, elem(n), val);
    return this;
}


Ob* RblStack::subObject(int start, int size) {
    PROTECT_THIS(RblStack);
    RblStack* new_stack = RblStack::create();
    while (size--) {
        new_stack->push(SELF->nth(start++));
    }

    return new_stack;
}


DEF("stack-new", stackNew, 0, 0) { return RblStack::create(); }


DEF("stack-depth", stackDepth, 1, 1) {
    CHECK(0, RblStack, stk);
    return stk->nElems;
}


DEF("stack-empty?", stackIsEmpty, 1, 1) {
    CHECK(0, RblStack, stk);
    return RBLBOOL(stk->isEmpty());
}


DEF("stack-push", stackPush, 2, 2) {
    CHECK(0, RblStack, stk);
    stk->push(ARG(1));
    return stk;
}


DEF("stack-pop", stackPop, 1, 1) {
    CHECK(0, RblStack, stk);
    return stk->isEmpty() ? NIV : stk->pop();
}


DEF("stack-top", stackTop, 1, 1) {
    CHECK(0, RblStack, stk);
    return stk->isEmpty() ? NIV : stk->top();
}


DEF("stack-reset", stackReset, 1, 1) {
    CHECK(0, RblStack, stk);
    stk->reset();
    return stk;
}
