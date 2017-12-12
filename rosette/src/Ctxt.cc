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
 @EC */

#ifdef __GNUG__
#pragma implementation
#endif

#include "Ctxt.h"

#include "Code.h"
#include "Operation.h"
#include "Prim.h"
#include "Tuple.h"
#include "Vm.h"

#include "BuiltinClass.h"

#include <assert.h>
#if !defined(GCC27X)
#include <new.h>
#endif

extern Ob* emptyMbox;
extern Ob* lockedMbox;


BUILTIN_CLASS(Ctxt) {
    OB_FIELD("mbox", Ctxt, mbox);
    OB_FIELD("tag", Ctxt, tag);
    BIT_FIELD("nargs", Ctxt, nargs, BITS(Byte));
    BIT_FIELD("outstanding", Ctxt, outstanding, BITS(Byte));
    BIT_FIELD("pc", Ctxt, pc, BITS(Word16));
    OB_FIELD("rslt", Ctxt, rslt);
    OB_FIELD("trgt", Ctxt, trgt);
    OB_FIELD("argvec", Ctxt, argvec);
    OB_FIELD("env", Ctxt, env);
    OB_FIELD("code", Ctxt, code);
    OB_FIELD("ctxt", Ctxt, ctxt);
    OB_FIELD("self", Ctxt, self2);
    OB_FIELD("self-env", Ctxt, selfEnv);
    OB_FIELD("rcvr", Ctxt, rcvr);
    OB_FIELD("monitor", Ctxt, monitor);
}

BUILTIN_CLASS(UpcallCtxt) {
    OB_FIELD("mbox", Ctxt, mbox);
    OB_FIELD("tag", Ctxt, tag);
    BIT_FIELD("nargs", Ctxt, nargs, BITS(Byte));
    BIT_FIELD("outstanding", Ctxt, outstanding, BITS(Byte));
    BIT_FIELD("pc", Ctxt, pc, BITS(Word16));
    OB_FIELD("rslt", Ctxt, rslt);
    OB_FIELD("trgt", Ctxt, trgt);
    OB_FIELD("argvec", Ctxt, argvec);
    OB_FIELD("env", Ctxt, env);
    OB_FIELD("code", Ctxt, code);
    OB_FIELD("ctxt", Ctxt, ctxt);
    OB_FIELD("self", Ctxt, self2);
    OB_FIELD("self-env", Ctxt, selfEnv);
    OB_FIELD("rcvr", Ctxt, rcvr);
    OB_FIELD("monitor", Ctxt, monitor);
}


inline Ctxt::Ctxt(int sz, Ob* meta, Ob* parent, Ob* mbox, Code* code,
                  Tuple* argvec, Ctxt* ctxt, Location loc)
    : MboxOb(sz, meta, parent, mbox),
      tag(loc),
      nargs(argvec->numberOfElements()),
      outstanding(1),
      pc(0),
      rslt(NIV),
      trgt(NIV),
      argvec(argvec),
      env(ctxt->env),
      ctxt(ctxt->ctxt),
      code(code),
      self2(ctxt->self2),
      selfEnv(ctxt->selfEnv),
      rcvr(ctxt->rcvr) {}


Ctxt::Ctxt(Tuple* t, Ctxt* p)
    : MboxOb(sizeof(Ctxt), CLASS_META(Ctxt), CLASS_SBO(Ctxt), emptyMbox),
      tag(LocRslt),
      nargs(t->numberOfElements()),
      outstanding(0),
      pc(0),
      rslt(NIV),
      trgt(NIV),
      argvec(t),
      env(p->env),
      ctxt(p),
      code(p->code),
      self2(p->self2),
      selfEnv(p->selfEnv),
      rcvr(p->rcvr),
      monitor(p->monitor) {
    Ctxt::updateCnt();
}


Ctxt::Ctxt(Code* c, Tuple* t, Ctxt* p, int o)
    : MboxOb(sizeof(Ctxt), CLASS_META(Ctxt), CLASS_SBO(Ctxt), emptyMbox),
      tag(LocRslt),
      nargs(t->numberOfElements()),
      outstanding(o),
      pc(0),
      rslt(NIV),
      trgt(NIV),
      argvec(t),
      code(c),
      ctxt(p) {
    if (p != NIV) {
        this->env = p->env;
        this->self2 = p->self2;
        this->selfEnv = p->selfEnv;
        this->rcvr = p->rcvr;
        this->monitor = p->monitor;
    }
    else {
        this->env = GlobalEnv;
        this->self2 = NIV;
        this->selfEnv = TopEnv;
        this->rcvr = NIV;
        this->monitor = vm->currentMonitor;
    }
    Ctxt::updateCnt();
}


Ctxt::Ctxt(Ob* trgt, Tuple* argvec)
    : MboxOb(sizeof(Ctxt), CLASS_META(Ctxt), CLASS_SBO(Ctxt), emptyMbox),
      tag(LocLimbo),
      nargs(argvec->numberOfElements()),
      outstanding(0),
      pc(0),
      rslt(NIV),
      trgt(trgt),
      argvec(argvec),
      code((Code*)NIV),
      ctxt((Ctxt*)NIV),
      env(GlobalEnv),
      self2(NIV),
      selfEnv(NIV),
      rcvr(NIV),
      monitor(vm->currentMonitor) {
    Ctxt::updateCnt();
}


Ctxt* Ctxt::create(Tuple* t, Ctxt* p) {
    void* loc = PALLOC2(sizeof(Ctxt), t, p);
    return NEW(loc) Ctxt(t, p);
}


Ctxt* Ctxt::create(Code* code, Tuple* t, Ctxt* p, int o) {
    void* loc = PALLOC3(sizeof(Ctxt), code, t, p);
    return NEW(loc) Ctxt(code, t, p, o);
}


Ctxt* Ctxt::create(Ob* trgt, Tuple* argvec) {
    void* loc = PALLOC2(sizeof(Ctxt), trgt, argvec);
    return NEW(loc) Ctxt(trgt, argvec);
}


int Ctxt::traversePtrs(PSOb__PSOb f) {
    int sum = 0;

    sum += useIfPtr(&meta(), f);
    sum += useIfPtr(&parent(), f);
    sum += useIfPtr(&mbox, f);

    for (short int i = NumberOfCtxtRegs; i--;)
        sum += useIfPtr(&reg(i), f);

    return sum;
}


int Ctxt::traversePtrs(SI__PSOb f) {
    int sum = 0;

    sum += useIfPtr(meta(), f);
    sum += useIfPtr(parent(), f);
    sum += useIfPtr(mbox, f);

    for (short int i = NumberOfCtxtRegs; i--;)
        sum += useIfPtr(reg(i), f);

    return sum;
}


void Ctxt::traversePtrs(V__PSOb f) {
    useIfPtr(meta(), f);
    useIfPtr(parent(), f);
    useIfPtr(mbox, f);

    for (short int i = NumberOfCtxtRegs; i--;)
        useIfPtr(reg(i), f);
}


extern Code* rtnNxtCode;


bool Ctxt::rcv(Ob* result, Location loc) {
    if (store(loc, this, result))
        return TRUE;
    else {
        if (--outstanding == 0)
            scheduleStrand();
        return FALSE;
    }
}


bool Ctxt::applyK(Ob* result, Location tag) { return ctxt->rcv(result, tag); }


void Ctxt::scheduleStrand() { vm->scheduleStrand(this); }


void Ctxt::prepare() {
    /*
     * This is necessary because the compile sometimes arranges to
     * provide an argvec that is acually longer than nargs indicates.  If
     * we are about to expose the context to the outside world, we need
     * to clean it up so that it appears consistent.
     */

    PROTECT_THIS(Ctxt);
    Tuple* new_argvec = argvec->makeSlice(0, nargs);
    ASSIGN(SELF, argvec, new_argvec);
}


Ob* Ctxt::missingBindingError(Ob* symbol) {
    PROTECT_THIS(Ctxt);

    SELF->prepare();

    Tuple* new_argvec = Tuple::create(2, NIV);
    new_argvec->elem(0) = SELF;
    new_argvec->elem(1) = symbol;

    Ctxt* new_ctxt = Ctxt::create(oprnMissingBinding, new_argvec);
    new_ctxt->monitor = vm->systemMonitor;

    return BASE(oprnMissingBinding)->dispatch(new_ctxt);
}


Ob* Ctxt::vmError() {
    PROTECT_THIS(Ctxt);

    SELF->prepare();
    Tuple* new_argvec = Tuple::create(1, SELF);
    Ctxt* new_ctxt = Ctxt::create(oprnVmError, new_argvec);
    new_ctxt->monitor = vm->systemMonitor;

    return BASE(oprnVmError)->dispatch(new_ctxt);
}


UpcallCtxt::UpcallCtxt(Code* code, Tuple* argvec, Ctxt* ctxt, Location loc)
    : Ctxt(sizeof(UpcallCtxt), CLASS_META(Ctxt), CLASS_SBO(Ctxt), emptyMbox,
           code, argvec, ctxt, loc) {
    UpcallCtxt::updateCnt();
}


UpcallCtxt* UpcallCtxt::create(Code* code, Tuple* argvec, Ctxt* ctxt,
                               Location tag) {
    void* loc = PALLOC3(sizeof(UpcallCtxt), code, argvec, ctxt);
    return NEW(loc) UpcallCtxt(code, argvec, ctxt, tag);
}


bool UpcallCtxt::applyK(Ob* val, Location loc) {
#ifdef DEBUG
    assert(loc == tag);
#endif

    if (store(loc, ctxt, val))
        return TRUE;
    else {
        ctxt->scheduleStrand();
        return FALSE;
    }
}


DEF("ctxt-rtn", ctxtRtn, 2, 2) {
    CHECK(0, Ctxt, k);
    return k->ret(ARG(1)) ? INVALID : NIV;
}


DEF("ctxt-resume", ctxtResume, 1, 1) {
    CHECK(0, Ctxt, k);
    if (k->argvec->numberOfElements() > 0)
        return BASE(k->argvec->elem(0))->receive(k);
    else
        return INVALID;
}
