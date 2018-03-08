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

#include "BinaryOb.h"
#include "Ctxt.h"
#include "Operation.h"
#include "Prim.h"
#include "Tuple.h"
#include "BuiltinClass.h"

BUILTIN_CLASS(Prim) { OB_FIELD("id", Prim, id); }


Prim::Prim(char* s, PRIMFN* f, int min, int max)
    : BinaryOb(sizeof(Prim), CLASS_META(Prim), CLASS_SBO(Prim)),
      id(SYMBOL(s)),
      fn(f),
      minargs(min),
      maxargs(max) {
    if (primcount == MaxPrims) {
        suicide("too many primitives");
    }

    this->primnum = primcount++;
    Prim::updateCnt();
}


Prim* Prim::create(char* s, PRIMFN* f, int min, int max) {
    void* loc = PALLOC(sizeof(Prim));
    return new (loc) Prim(s, f, min, max);
}


int Prim::traversePtrs(PSOb__PSOb f) {
    return BinaryOb::traversePtrs(f) + useIfPtr(&id, f);
}


int Prim::traversePtrs(SI__PSOb f) {
    return BinaryOb::traversePtrs(f) + useIfPtr(id, f);
}


void Prim::traversePtrs(V__PSOb f) {
    BinaryOb::traversePtrs(f);
    useIfPtr(id, f);
}


Prim* Prim::InlineablePrimP() { return this; }


int Prim::primNumber() { return primnum; }


Ob* Prim::dispatchHelper(Ctxt* ctxt) {
    int n = ctxt->nargs;
    return (minargs <= n && n <= maxargs ? (*fn)(this, ctxt)
                                         : mismatch(ctxt, minargs, maxargs));
}


Ob* Prim::dispatch(Ctxt* ctxt) {
    if (debugging_level)
        printf("\t%s\n", BASE(id)->asCstring());

    PROTECT(ctxt);
    Ob* result = dispatchHelper(ctxt);
    if (result != INVALID && result != UPCALL && result != DEADTHREAD) {
        ctxt->ret(result);
    }

    return result;
}


Ob* Prim::invoke(Ctxt* ctxt) {
    /*
     * For a prim to be invoked through invoke, it must have been bound
     * to an operation.  In that case, we want the behavior to be as
     * though the prim were actually invoked from a method that will
     * unlock the rcvr if the operation is asynchronous.  That is, if the
     * operation is synchronous, the mythical method should look like
     *
     * 	(method [& args] (*prim* (self) & args))
     *
     * and if the operation is asynchronous, the mythical method should
     * look like
     *
     * 	(method [& args] (*prim* (self) & args) (update!))
     */
    PROTECT(ctxt);
    Ob* result = dispatch(ctxt);
    if (!BASE(ctxt->trgt)->isSynchronousTrgt()) {
        BASE(ctxt->arg(0))->updateNoArgs();
    }

    return result;
}


Prim* Prim::inlineTbl[MaxPrims] = {0};
int Prim::primcount = 0;

void BuiltinPrim::init() const {
    Prim* p = (Prim*)heap->tenure(
        Prim::create(record->name, record->fn, record->min, record->max));
    *(record->clientPrim) = p;
    Define(record->name, p);
    Prim::inlineTbl[p->primNumber()] = p;
}


void BuiltinPrim::initBuiltinPrims() {
    for (const BuiltinPrim* bpp = BuiltinPrim::root; bpp; bpp = bpp->link) {
        bpp->init();
    }
}


BuiltinPrim* BuiltinPrim::root = 0;


DEF("runtime-error", obRuntimeError, 1, MaxArgs) {
    fprintf(stderr, "*** runtime error:\n");
    for (int i = 0; i < NARGS; i++) {
        putc('\t', stderr);
        BASE(ARG(i))->printQuotedOn(stderr);
        putc('\n', stderr);
    }

    // NB(leaf): I think it sucks to have a runtime error that can
    // cause the binary to return 0 to the shell. Aborting here makes
    // a runtime error into a hard failure, instead of a soft one.
    // The perror() serves as a reminder to people what the problem
    // might be.
    perror("Nota Bene: did you forget to set ulimit -s unlimited?");
    abort();
}

DEF_OPRN(Sync, "vm-error", oprnVmError, obRuntimeError);
DEF_OPRN(Sync, "runtime-error", oprnRuntimeError, obRuntimeError);
DEF_OPRN(Sync, "missing-method", oprnMissingMethod, obRuntimeError);
DEF_OPRN(Sync, "missing-binding", oprnMissingBinding, obRuntimeError);
DEF_OPRN(Sync, "formals-mismatch", oprnFormalsMismatch, obRuntimeError);
