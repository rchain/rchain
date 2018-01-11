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

#include "Operation.h"
#include "Ctxt.h"
#include "Prim.h"
#include "Tuple.h"
#include "BuiltinClass.h"


BUILTIN_CLASS(StdOprn) {
    OB_FIELD_INDIRECT("id", STDOPRN_ID_SLOT);
    OB_FIELD_INDIRECT("sync", STDOPRN_SYNC_SLOT);
}


StdOprn::StdOprn(pExt ext)
    : Actor(sizeof(StdOprn), CLASS_META(StdOprn), CLASS_SBO(StdOprn),
            lockedMbox, ext) {
    StdOprn::updateCnt();
}


StdOprn* StdOprn::create(pOb id, pOb sync) {
    PROTECT(id);
    PROTECT(sync);
    StdExtension* ext = StdExtension::create(BUILTIN_STDOPRN_SLOTS);
    ext->slot(STDOPRN_ID_SLOT) = id;
    ext->slot(STDOPRN_SYNC_SLOT) = sync;
    void* loc = PALLOC1(sizeof(StdOprn), ext);
    return new (loc) StdOprn(ext);
}


bool StdOprn::isSynchronousTrgt() {
    return BOOLVAL(extension->slot(STDOPRN_SYNC_SLOT));
}


pOb StdOprn::dispatch(pCtxt ctxt) {
    if (debugging_level) {
        printf("\toprn %s\n",
               BASE(extension->slot(STDOPRN_ID_SLOT))->asCstring());
    }

    if (0 < ctxt->nargs) {
        return BASE(ctxt->arg(0))->lookupAndInvoke(ctxt);
    } else {
        return runtimeError(ctxt, "no argument for dispatch");
    }
}


BuiltinOprn::BuiltinOprn(char* name, char* type, StdOprn** oprn,
                         Prim** topBinding) {
    this->name = name;
    this->sync = (strcmp(type, "Sync") == 0);
    this->clientOprn = oprn;
    this->topBinding = topBinding;
    this->link = BuiltinOprn::root;
    BuiltinOprn::root = this;
}


void BuiltinOprn::init() {
    StdOprn* o =
        (StdOprn*)heap->tenure(StdOprn::create(SYMBOL(name), RBLBOOL(sync)));
    *clientOprn = o;
    Define(name, o);
    Define(o, *topBinding, TopSBO);
}


void BuiltinOprn::initBuiltinOprns() {
    for (BuiltinOprn* bop = BuiltinOprn::root; bop; bop = bop->link) {
        bop->init();
    }
}


BuiltinOprn* BuiltinOprn::root = 0;


DEF("oprn-new", oprnNew, 0, 1) {
    return StdOprn::create((NARGS == 1 ? ARG(0) : Qanon), RBLFALSE);
}


DEF("syncoprn-new", syncoprnNew, 0, 1) {
    return StdOprn::create((NARGS == 1 ? ARG(0) : Qanon), RBLTRUE);
}
