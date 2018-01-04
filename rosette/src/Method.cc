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

#include "Method.h"

#include "Code.h"
#include "Ctxt.h"
#include "Prim.h"
#include "Tuple.h"

#include "BuiltinClass.h"


BUILTIN_CLASS(StdMthd) {
    OB_FIELD("code", StdMthd, code);
    OB_FIELD("id", StdMthd, id);
    OB_FIELD("source", StdMthd, source);
}


StdMthd::StdMthd(int sz, Ob* meta, Ob* parent, Code* code, Ob* id, Ob* source)
    : Ob(sz, meta, parent), code(code), id(id), source(source) {}


StdMthd::StdMthd(Code* code, Ob* id, Ob* source)
    : Ob(sizeof(StdMthd), CLASS_META(StdMthd), CLASS_SBO(StdMthd)),
      code(code),
      id(id),
      source(source) {
    StdMthd::updateCnt();
}


StdMthd* StdMthd::create(Code* code, Ob* id, Ob* source) {
    void* loc = PALLOC3(sizeof(StdMthd), code, id, source);
    return new (loc) StdMthd(code, id, source);
}


Ob* StdMthd::dispatch(Ctxt* ctxt) {
    if (debugging_level) {
        printf("\tlocal mthd %s\n", BASE(id)->asCstring());
    }

    /*
     * This is the path followed when invoking a "local" method.  Because
     * such an invocation doesn't supply the usual "self" argument that a
     * method expects, we have to fix things up just a little bit here.
     */
    PROTECT_THIS(StdMthd);
    PROTECT(ctxt);
    Tuple* newArgvec = cons(ctxt->self2, ctxt->argvec);
    ctxt->code = SELF->code;
    ctxt->env = ctxt->selfEnv;
    ctxt->argvec = newArgvec;
    ctxt->nargs++;
    ctxt->rslt = NIV;
    ctxt->pc = 0;
    CHECK_STORE(ctxt, (VAL(SELF->code), VAL(ctxt->selfEnv), VAL(newArgvec)));
    ctxt->scheduleStrand();
    return SUSPENDED;
}


Ob* StdMthd::invoke(Ctxt* ctxt) {
    PROTECT_THIS(StdMthd);
    PROTECT(ctxt);
    Ob* surrogate = BASE(ctxt->arg(0))->dup();
    ctxt->self2 = ctxt->arg(0);
    ctxt->selfEnv = surrogate;
    ctxt->env = surrogate;
    ctxt->code = SELF->code;
    ctxt->rslt = NIV;
    ctxt->pc = 0;
    CHECK_STORE(
        ctxt, (VAL(ctxt->arg(0)), VAL(surrogate), VAL(SELF->code), VAL(ctxt)));
    ctxt->scheduleStrand();
    return SUSPENDED;
}


BUILTIN_CLASS(ReflectiveMthd) {
    OB_FIELD("code", ReflectiveMthd, code);
    OB_FIELD("id", ReflectiveMthd, id);
    OB_FIELD("source", ReflectiveMthd, source);
}


ReflectiveMthd::ReflectiveMthd(Code* code, Ob* id, Ob* source)
    : StdMthd(sizeof(ReflectiveMthd), CLASS_META(ReflectiveMthd),
              CLASS_SBO(ReflectiveMthd), code, id, source) {
    ReflectiveMthd::updateCnt();
}


ReflectiveMthd* ReflectiveMthd::create(Code* code, Ob* id, Ob* source) {
    void* loc = PALLOC3(sizeof(ReflectiveMthd), code, id, source);
    return new (loc) ReflectiveMthd(code, id, source);
}


Ob* ReflectiveMthd::dispatch(Ctxt*) {
    NI("dispatch");
    return INVALID;
}


Ob* ReflectiveMthd::invoke(Ctxt* ctxt) {
    PROTECT_THIS(ReflectiveMthd);
    PROTECT(ctxt);
    Tuple* expansionTuple = Tuple::create(2, NIV);
    expansionTuple->elem(0) = ctxt->code;
    expansionTuple->elem(1) = ctxt->env;
    PROTECT(expansionTuple);
    Tuple* ctxtTuple = Tuple::create(2, NIV);
    ctxtTuple->elem(0) = ctxt;
    ctxtTuple->elem(1) = expansionTuple;
    PROTECT(ctxtTuple);
    Tuple* newArgvec = ctxt->argvec->makeSlice(0, ctxt->nargs);
    newArgvec->elem(0) = ctxtTuple;
    PROTECT(newArgvec);
    Ob* surrogate = BASE(ctxt->arg(0))->dup();
    PROTECT(surrogate);
    Ctxt* newCtxt = Ctxt::create(ctxt->trgt, newArgvec);
    newCtxt->self2 = ctxt->arg(0);
    newCtxt->selfEnv = surrogate;
    ;
    newCtxt->env = surrogate;
    newCtxt->code = SELF->code;
    newCtxt->scheduleStrand();
    return SUSPENDED;
}


DEF("method-new", makeMethod, 1, 3) {
    CHECK(0, Code, code);

    Ob* sp = NIV;
    Ob* id = Qanon;

    switch (NARGS) {
    case 3:
        sp = ARG(2);
    case 2:
        id = ARG(1);
    }

    return StdMthd::create(code, id, sp);
}


DEF("reflective-method-new", makeReflectiveMethod, 1, 3) {
    CHECK(0, Code, code);

    Ob* sp = NIV;
    Ob* id = Qanon;

    switch (NARGS) {
    case 3:
        sp = ARG(2);
    case 2:
        id = ARG(1);
    }

    return ReflectiveMthd::create(code, id, sp);
}
