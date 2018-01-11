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

#include "Proc.h"
#include "Code.h"
#include "Ctxt.h"
#include "Prim.h"
#include "Tuple.h"

#include "BuiltinClass.h"


BUILTIN_CLASS(Proc) {
    OB_FIELD("env", Proc, env);
    OB_FIELD("code", Proc, code);
    OB_FIELD("id", Proc, id);
    OB_FIELD("source", Proc, source);
}


Proc::Proc(Ob* env, Code* code, Ob* id, Ob* source)
    : Ob(sizeof(Proc), CLASS_META(Proc), CLASS_SBO(Proc)),
      env(env),
      code(code),
      id(id),
      source(source) {
    Proc::updateCnt();
}


Proc* Proc::create(Ob* env, Code* code, Ob* id, Ob* source) {
    void* loc = PALLOC4(sizeof(Proc), env, code, id, source);
    return new (loc) Proc(env, code, id, source);
}


Ob* Proc::dispatch(Ctxt* ctxt) {
    if (debugging_level) {
        printf("\tproc %s\n", BASE(id)->asCstring());
    }

    ctxt->code = this->code;
    ctxt->env = this->env;
    ctxt->rslt = NIV;
    ctxt->pc = 0;
    CHECK_STORE(ctxt, (VAL(this->code), VAL(this->env)));
    ctxt->scheduleStrand();
    return SUSPENDED;
}


Ob* Proc::invoke(Ctxt* ctxt) { return dispatch(ctxt); }


DEF("proc-new", makeProc, 2, 4) {
    CHECK(1, Code, code);
    Ob* sp = NIV;
    Ob* id = Qanon;

    switch (NARGS) {
    case 4:
        sp = ARG(3);
    case 3:
        id = ARG(2);
    }

    return Proc::create(ARG(0), code, id, sp);
}
