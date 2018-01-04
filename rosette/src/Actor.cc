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

#include "rosette.h"
#include "Ctxt.h"
#include "Meta.h"
#include "Ob.h"
#include "Operation.h"
#include "Prim.h"
#include "Tuple.h"
#include "BuiltinClass.h"

#include <algorithm>
#include <memory.h>


BUILTIN_CLASS(StdExtension) {}


StdExtension::StdExtension(pOb meta, pOb parent, int nslots)
    : Ob(sizeof(StdExtension) + nslots * sizeof(pOb), meta, parent) {
    while (nslots--) {
        this->slot(nslots) = INVALID;
    }

    StdExtension::updateCnt();
}


/*
 * This constructor is global because Tuple.cc needs access to it.
 */

StdExtension::StdExtension(pOb newmeta, pOb newparent)
    : Ob(BuildInPlace, newmeta, newparent) {}


StdExtension::StdExtension(int nslots)
    : Ob(sizeof(StdExtension) + nslots * sizeof(pOb), CLASS_META(StdExtension),
         CLASS_SBO(StdExtension)) {
    while (nslots--) {
        this->slot(nslots) = INVALID;
    }

    StdExtension::updateCnt();
}


StdExtension::StdExtension(pTuple proto)
    : Ob(SIZE(proto), CLASS_META(StdExtension), CLASS_SBO(StdExtension)) {
    memcpy(&slot(0), &proto->elem(0), proto->numberOfElements() * sizeof(pOb));
    StdExtension::updateCnt();
}


StdExtension* StdExtension::create(pOb meta, pOb parent, int nslots) {
    void* loc =
        PALLOC2(sizeof(StdExtension) + nslots * sizeof(pOb), meta, parent);
    return new (loc) StdExtension(meta, parent, nslots);
}


StdExtension* StdExtension::create(int nslots) {
    void* loc = PALLOC(sizeof(StdExtension) + nslots * sizeof(pOb));
    return new (loc) StdExtension(nslots);
}


StdExtension* StdExtension::create(pTuple argvec) {
    void* loc = PALLOC1(SIZE(argvec), argvec);
    return new (loc) StdExtension(argvec);
}


BUILTIN_CLASS(Actor) {}


Actor::Actor(pOb meta, pOb parent, pExt ext)
    : MboxOb(sizeof(Actor), meta, parent, lockedMbox), extension(ext) {
    if (meta != INVALID) {
        BASE(meta)->addRef();
    }

    Actor::updateCnt();
}


Actor* Actor::create() {
    StdMeta* meta = StdMeta::create(NIL, FIXNUM(1));
    PROTECT(meta);
    StdExtension* ext = StdExtension::create(0);
    void* loc = PALLOC1(sizeof(Actor), ext);
    return new (loc) Actor(meta, INVALID, ext);
}


Actor* Actor::create(pOb meta, pOb parent, pExt ext) {
    void* loc = PALLOC3(sizeof(Actor), meta, parent, ext);
    return new (loc) Actor(meta, parent, ext);
}


pOb Actor::container() { return extension; }


int Actor::addSlot(pOb, pOb val) {
    PROTECT_THIS(Actor);
    int n = SELF->extension->numberOfSlots();
    pExt ext = (pExt)SELF->extension->rcons(val);
    ASSIGN(SELF, extension, ext);
    return n;
}


pOb Actor::dup() {
    PROTECT_THIS(Actor);
    const int sz = SIZE(SELF);
    pOb ob = (pOb)PALLOC(sz);
    obcpy(ob, SELF, sz);
    ob->updateCnt();
    return ob;
}


pOb Actor::cloneTo(pOb new_meta, pOb new_parent) {
    PROTECT_THIS(Actor);
    PROTECT(new_parent);
    PROTECT(new_meta);
    pExt new_ext = (pExt)SELF->extension->cloneTo(new_meta, new_parent);
    PROTECT(new_ext);
    Actor* new_self = (Actor*)SELF->Ob::cloneTo(new_meta, new_parent);
    new_meta->deleteRef();  // Refs from extensions don't count.
    new_self->mbox = lockedMbox;
    new_self->extension = new_ext;
    return new_self;
}


pOb Actor::updateNoArgs() {
    PROTECT_THIS(Actor);
    mbox->nextMsg(SELF, NIL);
    return SELF;
}


pOb Actor::update(bool enabled_set_provided, pCtxt ctxt) {
    PROTECT_THIS(Actor);
    PROTECT(ctxt);
    pOb result = SELF;
    PROTECT(result);

    if (ctxt->nargs > enabled_set_provided) {
        pExt new_extension = (pExt)SELF->extension->clone();
        ASSIGN(SELF, extension, new_extension);
        result = SELF->Ob::update(enabled_set_provided, ctxt);
    }

    SELF->mbox->nextMsg(SELF, enabled_set_provided ? ctxt->arg(0) : NIL);
    return result;
}


pOb Actor::updateByLoc(bool enabled_set_provided, pCtxt ctxt) {
    PROTECT_THIS(Actor);
    PROTECT(ctxt);
    pOb result = SELF;
    PROTECT(result);

    if (ctxt->nargs > enabled_set_provided) {
        pExt new_extension = (pExt)SELF->extension->clone();
        ASSIGN(SELF, extension, new_extension);
        result = SELF->Ob::updateByLoc(enabled_set_provided, ctxt);
    }

    SELF->mbox->nextMsg(SELF, enabled_set_provided ? ctxt->arg(0) : NIL);
    return result;
}


pOb Actor::becomeNew(pOb proto, pCtxt ctxt) {
    PROTECT_THIS(Actor);
    PROTECT(proto);
    PROTECT(ctxt);
    proto = BASE(proto);

    pExt new_container = (pExt)proto->container()->clone();
    ASSIGN(SELF, extension, new_container);
    SELF->meta()->deleteRef();
    proto->meta()->addRef();
    ASSIGN(SELF, meta(), proto->meta());
    ASSIGN(SELF, parent(), proto->parent());
    SELF->clobberVtbl(proto);

    return SELF;
}

extern StdOprn* oprnLookupAndInvoke;

pOb Actor::dispatch(pCtxt ctxt) {
    PROTECT(ctxt);
    Tuple* av = Tuple::create(2, NIV);

    ASSIGN(av, elem(0), ctxt->trgt);
    ASSIGN(av, elem(1), ctxt);

    pCtxt c = Ctxt::create(oprnLookupAndInvoke, av);
    c->nargs = 2;

    return BASE(c->trgt)->dispatch(c);
}


pOb Actor::lookupAndInvoke(pCtxt ctxt) {
    if (BASE(ctxt->trgt)->isSynchronousTrgt()) {
        return Ob::lookupAndInvoke(ctxt);
    } else {
        receive(ctxt);
        return SUSPENDED;
    }
}


void Actor::schedule(pCtxt task) {
    /*
     * This is the code that is invoked when a message (the task formal)
     * is removed from an actor's mailbox.
     */
    Ob::lookupAndInvoke(task);
}


static const int MinimumTblObjectSlots = 4;


BUILTIN_CLASS(TblObject) {}


TblObject::TblObject(pExt ext, pOb validExtent, pTuple keyVec)
    : Actor(sizeof(TblObject), CLASS_META(TblObject), CLASS_SBO(TblObject),
            emptyMbox, ext),
      validExtent(validExtent),
      keyVec(keyVec) {
    TblObject::updateCnt();
}


TblObject* TblObject::create() {
    StdExtension* ext = StdExtension::create(0);
    void* loc = PALLOC1(sizeof(TblObject), ext);
    return new (loc) TblObject(ext, FIXNUM(0), NIL);
}


int TblObject::addSlot(pOb key, pOb val) {
    PROTECT_THIS(TblObject);

    const int n = FIXVAL(validExtent);

    if (n >= extension->numberOfSlots()) {
        PROTECT(key);
        PROTECT(val);
        const int new_size = std::max(2 * n, MinimumTblObjectSlots);
        StdExtension* ext = StdExtension::create(new_size);
        memcpy(&ext->slot(0), &SELF->extension->slot(0), n * sizeof(pOb));
        ASSIGN(SELF, extension, ext);
        pTuple keyVec = Tuple::create(new_size, NIV);
        memcpy(&keyVec->elem(0), &SELF->keyVec->elem(0), n * sizeof(pOb));
        ASSIGN(SELF, keyVec, keyVec);
    }

    ASSIGN(SELF->extension, slot(n), val);
    ASSIGN(SELF->keyVec, elem(n), key);
    FIXNUM_INC(SELF->validExtent);
    return n;
}


pOb TblObject::entryKey(int n) { return keyVec->elem(n); }


DEF("actor-new", actorNew, 3, 3) {
    //	(actor-new foo-super ['x 'y ] [23 43])

    CHECK(1, Tuple, keyVec);
    PROTECT(keyVec);
    CHECK(2, Tuple, vals);
    PROTECT(vals);
    pOb sbo = ARG(0);
    PROTECT(sbo);

    if (keyVec->numberOfElements() != vals->numberOfElements()) {
        return PRIM_ERROR("non-conforming keys and initial values");
    }

    pOb meta = StdMeta::create(keyVec, FIXNUM(0), RBLTRUE);
    PROTECT(meta);
    pExt ext = StdExtension::create(vals);

    return Actor::create(meta, sbo, ext);
}


#define EnabledSetProvided true


DEF("update!", actorUpdateBang, 0, MaxArgs) {
    return BASE(__CTXT__->self2)->update(!EnabledSetProvided, __CTXT__);
}


DEF("update!!", actorUpdateBangBang, 0, MaxArgs) {
    return BASE(__CTXT__->self2)->updateByLoc(!EnabledSetProvided, __CTXT__);
}


DEF("next!", actorNextBang, 1, MaxArgs) {
    return BASE(__CTXT__->self2)->update(EnabledSetProvided, __CTXT__);
}


DEF("next!!", actorNextBangBang, 1, MaxArgs) {
    return BASE(__CTXT__->self2)->updateByLoc(EnabledSetProvided, __CTXT__);
}
