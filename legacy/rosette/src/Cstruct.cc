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

#include "Ctxt.h"

#include <unistd.h>

#include "Cstruct.h"
#include "RblAtom.h"
#include "BinaryOb.h"
#include "Heap.h"
#include "MI.h"
#include "Ob.h"
#include "Operation.h"
#include "Prim.h"
#include "Reader.h"
#include "RBLstream.h"
#include "RBLstring.h"
#include "Tuple.h"
#include "Vm.h"
#include "Table.h"
#include "Addr.h"

#include "BuiltinClass.h"
#include "ModuleInit.h"

#ifdef MAP_BACK_ADDRESS
extern uint32_t nontrivial_pre_fixnum_to_addr(int);
extern int nontrivial_addr_to_pre_fixnum(Ob*);
#endif

/*  */

// some externs we need for consing up new actors and type-checking
// they live in BigBang.cc

extern pOb obSBO;
extern Tuple* protoTuple;
extern RBLstring* protoString;
extern StdOprn* oprnKind;

extern GenericDescriptor* obGenericDescriptor;
extern NullDescriptor* obNullDescriptor;
extern AtomicDescriptor* obAtomicDescriptor;
extern CStructure* obCStructure;
extern CArray* obCArray;
extern CharArray* obCharArray;
extern CharArray0* obCharArray0;
extern CRef* obCRef;
extern CharRef* obCharRef;
extern CRef0* obCRef0;
extern CharRef0* obCharRef0;
extern CUnion* obCUnion;
extern AtomicDescriptor* obChar;

// these live in Basic-support.cc
extern Ob* newSBO(Ob* proto_sbo, Ob* id, Ob* prnt, Ob* ctxt);
extern Ob* genActor(Ob* proto, Ob* sbo);
extern uint32_t mem_get_field(uint32_t* addr, int offset, int span, int sign);
extern uint32_t* mem_set_field(uint32_t* addr, int offset, int span,
                               uint32_t bits);

// some useful defines so i don't have to remember the accessors

// NB(leaf): From BaseSupp.cc.
extern uint32_t local_page_size;

#define TUPLE_HEAD(a_tup, index) (a_tup->elem(index))
#define TUPLE_TAIL(a_tup, index) (index++)
#define NEWTUPLE(a_tup) (Tuple::create(1, a_tup))
#define NULLP(a_tup, index) (a_tup->numberOfElements() == index)
#define NULLTAILP(a_tup, index) (a_tup->numberOfElements() == (index + 1))
#define TUPLEP(obj) ((protoTuple->typep(BASE(obj)) == RBLTRUE))
#define STRINGP(obj) ((protoString->typep(BASE(obj)) == RBLTRUE))
#define TYPEP(obj1, obj2) (obj1->typep(obj2) == RBLTRUE)
#define TYPEGTRP(obj1, obj2) (typeGreaterEq(obj1, obj2))
#define ISNULLP(obj) (obj->isNullP() == RBLTRUE)
#define GET_STRING(rstr) ((const char*)&(((RBLstring*)rstr)->byte(0)))

/************************************************************/
/***                                                      ***/
/***        The occurs-check tag representation.          ***/
/***                                                      ***/
/************************************************************/

#define OCTAGCOUNT 4

Tuple* makeOCTag(Ob* self, uint32_t nid) {
    PROTECT(self);
    Tuple* OCTag = Tuple::create(OCTAGCOUNT, INVALID);
    OCTag->setNth(0, FIXNUM(nid));
    OCTag->setNth(1, self);
    OCTag->setNth(2, self->meta());
    OCTag->setNth(3, self->parent());

    return (OCTag);
}

/*
Ob*
makeOCTag(GenericDescriptor* self, uint32_t base)
{
  return( FIXNUM(self->absoluteAddress(base)) );
}
*/

int OCHit(pOb key1, pOb key2) {
    if ((TUPLEP(key1)) && TUPLEP(key2)) {
        Tuple* t1 = (Tuple*)key1;
        Tuple* t2 = (Tuple*)key2;

        if ((t1->numberOfElements() == t2->numberOfElements()) &&
            t1->numberOfElements() == OCTAGCOUNT) {
            int offsetAndBaseEq = t1->nth(0) == t2->nth(0);
            int parentsEq = t1->nth(2) == t2->nth(2);
            int metasEq = t1->nth(3) == t2->nth(3);
            int selvesEq = t1->nth(1) == t2->nth(1);
            return ((offsetAndBaseEq && parentsEq && metasEq) || (selvesEq));
        }

        return 0;
    }

    return 0;
}

RblTable* makeOccursCheckTable() {
    RblTable* OCTab = RblTable::create();
    // OCTab->hitFn = &OCHit;

    return (OCTab);
}

/************************************************************/
/***                                                      ***/
/*** GenericDescriptor is the root of the Descriptor tree ***/
/***                                                      ***/
/************************************************************/

DEF("select", obSelect, 3, 3) {
    CHECK_FIXNUM(1, base);
    CHECK(2, Tuple, accessors);
    PROTECT(accessors);
    return (((GenericDescriptor*)(BASE(ARG(0))))
                ->select(__CTXT__, base, accessors));
}

DEF("S-get", obSGet, 3, 3) {
    CHECK_FIXNUM(1, base);
    CHECK(2, Tuple, accessors);
    PROTECT(accessors);
    return (
        ((GenericDescriptor*)(BASE(ARG(0))))->sGet(__CTXT__, base, accessors));
}

DEF("S-desc", obSDesc, 3, 3) {
    CHECK_FIXNUM(1, base);
    CHECK(2, Tuple, accessors);
    PROTECT(accessors);
    return (
        ((GenericDescriptor*)(BASE(ARG(0))))->sDesc(__CTXT__, base, accessors));
}

DEF("S-deref", obSDeref, 3, 3) {
    CHECK_FIXNUM(1, base);
    CHECK(2, Tuple, accessors);
    PROTECT(accessors);
    return (((GenericDescriptor*)(BASE(ARG(0))))
                ->sDeref(__CTXT__, base, accessors));
}

DEF("S-set", obSSet, 3, 3) {
    CHECK_FIXNUM(1, base);
    CHECK(2, Tuple, accessors);
    PROTECT(accessors);
    Ob* val = TUPLE_HEAD(accessors, 0);
    PROTECT(val);
    return (((GenericDescriptor*)(BASE(ARG(0))))
                ->sSet(__CTXT__, base, val, accessors, 1));
}

DEF("S-tupleSet", obSTupleSet, 4, 4) {
    CHECK_FIXNUM(1, base);
    CHECK(2, Tuple, val);
    PROTECT(val);
    CHECK(3, Tuple, accessors);
    PROTECT(accessors);
    return (((GenericDescriptor*)(BASE(ARG(0))))
                ->sTupleSet(__CTXT__, base, val, accessors));
}

DEF("null", obNull, 1, 1) {
    return (((GenericDescriptor*)(BASE(ARG(0))))->nullDescriptor(__CTXT__));
}

DEF("prim-flatten", obFlatten, 2, 2) {
    CHECK_TYPE(0, GenericDescriptor, desc);
    CHECK_FIXNUM(1, base);
    PROTECT(__CTXT__);
    PROTECT(desc);
    RblTable* tbl = makeOccursCheckTable();
    return desc->flatten(__CTXT__, base, tbl);
}

DEF("-A->", obConvertActualArg, 2, 2) {
    return (FIXNUM((((GenericDescriptor*)(BASE(ARG(0))))
                        ->convertActualArg(__CTXT__, ARG(1)))
                       .val));
}

DEF(">-A-", obConvertActualRslt, 2, 2) {
    CHECK_FIXNUM(1, rslt);
    return (((GenericDescriptor*)(BASE(ARG(0))))
                ->convertActualRslt(__CTXT__, rslt));
}

DEF("csNth", obCSNth, 3, 3) {
    CHECK_FIXNUM(1, base);
    CHECK(2, Tuple, ipath);
    PROTECT(ipath);
    Ob* val = TUPLE_HEAD(ipath, 0);
    PROTECT(val);
    return (((GenericDescriptor*)(BASE(ARG(0))))
                ->nthBase(__CTXT__, base, FIXVAL(val), ipath, 0));
}

DEF_OPRN(Std, "select", oprnSelect, obSelect);
DEF_OPRN(Std, "S-get", oprnSGet, obSGet);
DEF_OPRN(Std, "S-desc", oprnSDesc, obSDesc);
DEF_OPRN(Std, "S-deref", oprnSDeref, obSDeref);
DEF_OPRN(Std, "S-set", oprnSSet, obSSet);
DEF_OPRN(Std, "S-tupleSet", oprnSTupleSet, obSTupleSet);
DEF_OPRN(Std, "null", oprnNull, obNull);
DEF_OPRN(Std, "flatten", oprnFlatten, obFlatten);
DEF_OPRN(Std, "-A->", oprnConvertActualArg, obConvertActualArg);
DEF_OPRN(Std, ">-A-", oprnConvertActualRslt, obConvertActualRslt);
DEF_OPRN(Std, "nth", oprnCSNth, obCSNth);


BUILTIN_CLASS(GenericDescriptor) {
    ADDR_FIELD("offset!", GenericDescriptor, _offset);
    BIT_FIELD("align-to!", GenericDescriptor, _align_to, BITS(uint32_t));
    BIT_FIELD("size!", GenericDescriptor, _size, BITS(uint32_t));
    OB_FIELD("mnemonic", GenericDescriptor, mnemonic);
    OB_FIELD("imported", GenericDescriptor, imported);
    OB_FIELD("free-on-gc", GenericDescriptor, freeStructOnGC);
}

GenericDescriptor::GenericDescriptor(pExt ext)
    //: MboxOb(sizeof(GenericDescriptor), CLASS_META(GenericDescriptor),
    //	 CLASS_SBO(GenericDescriptor), emptyMbox),
    //	 mnemonic(NIV)
    : Actor(sizeof(GenericDescriptor), CLASS_META(GenericDescriptor),
            CLASS_SBO(GenericDescriptor), emptyMbox, ext),
      _offset(0),
      _align_to(0),
      _size(0),
      mnemonic(NIV),
      imported(RBLFALSE),
      freeStructOnGC(RBLFALSE) {
    GenericDescriptor::updateCnt();
}

GenericDescriptor::GenericDescriptor(int s, pOb m, pOb sbo, pOb mbx, pExt ext)
    //: MboxOb(s, m, sbo, mbx), mnemonic(NIV)
    : Actor(s, m, sbo, mbx, ext),
      _offset(0),
      _align_to(0),
      _size(0),
      mnemonic(NIV),
      imported(RBLFALSE),
      freeStructOnGC(RBLFALSE) {
    GenericDescriptor::updateCnt();
}


GenericDescriptor::~GenericDescriptor() {
    if (freeStructOnGC == RBLTRUE) {
        delete (GenericDescriptor*)_offset;
    }
}


GenericDescriptor* GenericDescriptor::create() {
    StdExtension* ext = StdExtension::create(0);
    void* loc = PALLOC1(sizeof(GenericDescriptor), ext);
    return new (loc) GenericDescriptor(ext);
}

int GenericDescriptor::traversePtrs(PSOb__PSOb f) {
    int sum = 0;

    sum += useIfPtr(&meta(), f);
    sum += useIfPtr(&parent(), f);
    sum += useIfPtr(&mbox, f);
    sum += useIfPtr(&extension, f);

    sum += useIfPtr(&mnemonic, f);
    sum += useIfPtr(&imported, f);
    sum += useIfPtr(&freeStructOnGC, f);

    return sum;
}

int GenericDescriptor::traversePtrs(SI__PSOb f) {
    int sum = 0;

    sum += useIfPtr(meta(), f);
    sum += useIfPtr(parent(), f);
    sum += useIfPtr(mbox, f);
    sum += useIfPtr(extension, f);

    sum += useIfPtr(mnemonic, f);
    sum += useIfPtr(imported, f);
    sum += useIfPtr(freeStructOnGC, f);

    return sum;
}

void GenericDescriptor::traversePtrs(V__PSOb f) {
    useIfPtr(meta(), f);
    useIfPtr(parent(), f);
    useIfPtr(mbox, f);
    useIfPtr(extension, f);

    useIfPtr(mnemonic, f);
    useIfPtr(imported, f);
    useIfPtr(freeStructOnGC, f);
}

Ob* GenericDescriptor::oprnSwitch(Ctxt* ctxt, uint32_t base, Tuple* path,
                                  int pindex) {
    PROTECT_THIS(GenericDescriptor);
    PROTECT(ctxt);
    PROTECT(path);
    Ob* head = TUPLE_HEAD(path, pindex);
    PROTECT(head);
    TUPLE_TAIL(path, pindex);

    if (head == oprnSelect)
        return SELF->select(ctxt, base, path, pindex);
    if (head == oprnSGet)
        return SELF->sGet(ctxt, base, path, pindex);
    if (head == oprnSDesc)
        return SELF->sDesc(ctxt, base, path, pindex);
    if (head == oprnSDeref) {
        return SELF->sDeref(ctxt, base, path, pindex);
    } else {
        if (head == oprnCSNth) {
            return SELF->nthBase(ctxt, base, FIXVAL(TUPLE_HEAD(path, pindex)),
                                 path, pindex);
        } else {
            TUPLE_TAIL(path, pindex);
            if (head == oprnSSet) {
                return SELF->sSet(ctxt, base, TUPLE_HEAD(path, (pindex - 1)),
                                  path, pindex);
            }
            if (head == oprnSTupleSet) {
                return SELF->sTupleSet(ctxt, base,
                                       (Tuple*)TUPLE_HEAD(path, (pindex - 1)),
                                       path, pindex);
            }
        }
    }

    return runtimeError(ctxt, "invalid c-structure operation", head);
}

Ob* GenericDescriptor::nullDescriptor(Ctxt* ctxt) {
    // Ask the sbo of dsc to find the value of a class slot nullDesc, if
    // it is #ABSENT, then cons up a new NullDescriptor and cache it in
    // the slot.

    // Needs protection code.
    PROTECT_THIS(GenericDescriptor);
    PROTECT(ctxt);

    Ob* prnt = SELF->parent();
    PROTECT(prnt);
    Ob* mta = prnt->meta();
    PROTECT(mta);

    Ob* rslt;

    if ((rslt = mta->get(prnt, oprnNull, ctxt)) == ABSENT) {
        Ob* knd = mta->get(prnt, oprnKind, ctxt);
        if (knd == ABSENT) {
            return runtimeError(ctxt, "invalid kind ", prnt);
        }

        const char* kname = BASE(knd)->asCstring();
        const char* np = "Null";
        char* nullactstr = new char[strlen(kname) + 5];
        (void)strcat(strcpy(nullactstr, (char*)np), kname);
        Ob* nullactid = SYMBOL(nullactstr);
        PROTECT(nullactid);

        Ob* ns = newSBO(obSBO, nullactid, CLASS_SBO(NullDescriptor), ctxt);
        PROTECT(ns);
        Ob* nd = genActor(obNullDescriptor, ns);
        PROTECT(nd);
        mta->add(prnt, oprnNull, nd, ctxt);
        return nd;
    }

    return rslt;
}

convertArgReturnPair GenericDescriptor::convertActualArg(Ctxt* ctxt, Ob* obj) {
    cnvArgRetPair.val = (uint32_t)-1;
    cnvArgRetPair.failp = 1;
    return cnvArgRetPair;
}

Ob* GenericDescriptor::convertActualRslt(Ctxt* ctxt, uint32_t obj) {
    if (obj == 0) {
        return nullDescriptor(ctxt);
    } else {
        GenericDescriptor* rslt = (GenericDescriptor*)sBox(obj);
        rslt->imported = RBLTRUE;
        if ((rslt->freeStructOnGC = freeStructOnGC) == RBLTRUE) {
            heap->registerForeignOb(rslt);
        }
        return rslt;
    }
}

Ob* GenericDescriptor::sGet(Ctxt* ctxt, uint32_t base, Tuple* path,
                            int pindex) {
    if (NULLP(path, pindex)) {
        return sBox(base + _offset);
    }

    return oprnSwitch(ctxt, base, path, pindex);
}

Ob* GenericDescriptor::sDesc(Ctxt* ctxt, uint32_t base, Tuple* path,
                             int pindex) {
    /* remember to ask about gc protection */
    if (NULLP(path, pindex)) {
        return sBox(base + _offset);
    }

    return oprnSwitch(ctxt, base, path, pindex);
}

Ob* GenericDescriptor::sDeref(Ctxt* ctxt, uint32_t, Tuple* path, int) {
    return runtimeError(ctxt, "Cannot de-reference ", path);
}

Ob* GenericDescriptor::select(Ctxt* ctxt, uint32_t, Tuple* path, int) {
    return runtimeError(ctxt, "Cannot select with: ", path);
}

Ob* GenericDescriptor::sSet(Ctxt* ctxt, uint32_t base, Ob* val, Tuple* path,
                            int pindex) {
    if (TYPEP(this, val)) {
        memcpy((void*)(base + _offset),
               (void*)(((GenericDescriptor*)val)->_offset), (int)_size);
        if (NULLP(path, pindex)) {
            return NIV;
        } else {
            oprnSwitch(ctxt, base, path, pindex);
        }
    } else if (TUPLEP(val)) {
        return sTupleSet(ctxt, base, (Tuple*)val, path, pindex);
    }

    return runtimeError(ctxt, "S-set type-mismatch ", val);
}

Ob* GenericDescriptor::sTupleSet(Ctxt* ctxt, uint32_t base, Tuple* val,
                                 Tuple* path, int pindex) {
    // Needs protection code.
    PROTECT_THIS(GenericDescriptor);
    PROTECT(ctxt);
    PROTECT(path);
    PROTECT(val);
    int vindex = 0;
    Ob* tupHead = INVALID;
    PROTECT(tupHead);

    for (uint32_t addr = base; !(NULLP(val, vindex));
         addr = addr + SELF->_size) {
        tupHead = TUPLE_HEAD(val, vindex);
        TUPLE_TAIL(val, vindex);
        (void)(SELF->sSet(ctxt, addr, tupHead, NIL));
    }

    if (NULLP(path, pindex)) {
        return NIV;
    } else {
        TUPLE_TAIL(path, pindex);
        return (SELF->oprnSwitch(ctxt, base, path, pindex));
    }
}

Ob* GenericDescriptor::nthBase(Ctxt* ctxt, uint32_t base, int i, Tuple* path,
                               int pindex) {
    PROTECT_THIS(GenericDescriptor);
    PROTECT(ctxt);
    PROTECT(path);
    uint32_t newBase = (base + (i * SELF->_size));
    if (NULLP(path, pindex)) {
        return SELF->sBox(newBase + SELF->_offset);
    } else {
        TUPLE_TAIL(path, pindex);
        return SELF->oprnSwitch(ctxt, newBase, path, pindex);
    }
}

Ob* GenericDescriptor::flatten(Ctxt* ctxt, uint32_t, RblTable*) {
    return runtimeError(ctxt, "method undefined for ", this);
}

uint32_t GenericDescriptor::absoluteAddress(uint32_t base) {
    uint32_t newbase = (base + _offset);
    uint32_t newoff = newbase % 4;
    return (uint32_t)(mem_get_field((uint32_t*)(newbase - newoff),
                                    (int)(newoff * 8), (int)(_size * 8),
                                    BOOLVAL(RBLFALSE)));
}

void GenericDescriptor::setAddrContents(uint32_t base, uint32_t val) {
    uint32_t newbase, offset;
    newbase = base + _offset;
    offset = newbase % 4;

    mem_set_field((uint32_t*)(newbase - offset), (int)(offset * 8),
                  (int)(_size * 8), val);
}

/************************************************************************/
/***                                                                  ***/
/***  NullDescriptor ensures unique nulls for each descriptor class.  ***/
/***                                                                  ***/
/************************************************************************/

BUILTIN_CLASS(NullDescriptor) {}

NullDescriptor::NullDescriptor(pExt ext)
    : GenericDescriptor(sizeof(NullDescriptor), CLASS_META(NullDescriptor),
                        CLASS_SBO(NullDescriptor), emptyMbox, ext) {
    NullDescriptor::updateCnt();
}

NullDescriptor* NullDescriptor::create() {
    StdExtension* ext = StdExtension::create(0);
    void* loc = PALLOC1(sizeof(NullDescriptor), ext);
    return new (loc) NullDescriptor(ext);
}

Ob* NullDescriptor::sGet(Ctxt*, uint32_t, Tuple*, int) {
    //(pure (S-get & r) (self))
    return this;
}

Ob* NullDescriptor::sDesc(Ctxt*, uint32_t, Tuple*, int) {
    //(pure (S-desc & r) (self))
    return this;
}

Ob* NullDescriptor::sDeref(Ctxt* ctxt, uint32_t, Tuple* path, int) {
    //(pure (S-deref & r) (RuntimeError (self) "Cannot deref(^) " r))
    return runtimeError(ctxt, "Cannot deref(^) ", path);
}

Ob* NullDescriptor::select(Ctxt* ctxt, uint32_t, Tuple* path, int) {
    //(pure (select & r) (RuntimeError (self) "Cannot select " r))
    return runtimeError(ctxt, "Cannot select ", path);
}

Ob* NullDescriptor::sSet(Ctxt* ctxt, uint32_t, Ob*, Tuple* path, int) {
    //(pure (S-set & r) (RuntimeError (self) "Cannot set(:=) " r))
    return runtimeError(ctxt, "Cannot set(:=) ", path);
}

Ob* NullDescriptor::nthBase(Ctxt* ctxt, uint32_t, int, Tuple* path, int) {
    //(pure (nth & r) (RuntimeError (self) "Cannot index " r))
    return runtimeError(ctxt, "Cannot index ", path);
}

Ob* NullDescriptor::flatten(Ctxt*, uint32_t, RblTable*) { return this; }

uint32_t NullDescriptor::absoluteAddress(uint32_t base) { return (uint32_t)0; }

pOb NullDescriptor::isNullP() { return RBLTRUE; }

/************************************************************************/
/***                                                                  ***/
/***         AtomicDescriptor handles basic machine entities.         ***/
/***                                                                  ***/
/************************************************************************/

BUILTIN_CLASS(AtomicDescriptor) {
    ADDR_FIELD("offset!", GenericDescriptor, _offset);
    BIT_FIELD("align-to!", GenericDescriptor, _align_to, BITS(uint32_t));
    BIT_FIELD("size!", GenericDescriptor, _size, BITS(uint32_t));
    OB_FIELD("mnemonic", GenericDescriptor, mnemonic);
    OB_FIELD("imported", NullDescriptor, imported);
    OB_FIELD("free-on-gc", NullDescriptor, freeStructOnGC);
    OB_FIELD("signed!", AtomicDescriptor, _signed);
}

AtomicDescriptor::AtomicDescriptor(RblBool* sgn, pExt ext)
    : GenericDescriptor(sizeof(AtomicDescriptor), CLASS_META(AtomicDescriptor),
                        CLASS_SBO(AtomicDescriptor), emptyMbox, ext),
      _signed(sgn) {
    AtomicDescriptor::updateCnt();
}

AtomicDescriptor::AtomicDescriptor(RblBool* sgn, int s, pOb mta, pOb prnt,
                                   pOb mbx, pExt ext)
    : GenericDescriptor(s, mta, prnt, mbx, ext), _signed(sgn) {
    AtomicDescriptor::updateCnt();
}

AtomicDescriptor* AtomicDescriptor::create(RblBool* b) {
    PROTECT(b);
    StdExtension* ext = StdExtension::create(0);
    void* loc = PALLOC1(sizeof(AtomicDescriptor), ext);
    return new (loc) AtomicDescriptor(b, ext);
}

AtomicDescriptor* AtomicDescriptor::create() {
    StdExtension* ext = StdExtension::create(0);
    void* loc = PALLOC1(sizeof(AtomicDescriptor), ext);
    return new (loc) AtomicDescriptor((RblBool*)RBLFALSE, ext);
}

int AtomicDescriptor::traversePtrs(PSOb__PSOb f) {
    return GenericDescriptor::traversePtrs(f) + useIfPtr(&_signed, f);
}

int AtomicDescriptor::traversePtrs(SI__PSOb f) {
    return GenericDescriptor::traversePtrs(f) + useIfPtr(_signed, f);
}

void AtomicDescriptor::traversePtrs(V__PSOb f) {
    GenericDescriptor::traversePtrs(f);
    useIfPtr(_signed, f);
}

convertArgReturnPair AtomicDescriptor::convertActualArg(Ctxt* ctxt, Ob* obj) {
    cnvArgRetPair.failp = 0;
    if (IS_FIXNUM(obj)) {
        cnvArgRetPair.val = (uint32_t)FIXVAL(obj);
    } else if (TYPEP(this, obj)) {
        cnvArgRetPair.val = (uint32_t)FIXVAL(sGet(ctxt, 0, NIL));
    } else {
        cnvArgRetPair.val = (uint32_t)-1;
        cnvArgRetPair.failp = 1;
    }
    return cnvArgRetPair;
}

Ob* AtomicDescriptor::convertActualRslt(Ctxt*, uint32_t obj) {
    return FIXNUM(obj);
}

Ob* AtomicDescriptor::sGet(Ctxt* ctxt, uint32_t base, Tuple* path, int pindex) {
    // Needs protection code.

    PROTECT_THIS(AtomicDescriptor);
    PROTECT(ctxt);
    PROTECT(path);

    if (NULLP(path, pindex)) {
        return (FIXNUM(SELF->absoluteAddress(base)));
    }

    return (SELF->oprnSwitch(ctxt, base, path, pindex));
}

Ob* AtomicDescriptor::sSet(Ctxt* ctxt, uint32_t base, Ob* val, Tuple* path,
                           int pindex) {
    // Needs protection code.
    PROTECT_THIS(AtomicDescriptor);
    PROTECT(ctxt);
    PROTECT(val);
    PROTECT(path);

    if (IS_FIXNUM(val)) {
        SELF->setAddrContents(base, (uint32_t)FIXVAL(val));
        if (!NULLP(path, pindex)) {
            return (SELF->oprnSwitch(ctxt, base, path, pindex));
        }
        return NIV;
    } else if (TUPLEP(val)) {
        return (SELF->sTupleSet(ctxt, base, (Tuple*)val, path, pindex));
    }

    return runtimeError(ctxt, "S-set type-mismatch ", val);
}

Ob* AtomicDescriptor::flatten(Ctxt* ctxt, uint32_t base, RblTable*) {
    return sGet(ctxt, base, NIL);
}

uint32_t AtomicDescriptor::absoluteAddress(uint32_t base) {
    uint32_t newbase = (base + _offset);
    uint32_t newoff = newbase % 4;
    return (uint32_t)(mem_get_field((uint32_t*)(newbase - newoff),
                                    (int)(newoff * 8), (int)(_size * 8),
                                    BOOLVAL(_signed)));
}

/************************************************************************/
/***                                                                  ***/
/***        Cstructure does the real work of modeling structs.        ***/
/***                                                                  ***/
/************************************************************************/

BUILTIN_CLASS(CStructure) {
    ADDR_FIELD("offset!", GenericDescriptor, _offset);
    BIT_FIELD("align-to!", GenericDescriptor, _align_to, BITS(uint32_t));
    BIT_FIELD("size!", GenericDescriptor, _size, BITS(uint32_t));
    OB_FIELD("mnemonic", GenericDescriptor, mnemonic);
    OB_FIELD("imported", CStructure, imported);
    OB_FIELD("free-on-gc", CStructure, freeStructOnGC);
    OB_FIELD("descs", CStructure, _descs);
    OB_FIELD("fieldNames", CStructure, _fieldNames);
}

CStructure::CStructure(RblTable* desks, Tuple* fnames, pExt ext)
    : GenericDescriptor(sizeof(CStructure), CLASS_META(CStructure),
                        CLASS_SBO(CStructure), emptyMbox, ext),
      _descs(desks),
      _fieldNames(fnames) {
    CStructure::updateCnt();
}


CStructure* CStructure::create(RblTable* tbl, Tuple* tup) {
    PROTECT(tbl);
    PROTECT(tup);
    StdExtension* ext = StdExtension::create(0);
    void* loc = PALLOC1(sizeof(CStructure), ext);
    return new (loc) CStructure(tbl, tup, ext);
}

CStructure* CStructure::create() {
    RblTable* tmp = RblTable::create();
    PROTECT(tmp);
    StdExtension* ext = StdExtension::create(0);
    void* loc = PALLOC1(sizeof(CStructure), ext);
    return new (loc) CStructure(tmp, NIL, ext);
}

int CStructure::traversePtrs(PSOb__PSOb f) {
    int sum = GenericDescriptor::traversePtrs(f);
    sum += useIfPtr(&_descs, f);
    sum += useIfPtr(&_fieldNames, f);
    return sum;
}

int CStructure::traversePtrs(SI__PSOb f) {
    int sum = GenericDescriptor::traversePtrs(f);
    sum += useIfPtr(_descs, f);
    sum += useIfPtr(_fieldNames, f);
    return sum;
}

void CStructure::traversePtrs(V__PSOb f) {
    GenericDescriptor::traversePtrs(f);
    useIfPtr(_descs, f);
    useIfPtr(_fieldNames, f);
}

Ob* CStructure::select(Ctxt* ctxt, uint32_t base, Tuple* path, int pindex) {
    // Needs protection code.
    PROTECT_THIS(CStructure);
    PROTECT(ctxt);
    PROTECT(path);

    if (NULLP(path, pindex)) {
        return runtimeError(ctxt, "Invalid path ", path);
    } else {
        Ob* symb = TUPLE_HEAD(path, pindex);
        PROTECT(symb);
        TUPLE_TAIL(path, pindex);
        GenericDescriptor* d = (GenericDescriptor*)(SELF->_descs->getKey(symb));
        PROTECT(d);
        if (d == ABSENT) {
            return runtimeError(ctxt, "Bad selector ", symb);
        } else if (NULLP(path, pindex)) {
            return (SELF->sBox(base + SELF->_offset + d->_offset));
        }

        return d->oprnSwitch(ctxt, (base + SELF->_offset), path, pindex);
    }
}

Ob* CStructure::sTupleSet(Ctxt* ctxt, uint32_t base, Tuple* val, Tuple* path,
                          int pindex) {
    // Needs protection code.

    PROTECT_THIS(CStructure);
    PROTECT(ctxt);
    PROTECT(val);
    PROTECT(path);

    // Check this more carefully... LGM
    if (TUPLEP(TUPLE_HEAD(val, 0))) {
        return GenericDescriptor::sTupleSet(ctxt, base, val, path, pindex);
    }

    if (val->numberOfElements() <= SELF->_fieldNames->numberOfElements()) {
        GenericDescriptor* entry;
        int vindex = 0;
        Ob* x = TUPLE_HEAD(val, vindex);
        PROTECT(x);
        for (int i = 0; !(NULLP(val, vindex)); i++) {
            TUPLE_TAIL(val, vindex);
            entry = (GenericDescriptor*)(SELF->_descs->getKey(
                SELF->_fieldNames->elem(i)));
            entry->sSet(ctxt, (base + SELF->_offset), x, NIL);
            x = TUPLE_HEAD(val, vindex);
        }

        return NIV;
    }

    return runtimeError(ctxt, "S-tupleSet too many elements ", val);
}

Ob* CStructure::flatten(Ctxt* ctxt, uint32_t base, RblTable* occtxt) {
    PROTECT_THIS(CStructure);
    PROTECT(ctxt);
    PROTECT(occtxt);
    Ob* chck = INVALID;
    PROTECT(chck);
    Ob* selfTag = makeOCTag(SELF, base + SELF->_offset);
    PROTECT(selfTag);

    if ((chck = occtxt->getKey(selfTag)) == ABSENT) {
        int fnlen = SELF->_fieldNames->numberOfElements();
        Tuple* rslt = Tuple::create(2 * fnlen, INVALID);
        PROTECT(rslt);

        Ob* symb = INVALID;
        Ob* tmp = INVALID;
        GenericDescriptor* crc = (GenericDescriptor*)INVALID;
        PROTECT(symb);
        PROTECT(tmp);
        PROTECT(crc);

        occtxt->addKey(selfTag, rslt);

        for (int i = 0; i < fnlen; i++) {
            // Get the symbol that names the field
            symb = SELF->_fieldNames->nth(i);

            // Get the value associated with the field
            crc = ((GenericDescriptor*)(SELF->_descs->getKey(symb)));

            // Create a tag to for Occurs Check
            tmp = crc->flatten(ctxt, base + SELF->_offset, occtxt);

            rslt->setNth(2 * i, symb);
            rslt->setNth(2 * i + 1, tmp);
        }

        return rslt;
    }

    return chck;
}

/************************************************************************/
/***                                                                  ***/
/***                  CArray - guess what this does.                  ***/
/***                                                                  ***/
/************************************************************************/

BUILTIN_CLASS(CArray) {
    ADDR_FIELD("offset!", GenericDescriptor, _offset);
    BIT_FIELD("align-to!", GenericDescriptor, _align_to, BITS(uint32_t));
    BIT_FIELD("size!", GenericDescriptor, _size, BITS(uint32_t));
    OB_FIELD("mnemonic", GenericDescriptor, mnemonic);
    OB_FIELD("imported", CArray, imported);
    OB_FIELD("free-on-gc", CArray, freeStructOnGC);
    BIT_FIELD("numElems", CArray, _numElems, BITS(uint16_t));
    OB_FIELD("elemDesc", CArray, _elemDesc);
}

CArray::CArray(uint16_t cnt, GenericDescriptor* elemd, pExt ext)
    : GenericDescriptor(sizeof(CArray), CLASS_META(CArray), CLASS_SBO(CArray),
                        emptyMbox, ext),
      _numElems(cnt),
      _elemDesc(elemd) {
    CArray::updateCnt();
}

CArray::CArray(int s, pOb m, pOb p, pOb mbx, pExt ext, uint16_t cnt,
               GenericDescriptor* elemd)
    : GenericDescriptor(s, m, p, mbx, ext), _numElems(cnt), _elemDesc(elemd) {
    CArray::updateCnt();
}

CArray* CArray::create(uint16_t cnt, GenericDescriptor* elmd) {
    PROTECT(elmd);
    StdExtension* ext = StdExtension::create(0);
    void* loc = PALLOC1(sizeof(CArray), ext);
    return new (loc) CArray(cnt, elmd, ext);
}

CArray* CArray::create() {
    GenericDescriptor* tmp = GenericDescriptor::create();
    PROTECT(tmp);
    StdExtension* ext = StdExtension::create(0);
    void* loc = PALLOC1(sizeof(CArray), ext);
    return new (loc) CArray(0, tmp, ext);
}

int CArray::traversePtrs(PSOb__PSOb f) {
    return GenericDescriptor::traversePtrs(f) + useIfPtr(&_elemDesc, f);
}

int CArray::traversePtrs(SI__PSOb f) {
    return GenericDescriptor::traversePtrs(f) + useIfPtr(_elemDesc, f);
}

void CArray::traversePtrs(V__PSOb f) {
    GenericDescriptor::traversePtrs(f);
    useIfPtr(_elemDesc, f);
}

Ob* CArray::sTupleSet(Ctxt* ctxt, uint32_t base, Tuple* val, Tuple* path,
                      int pindex) {
    if (val->numberOfElements() <= _numElems) {
        return (_elemDesc->sTupleSet(ctxt, base + _offset, val, path, pindex));
    }

    return (runtimeError(ctxt, "S-tupleSet too many elements ", val));
}

Ob* CArray::nthBase(Ctxt* ctxt, uint32_t base, int i, Tuple* path, int pindex) {
    // Needs protection code.

    if ((i >= 0) && (i <= _numElems)) {
        uint32_t addr = (base + _offset + (i * _elemDesc->_size));

        if (NULLP(path, pindex)) {
            return (_elemDesc->sBox(addr));
        }

        TUPLE_TAIL(path, pindex);
        return (_elemDesc->oprnSwitch(ctxt, addr, path, pindex));
    }
    return runtimeError(ctxt, "Index out of bounds ", path);
}

Ob* CArray::flatten(Ctxt* ctxt, uint32_t base, RblTable* occtxt) {
    PROTECT_THIS(CArray);
    PROTECT(ctxt);
    PROTECT(occtxt);
    Ob* chck = INVALID;
    PROTECT(chck);
    Ob* selfTag = makeOCTag(SELF, SELF->_offset + base);
    PROTECT(selfTag);

    if ((chck = occtxt->getKey(selfTag)) == ABSENT) {
        Tuple* rslt = Tuple::create(SELF->_numElems, INVALID);
        PROTECT(rslt);
        occtxt->addKey(selfTag, rslt);
        for (int i = 0; i < SELF->_numElems; i++) {
            uint32_t addrKey =
                base + SELF->_offset + (i * sizeof(SELF->_elemDesc));
            rslt->setNth(i, SELF->_elemDesc->flatten(ctxt, addrKey, occtxt));
        }

        return rslt;
    }

    return chck;
}

/************************************************************************/
/***                                                                  ***/
/***                CharArray - guess what this does.                 ***/
/***                                                                  ***/
/************************************************************************/

BUILTIN_CLASS(CharArray) {
    ADDR_FIELD("offset!", GenericDescriptor, _offset);
    BIT_FIELD("align-to!", GenericDescriptor, _align_to, BITS(uint32_t));
    BIT_FIELD("size!", GenericDescriptor, _size, BITS(uint32_t));
    OB_FIELD("mnemonic", GenericDescriptor, mnemonic);
    OB_FIELD("imported", CharArray, imported);
    OB_FIELD("free-on-gc", CharArray, freeStructOnGC);
    BIT_FIELD("numElems", CArray, _numElems, BITS(uint16_t));
    OB_FIELD("elemDesc", CArray, _elemDesc);
}

CharArray::CharArray(uint16_t cnt, GenericDescriptor* elemd, pExt ext)
    : CArray(sizeof(CharArray), CLASS_META(CharArray), CLASS_SBO(CharArray),
             emptyMbox, ext, cnt, elemd) {
    CharArray::updateCnt();
}

CharArray::CharArray(int s, pOb m, pOb p, pOb mbx, pExt ext, uint16_t cnt,
                     GenericDescriptor* elemd)
    : CArray(s, m, p, mbx, ext, cnt, elemd) {
    CharArray::updateCnt();
}

CharArray* CharArray::create(uint16_t cnt, GenericDescriptor* elmd) {
    PROTECT(elmd);
    StdExtension* ext = StdExtension::create(0);
    void* loc = PALLOC1(sizeof(CharArray), ext);
    return new (loc) CharArray(cnt, elmd, ext);
}

CharArray* CharArray::create() {
    GenericDescriptor* tmp = GenericDescriptor::create();
    PROTECT(tmp);
    StdExtension* ext = StdExtension::create(0);
    void* loc = PALLOC1(sizeof(CharArray), ext);
    return new (loc) CharArray(0, tmp, ext);
}

Ob* CharArray::sSet(Ctxt* ctxt, uint32_t base, Ob* val, Tuple* path,
                    int pindex) {
    if (STRINGP(val)) {
        uint32_t addr = base + _offset;
        if (addr >= local_page_size) {
            char* tmp = (char*)GET_STRING(val);
            (void)strncpy((char*)addr, tmp, _numElems);
            return NIV;
        }

        return runtimeError(ctxt, "invalid address ", FIXNUM(addr));
    }

    return CArray::sSet(ctxt, base, val, path, pindex);
}

Ob* CharArray::flatten(Ctxt* ctxt, uint32_t base, RblTable* occtxt) {
    PROTECT_THIS(CharArray);
    PROTECT(ctxt);
    PROTECT(occtxt);
    Ob* chck = INVALID;
    PROTECT(chck);
    Ob* selfTag = makeOCTag(SELF, base + SELF->_offset);
    PROTECT(selfTag);

    if ((chck = occtxt->getKey(selfTag)) == ABSENT) {
        uint32_t addr = base + _offset;
        if (addr >= local_page_size) {
            Ob* rslt = RBLstring::create((int)_size, (char*)addr);
            PROTECT(rslt);
            occtxt->addKey(selfTag, rslt);
            return rslt;
        }

        return runtimeError(ctxt, "invalid address");
    }

    return chck;
}

/************************************************************************/
/***                                                                  ***/
/***                CharArray0 - treats char arrays as null terminated***/
/***                                                                  ***/
/************************************************************************/

BUILTIN_CLASS(CharArray0) {
    ADDR_FIELD("offset!", GenericDescriptor, _offset);
    BIT_FIELD("align-to!", GenericDescriptor, _align_to, BITS(uint32_t));
    BIT_FIELD("size!", GenericDescriptor, _size, BITS(uint32_t));
    OB_FIELD("mnemonic", GenericDescriptor, mnemonic);
    OB_FIELD("imported", CharArray, imported);
    OB_FIELD("free-on-gc", CharArray, freeStructOnGC);
    BIT_FIELD("numElems", CArray, _numElems, BITS(uint16_t));
    OB_FIELD("elemDesc", CArray, _elemDesc);
}

CharArray0::CharArray0(uint16_t cnt, GenericDescriptor* elemd, pExt ext)
    : CharArray(sizeof(CharArray0), CLASS_META(CharArray0),
                CLASS_SBO(CharArray0), emptyMbox, ext, cnt, elemd) {
    CharArray0::updateCnt();
}

CharArray0* CharArray0::create(uint16_t cnt, GenericDescriptor* elmd) {
    PROTECT(elmd);
    StdExtension* ext = StdExtension::create(0);
    void* loc = PALLOC1(sizeof(CharArray0), ext);
    return new (loc) CharArray0(cnt, elmd, ext);
}

CharArray0* CharArray0::create() {
    GenericDescriptor* tmp = GenericDescriptor::create();
    PROTECT(tmp);
    StdExtension* ext = StdExtension::create(0);
    void* loc = PALLOC1(sizeof(CharArray0), ext);
    return new (loc) CharArray0(0, tmp, ext);
}

Ob* CharArray0::flatten(Ctxt* ctxt, uint32_t base, RblTable* occtxt) {
    PROTECT_THIS(CharArray);
    PROTECT(ctxt);
    PROTECT(occtxt);
    Ob* chck = INVALID;
    PROTECT(chck);
    Ob* selfTag = makeOCTag(SELF, base + SELF->_offset);
    PROTECT(selfTag);

    if ((chck = occtxt->getKey(selfTag)) == ABSENT) {
        uint32_t addr = base + _offset;
        if (addr >= local_page_size) {
            Ob* rslt = RBLstring::create((char*)addr);
            PROTECT(rslt);
            occtxt->addKey(selfTag, rslt);
            return rslt;
        }

        return runtimeError(ctxt, "invalid address");
    }

    return chck;
}

/************************************************************************/
/***                                                                  ***/
/***                 CRef models pointers to pointers.                ***/
/***                                                                  ***/
/************************************************************************/

BUILTIN_CLASS(CRef) {
    ADDR_FIELD("offset!", GenericDescriptor, _offset);
    BIT_FIELD("align-to!", GenericDescriptor, _align_to, BITS(uint32_t));
    BIT_FIELD("size!", GenericDescriptor, _size, BITS(uint32_t));
    OB_FIELD("mnemonic", GenericDescriptor, mnemonic);
    OB_FIELD("imported", CRef, imported);
    OB_FIELD("free-on-gc", CRef, freeStructOnGC);
    OB_FIELD("desc", CRef, _desc);
}

CRef::CRef(GenericDescriptor* elemd, pExt ext)
    : GenericDescriptor(sizeof(CRef), CLASS_META(CRef), CLASS_SBO(CRef),
                        emptyMbox, ext),
      _desc(elemd) {
    CRef::updateCnt();
}

CRef::CRef(GenericDescriptor* elemd, int s, pOb m, pOb p, pOb mbx, pExt ext)
    : GenericDescriptor(s, m, p, mbx, ext), _desc(elemd) {
    CRef::updateCnt();
}

CRef* CRef::create(GenericDescriptor* elemd) {
    PROTECT(elemd);
    StdExtension* ext = StdExtension::create(0);
    void* loc = PALLOC1(sizeof(CRef), ext);
    return new (loc) CRef(elemd, ext);
}

CRef* CRef::create() {
    GenericDescriptor* tmp = GenericDescriptor::create();
    PROTECT(tmp);
    StdExtension* ext = StdExtension::create(0);
    void* loc = PALLOC1(sizeof(CRef), ext);
    return new (loc) CRef(tmp, ext);
}

int CRef::traversePtrs(PSOb__PSOb f) {
    return GenericDescriptor::traversePtrs(f) + useIfPtr(&_desc, f);
}

int CRef::traversePtrs(SI__PSOb f) {
    return GenericDescriptor::traversePtrs(f) + useIfPtr(_desc, f);
}

void CRef::traversePtrs(V__PSOb f) {
    GenericDescriptor::traversePtrs(f);
    useIfPtr(_desc, f);
}

convertArgReturnPair CRef::convertActualArg(Ctxt* ctxt, Ob* obj) {
    PROTECT_THIS(CRef);
    PROTECT(ctxt);
    PROTECT(obj);
    cnvArgRetPair.failp = 0;

    GenericDescriptor* gobj = (GenericDescriptor*)obj;
    if (TYPEP(SELF, obj)) {
        if (gobj->_offset >= local_page_size) {
            uint32_t addr = gobj->absoluteAddress(0);
            if (addr >= local_page_size) {
                cnvArgRetPair.val = addr;
            } else {
                cnvArgRetPair.val = (uint32_t)-1;
                cnvArgRetPair.failp = 1;
            }
        } else {
            cnvArgRetPair.val = (uint32_t)-1;
            cnvArgRetPair.failp = 1;
        }
    } else if (TYPEP(SELF->_desc, obj)) {
        if (gobj->_offset >= local_page_size) {
            cnvArgRetPair.val = gobj->_offset;
        } else {
            cnvArgRetPair.val = (uint32_t)-1;
            cnvArgRetPair.failp = 1;
        }
    } else if (SELF->_desc->nullDescriptor(ctxt) == obj) {
        cnvArgRetPair.val = 0;
    } else {
        cnvArgRetPair.val = (uint32_t)-1;
        cnvArgRetPair.failp = 1;
    }

    return cnvArgRetPair;
}

Ob* CRef::convertActualRslt(Ctxt* ctxt, uint32_t obj) {
    if (obj == 0) {
        return _desc->nullDescriptor(ctxt);
    } else {
        GenericDescriptor* rslt = (GenericDescriptor*)_desc->sBox(obj);
        rslt->imported = RBLTRUE;
        if ((rslt->freeStructOnGC = freeStructOnGC) == RBLTRUE) {
            heap->registerForeignOb(rslt);
        }

        return rslt;
    }
}

Ob* CRef::sDeref(Ctxt* ctxt, uint32_t base, Tuple* path, int pindex) {
    PROTECT(ctxt);
    PROTECT(path);
    uint32_t bddr = absoluteAddress(base);
    GenericDescriptor* d =
        ((bddr == 0) ? (GenericDescriptor*)(_desc->nullDescriptor(ctxt))
                     : _desc);

    if (NULLP(path, pindex)) {
        return (d->sBox(bddr));
    }

    return (d->oprnSwitch(ctxt, bddr, path, pindex));
}

Ob* CRef::sSet(Ctxt* ctxt, uint32_t base, Ob* val, Tuple* path, int pindex) {
    if (TUPLEP(val)) {
        return sTupleSet(ctxt, base, (Tuple*)val, path, pindex);
    } else {
        uint32_t msetval;

        if (TYPEP(this, val)) {
            msetval = ((GenericDescriptor*)val)->absoluteAddress(0);
        } else if (TYPEP(_desc, val)) {
            msetval = ((GenericDescriptor*)val)->_offset;
        } else if ((val == this->nullDescriptor(ctxt)) ||
                   (val == _desc->nullDescriptor(ctxt))) {
            msetval = 0;
        } else {
            return runtimeError(ctxt, "wrong type for assignment ", val);
        }

        setAddrContents(base, msetval);

        if (NULLP(path, pindex)) {
            return NIV;
        } else {
            TUPLE_TAIL(path, pindex);
            return oprnSwitch(ctxt, base, path, pindex);
        }
    }
}

Ob* CRef::nthBase(Ctxt* ctxt, uint32_t base, int i, Tuple* path, int pindex) {
    uint32_t newbase = base + (i * _size);
    if (NULLP(path, pindex)) {
        return sBox(newbase + _offset);
    } else {
        TUPLE_TAIL(path, pindex);
        return oprnSwitch(ctxt, newbase, path, pindex);
    }
}

Ob* CRef::flatten(Ctxt* ctxt, uint32_t base, RblTable* occtxt) {
    PROTECT_THIS(CRef);
    PROTECT(ctxt);
    PROTECT(occtxt);
    Ob* chck = INVALID;
    PROTECT(chck);
    Ob* selfTag = makeOCTag(SELF, base + SELF->_offset);
    PROTECT(selfTag);

    uint32_t addr = SELF->absoluteAddress(base);

    // Note: must check for nullness first.

    if (addr == 0) {
        Ob* rslt = (GenericDescriptor*)(SELF->_desc->nullDescriptor(ctxt));
        PROTECT(rslt);
        occtxt->addKey(selfTag, rslt);
        return rslt;
    } else {
        if ((chck = occtxt->getKey(selfTag)) == ABSENT) {
            Tuple* rslt = Tuple::create(2, INVALID);
            PROTECT(rslt);
            rslt->setNth(0, SYMBOL("==>"));
            occtxt->addKey(selfTag, rslt);
            rslt->setNth(1, SELF->_desc->flatten(ctxt, addr, occtxt));
            return rslt;
        }

        return chck;
    }
}

/************************************************************************/
/***                                                                  ***/
/***                 CharRef models pointers to pointers.             ***/
/***                                                                  ***/
/************************************************************************/

BUILTIN_CLASS(CharRef) {
    ADDR_FIELD("offset!", GenericDescriptor, _offset);
    BIT_FIELD("align-to!", GenericDescriptor, _align_to, BITS(uint32_t));
    BIT_FIELD("size!", GenericDescriptor, _size, BITS(uint32_t));
    OB_FIELD("mnemonic", GenericDescriptor, mnemonic);
    OB_FIELD("imported", CharRef, imported);
    OB_FIELD("free-on-gc", CharRef, freeStructOnGC);
    OB_FIELD("desc", CharRef, _desc);
}

CharRef::CharRef(GenericDescriptor* elemd, pExt ext)
    : CRef(elemd, sizeof(CharRef), CLASS_META(CharRef), CLASS_SBO(CharRef),
           emptyMbox, ext) {
    CharRef::updateCnt();
}

CharRef::CharRef(GenericDescriptor* elemd, int s, pOb m, pOb p, pOb mbx,
                 pExt ext)
    : CRef(elemd, s, m, p, mbx, ext) {
    CharRef::updateCnt();
}

CharRef* CharRef::create(GenericDescriptor* elemd) {
    PROTECT(elemd);
    StdExtension* ext = StdExtension::create(0);
    void* loc = PALLOC1(sizeof(CharRef), ext);
    return new (loc) CharRef(elemd, ext);
}

CharRef* CharRef::create() {
    GenericDescriptor* tmp = GenericDescriptor::create();
    PROTECT(tmp);
    StdExtension* ext = StdExtension::create(0);
    void* loc = PALLOC1(sizeof(CharRef), ext);
    return new (loc) CharRef(tmp, ext);
}

Ob* CharRef::sSet(Ctxt* ctxt, uint32_t base, Ob* val, Tuple* path, int pindex) {
    PROTECT_THIS(CharRef);
    PROTECT(ctxt);
    PROTECT(val);
    PROTECT(path);
    if (STRINGP(val)) {
        RBLstring* stmp = (RBLstring*)val;
        char* saddr = new char[stmp->numberOfBytes()];

#ifdef MEMORYCAUTIOUS
        if (prev >= local_page_size) {
            (void)free((char*)prev);
        }
#endif

        (void)strcpy((char*)saddr, (GET_STRING(val)));
        SELF->setAddrContents(base, (uint32_t)saddr);
        return NIV;
    } else {
        return SELF->CRef::sSet(ctxt, base, val, path, pindex);
    }
}

convertArgReturnPair CharRef::convertActualArg(Ctxt* ctxt, Ob* arg) {
    GenericDescriptor* obj = (GenericDescriptor*)arg;
    cnvArgRetPair.failp = 0;

    if (TYPEP(this, obj)) {
        // What goes here?
        cnvArgRetPair.val = absoluteAddress(0);
    } else if (TYPEGTRP(obCharArray, obj)) {
        if (obj->_offset >= local_page_size) {
            cnvArgRetPair.val = obj->_offset;
        } else {
            cnvArgRetPair.val = (uint32_t)-1;
            cnvArgRetPair.failp = 1;
        }
    } else if (STRINGP(obj)) {
        cnvArgRetPair.val = (uint32_t)(GET_STRING(obj));
    } else if (IS_FIXNUM(obj)) {
        cnvArgRetPair.val = (uint32_t)(PRE_FIXNUM_TO_ADDR((FIXVAL(obj))));
    } else if (IS_A(obj, ByteVec)) {
        cnvArgRetPair.val = (uint32_t)((char*)&(((ByteVec*)obj)->byte(0)));
    } else if (obj->isNullP()) {
        cnvArgRetPair.val = 0;
    } else {
        cnvArgRetPair.val = (uint32_t)-1;
        cnvArgRetPair.failp = 1;
    }

    return cnvArgRetPair;
}

/************************************************************************/
/***                                                                  ***/
/***                 CRef0 models null terminated arrays              ***/
/***                                                                  ***/
/************************************************************************/

BUILTIN_CLASS(CRef0) {
    ADDR_FIELD("offset!", GenericDescriptor, _offset);
    BIT_FIELD("align-to!", GenericDescriptor, _align_to, BITS(uint32_t));
    BIT_FIELD("size!", GenericDescriptor, _size, BITS(uint32_t));
    OB_FIELD("mnemonic", GenericDescriptor, mnemonic);
    OB_FIELD("imported", CRef0, imported);
    OB_FIELD("free-on-gc", CRef0, freeStructOnGC);
    OB_FIELD("desc", CRef0, _desc);
}


CRef0::CRef0(GenericDescriptor* elemd, pExt ext)
    : CRef(elemd, sizeof(CRef0), CLASS_META(CRef0), CLASS_SBO(CRef0), emptyMbox,
           ext) {
    CRef0::updateCnt();
}

CRef0::CRef0(GenericDescriptor* elemd, int s, pOb mta, pOb prnt, pOb mbx,
             pExt ext)
    : CRef(elemd, s, mta, prnt, mbx, ext) {
    CRef0::updateCnt();
}

CRef0* CRef0::create(GenericDescriptor* elemd) {
    PROTECT(elemd);
    StdExtension* ext = StdExtension::create(0);
    void* loc = PALLOC1(sizeof(CRef0), ext);
    return new (loc) CRef0(elemd, ext);
}

CRef0* CRef0::create() {
    GenericDescriptor* tmp = GenericDescriptor::create();
    PROTECT(tmp);
    StdExtension* ext = StdExtension::create(0);
    void* loc = PALLOC1(sizeof(CRef0), ext);
    return new (loc) CRef0(tmp, ext);
}

Ob* CRef0::sGet(Ctxt* ctxt, uint32_t base, Tuple* path, int pindex) {
    if (NULLP(path, pindex)) {
        return flatten(ctxt, base, makeOccursCheckTable());
    } else {
        return runtimeError(ctxt, "cannot access ", path);
    }
}

Ob* CRef0::flatten(Ctxt* ctxt, uint32_t base, RblTable* occtxt) {
    PROTECT_THIS(CRef0);
    PROTECT(ctxt);
    PROTECT(occtxt);
    Ob* chck = INVALID;
    PROTECT(chck);
    Ob* selfTag = makeOCTag(SELF, base + SELF->_offset);
    // Ob* selfTag = makeOCTag( SELF, base );
    PROTECT(selfTag);
    if ((chck = occtxt->getKey(selfTag)) == ABSENT) {
        uint32_t addr = SELF->absoluteAddress(base);
        if (addr == 0) {
            Ob* rslt = SELF->_desc->nullDescriptor(ctxt);
            PROTECT(rslt);
            occtxt->addKey(selfTag, rslt);
        } else {
            uint32_t skip = sizeof(SELF->_desc);
            Tuple* rslt = NIL;
            PROTECT(rslt);
            occtxt->addKey(selfTag, rslt);
            GenericDescriptor* itr = SELF->_desc;
            Ob* tmp = itr->flatten(ctxt, addr, occtxt);
            uint32_t iddr = addr + skip;
            PROTECT(tmp);
            for (; !(ISNULLP(tmp)); iddr = iddr + skip) {
                rslt = ::rcons(rslt, tmp);
                occtxt->addKey(selfTag, rslt);
                tmp = itr->flatten(ctxt, iddr, occtxt);
            }

            return rslt;
        }
    }

    return chck;
}


/************************************************************************/
/***                                                                  ***/
/***               Char handles basic machine entities.               ***/
/***                                                                  ***/
/************************************************************************/

BUILTIN_CLASS(CharRef0) {
    ADDR_FIELD("offset!", GenericDescriptor, _offset);
    BIT_FIELD("align-to!", GenericDescriptor, _align_to, BITS(uint32_t));
    BIT_FIELD("size!", GenericDescriptor, _size, BITS(uint32_t));
    OB_FIELD("mnemonic", GenericDescriptor, mnemonic);
    OB_FIELD("imported", CharRef0, imported);
    OB_FIELD("free-on-gc", CharRef0, freeStructOnGC);
    OB_FIELD("desc", CRef0, _desc);
}

CharRef0::CharRef0(pExt ext)
    : CRef0((GenericDescriptor*)RBLFALSE, sizeof(CharRef0),
            CLASS_META(CharRef0), CLASS_SBO(CharRef0), emptyMbox, ext) {
    CharRef0::updateCnt();
}

CharRef0* CharRef0::create() {
    StdExtension* ext = StdExtension::create(0);
    void* loc = PALLOC1(sizeof(CharRef0), ext);
    return new (loc) CharRef0(ext);
}

Ob* CharRef0::flatten(Ctxt* ctxt, uint32_t base, RblTable* occtxt) {
    PROTECT_THIS(CharRef0);
    PROTECT(ctxt);
    PROTECT(occtxt);

    uint32_t addr = SELF->absoluteAddress(base);
    if (addr == 0) {
        return (obChar->nullDescriptor(ctxt));
    } else {
        if (addr >= local_page_size) {
            return (RBLstring::create((char*)addr));
        } else {
            return runtimeError(ctxt, "invalid address");
        }
    }
}

Ob* CharRef0::sSet(Ctxt* ctxt, uint32_t base, Ob* val, Tuple* path,
                   int pindex) {
    if (STRINGP(val)) {
        int valsize = strlen(GET_STRING(val));
        char* tmp = new char[valsize];

#ifdef MEMORYCAUTIOUS
        (void)free((char*)addr);
#endif

        (void)strcpy(tmp, GET_STRING(val));

        setAddrContents(base, (uint32_t)tmp);

        return NIV;
    } else {
        return CRef0::sSet(ctxt, base, val, path, pindex);
    }
}

convertArgReturnPair CharRef0::convertActualArg(Ctxt* ctxt, Ob* arg) {
    GenericDescriptor* obj = (GenericDescriptor*)arg;
    cnvArgRetPair.failp = 0;

    if (STRINGP(obj)) {
        cnvArgRetPair.val = (uint32_t)(GET_STRING(obj));
    } else {
        return CRef::convertActualArg(ctxt, arg);
    }

    return cnvArgRetPair;
}

/************************************************************************/
/***                                                                  ***/
/***                  CUnions - can you say modeling?                 ***/
/***                                                                  ***/
/************************************************************************/

BUILTIN_CLASS(CUnion) {
    ADDR_FIELD("offset!", GenericDescriptor, _offset);
    BIT_FIELD("align-to!", GenericDescriptor, _align_to, BITS(uint32_t));
    BIT_FIELD("size!", GenericDescriptor, _size, BITS(uint32_t));
    OB_FIELD("mnemonic", GenericDescriptor, mnemonic);
    OB_FIELD("imported", CUnion, imported);
    OB_FIELD("free-on-gc", CUnion, freeStructOnGC);
    OB_FIELD("descs", CUnion, _descs);
    OB_FIELD("fieldNames", CUnion, _fieldNames);
}

CUnion::CUnion(RblTable* desks, Tuple* fnames, pExt ext)
    : GenericDescriptor(sizeof(CUnion), CLASS_META(CUnion), CLASS_SBO(CUnion),
                        emptyMbox, ext),
      _descs(desks),
      _fieldNames(fnames) {
    CUnion::updateCnt();
}

CUnion* CUnion::create(RblTable* tbl, Tuple* tup) {
    PROTECT(tbl);
    PROTECT(tup);
    StdExtension* ext = StdExtension::create(0);
    void* loc = PALLOC1(sizeof(CUnion), ext);
    return new (loc) CUnion(tbl, tup, ext);
}

CUnion* CUnion::create() {
    RblTable* tmp = RblTable::create();
    PROTECT(tmp);
    StdExtension* ext = StdExtension::create(0);
    void* loc = PALLOC1(sizeof(CUnion), ext);
    return new (loc) CUnion(tmp, NIL, ext);
}

int CUnion::traversePtrs(PSOb__PSOb f) {
    int sum = GenericDescriptor::traversePtrs(f);
    sum += useIfPtr(&_descs, f);
    sum += useIfPtr(&_fieldNames, f);
    return sum;
}

int CUnion::traversePtrs(SI__PSOb f) {
    int sum = GenericDescriptor::traversePtrs(f);
    sum += useIfPtr(_descs, f);
    sum += useIfPtr(_fieldNames, f);
    return sum;
}

void CUnion::traversePtrs(V__PSOb f) {
    GenericDescriptor::traversePtrs(f);
    useIfPtr(_descs, f);
    useIfPtr(_fieldNames, f);
}

Ob* CUnion::select(Ctxt* ctxt, uint32_t base, Tuple* path, int pindex) {
    // Needs protection code.
    PROTECT_THIS(CUnion);
    PROTECT(ctxt);
    PROTECT(path);

    if (NULLP(path, pindex)) {
        return runtimeError(ctxt, "Invalid path ", path);
    } else {
        Ob* symb = TUPLE_HEAD(path, pindex);
        PROTECT(symb);
        TUPLE_TAIL(path, pindex);
        GenericDescriptor* d = (GenericDescriptor*)(SELF->_descs->getKey(symb));
        PROTECT(d);
        if (d == ABSENT) {
            return runtimeError(ctxt, "Bad selector ", symb);
        } else if (NULLP(path, pindex)) {
            return SELF->sBox(base + SELF->_offset + d->_offset);
        } else {
            return d->oprnSwitch(ctxt, (base + SELF->_offset), path, pindex);
        }
    }
}

Ob* CUnion::flatten(Ctxt* ctxt, uint32_t base, RblTable* occtxt) {
    PROTECT_THIS(CUnion);
    PROTECT(ctxt);
    PROTECT(occtxt);
    Ob* chck = INVALID;
    PROTECT(chck);
    Ob* selfTag = makeOCTag(SELF, base + SELF->_offset);

    if ((chck = occtxt->getKey(selfTag)) == ABSENT) {
        int fnlen = SELF->_fieldNames->numberOfElements();
        Tuple* rslt = Tuple::create(2 * fnlen, INVALID);
        PROTECT(rslt);

        Ob* symb = INVALID;
        Ob* tmp = INVALID;
        GenericDescriptor* crc = (GenericDescriptor*)INVALID;
        PROTECT(symb);
        PROTECT(tmp);
        PROTECT(crc);

        occtxt->addKey(selfTag, rslt);
        for (int i = 0; i < fnlen; i++) {
            symb = SELF->_fieldNames->nth(i);

            crc = ((GenericDescriptor*)(SELF->_descs->getKey(symb)));

            tmp = crc->flatten(ctxt, base + SELF->_offset, occtxt);

            rslt->setNth(2 * i, symb);
            rslt->setNth(2 * i + 1, tmp);
        }

        return rslt;
    } else {
        return chck;
    }
}

MODULE_INIT(Cstruct) {
    obGenericDescriptor =
        (GenericDescriptor*)(heap->tenure(GenericDescriptor::create()));
    Define("GenericDescriptor", obGenericDescriptor);

    obNullDescriptor =
        (NullDescriptor*)(heap->tenure(NullDescriptor::create()));
    Define("NullDescriptor", obNullDescriptor);

    obAtomicDescriptor =
        (AtomicDescriptor*)(heap->tenure(AtomicDescriptor::create()));
    CLASS_SBO(AtomicDescriptor)->parent() = CLASS_SBO(GenericDescriptor);
    Define("AtomicDescriptor", obAtomicDescriptor);

    obChar = (AtomicDescriptor*)(heap->tenure(AtomicDescriptor::create()));
    Define("Char", obChar);

    obCStructure = (CStructure*)(heap->tenure(CStructure::create()));
    CLASS_SBO(CStructure)->parent() = CLASS_SBO(GenericDescriptor);
    Define("CStructure", obCStructure);

    obCArray = (CArray*)(heap->tenure(CArray::create()));
    CLASS_SBO(CArray)->parent() = CLASS_SBO(GenericDescriptor);
    Define("CArray", obCArray);

    obCharArray = (CharArray*)(heap->tenure(CharArray::create()));
    CLASS_SBO(CharArray)->parent() = CLASS_SBO(CArray);
    Define("CharArray", obCharArray);

    obCharArray0 = (CharArray0*)(heap->tenure(CharArray0::create()));
    CLASS_SBO(CharArray0)->parent() = CLASS_SBO(CharArray);
    Define("CharArray0", obCharArray0);

    obCRef = (CRef*)(heap->tenure(CRef::create()));
    CLASS_SBO(CRef)->parent() = CLASS_SBO(GenericDescriptor);
    Define("CRef", obCRef);

    obCharRef = (CharRef*)(heap->tenure(CharRef::create()));
    CLASS_SBO(CharRef)->parent() = CLASS_SBO(CRef);
    Define("CharRef", obCharRef);

    obCRef0 = (CRef0*)(heap->tenure(CRef0::create()));
    CLASS_SBO(CRef0)->parent() = CLASS_SBO(CRef);
    Define("CRef0", obCRef0);

    obCharRef0 = (CharRef0*)(heap->tenure(CharRef0::create()));
    CLASS_SBO(CharRef0)->parent() = CLASS_SBO(CRef0);
    Define("CharRef0", obCharRef0);

    obCUnion = (CUnion*)(heap->tenure(CUnion::create()));
    CLASS_SBO(CUnion)->parent() = CLASS_SBO(GenericDescriptor);
    Define("CUnion", obCUnion);

    Define("C++-Cstructs", RBLTRUE);
}
