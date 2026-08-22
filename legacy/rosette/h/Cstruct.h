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

#if !defined(_RBL_Cstruct_h)
#define _RBL_Cstruct_h 1

#include "rosette.h"

#include "Ob.h"

class GenericDescriptor : public Actor {
    STD_DECLS(GenericDescriptor);

   protected:
    GenericDescriptor(pExt);
    GenericDescriptor(int, pOb, pOb, pOb, pExt);

    virtual int traversePtrs(PSOb__PSOb);
    virtual int traversePtrs(SI__PSOb);
    virtual void traversePtrs(V__PSOb);

   public:
    /* This seems to be the base-level protocol provided by descriptors.
     *
     * (method (S-get base path) ...)
     * (method (S-desc base path) ...)
     * (method (S-deref base path) ...)
     * (method (select base r) ...)
     * (method (S-set base [val & r]) ...)
     * (method (S-tupleSet base [val & r]) ...)
     * (method (nth base [i & path]) ...)
     *
     */

    uint32_t _offset, _align_to, _size; /* memory map */
    Ob* mnemonic;                       /* was consed up from rosette heap */
    Ob* imported;
    /* was returned by a foreign function or is a */
    /* substructure of a critter returned by a ff */

    Ob* freeStructOnGC;

    static GenericDescriptor* create();

    virtual ~GenericDescriptor();

    virtual Ob* sGet(Ctxt* ctxt, uint32_t base, Tuple* path, int pindex = 0);
    virtual Ob* sDesc(Ctxt* ctxt, uint32_t base, Tuple* path, int pindex = 0);
    virtual Ob* sDeref(Ctxt* ctxt, uint32_t base, Tuple* path, int pindex = 0);
    virtual Ob* select(Ctxt* ctxt, uint32_t base, Tuple* path, int pindex = 0);
    virtual Ob* sSet(Ctxt* ctxt, uint32_t base, Ob* val, Tuple* path,
                     int pindex = 0);
    virtual Ob* sTupleSet(Ctxt* ctxt, uint32_t base, Tuple* val, Tuple* path,
                          int pindex = 0);
    virtual Ob* nthBase(Ctxt* ctxt, uint32_t base, int i, Tuple* path,
                        int pindex = 0);
    virtual Ob* flatten(Ctxt* ctxt, uint32_t base, RblTable*);

    virtual convertArgReturnPair convertActualArg(Ctxt*, Ob*);
    virtual Ob* convertActualRslt(Ctxt*, uint32_t);

    Ob* nullDescriptor(Ctxt*);

    virtual Ob* oprnSwitch(Ctxt* ctxt, uint32_t base, Tuple* path,
                           int pindex = 0);

    Ob* sBox(uint32_t off) {
        GenericDescriptor* rslt = (GenericDescriptor*)cloneTo(meta(), parent());
        rslt->mbox = emptyMbox;
        rslt->_offset = off;
        rslt->imported = imported;
        rslt->freeStructOnGC = freeStructOnGC;

        /* should rslt be registered as a foreign ob? */
        return (rslt);
    }

    virtual uint32_t absoluteAddress(uint32_t base);
    void setAddrContents(uint32_t base, uint32_t val);
};


class NullDescriptor : public GenericDescriptor {
    STD_DECLS(NullDescriptor);

   protected:
    NullDescriptor(pExt);

   public:
    static NullDescriptor* create();

    virtual Ob* sGet(Ctxt* ctxt, uint32_t base, Tuple* path, int pindex = 0);
    virtual Ob* sDesc(Ctxt* ctxt, uint32_t base, Tuple* path, int pindex = 0);
    virtual Ob* sDeref(Ctxt* ctxt, uint32_t base, Tuple* path, int pindex = 0);
    virtual Ob* select(Ctxt* ctxt, uint32_t base, Tuple* path, int pindex = 0);
    virtual Ob* sSet(Ctxt* ctxt, uint32_t base, Ob* val, Tuple* path,
                     int pindex = 0);
    virtual Ob* nthBase(Ctxt* ctxt, uint32_t base, int i, Tuple* path,
                        int pindex = 0);
    virtual Ob* flatten(Ctxt* ctxt, uint32_t base, RblTable*);

    virtual Ob* isNullP();

    virtual uint32_t absoluteAddress(uint32_t base);
};


class AtomicDescriptor : public GenericDescriptor {
    STD_DECLS(AtomicDescriptor);

   protected:
    AtomicDescriptor(RblBool*, pExt);
    AtomicDescriptor(RblBool*, int, pOb, pOb, pOb, pExt);

    virtual int traversePtrs(PSOb__PSOb);
    virtual int traversePtrs(SI__PSOb);
    virtual void traversePtrs(V__PSOb);

   public:
    RblBool* _signed;

    static AtomicDescriptor* create(RblBool*);
    static AtomicDescriptor* create();

    virtual Ob* sGet(Ctxt* ctxt, uint32_t base, Tuple* path, int pindex = 0);
    virtual Ob* sSet(Ctxt* ctxt, uint32_t base, Ob* val, Tuple* path,
                     int pindex = 0);
    virtual Ob* flatten(Ctxt* ctxt, uint32_t base, RblTable*);

    virtual convertArgReturnPair convertActualArg(Ctxt*, Ob*);
    virtual Ob* convertActualRslt(Ctxt*, uint32_t);
    virtual uint32_t absoluteAddress(uint32_t base);
};


class CStructure : public GenericDescriptor {
    STD_DECLS(CStructure);

   protected:
    CStructure(RblTable*, Tuple*, pExt);

    virtual int traversePtrs(PSOb__PSOb);
    virtual int traversePtrs(SI__PSOb);
    virtual void traversePtrs(V__PSOb);

   public:
    RblTable* _descs;
    Tuple* _fieldNames;

    static CStructure* create(RblTable*, Tuple*);
    static CStructure* create();

    virtual Ob* select(Ctxt* ctxt, uint32_t base, Tuple* path, int pindex = 0);
    virtual Ob* sTupleSet(Ctxt* ctxt, uint32_t base, Tuple* val, Tuple* path,
                          int pindex = 0);
    virtual Ob* flatten(Ctxt* ctxt, uint32_t base, RblTable*);
};


class CArray : public GenericDescriptor {
    STD_DECLS(CArray);

   protected:
    CArray(uint16_t, GenericDescriptor*, pExt);
    CArray(int s, pOb m, pOb p, pOb mbx, pExt, uint16_t, GenericDescriptor*);

    virtual int traversePtrs(PSOb__PSOb);
    virtual int traversePtrs(SI__PSOb);
    virtual void traversePtrs(V__PSOb);

   public:
    uint16_t _numElems;
    uint16_t filler_up_please;
    GenericDescriptor* _elemDesc;

    static CArray* create(uint16_t, GenericDescriptor*);
    static CArray* create();

    virtual Ob* sTupleSet(Ctxt* ctxt, uint32_t base, Tuple* val, Tuple* path,
                          int pindex = 0);
    virtual Ob* nthBase(Ctxt* ctxt, uint32_t base, int i, Tuple* path,
                        int pindex = 0);
    virtual Ob* flatten(Ctxt* ctxt, uint32_t base, RblTable*);
};


class CharArray : public CArray {
    STD_DECLS(CharArray);

   protected:
    CharArray(uint16_t, GenericDescriptor*, pExt);
    CharArray(int s, pOb m, pOb p, pOb mbx, pExt, uint16_t, GenericDescriptor*);

   public:
    static CharArray* create(uint16_t, GenericDescriptor*);
    static CharArray* create();

    virtual Ob* sSet(Ctxt* ctxt, uint32_t base, Ob* val, Tuple* path,
                     int pindex = 0);
    virtual Ob* flatten(Ctxt* ctxt, uint32_t base, RblTable*);
};


class CharArray0 : public CharArray {
    STD_DECLS(CharArray0);

   protected:
    CharArray0(uint16_t, GenericDescriptor*, pExt);

   public:
    static CharArray0* create(uint16_t, GenericDescriptor*);
    static CharArray0* create();

    virtual Ob* flatten(Ctxt* ctxt, uint32_t base, RblTable*);
};


class CRef : public GenericDescriptor {
    STD_DECLS(CRef);

   protected:
    CRef(GenericDescriptor*, pExt);
    CRef(GenericDescriptor*, int, pOb, pOb, pOb, pExt);

    virtual int traversePtrs(PSOb__PSOb);
    virtual int traversePtrs(SI__PSOb);
    virtual void traversePtrs(V__PSOb);

   public:
    GenericDescriptor* _desc;

    static CRef* create(GenericDescriptor*);
    static CRef* create();

    virtual Ob* sDeref(Ctxt* ctxt, uint32_t base, Tuple* path, int pindex = 0);
    virtual Ob* sSet(Ctxt* ctxt, uint32_t base, Ob* val, Tuple* path,
                     int pindex = 0);
    virtual Ob* nthBase(Ctxt* ctxt, uint32_t base, int i, Tuple* path,
                        int pindex = 0);
    virtual Ob* flatten(Ctxt* ctxt, uint32_t base, RblTable*);

    virtual convertArgReturnPair convertActualArg(Ctxt*, Ob*);
    virtual Ob* convertActualRslt(Ctxt*, uint32_t);
};


class CharRef : public CRef {
    STD_DECLS(CharRef);

   protected:
    CharRef(GenericDescriptor*, pExt);
    CharRef(GenericDescriptor*, int, pOb, pOb, pOb, pExt);

   public:
    static CharRef* create(GenericDescriptor*);
    static CharRef* create();

    virtual Ob* sSet(Ctxt* ctxt, uint32_t base, Ob* val, Tuple* path,
                     int pindex = 0);

    virtual convertArgReturnPair convertActualArg(Ctxt*, Ob*);
};

class CRef0 : public CRef {
    STD_DECLS(CRef0);

   protected:
    CRef0(GenericDescriptor*, pExt);
    CRef0(GenericDescriptor*, int, pOb, pOb, pOb, pExt);

   public:
    static CRef0* create();
    static CRef0* create(GenericDescriptor*);

    virtual Ob* sGet(Ctxt* ctxt, uint32_t base, Tuple* path, int pindex = 0);
    virtual Ob* flatten(Ctxt* ctxt, uint32_t base, RblTable*);
};


class CharRef0 : public CRef0 {
    STD_DECLS(CharRef0);

   protected:
    CharRef0(pExt);

   public:
    static CharRef0* create();

    virtual Ob* flatten(Ctxt*, uint32_t, RblTable*);
    virtual Ob* sSet(Ctxt* ctxt, uint32_t base, Ob* val, Tuple* path,
                     int pindex = 0);
    virtual convertArgReturnPair convertActualArg(Ctxt*, Ob*);
};


class CUnion : public GenericDescriptor {
    STD_DECLS(CUnion);

   protected:
    CUnion(RblTable*, Tuple*, pExt);

    virtual int traversePtrs(PSOb__PSOb);
    virtual int traversePtrs(SI__PSOb);
    virtual void traversePtrs(V__PSOb);

   public:
    RblTable* _descs;
    Tuple* _fieldNames;

    static CUnion* create(RblTable*, Tuple*);
    static CUnion* create();

    virtual Ob* select(Ctxt* ctxt, uint32_t base, Tuple* path, int pindex = 0);
    virtual Ob* flatten(Ctxt* ctxt, uint32_t base, RblTable*);
};

#endif
