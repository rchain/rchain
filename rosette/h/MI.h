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
 *
 @EC */

#if !defined(_RBL_MI_h)
#define _RBL_MI_h

#ifdef __GNUG__
#pragma interface
#endif

#include "rosette.h"

#include "Ob.h"
#include "Tuple.h"


class MIActor : public Actor {
    STD_DECLS(MIActor);

   protected:
    MIActor(pExt);

   public:
    static MIActor* create(Tuple*);
    Tuple* classPrecedenceList();
    virtual Ob* lookup(Ob*, Ctxt*);
    virtual bool hasParentp(pOb);
    virtual pOb typeLub(pOb);
};


static const int MI_CPL_SLOT = 0;

static const int BUILTIN_MI_SLOTS = 1;

inline Tuple* MIActor::classPrecedenceList() {
    return (Tuple*)extension->slot(MI_CPL_SLOT);
}


class ProductType : public Actor {
    STD_DECLS(ProductType);

   protected:
    ProductType(pExt);

   public:
    static ProductType* create(Tuple*, pOb);
    virtual bool isCoveredByp(pOb);
    virtual pOb typeLub(pOb);
    pOb star();
    Tuple* definite();
    int numberOfElements();
    pOb elem(int);
    pOb elemR(int);
    virtual bool typeMatchesp(pOb);
};


static const int PROD_TYPE_TEMPLATE_SLOT = 0;
static const int PROD_REST_TYPE_SLOT = 1;

static const int BUILTIN_ProductType_SLOTS = 2;

inline pOb ProductType::star() { return extension->slot(PROD_REST_TYPE_SLOT); }

inline Tuple* ProductType::definite() {
    return (Tuple*)(extension->slot(PROD_TYPE_TEMPLATE_SLOT));
}

inline int ProductType::numberOfElements() {
    return definite()->numberOfElements();
}

inline pOb ProductType::elem(int i) { return definite()->elem(i); }

inline pOb ProductType::elemR(int i) {
    if (i < definite()->numberOfElements())
        return elem(i);
    else
        return star();
}


class SumType : public Actor {
    STD_DECLS(SumType);

   protected:
    SumType(pExt);

   public:
    static SumType* create(Tuple*);
    Tuple* types();
    pOb elem(int);
    int numberOfElements();
    virtual bool compositeCoversp(pOb);
    virtual bool isCoveredByp(pOb);
    pOb dominator();
    virtual pOb typeLub(pOb);
    pOb normalize();
};


static const int SUM_TYPE_TYPES_SLOT = 0;

static const int BUILTIN_SumType_SLOTS = 1;

inline Tuple* SumType::types() {
    return (Tuple*)extension->slot(SUM_TYPE_TYPES_SLOT);
}

inline pOb SumType::elem(int i) { return types()->elem(i); }

inline int SumType::numberOfElements() { return types()->numberOfElements(); }


class MultiMethod : public Actor {
    STD_DECLS(MultiMethod);

   protected:
    MultiMethod(pExt);

   public:
    static MultiMethod* create();
    Tuple* procList();
    pOb elem(int);
    int numberOfElements();
    pOb matchAndDispatch(pCtxt);
};


static const int MM_PROC_LIST_SLOT = 0;

static const int BUILTIN_MultiMethod_SLOTS = 1;

inline Tuple* MultiMethod::procList() {
    return (Tuple*)extension->slot(MM_PROC_LIST_SLOT);
}

inline pOb MultiMethod::elem(int i) { return procList()->elem(i); }

inline int MultiMethod::numberOfElements() {
    return procList()->numberOfElements();
}

inline bool typeLessEq(pOb x, pOb y) { return BASE(y)->coversp(x); }

inline bool typeGreaterEq(pOb x, pOb y) { return BASE(x)->coversp(y); }

inline bool typeEq(pOb x, pOb y) {
    return (BASE(x)->coversp(y) && BASE(y)->coversp(x));
}

inline bool typeLess(pOb x, pOb y) {
    return (typeLessEq(x, y) && !typeEq(x, y));
}

inline bool typeGreater(pOb x, pOb y) {
    return (typeGreaterEq(x, y) && !typeEq(x, y));
}

#endif
