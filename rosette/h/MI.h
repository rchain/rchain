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

#if !defined(_RBL_MI_h)
#define _RBL_MI_h

#include "rosette.h"
#include "Ob.h"
#include "Tuple.h"

static const int MI_CPL_SLOT = 0;
static const int BUILTIN_MI_SLOTS = 1;

class MIActor : public Actor {
    STD_DECLS(MIActor);

   protected:
    MIActor(pExt);

   public:
    static MIActor* create(Tuple*);
    Tuple* classPrecedenceList() {
        return (Tuple*)extension->slot(MI_CPL_SLOT);
    }

    virtual Ob* lookup(Ob*, Ctxt*);
    virtual bool hasParentp(pOb);
    virtual pOb typeLub(pOb);
};


static const int PROD_TYPE_TEMPLATE_SLOT = 0;
static const int PROD_REST_TYPE_SLOT = 1;
static const int BUILTIN_ProductType_SLOTS = 2;

class ProductType : public Actor {
    STD_DECLS(ProductType);

   protected:
    ProductType(pExt);

   public:
    static ProductType* create(Tuple*, pOb);
    virtual bool isCoveredByp(pOb);
    virtual pOb typeLub(pOb);

    pOb star() { return extension->slot(PROD_REST_TYPE_SLOT); }

    Tuple* definite() {
        return (Tuple*)(extension->slot(PROD_TYPE_TEMPLATE_SLOT));
    }

    virtual bool typeMatchesp(pOb);

    int numberOfElements() { return definite()->numberOfElements(); }
    pOb elem(int i) { return definite()->elem(i); }

    pOb elemR(int i) {
        if (i < definite()->numberOfElements()) {
            return elem(i);
        } else {
            return star();
        }
    }
};


static const int SUM_TYPE_TYPES_SLOT = 0;
static const int BUILTIN_SumType_SLOTS = 1;


class SumType : public Actor {
    STD_DECLS(SumType);

   protected:
    SumType(pExt);

   public:
    static SumType* create(Tuple*);
    virtual bool compositeCoversp(pOb);
    virtual bool isCoveredByp(pOb);
    pOb dominator();
    virtual pOb typeLub(pOb);
    pOb normalize();

    Tuple* types() { return (Tuple*)extension->slot(SUM_TYPE_TYPES_SLOT); }

    pOb elem(int i) { return types()->elem(i); }
    int numberOfElements() { return types()->numberOfElements(); }
};


static const int MM_PROC_LIST_SLOT = 0;
static const int BUILTIN_MultiMethod_SLOTS = 1;

class MultiMethod : public Actor {
    STD_DECLS(MultiMethod);

   protected:
    MultiMethod(pExt);

   public:
    static MultiMethod* create();
    pOb matchAndDispatch(pCtxt);

    Tuple* procList() { return (Tuple*)extension->slot(MM_PROC_LIST_SLOT); }

    pOb elem(int i) { return procList()->elem(i); }

    int numberOfElements() { return procList()->numberOfElements(); }
};

bool typeLessEq(pOb x, pOb y);
bool typeGreaterEq(pOb x, pOb y);
bool typeEq(pOb x, pOb y);
bool typeLess(pOb x, pOb y);
bool typeGreater(pOb x, pOb y);

#endif
