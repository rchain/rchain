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

#include "MI.h"

#include "Interrupt.h"
#include "Meta.h"
#include "Prim.h"
#include "Proc.h"
#include "RblAtom.h"
#include "Tuple.h"

#include "BuiltinClass.h"
#include "ModuleInit.h"

#define min(x, y) ((x) <= (y) ? (x) : (y))
#define max(x, y) ((x) <= (y) ? (y) : (x))

extern TblObject* TopSBO;

/* Type system methods on Ob */
pOb Ob::typep(pOb x) {
    return RBLBOOL((this->meta() == BASE(x)->meta()) &&
                   (this->parent() == BASE(x)->parent()));
}

bool Ob::hasParentp(pOb X) {
    if ((X == TopSBO) || (BASE(X) == this)) {
        return true;
    } else if (this == TopSBO) {
        return false;
    }

    return BASE(parent())->hasParentp(X);
}

bool Ob::compositeCoversp(pOb y) {
    if (this->typep(y) == RBLTRUE) {
        return true;
    }

    return BASE(y)->hasParentp(this);
}

bool Ob::isCoveredByp(pOb x) { return BASE(x)->compositeCoversp(this->self()); }

bool Ob::coversp(pOb y) {
    if ((this == BASE(y)) || (this == TopSBO)) {
        return true;
    } else if (y == TopSBO) {
        return false;
    }

    return BASE(y)->isCoveredByp(this);
}

bool RblAtom::coversp(pOb y) { return ((atom == y) && (y != NIV)); }

pOb Ob::typeLub(pOb x) {
    if (this->coversp(x)) {
        return this->self();
    } else {
        pOb y = this->self();
        if (BASE(x)->coversp(y)) {
            return x;
        } else {
            return (BASE(this->parent()))->typeLub(x);
        }
    }
}

bool Ob::typeMatchesp(pOb x) { return this->coversp(x); }

/* Type system methods on Tuple */

/*
 typeMatcher is intended to be used to determine if the actuals (this)
 are an instance of some ProductType at invoke time in a multimethod
 */
bool Tuple::typeMatcher(Tuple* type_template, pOb rest_type) {
    Tuple* typT = type_template;
    int N = numberOfElements();
    int M = typT->numberOfElements();

    if (N < M) {
        return false;
    }

    int i = 0;

    for (; i < min(N, M); i++) {
        if (!(BASE(typT->elem(i)))->typeMatchesp(elem(i))) {
            return false;
        }
    }

    return elemsCoveredByp(rest_type, i);
}

bool Tuple::elemsCoveredByp(pOb typ, int i) {
    int N = numberOfElements();

    for (; i < N; i++) {
        if (!BASE(typ)->coversp(elem(i))) {
            return false;
        }
    }

    return true;
}

pOb tupleLub(Tuple* obs) {
    if (obs == NIL)
        return NIV;

    pOb u = obs->elem(0);
    PROTECT(obs);

    for (int i = 1; i < obs->numberOfElements(); i++) {
        u = BASE(u)->typeLub(obs->elem(i));
    }

    return u;
}

/* methods on MIActors */

BUILTIN_CLASS(MIActor) { OB_FIELD_INDIRECT("cpl", MI_CPL_SLOT); }


MIActor::MIActor(pExt ext)
    : Actor(sizeof(MIActor), CLASS_META(MIActor), CLASS_SBO(MIActor),
            lockedMbox, ext) {
    MIActor::updateCnt();
}


MIActor* MIActor::create(Tuple* class_precedence_list) {
    PROTECT(class_precedence_list);
    StdExtension* ext = StdExtension::create(BUILTIN_MI_SLOTS);
    ext->slot(MI_CPL_SLOT) = class_precedence_list;
    void* loc = PALLOC1(sizeof(MIActor), ext);
    return new (loc) MIActor(ext);
}


Ob* MIActor::lookup(Ob* key, Ctxt* ctxt) {
    if (interruptPending)
        return ABSENT;

    Tuple* cpl = classPrecedenceList();

    if (!IS_A(cpl, Tuple)) {
        return BASE(cpl)->lookup(key, ctxt);
    }

    int N = cpl->numberOfElements();

    if (N == 0) {
        return ABSENT;
    }

    for (int i = 0; i < N - 1; i++) {
        Ob* effective_base = BASE(cpl->elem(i));
        Ob* result =
            BASE(effective_base->meta())->get(effective_base, key, ctxt);
        if (result != ABSENT) {
            return result;
        }
    }

    return BASE(cpl->elem(N - 1))->lookup(key, ctxt);
}

bool MIActor::hasParentp(pOb typ) {
    if ((this == typ) || (typ == TopSBO)) {
        return true;
    }

    if (interruptPending) {
        return false;
    }

    Tuple* cpl = classPrecedenceList();

    if (!IS_A(cpl, Tuple)) {
        return BASE(typ)->coversp(cpl); /* assume: cpl is a type */
    }

    int N = cpl->numberOfElements();

    if (N == 0) {
        return false;
    }

    for (int i = 0; i < N; i++) {
        if (BASE(cpl->elem(i))->hasParentp(typ)) {
            return true;
        }
    }

    return false;
}

pOb MIActor::typeLub(pOb x) {
    if (this->coversp(x)) {
        return this;
    } else if (BASE(x)->coversp(this)) {
        return x;
    } else {
        Tuple* cpl = classPrecedenceList();
        if (!IS_A(cpl, Tuple)) {
            return BASE(x)->typeLub(cpl);
        } else {
            int N = cpl->numberOfElements();
            if (N == 0) {
                return NIV; /* there is no LUB! */
            } else {
                PROTECT(cpl);
                PROTECT(x);
                pOb u = BASE(x)->typeLub(cpl->elem(0));
                PROTECT(u);
                for (int i = 1; i < N; i++) {
                    pOb v = BASE(x)->typeLub(cpl->elem(i));

                    /* does coversp ever do a typeLub ?? */
                    if (BASE(u)->coversp(v)) {
                        u = v;
                    } else if (BASE(v)->coversp(u)) {
                        continue;
                    } else {
                        /* disambiguates u and v non comparable */
                        u = BASE(u)->typeLub(v);
                    }
                }
                return u;
            }
        }
    }
}

/* ProductType methods */

BUILTIN_CLASS(ProductType) {
    OB_FIELD_INDIRECT("star", PROD_REST_TYPE_SLOT);
    OB_FIELD_INDIRECT("definite", PROD_TYPE_TEMPLATE_SLOT);
}

ProductType::ProductType(pExt ext)
    : Actor(sizeof(ProductType), CLASS_META(ProductType),
            CLASS_SBO(ProductType), lockedMbox, ext) {
    ProductType::updateCnt();
}


ProductType* ProductType::create(Tuple* type_template, pOb rest_type) {
    PROTECT(type_template);
    PROTECT(rest_type);
    StdExtension* ext = StdExtension::create(BUILTIN_ProductType_SLOTS);
    ext->slot(PROD_TYPE_TEMPLATE_SLOT) = type_template;
    ext->slot(PROD_REST_TYPE_SLOT) = rest_type;
    void* loc = PALLOC1(sizeof(ProductType), ext);
    return new (loc) ProductType(ext);
}

bool ProductType::typeMatchesp(pOb actuals) {
    if (IS_A(actuals, Tuple)) {
        return ((Tuple*)actuals)->typeMatcher(definite(), star());
    } else {
        return false;
    }
}

bool ProductType::isCoveredByp(pOb x) {
    /* the idiom !typeLessEq is NOT EQUIV to typeGreater since the latter
     * will report false when the types are incomparable
     */
    if (IS_A(x, ProductType)) {
        ProductType* w = (ProductType*)x;
        if ((this->star() != w->star()) &&
            (!typeLessEq(this->star(), w->star()))) {
            return false;
        }

        int N = this->numberOfElements();
        int M = w->numberOfElements();

        if (N < M) {
            return false;
        }

        for (int i = 0; i < N; i++) {
            if (!typeLessEq(this->elem(i), w->elemR(i))) {
                return false;
            }
        }

        return true;
    } else {
        return BASE(x)->compositeCoversp(this);
    }
}

pOb ProductType::typeLub(pOb x) {
    if (!IS_A(x, ProductType)) {
        return TopSBO; /* except when x is a SumType !! */
    } else {
        ProductType* v = this;
        ProductType* w = (ProductType*)x;
        int N = v->numberOfElements();
        int M = w->numberOfElements();
        pOb newRest = BASE(v->star())->typeLub(w->star());

        if (M < N) { /* maybe swap v and w */
            ProductType* tmp = v;
            v = w;
            w = tmp;
            N = M;
            M = w->numberOfElements();
        }

        PROTECT(v);
        PROTECT(w);
        Tuple* tT = Tuple::create(N, NIV);

        int i = 0;
        for (; i < N; i++) {
            ASSIGN(tT, elem(i), BASE(v->elem(i))->typeLub(w->elem(i)));
        }

        /* maybe more elements in definite(w) to consider for newRest */
        for (i = N; i < M; i++) {
            newRest = BASE(newRest)->typeLub(w->elem(i));
        }

        return ProductType::create(tT, newRest);
    }
}


/* SumType methods */

BUILTIN_CLASS(SumType) { OB_FIELD_INDIRECT("types", SUM_TYPE_TYPES_SLOT); }


SumType::SumType(pExt ext)
    : Actor(sizeof(SumType), CLASS_META(SumType), CLASS_SBO(SumType),
            lockedMbox, ext) {
    SumType::updateCnt();
}


SumType* SumType::create(Tuple* sum_types) {
    PROTECT(sum_types);
    StdExtension* ext = StdExtension::create(BUILTIN_SumType_SLOTS);
    ext->slot(SUM_TYPE_TYPES_SLOT) = sum_types;
    void* loc = PALLOC1(sizeof(SumType), ext);
    return new (loc) SumType(ext);
}

bool SumType::isCoveredByp(pOb X) {
    /* a sum is covered by X iff EVERY elem of types is covered by X */
    for (int i = 0; i < numberOfElements(); i++) {
        if (!BASE(X)->coversp(elem(i))) {
            return false;
        }
    }

    return true;
}

bool SumType::compositeCoversp(pOb X) {
    /* a SumType covers X iff SOME elem of types covers X */
    for (int i = 0; i < numberOfElements(); i++) {
        if (BASE(elem(i))->coversp(X)) {
            return true;
        }
    }

    return false;
}

pOb SumType::dominator() { return tupleLub(types()); }

pOb SumType::typeLub(pOb x) {
    if (this->coversp(x)) {
        return this;
    } else if (BASE(x)->coversp(this)) {
        return x;
    } else if (!IS_A(x, SumType)) {
        return SumType::create(cons(x, types()))->normalize();
    } else {
        Tuple* els = Tuple::create(this->types(), ((SumType*)x)->types());
        return SumType::create(els)->normalize();
    }
}

pOb SumType::normalize() { return this; }

/* MultiMethod methods */

BUILTIN_CLASS(MultiMethod) { OB_FIELD_INDIRECT("procList", MM_PROC_LIST_SLOT); }


MultiMethod::MultiMethod(pExt ext)
    : Actor(sizeof(MultiMethod), CLASS_META(MultiMethod),
            CLASS_SBO(MultiMethod), lockedMbox, ext) {
    MultiMethod::updateCnt();
}


MultiMethod* MultiMethod::create() {
    StdExtension* ext = StdExtension::create(BUILTIN_MultiMethod_SLOTS);
    ext->slot(MM_PROC_LIST_SLOT) = NIL;
    void* loc = PALLOC1(sizeof(MultiMethod), ext);
    return new (loc) MultiMethod(ext);
}

/* the procList contains procs, each of which contains a tuple of at least
 * one element that is presumed to be the type of the arguments to the
 * proc.  Additionally it is probably useful for the second element of the
 * id tuple to point back to the containing MultiMethod and the third
 * element to be a FIXNUM that indexes the procList by the next proc to
 * call
 */

pOb MultiMethod::matchAndDispatch(pCtxt ctxt) {
    Proc* p;
    Tuple* pid;
    pOb typ;

    for (int i = 0; i < numberOfElements(); i++) {
        p = (Proc*)elem(i);
        if (!IS_A(p, Proc)) {
            return this->runtimeError(ctxt, "non proc in proc list");
        }

        pid = (Tuple*)(p->id);

        if (!IS_A(pid, Tuple) || (pid->numberOfElements() < 1)) {
            return this->runtimeError(ctxt, "badly formed proc id");
        }

        typ = pid->elem(0);

        if (BASE(typ)->typeMatchesp(ctxt->argvec)) {
            return p->dispatch(ctxt);
        }
    }

    return this->runtimeError(ctxt, "type mismatch - no matching proc");
}

bool typeLessEq(pOb x, pOb y) { return BASE(y)->coversp(x); }
bool typeGreaterEq(pOb x, pOb y) { return BASE(x)->coversp(y); }
bool typeEq(pOb x, pOb y) {
    return (BASE(x)->coversp(y) && BASE(y)->coversp(x));
}
bool typeLess(pOb x, pOb y) { return (typeLessEq(x, y) && !typeEq(x, y)); }
bool typeGreater(pOb x, pOb y) {
    return (typeGreaterEq(x, y) && !typeEq(x, y));
}

DEF("multiMethod-lookup-and-invoke", multiMethodLookupAndInvoke, 2, 2) {
    CHECK(0, MultiMethod, mm)
    CHECK(1, Ctxt, ctxt);

    if (debugging_level) {
        printf("\t%s\n", mm->asCstring());
    }

    return mm->matchAndDispatch(ctxt);
}

/* Type system primitives */

DEF("type?", obTypep, 2, 2) {
    /* is ARG(1) of the same type as ARG(0)? */
    return BASE(ARG(0))->typep(ARG(1));
}

DEF("type>=", obTypeGrtrEqp, 2, 2) {
    return RBLBOOL(typeGreaterEq(ARG(0), ARG(1)));
}

DEF("type<=", obTypeLessEqp, 2, 2) {
    return RBLBOOL(typeLessEq(ARG(0), ARG(1)));
}

DEF("type=", obTypeEqp, 2, 2) { return RBLBOOL(typeEq(ARG(0), ARG(1))); }

DEF("type>", obTypeGreaterp, 2, 2) {
    return RBLBOOL(typeGreater(ARG(0), ARG(1)));
}

DEF("type<", obTypeLessp, 2, 2) { return RBLBOOL(typeLess(ARG(0), ARG(1))); }

DEF("type<>", obTypesIncomparable, 2, 2) {
    return RBLBOOL(!(typeLessEq(ARG(0), ARG(1)) || typeLessEq(ARG(1), ARG(0))));
}

DEF("type!=", obTypesInequalp, 2, 2) {
    return RBLBOOL(!typeEq(ARG(0), ARG(1)));
}

DEF("hasParent?", obHasParentp, 2, 2) {
    /* does ARG(0) have ARG(1) as a parent (indirectly)? */
    return RBLBOOL(BASE(ARG(0))->hasParentp(ARG(1)));
}

DEF("typeMatches?", obTypeMatchesp, 2, 2) {
    return RBLBOOL(BASE(ARG(0))->typeMatchesp(ARG(1)));
}

DEF("typeLub", obsTypeLub, 1, MaxArgs) { return tupleLub(ARGS); }

pOb typeDominator(pOb x) {
    if (IS_A(x, SumType)) {
        return ((SumType*)x)->dominator();
    } else {
        return x;
    }
}

DEF("typeDominator", obTypeDominator, 1, 1) { return typeDominator(ARG(0)); }

DEF("sumNormalize", normalizeSumType, 1, 1) {
    CHECK(0, SumType, x);
    return x->normalize();
}

/* Type system initialization */
MODULE_INIT(MI) {
    Define("MIActor", MIActor::create(NIL));
    Define("ProductType", ProductType::create(NIL, NIV));
    Define("SumType", SumType::create(NIL));
    Define("MultiMethod", MultiMethod::create());
}
