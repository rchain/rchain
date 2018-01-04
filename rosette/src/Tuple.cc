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

#include "Tuple.h"
#include "Ctxt.h"
#include "MI.h"
#include "Prim.h"
#include "BuiltinClass.h"

#include <memory.h>


Tuple* NIL = (Tuple*)INVALID;


BUILTIN_CLASS(Tuple) {}


Tuple::Tuple(int size, Tuple* master, int offset, int n, Ob* init)
    : Ob(sizeof(Tuple) + size * sizeof(Ob*), CLASS_META(Tuple),
         CLASS_SBO(Tuple)) {
    Ob** p = &elem(0);
    Ob** q = &master->elem(offset);
    int i = n;
    for (; i--;) {
        *p++ = *q++;
    }

    for (i = size - n; i--;) {
        *p++ = init;
    }

    Tuple::updateCnt();
}


Tuple::Tuple(int n, Ob* init)
    : Ob(sizeof(Tuple) + n * sizeof(Ob*), CLASS_META(Tuple), CLASS_SBO(Tuple)) {
    short int m = n;
    Ob** p = &elem(0);
    while (m--) {
        *p++ = init;
    }

    Tuple::updateCnt();
}


Tuple::Tuple(Ob** p, int n)
    : Ob(sizeof(Tuple) + n * sizeof(Ob*), CLASS_META(Tuple), CLASS_SBO(Tuple)) {
    Ob** s = &elem(0);
    for (short int i = n; i--;) {
        *s++ = *p++;
    }

    Tuple::updateCnt();
}


Tuple::Tuple(int size, int offset, Tuple* rest)
    : Ob(sizeof(Tuple) + size * sizeof(Ob*), CLASS_META(Tuple),
         CLASS_SBO(Tuple)) {
    Ob** newp = &elem(0);
    Ob** oldp = &rest->elem(0);
    int i = offset;
    for (; i--;) {
        *newp++ = INVALID;
    }

    for (i = rest->numberOfElements(); i--;) {
        *newp++ = *oldp++;
    }

    Tuple::updateCnt();
}


Tuple::Tuple(Tuple* t1, Tuple* t2)
    : Ob(sizeof(Tuple) +
             (t1->numberOfElements() + t2->numberOfElements()) * sizeof(Ob*),
         CLASS_META(Tuple), CLASS_SBO(Tuple)) {
    short int n1 = t1->numberOfElements();
    short int n2 = t2->numberOfElements();
    Ob** newp = &elem(0);
    Ob** oldp = &t1->elem(0);
    while (n1--) {
        *newp++ = *oldp++;
    }

    oldp = &t2->elem(0);
    while (n2--) {
        *newp++ = *oldp++;
    }

    Tuple::updateCnt();
}


Tuple::Tuple(Tuple* t, int n, Tuple* rest)
    : Ob(sizeof(Tuple) + (n + 1) * sizeof(Ob*), CLASS_META(Tuple),
         CLASS_SBO(Tuple)) {
    Ob** newp = &elem(0);
    Ob** oldp = &t->elem(0);
    for (short int i = n; i--;) {
        *newp++ = *oldp++;
    }

    *newp = rest;
    Tuple::updateCnt();
}


Tuple::Tuple(Tuple* t) : Ob(SIZE(t), t->meta(), t->parent()) {
    Ob** newp = &elem(0);
    Ob** oldp = &t->elem(0);
    for (short int i = t->numberOfElements(); i--;) {
        *newp++ = *oldp++;
    }

    Tuple::updateCnt();
}


Tuple* Tuple::create() {
    /*
     * This should only be called from BigBang, and then only to
     * initialize NIL.
     */
    if (NIL == INVALID) {
        void* loc = PALLOC(sizeof(Tuple));
        return new (loc) Tuple(0, INVALID);
    } else {
        return NIL;
    }
}


Tuple* Tuple::create(int size, Tuple* master, int offset, int n, Ob* init) {
    if (size == 0) {
        return NIL;
    }

    void* loc = PALLOC2(sizeof(Tuple) + size * sizeof(Ob*), master, init);
    return new (loc) Tuple(size, master, offset, n, init);
}


Tuple* Tuple::create(int n, Ob* init) {
    if (n == 0) {
        return NIL;
    }

    void* loc = PALLOC1(sizeof(Tuple) + n * sizeof(Ob*), init);
    return new (loc) Tuple(n, init);
}


Tuple* Tuple::create(Ob** p, int n) {
    void* loc = PALLOC(sizeof(Tuple) + n * sizeof(Ob*));
    return new (loc) Tuple(p, n);
}


Tuple* Tuple::create(int size, int offset, Tuple* rest) {
    if (size == 0) {
        return NIL;
    }

    void* loc = PALLOC1(sizeof(Tuple) + size * sizeof(Ob*), rest);
    return new (loc) Tuple(size, offset, rest);
}


Tuple* Tuple::create(Tuple* t1, Tuple* t2) {
    if (t1 == NIL) {
        return t2;
    }

    if (t2 == NIL) {
        return t1;
    }

    void* loc = PALLOC2(SIZE(t1) + SIZE(t2) - sizeof(Tuple), t1, t2);
    return new (loc) Tuple(t1, t2);
}


Tuple* Tuple::create(Tuple* t, int n) {
    PROTECT(t);
    Tuple* rest = t->makeTail(n);
    void* loc = PALLOC1(sizeof(Tuple) + (n + 1) * sizeof(Ob*), rest);
    return new (loc) Tuple(t, n, rest);
}


Tuple* Tuple::create(Tuple* t) {
    if (t == NIL) {
        return NIL;
    }

    void* loc = PALLOC1(SIZE(t), t);
    return new (loc) Tuple(t);
}


StdExtension* Tuple::becomeExtension(Ob* new_meta, Ob* new_parent) {
    return new (this) StdExtension(new_meta, new_parent);
}


Tuple* Tuple::makeSlice(int offset, int howmany_) {
    return (this == NIL ? NIL
                        : Tuple::create(howmany_, this, offset, howmany_));
}


Tuple* Tuple::makeTail(int entriesToSkip) {
    int size = numberOfElements() - entriesToSkip;
    return Tuple::create(size, this, entriesToSkip, size);
}


Ob* Tuple::indexedSize() { return FIXNUM(numberOfElements()); }


Ob* Tuple::nth(int n) { return elem(n); }


Ob* Tuple::setNth(int n, Ob* x) {
    ASSIGN(this, elem(n), x);
    return this;
}


Ob* Tuple::cloneTo(Ob* new_meta, Ob* new_parent) {
    return (this == NIL ? NIL : Ob::cloneTo(new_meta, new_parent));
}


Ob* Tuple::subObject(int i, int n) { return makeSlice(i, n); }


bool Tuple::accepts(Ctxt* msg) {
    if (this == NIL) {
        return true;
    } else {
        int n = numberOfElements();
        for (int i = 0; i < n; i++) {
            if (BASE(elem(i))->matches(msg)) {
                return true;
            }
        }

        return false;
    }
}


bool Tuple::matches(Ctxt* msg) {
    int n = numberOfElements();
    if (n > 0 && n <= msg->nargs) {
        if (elem(0) != msg->trgt) {
            return false;
        }

        /*
         * Skip msg->arg(0), since it is actually the receiver of the
         * message, i.e.,
         *
         * 	(oprn rcvr arg1 arg2 ...)
         *
         * results in an argvec of
         *
         * 	[rcvr arg1 arg2 ...]
         */

        for (int i = 1; i < n; i++) {
            if (elem(i) != msg->arg(i)) {
                return false;
            }
        }

        return true;
    } else {
        return false;
    }
}

bool Tuple::matches(Tuple* msg) {
    if (this == NIL) {
        return true;
    }

    int n = numberOfElements();
    if (n > 0 && n <= msg->numberOfElements()) {
        for (int i = 0; i < n; i++) {
            if ((elem(i) != msg->elem(i)) && (elem(i) != NIV)) {
                if (IS_A(elem(i), Tuple)) {
                    if (IS_A(msg->elem(i), Tuple)) {
                        if (!((Tuple*)elem(i))->matches((Tuple*)msg->elem(i))) {
                            return false;
                        } else {
                            continue;
                        }
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }
            }
        }

        return true;
    }

    return false;
}


Tuple* cons(Ob* o, Tuple* t) {
    PROTECT(o);
    PROTECT(t);
    int size = t->numberOfElements();
    int newsize = size + 1;
    Tuple* t2 = Tuple::create(newsize, 1, t);
    t2->elem(0) = o;
    return t2;
}


Tuple* consstar(Tuple* prefix, int prefixsize, Tuple* suffix) {
    PROTECT(prefix);
    PROTECT(suffix);
    if (prefixsize == 0) {
        return suffix;
    }

    int suffixsize = suffix->numberOfElements();
    int resultsize = prefixsize + suffixsize;
    Tuple* result = Tuple::create(resultsize, prefixsize, suffix);
    Ob** prefixp = &prefix->elem(0);
    Ob** resultp = &result->elem(0);
    for (short int i = prefixsize; i--;) {
        *resultp++ = *prefixp++;
    }

    return result;
}


Tuple* rcons(Tuple* t, Ob* o) {
    PROTECT(t);
    PROTECT(o);
    int size = t->numberOfElements();
    int newsize = size + 1;
    Tuple* t2 = Tuple::create(newsize, t, 0, size);
    t2->elem(size) = o;
    return t2;
}


Tuple* concat(Tuple* t1, Tuple* t2) { return Tuple::create(t1, t2); }


DEF("null?", obNullQ, 1, 1) { return (ARG(0) == NIL ? RBLTRUE : RBLFALSE); }


DEF("tuple-cons", tplCons, 2, 2) {
    CHECK(1, Tuple, tail);
    return cons(ARG(0), tail);
}


DEF("tuple-cons*", tplConsStar, 1, MaxArgs) {
    int n = NARGS - 1;
    CHECK(n, Tuple, tail);
    return consstar(ARGS, n, tail);
}


DEF("tuple-rcons", tplRcons, 2, 2) {
    CHECK(0, Tuple, prefix);
    return rcons(prefix, ARG(1));
}


DEF("tuple-concat", tplConcat, 0, MaxArgs) {
    Tuple* result = NIL;
    int resultsize = 0;

    int i = NARGS;
    for (; i--;) {
        CHECK(i, Tuple, t);
        resultsize += t->numberOfElements();
    }

    PROTECT(__CTXT__);

    switch (NARGS) {
    case 0:
        break;

    case 1:
        result = (Tuple*)ARG(0);
        break;

    default:
        if (resultsize == 0) {
            result = NIL;
        } else {
            result = Tuple::create(resultsize, NIV);
            Ob** resultp = &result->elem(0);
            for (i = 0; i < NARGS; i++) {
                Tuple* t = (Tuple*)ARG(i);
                int n = t->numberOfElements();
                memcpy(resultp, &t->elem(0), n * sizeof(Ob*));
                resultp += n;
            }
        }

        break;
    }

    return result;
}


DEF("tuple-safe-nth", tplSafeNth, 2, 2) {
    CHECK(0, Tuple, t);
    CHECK_FIXNUM(1, n);

    if (n < 0) {
        return MIN_FIXNUM;
    } else if (n < t->numberOfElements()) {
        return t->elem(n);
    } else {
        return MAX_FIXNUM;
    }
}


DEF("tuple-xchg", tplXchg, 3, 3) {
    CHECK(0, Tuple, t);
    CHECK_FIXNUM(1, i);
    CHECK_FIXNUM(2, j);

    Ob* temp = t->elem(i);
    t->elem(i) = t->elem(j);
    t->elem(j) = temp;

    return t;
}


DEF("tuple-head", tplHead, 1, 1) {
    CHECK(0, Tuple, t);
    return (t->numberOfElements() > 0 ? t->elem(0) : NIL);
}

DEF("tuple-last", tplLast, 1, 1) {
    CHECK(0, Tuple, t);
    int n = t->numberOfElements();
    return (n > 0 ? t->elem(n - 1) : NIL);
}

DEF("tuple-tail", tplTail, 1, 1) {
    CHECK(0, Tuple, t);
    int n = t->numberOfElements();
    return (n > 0 ? t->makeSlice(1, n - 1) : NIL);
}


DEF("tuple-new", tplNew, 1, MaxArgs) {
    /*
     * WARNING: this primitive (like tplNewN, tplexprNew and tplexprNewN)
     * takes an extra argument in the leading slot, allowing these
     * primitives to be bound directly to operations.  That is, is the
     * operation new is bound to tplNew in a prototypical tuple Tuple,
     * then the expression
     *
     * 	(new Tuple a b c)
     *
     * will be equivalent to
     *
     * 	(tplNew Tuple a b c)
     *
     * Since we want to produce [a b c], we need to ignore the first arg.
     */
    return ARGS->makeSlice(1, NARGS - 1);
}


DEF("tuple-new-n", tplNewN, 3, 3) {
    /*
     * See warning in tplNew about the rationale for ignoring ARG(0).
     *
     * 	(tplNewN dummy n init)
     *
     * For example,
     *
     * 	(tplNewN [] 3 'a)
     *
     * yields
     *
     * 	['a 'a 'a]
     */

    CHECK_FIXNUM(1, n);
    return (n <= 0 ? NIL : Tuple::create(n, ARG(2)));
}

DEF("tuple-mem?", tplMemQ, 2, 2) {
    CHECK(0, Tuple, t);
    for (int i = 0; i < t->numberOfElements(); i++) {
        if (t->elem(i) == ARG(1)) {
            return RBLTRUE;
        }
    }
    return RBLFALSE;
}

DEF("tuple-matches?", tplMatchesP, 2, 2) {
    CHECK(0, Tuple, pat);
    CHECK(1, Tuple, t);
    return (pat->matches(t) ? RBLTRUE : RBLFALSE);
}
