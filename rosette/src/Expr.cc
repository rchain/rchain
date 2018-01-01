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

#include "Expr.h"
#include "Ctxt.h"
#include "Prim.h"
#include "Tuple.h"

#include "BuiltinClass.h"

#include <memory.h>


Expr::Expr(int sz, Ob* meta, Ob* parent) : Ob(sz, meta, parent) {}


LetExpr::LetExpr(int sz, Ob* meta, Ob* parent, TupleExpr* te, Ob* b)
    : Expr(sz, meta, parent), bindings(te), body(b) {}


MethodExpr::MethodExpr(int sz, Ob* meta, Ob* parent, Ob* i, Ob* f, Ob* b)
    : Expr(sz, meta, parent), identity(i), formals(f), body(b) {}


bool Expr::ConstantP() { return false; }


Ob* Expr::unquote() {
    NI("unquote");
    return INVALID;
}


BUILTIN_CLASS(BlockExpr) {
    OB_FIELD("sub-exprs", BlockExpr, subExprs);
    OB_FIELD("implicit", BlockExpr, implicit);
}


BlockExpr::BlockExpr(Tuple* init, Ob* implct)
    : Expr(sizeof(BlockExpr), CLASS_META(BlockExpr), CLASS_SBO(BlockExpr)),
      subExprs(init),
      implicit(implct) {
    BlockExpr::updateCnt();
}


BlockExpr* BlockExpr::create(Tuple* init, Ob* implicit) {
    void* loc = PALLOC2(sizeof(BlockExpr), init, implicit);
    return new (loc) BlockExpr(init, implicit);
}


int BlockExpr::numberOfSubExprs() { return subExprs->numberOfElements(); }
Ob* BlockExpr::indexedSize() { return FIXNUM(numberOfSubExprs()); }
Ob* BlockExpr::nth(int n) { return subExprs->elem(n); }
Ob* BlockExpr::setNth(int n, Ob* x) {
    ASSIGN(subExprs, elem(n), x);
    return this;
}
Ob* BlockExpr::subObject(int i, int n) {
    return BlockExpr::create(subExprs->makeSlice(i, n));
}


BUILTIN_CLASS(FreeExpr) {
    OB_FIELD("id-list", FreeExpr, freeIds);
    OB_FIELD("body", FreeExpr, body);
}


FreeExpr::FreeExpr(TupleExpr* f, Ob* b)
    : Expr(sizeof(FreeExpr), CLASS_META(FreeExpr), CLASS_SBO(FreeExpr)),
      freeIds(f),
      body(b) {
    FreeExpr::updateCnt();
}


FreeExpr* FreeExpr::create(TupleExpr* freeIds, Ob* body) {
    void* loc = PALLOC2(sizeof(FreeExpr), freeIds, body);
    return new (loc) FreeExpr(freeIds, body);
}


BUILTIN_CLASS(GotoExpr) { OB_FIELD("label", GotoExpr, label); }


GotoExpr::GotoExpr(Ob* name)
    : Expr(sizeof(GotoExpr), CLASS_META(GotoExpr), CLASS_SBO(GotoExpr)),
      label(name) {
    GotoExpr::updateCnt();
}


GotoExpr* GotoExpr::create(Ob* label_name) {
    void* loc = PALLOC1(sizeof(GotoExpr), label_name);
    return new (loc) GotoExpr(label_name);
}


BUILTIN_CLASS(IfExpr) {
    OB_FIELD("condition", IfExpr, condition);
    OB_FIELD("true-branch", IfExpr, trueBranch);
    OB_FIELD("false-branch", IfExpr, falseBranch);
}


IfExpr::IfExpr(Ob* c, Ob* t, Ob* f)
    : Expr(sizeof(IfExpr), CLASS_META(IfExpr), CLASS_SBO(IfExpr)),
      condition(c),
      trueBranch(t),
      falseBranch(f) {
    IfExpr::updateCnt();
}


IfExpr* IfExpr::create(Ob* c, Ob* t, Ob* f) {
    if (f == INVALID) {
        f = NullExpr::create();
    }

    void* loc = PALLOC3(sizeof(IfExpr), c, t, f);
    return new (loc) IfExpr(c, t, f);
}


BUILTIN_CLASS(LabelExpr) {
    OB_FIELD("label", LabelExpr, label);
    OB_FIELD("body", LabelExpr, body);
}


LabelExpr::LabelExpr(Ob* l, Ob* b)
    : Expr(sizeof(LabelExpr), CLASS_META(LabelExpr), CLASS_SBO(LabelExpr)),
      label(l),
      body(b) {
    LabelExpr::updateCnt();
}


LabelExpr* LabelExpr::create(Ob* label, Ob* body) {
    void* loc = PALLOC2(sizeof(LabelExpr), label, body);
    return new (loc) LabelExpr(label, body);
}


BUILTIN_CLASS(LetExpr) {
    OB_FIELD("bindings", LetExpr, bindings);
    OB_FIELD("body", LetExpr, body);
}


LetExpr::LetExpr(TupleExpr* te, Ob* b)
    : Expr(sizeof(LetExpr), CLASS_META(LetExpr), CLASS_SBO(LetExpr)),
      bindings(te),
      body(b) {
    LetExpr::updateCnt();
}


LetExpr* LetExpr::create(TupleExpr* te, Ob* b) {
    void* loc = PALLOC2(sizeof(LetExpr), te, b);
    return new (loc) LetExpr(te, b);
}


Ob* LetExpr::boundId(int n) { return ((TupleExpr*)bindings->elem(n))->elem(0); }

Ob* LetExpr::boundExpr(int n) {
    return ((TupleExpr*)bindings->elem(n))->elem(1);
}


BUILTIN_CLASS(LetrecExpr) {
    OB_FIELD("bindings", LetrecExpr, bindings);
    OB_FIELD("body", LetrecExpr, body);
}


LetrecExpr::LetrecExpr(TupleExpr* te, Ob* b)
    : LetExpr(sizeof(LetrecExpr), CLASS_META(LetrecExpr), CLASS_SBO(LetrecExpr),
              te, b) {
    LetrecExpr::updateCnt();
}


LetrecExpr* LetrecExpr::create(TupleExpr* te, Ob* b) {
    void* loc = PALLOC2(sizeof(LetrecExpr), te, b);
    return new (loc) LetrecExpr(te, b);
}


BUILTIN_CLASS(MethodExpr) {
    OB_FIELD("id", MethodExpr, identity);
    OB_FIELD("formals", MethodExpr, formals);
    OB_FIELD("body", MethodExpr, body);
}


MethodExpr::MethodExpr(Ob* i, Ob* f, Ob* b)
    : Expr(sizeof(MethodExpr), CLASS_META(MethodExpr), CLASS_SBO(MethodExpr)),
      identity(i),
      formals(f),
      body(b) {
    MethodExpr::updateCnt();
}


MethodExpr* MethodExpr::create(Ob* i, Ob* f, Ob* b) {
    void* loc = PALLOC3(sizeof(MethodExpr), i, f, b);
    return new (loc) MethodExpr(i, f, b);
}


BUILTIN_CLASS(NullExpr) {}


NullExpr::NullExpr()
    : Expr(sizeof(NullExpr), CLASS_META(NullExpr), CLASS_SBO(NullExpr)) {
    NullExpr::updateCnt();
}


NullExpr* NullExpr::create() {
    void* loc = PALLOC(sizeof(NullExpr));
    return new (loc) NullExpr();
}


BUILTIN_CLASS(ProcExpr) {
    OB_FIELD("id", ProcExpr, identity);
    OB_FIELD("formals", ProcExpr, formals);
    OB_FIELD("body", ProcExpr, body);
}


ProcExpr::ProcExpr(Ob* i, Ob* f, Ob* b)
    : MethodExpr(sizeof(ProcExpr), CLASS_META(ProcExpr), CLASS_SBO(ProcExpr), i,
                 f, b) {
    ProcExpr::updateCnt();
}


ProcExpr* ProcExpr::create(Ob* i, Ob* f, Ob* b) {
    void* loc = PALLOC3(sizeof(ProcExpr), i, f, b);
    return new (loc) ProcExpr(i, f, b);
}


BUILTIN_CLASS(QuoteExpr) { OB_FIELD("expr", QuoteExpr, expr); }


QuoteExpr::QuoteExpr(Ob* e)
    : Expr(sizeof(QuoteExpr), CLASS_META(QuoteExpr), CLASS_SBO(QuoteExpr)),
      expr(e) {
    QuoteExpr::updateCnt();
}


QuoteExpr* QuoteExpr::create(Ob* e) {
    void* loc = PALLOC1(sizeof(QuoteExpr), e);
    return new (loc) QuoteExpr(e);
}


bool QuoteExpr::ConstantP() { return true; }
Ob* QuoteExpr::unquote() { return expr; }


BUILTIN_CLASS(ReflectiveMethodExpr) {
    OB_FIELD("id", ReflectiveMethodExpr, identity);
    OB_FIELD("formals", ReflectiveMethodExpr, formals);
    OB_FIELD("body", ReflectiveMethodExpr, body);
}


ReflectiveMethodExpr::ReflectiveMethodExpr(Ob* i, Ob* f, Ob* b)
    : MethodExpr(sizeof(ReflectiveMethodExpr), CLASS_META(ReflectiveMethodExpr),
                 CLASS_SBO(ReflectiveMethodExpr), i, f, b) {
    ReflectiveMethodExpr::updateCnt();
}


ReflectiveMethodExpr* ReflectiveMethodExpr::create(Ob* i, Ob* f, Ob* b) {
    void* loc = PALLOC3(sizeof(ReflectiveMethodExpr), i, f, b);
    return new (loc) ReflectiveMethodExpr(i, f, b);
}


BUILTIN_CLASS(RequestExpr) {
    OB_FIELD("trgt", RequestExpr, target);
    OB_FIELD("msg", RequestExpr, msg);
}


RequestExpr::RequestExpr(Ob* t, TupleExpr* te)
    : Expr(sizeof(RequestExpr), CLASS_META(RequestExpr),
           CLASS_SBO(RequestExpr)),
      target(t),
      msg(te) {
    RequestExpr::updateCnt();
}


RequestExpr::RequestExpr(int sz, Ob* meta, Ob* parent, Ob* t, TupleExpr* te)
    : Expr(sz, meta, parent), target(t), msg(te) {}


RequestExpr* RequestExpr::create(Ob* t, TupleExpr* te) {
    void* loc = PALLOC2(sizeof(RequestExpr), t, te);
    return new (loc) RequestExpr(t, te);
}


BUILTIN_CLASS(SendExpr) {
    OB_FIELD("trgt", SendExpr, target);
    OB_FIELD("msg", SendExpr, msg);
}


SendExpr::SendExpr(Ob* t, TupleExpr* m)
    : RequestExpr(sizeof(SendExpr), CLASS_META(SendExpr), CLASS_SBO(SendExpr),
                  t, m) {
    SendExpr::updateCnt();
}


SendExpr* SendExpr::create(Ob* t, TupleExpr* m) {
    void* loc = PALLOC2(sizeof(SendExpr), t, m);
    return new (loc) SendExpr(t, m);
}


BUILTIN_CLASS(SeqExpr) { OB_FIELD("sub-exprs", SeqExpr, subExprs); }


SeqExpr::SeqExpr(Tuple* t)
    : Expr(sizeof(SeqExpr), CLASS_META(SeqExpr), CLASS_SBO(SeqExpr)),
      subExprs(t) {
    SeqExpr::updateCnt();
}


SeqExpr* SeqExpr::create(Tuple* init) {
    void* loc = PALLOC1(sizeof(SeqExpr), init);
    return new (loc) SeqExpr(init);
}


int SeqExpr::numberOfSubExprs() { return subExprs->numberOfElements(); }
Ob* SeqExpr::indexedSize() { return FIXNUM(numberOfSubExprs()); }
Ob* SeqExpr::nth(int n) { return subExprs->elem(n); }
Ob* SeqExpr::setNth(int n, Ob* x) {
    ASSIGN(subExprs, elem(n), x);
    return this;
}

Ob* SeqExpr::subObject(int i, int n) {
    return SeqExpr::create(subExprs->makeSlice(i, n));
}


BUILTIN_CLASS(SetExpr) {
    OB_FIELD("trgt", SetExpr, trgt);
    OB_FIELD("val", SetExpr, val);
}


SetExpr::SetExpr(Ob* t, Ob* v)
    : Expr(sizeof(SetExpr), CLASS_META(SetExpr), CLASS_SBO(SetExpr)),
      trgt(t),
      val(v) {
    SetExpr::updateCnt();
}


SetExpr* SetExpr::create(Ob* trgt, Ob* val) {
    void* loc = PALLOC2(sizeof(SetExpr), trgt, val);
    return new (loc) SetExpr(trgt, val);
}


BUILTIN_CLASS(TupleExpr) { OB_FIELD("rest", TupleExpr, rest); }


TupleExpr* NILexpr = (TupleExpr*)INVALID;


TupleExpr::TupleExpr()
    : Expr(sizeof(TupleExpr), CLASS_META(TupleExpr), CLASS_SBO(TupleExpr)),
      rest(INVALID) {
    /*
     * This constructor is used only once, during the BigBang to
     * initialize NILexpr.
     */
}


TupleExpr::TupleExpr(int n, Ob* r)
    : Expr(sizeof(TupleExpr) + n * sizeof(Ob*), CLASS_META(TupleExpr),
           CLASS_SBO(TupleExpr)),
      rest(r) {
    /*
     * This constructor assumes that the rest argument (r above) has been
     * "normalized", i.e., that it is either NILexpr or not a TupleExpr.
     * As long as all TupleExprs are created through some version of
     * TupleExpr::create, this assumption should always hold.
     */
    for (int i = 0; i < n; i++) {
        this->elem(i) = INVALID;
    }

    TupleExpr::updateCnt();
}


TupleExpr* TupleExpr::create() {
    void* loc = PALLOC(sizeof(TupleExpr));
    return new (loc) TupleExpr();
}


TupleExpr* TupleExpr::create(Ob** p, int n, Ob* r) {
    if (n == 0 && r == NILexpr) {
        return NILexpr;
    } else if (!IS_A(r, TupleExpr) || r == NILexpr) {
        void* loc = PALLOC1(sizeof(TupleExpr) + n * sizeof(Ob*), r);
        TupleExpr* result = new (loc) TupleExpr(n, r);
        memcpy(&result->elem(0), &p[0], n * sizeof(Ob*));
        return result;
    } else {
        TupleExpr* rp = (TupleExpr*)r;
        int m = rp->numberOfElements();
        void* loc = PALLOC1(sizeof(TupleExpr) + (n + m) * sizeof(Ob*), rp);
        TupleExpr* result = new (loc) TupleExpr(n + m, rp->rest);
        memcpy(&result->elem(0), &p[0], n * sizeof(Ob*));
        memcpy(&result->elem(n), &rp->elem(0), m * sizeof(Ob*));
        return result;
    }
}


TupleExpr* TupleExpr::create(int n, Ob* r) {
    if (n == 0 && r == NILexpr) {
        return NILexpr;
    } else if (!IS_A(r, TupleExpr) || r == NILexpr) {
        void* loc = PALLOC1(sizeof(TupleExpr) + n * sizeof(Ob*), r);
        TupleExpr* result = new (loc) TupleExpr(n, r);
        for (int i = 0; i < n; i++)
            result->elem(i) = INVALID;
        return result;
    } else {
        TupleExpr* rp = (TupleExpr*)r;
        int m = rp->numberOfElements();
        void* loc = PALLOC1(sizeof(TupleExpr) + (n + m) * sizeof(Ob*), rp);
        TupleExpr* result = new (loc) TupleExpr(n + m, rp->rest);
        for (int i = 0; i < n; i++)
            result->elem(i) = INVALID;
        memcpy(&result->elem(n), &rp->elem(0), m * sizeof(Ob*));
        return result;
    }
}


bool TupleExpr::allPairs() {
    for (int i = numberOfElements(); i--;) {
        if (!IS_A(elem(i), TupleExpr) ||
            ((TupleExpr*)elem(i))->numberOfElements() != 2) {
            return false;
        }
    }

    return true;
}


bool TupleExpr::allSymbols() {
    for (int i = numberOfElements(); i--;) {
        if (!IS_SYM(elem(i))) {
            return false;
        }
    }
    return true;
}


bool TupleExpr::ConstantP() {
    if (this == NILexpr) {
        return true;
    }

    if (!BASE(rest)->ConstantP()) {
        return false;
    }

    for (int i = numberOfElements(); i--;) {
        if (!BASE(elem(i))->ConstantP()) {
            return false;
        }
    }

    return true;
}


TupleExpr* TupleExpr::cons(Ob* val) {
    PROTECT_THIS(TupleExpr);
    PROTECT(val);
    const int n = SELF->numberOfElements();
    TupleExpr* result = TupleExpr::create(n + 1, SELF->rest);
    result->elem(0) = val;
    memcpy(&result->elem(1), &SELF->elem(0), n * sizeof(Ob*));
    return result;
}


TupleExpr* TupleExpr::makeSlice(int start, int span) {
    if (this == NILexpr || span == 0) {
        return NILexpr;
    }

    PROTECT_THIS(TupleExpr);
    TupleExpr* result = TupleExpr::create(span);
    memcpy(&result->elem(0), &SELF->elem(start), span * sizeof(Ob*));
    return result;
}


Ob* TupleExpr::cloneTo(Ob* new_meta, Ob* new_parent) {
    return (this == NILexpr ? NILexpr : Expr::cloneTo(new_meta, new_parent));
}


Ob* TupleExpr::unquote() {
    if (this == NILexpr) {
        return NIL;
    } else {
        PROTECT_THIS(TupleExpr);
        int n = SELF->numberOfElements();
        Tuple* t = Tuple::create(n, INVALID);
        PROTECT(t);
        for (int i = n; i--;) {
            Ob* v = BASE(SELF->elem(i))->unquote();
            ASSIGN(t, elem(i), v);
        }

        return t;
    }
}


Ob* TupleExpr::indexedSize() { return FIXNUM(numberOfElements()); }


Ob* TupleExpr::nth(int n) { return elem(n); }


Ob* TupleExpr::setNth(int n, Ob* x) {
    ASSIGN(this, elem(n), x);
    return this;
}


Ob* TupleExpr::subObject(int i, int n) { return makeSlice(i, n); }


Ob* blockify(int start, int n, Tuple* exprs) {
    if (n == 1) {
        return exprs->elem(start);
    } else {
        Tuple* subExprs = exprs->makeSlice(start, n);
        return BlockExpr::create(subExprs);
    }
}


DEF("blockexpr-basic-new", blkexprBX, 1, MaxArgs) {
    return blockify(0, NARGS, ARGS);
}


DEF("methodexpr-basic-new", mthdexprMX, 2, MaxArgs) {
    PROTECT(__CTXT__);
    Ob* body = blockify(1, NARGS - 1, ARGS);
    return MethodExpr::create(NIV, ARG(0), body);
}


DEF("procexpr-basic-new", procexprPX, 2, MaxArgs) {
    PROTECT(__CTXT__);
    Ob* body = blockify(1, NARGS - 1, ARGS);
    return ProcExpr::create(NIV, ARG(0), body);
}


DEF("reflectivemethodexpr-basic-new", reflectivemthdexprRMX, 2, MaxArgs) {
    PROTECT(__CTXT__);
    Ob* body = blockify(1, NARGS - 1, ARGS);
    return ReflectiveMethodExpr::create(NIV, ARG(0), body);
}


DEF("quoteexpr-basic-new", quoteexprQ, 1, 1) {
    return QuoteExpr::create(ARG(0));
}


DEF("requestexpr-basic-new", rqstexprRX, 1, MaxArgs) {
    int nargs = NARGS - 1;
    PROTECT(__CTXT__);
    TupleExpr* msg = TupleExpr::create(nargs);
    memcpy(&msg->elem(0), &ARG(1), nargs * sizeof(Ob*));
    return RequestExpr::create(ARG(0), msg);
}


DEF("sendexpr-basic-new", sendexprSX, 1, MaxArgs) {
    int nargs = NARGS - 1;
    PROTECT(__CTXT__);
    TupleExpr* msg = TupleExpr::create(nargs);
    memcpy(&msg->elem(0), &ARG(1), nargs * sizeof(Ob*));
    return SendExpr::create(ARG(0), msg);
}


DEF("seqexpr-basic-new", seqexprSqX, 1, MaxArgs) {
    if (NARGS == 1) {
        return ARG(0);
    } else {
        Tuple* subExprs = ARGS->makeSlice(0, NARGS);
        return SeqExpr::create(subExprs);
    }
}


DEF("tupleexpr-basic-new", tplexprTX, 0, MaxArgs) {
    if (NARGS == 0) {
        return NILexpr;
    }

    PROTECT(__CTXT__);
    TupleExpr* tx = TupleExpr::create(NARGS);
    memcpy(&tx->elem(0), &ARG(0), NARGS * sizeof(Ob*));
    return tx;
}


DEF("requestexpr->tuple", rqstexprToTuple, 1, 1) {
    CHECK(0, RequestExpr, rx);
    if (!IS_A(rx->msg, TupleExpr)) {
        return PRIM_MISMATCH(0, "proper RequestExpr");
    }

    PROTECT(rx);
    int n = rx->msg->numberOfElements();
    Tuple* result = Tuple::create(n + 1, NIV);
    memcpy(&result->elem(1), &rx->msg->elem(0), n * sizeof(Ob*));
    result->elem(0) = rx->target;
    return result;
}


DEF("tupleexpr-new-n", tplexprNewN, 4, 4) {
    /*
     * See warning in tplNew about the rationale for ignoring ARG(0).
     *
     * 	(tplexprNewN dummy rest n init)
     *
     * For example,
     *
     * 	(tplexprNewN '[] 'f 3 'a)
     *
     * yields
     *
     * 	'[a a a & f]
     */

    CHECK_FIXNUM(2, n);

    if (n == 0 && ARG(1) == NILexpr) {
        return NILexpr;
    }

    TupleExpr* result = TupleExpr::create(n, ARG(1));
    for (int i = 0; i < n; i++) {
        result->elem(i) = ARG(3);
    }

    return result;
}


DEF("tupleexpr-new", tplexprNew, 3, 3) {
    /*
     * See warning in tplNew about the rationale for ignoring ARG(0).
     *
     * 	(tplexprNew dummy rest tuple-of-subexprs)
     *
     * For example,
     *
     * 	(tplexprNew '[] 'f ['a 'b 'c])
     *
     * yields
     *
     * 	'[a b c & f]
     */

    CHECK(2, Tuple, subExprs);
    int nexprs = subExprs->numberOfElements();

    if (nexprs == 0 && ARG(1) == NILexpr) {
        return NILexpr;
    }

    PROTECT(__CTXT__);
    PROTECT(subExprs);
    TupleExpr* result = TupleExpr::create(nexprs, ARG(1));
    memcpy(&result->elem(0), &subExprs->elem(0), nexprs * sizeof(Ob*));
    return result;
}


DEF("tupleexpr->tuple", tplexprToTuple, 1, 1) {
    CHECK(0, TupleExpr, expr);

    if (expr == NILexpr) {
        return NIL;
    }

    PROTECT(expr);
    int n = expr->numberOfElements();
    Tuple* result = Tuple::create(n, NIV);
    memcpy(&result->elem(0), &expr->elem(0), n * sizeof(Ob*));
    return result;
}

DEF("tupleexpr-split", tplexprSplit, 2, 2) {
    CHECK(0, TupleExpr, expr);
    CHECK_FIXNUM(1, n);

    if (expr == NILexpr) {
        if (n == 0) {
            return Tuple::create(1, NILexpr);
        } else {
            return PRIM_ERROR("can't split");
        }
    }

    const int s = expr->numberOfElements();

    if (n > s) {
        return PRIM_ERROR("can't split");
    }

    TupleExpr* r;
    PROTECT(expr);
    PROTECT(r);
    if (s == n) {
        if (expr->rest == NILexpr) {
            r = NILexpr;
        } else {
            r = TupleExpr::create(0, expr->rest);
        }
    } else {
        r = expr->makeSlice(n, s - n);
        r->rest = expr->rest;
    }

    Tuple* ans = Tuple::create(n + 1, NIV);
    memcpy(&ans->elem(0), &expr->elem(0), n * sizeof(Ob*));
    ans->elem(n) = r;
    return ans;
}

DEF("tupleexpr-head", tplexprHead, 1, 1) {
    CHECK(0, TupleExpr, expr);

    if (expr == NILexpr) {
        return NILexpr;
    } else if (expr->numberOfElements() >= 1) {
        return expr->elem(0);
    } else {
        return PRIM_ERROR("no head");
    }
}

DEF("tupleexpr-tail", tplexprTail, 1, 1) {
    CHECK(0, TupleExpr, expr);

    if (expr == NILexpr) {
        return NILexpr;
    }

    const int n = expr->numberOfElements();
    if (n > 1) {
        PROTECT(expr);
        TupleExpr* ans = expr->makeSlice(1, n - 1);
        ans->rest = expr->rest;
        return ans;
    } else if (expr->rest == NILexpr) {
        return NILexpr;
    } else {
        return TupleExpr::create(0, expr->rest);
    }
}

DEF("tupleexpr-concat", tplexprConcat, 2, 2) {
    CHECK(0, TupleExpr, x1);
    CHECK(1, TupleExpr, x2);
    PROTECT(x1);
    PROTECT(x2);

    int n1 = x1->numberOfElements();
    int n2 = x2->numberOfElements();
    TupleExpr* ans = TupleExpr::create(n1 + n2, x2->rest);
    memcpy(&ans->elem(0), &x1->elem(0), n1 * sizeof(Ob*));
    memcpy(&ans->elem(n1), &x2->elem(0), n2 * sizeof(Ob*));
    ans->rest = x2->rest;
    return ans;
}
