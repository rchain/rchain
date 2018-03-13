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

#include "Pattern.h"
#include "Expr.h"
#include "Meta.h"
#include "Tuple.h"
#include "BuiltinClass.h"
#include "ModuleInit.h"

#include <assert.h>

pOb NILmeta;
CompoundPattern* NILpattern;
Template* NILtemplate;

Pattern::Pattern(int sz, Ob* meta, Ob* parent) : Ob(sz, meta, parent) {}


UNIMPLEMENTED_VOID(Pattern, stuffKeys, (Tuple*, int));
UNIMPLEMENTED(bool, Pattern, matchIntoArgvec, (Tuple*, int, Ob*, int));


bool Pattern::fail(Ob*) { return false; }


int Pattern::numberOfKeys(void) {
    NI("numberOfKeys");
    return 0;
}


Ob* Pattern::cloneTo(Ob* new_meta, Ob* new_parent) {
    return (this == NILpattern ? (Ob*)this : Ob::cloneTo(new_meta, new_parent));
}


BUILTIN_CLASS(IdPattern) { OB_FIELD("symbol", IdPattern, symbol); }


IdPattern::IdPattern(Ob* sym)
    : Pattern(sizeof(IdPattern), CLASS_META(IdPattern), CLASS_SBO(IdPattern)),
      symbol(sym) {
    IdPattern::updateCnt();
}


IdPattern* IdPattern::create(Ob* sym) {
    void* loc = PALLOC1(sizeof(IdPattern), sym);
    return new (loc) IdPattern(sym);
}


void IdPattern::stuffKeys(Tuple* keys, int offset) {
    ASSIGN(keys, elem(offset), symbol);
}


bool IdPattern::matchIntoArgvec(Tuple* argvec, int offset, Ob* val, int) {
    ASSIGN(argvec, elem(offset), val);
    return true;
}


int IdPattern::numberOfKeys(void) { return 1; }


BUILTIN_CLASS(ConstPattern) { OB_FIELD("val", ConstPattern, val); }


ConstPattern::ConstPattern(Ob* val)
    : Pattern(sizeof(ConstPattern), CLASS_META(ConstPattern),
              CLASS_SBO(ConstPattern)),
      val(val) {
    ConstPattern::updateCnt();
}


ConstPattern* ConstPattern::create(Ob* val) {
    void* loc = PALLOC1(sizeof(ConstPattern), val);
    return new (loc) ConstPattern(val);
}


bool ConstPattern::matchIntoArgvec(Tuple* argvec, int offset, Ob* val, int) {
    if (val == this->val) {
        ASSIGN(argvec, elem(offset), val);
        return true;
    } else {
        return fail(val);
    }
}


int ConstPattern::numberOfKeys(void) { return 1; }


void ConstPattern::stuffKeys(Tuple*, int) {}


CompoundPattern::CompoundPattern(int sz, Ob* meta, Ob* parent, TupleExpr* e)
    : Pattern(sz, meta, parent), expr(e) {}


int CompoundPattern::keyExtent(int) {
    NI("keyExtent");
    return 0;
}


Tuple* CompoundPattern::match(Tuple*, int) {
    NI("match");
    return (Tuple*)INVALID;
}


BUILTIN_CLASS(IdVecPattern) { OB_FIELD("expr", IdVecPattern, expr); }


IdVecPattern::IdVecPattern(TupleExpr* te)
    : CompoundPattern(sizeof(IdVecPattern), CLASS_META(IdVecPattern),
                      CLASS_SBO(IdVecPattern), te) {
    IdVecPattern::updateCnt();
}


IdVecPattern* IdVecPattern::create(TupleExpr* te) {
    void* loc = PALLOC1(sizeof(IdVecPattern), te);
    return new (loc) IdVecPattern(te);
}


int IdVecPattern::numberOfKeys(void) { return expr->numberOfElements(); }


void IdVecPattern::stuffKeys(Tuple* keys, int offset) {
    int n = expr->numberOfElements();
    while (n--) {
        ASSIGN(keys, elem(offset + n), expr->elem(n));
    }
}


int IdVecPattern::keyExtent(int componentNum) { return componentNum; }


Tuple* IdVecPattern::match(Tuple* argvec, int nargs) {
    int need = expr->numberOfElements();
    return (need == nargs ? argvec : (Tuple*)INVALID);
}


bool IdVecPattern::matchIntoArgvec(Tuple* argvec, int offset, Ob* val,
                                   int nargs) {
    int need = expr->numberOfElements();

    if (!IS_A(val, Tuple)) {
        return fail(val);
    }

    Tuple* valvec = (Tuple*)val;

    if (nargs < 0) {
        nargs = valvec->numberOfElements();
    }

    if (need == nargs) {
        while (need--) {
            ASSIGN(argvec, elem(need + offset), valvec->elem(need));
        }
        return true;
    } else {
        return fail(val);
    }
}


BUILTIN_CLASS(IdAmperRestPattern) {
    OB_FIELD("expr", IdAmperRestPattern, expr);
}


IdAmperRestPattern::IdAmperRestPattern(TupleExpr* te)
    : CompoundPattern(sizeof(IdAmperRestPattern),
                      CLASS_META(IdAmperRestPattern),
                      CLASS_SBO(IdAmperRestPattern), te) {
    IdAmperRestPattern::updateCnt();
}


IdAmperRestPattern* IdAmperRestPattern::create(TupleExpr* te) {
    void* loc = PALLOC1(sizeof(IdAmperRestPattern), te);
    return new (loc) IdAmperRestPattern(te);
}


void IdAmperRestPattern::stuffKeys(Tuple* keys, int offset) {
    int n = expr->numberOfElements();
    ASSIGN(keys, elem(offset + n), expr->rest);
    while (n--) {
        ASSIGN(keys, elem(offset + n), expr->elem(n));
    }
}


Tuple* IdAmperRestPattern::match(Tuple* argvec, int nargs) {
    int need = expr->numberOfElements();

    if (nargs >= need) {
        PROTECT(argvec);
        Tuple* result = Tuple::create(need + 1, argvec, 0, need);
        PROTECT(result);
        Tuple* rest = argvec->makeSlice(need, nargs - need);
        ASSIGN(result, elem(need), rest);
        return result;
    } else {
        return (Tuple*)INVALID;
    }
}


bool IdAmperRestPattern::matchIntoArgvec(Tuple* argvec, int offset, Ob* val,
                                         int nargs) {
    int need = expr->numberOfElements();

    if (!IS_A(val, Tuple)) {
        return fail(val);
    }

    Tuple* valvec = (Tuple*)val;

    if (nargs < 0) {
        nargs = valvec->numberOfElements();
    }

    if (nargs >= need) {
        PROTECT(argvec);
        PROTECT(valvec);
        Tuple* rest = valvec->makeSlice(need, nargs - need);
        ASSIGN(argvec, elem(need + offset), rest);
        while (need--) {
            ASSIGN(argvec, elem(need + offset), valvec->elem(need));
        }

        return true;
    } else {
        return fail(val);
    }
}


int IdAmperRestPattern::numberOfKeys(void) {
    return expr->numberOfElements() + 1;
}


int IdAmperRestPattern::keyExtent(int componentNum) { return componentNum; }


BUILTIN_CLASS(ComplexPattern) {
    OB_FIELD("expr", ComplexPattern, expr);
    OB_FIELD("patvec", ComplexPattern, patvec);
    OB_FIELD("offsetvec", ComplexPattern, offsetvec);
}


ComplexPattern::ComplexPattern(TupleExpr* te, Tuple* pv, Tuple* ov)
    : CompoundPattern(sizeof(ComplexPattern), CLASS_META(ComplexPattern),
                      CLASS_SBO(ComplexPattern), te),
      patvec(pv),
      offsetvec(ov) {
    ComplexPattern::updateCnt();
}


ComplexPattern* ComplexPattern::create(TupleExpr* te) {
    int n = te->numberOfElements();
    PROTECT(te);
    Tuple* pv = Tuple::create(n, INVALID);
    PROTECT(pv);
    Tuple* ov = Tuple::create(n + 1, INVALID);
    PROTECT(ov);

    int offset = 0;
    ov->elem(0) = FIXNUM(0);
    for (int i = 0; i < n; i++) {
        Pattern* pat = BASE(te->elem(i))->makePattern();
        ASSIGN(pv, elem(i), pat);
        offset += pat->numberOfKeys();
        ov->elem(i + 1) = FIXNUM(offset);
    }

    void* loc = PALLOC(sizeof(ComplexPattern));
    return new (loc) ComplexPattern(te, pv, ov);
}


int ComplexPattern::numberOfKeys(void) {
    return ComplexPattern::keyExtent(patvec->numberOfElements()) +
           IS_SYM(expr->rest);
}


void ComplexPattern::stuffKeys(Tuple* keys, int offset) {
    int n = patvec->numberOfElements();
    if (expr->rest != NILexpr) {
        ASSIGN(keys, elem(offset + FIXVAL(offsetvec->elem(n))), expr->rest);
    }

    while (n--) {
        ((Pattern*)patvec->elem(n))
            ->stuffKeys(keys, offset + FIXVAL(offsetvec->elem(n)));
    }
}


int ComplexPattern::keyExtent(int componentNum) {
    return FIXVAL(offsetvec->elem(componentNum));
}


Tuple* ComplexPattern::match(Tuple* argvec, int nargs) {
    // This code assumes that nargs > 0, so that "Tuple::create"
    // doesn't need to be guarded for NIL.

    PROTECT_THIS(ComplexPattern);
    PROTECT(argvec);
    Tuple* result = Tuple::create(SELF->numberOfKeys(), INVALID);
    PROTECT(result);

    bool success = SELF->matchIntoArgvec(result, 0, argvec, nargs);

    if (!success) {
        result = (Tuple*)INVALID;
    }

    return result;
}


bool ComplexPattern::matchIntoArgvec(Tuple* argvec, int offset, Ob* val,
                                     int nargs) {
    int need = patvec->numberOfElements();

    if (!IS_A(val, Tuple)) {
        return fail(val);
    }

    Tuple* valvec = (Tuple*)val;

    if (nargs < 0) {
        nargs = valvec->numberOfElements();
    }

    bool matched = true;
    PROTECT_THIS(ComplexPattern);
    PROTECT(argvec);
    PROTECT(valvec);

    if (expr->rest == NILexpr && nargs == need) {
        while (need-- && matched) {
            Pattern* pat = (Pattern*)SELF->patvec->elem(need);
            matched = pat->matchIntoArgvec(
                argvec, offset + SELF->keyExtent(need), valvec->elem(need));
        }
    } else if (expr->rest != NILexpr && nargs >= need) {
        Tuple* rest = valvec->makeSlice(need, nargs - need);
        ASSIGN(argvec, elem(offset + SELF->keyExtent(need)), rest);
        while (need-- && matched) {
            Pattern* pat = (Pattern*)SELF->patvec->elem(need);
            matched = pat->matchIntoArgvec(
                argvec, offset + SELF->keyExtent(need), valvec->elem(need));
        }
    } else {
        matched = fail(val);
    }

    return matched;
}


BUILTIN_CLASS(Template) {
    OB_FIELD("keys", Template, keytuple);
    OB_FIELD("pattern", Template, pat);
    OB_FIELD("keymeta", Template, keymeta);
}


Template::Template(Tuple* keytuple, Ob* keymeta, CompoundPattern* pat)
    : Ob(sizeof(Template), CLASS_META(Template), CLASS_SBO(Template)),
      keytuple(keytuple),
      pat(pat),
      keymeta(keymeta) {
    Template::updateCnt();
}


Template* Template::create(TupleExpr* te) {
    assert(te != NILexpr);

    CompoundPattern* pat;

    if (te->allSymbols()) {
        if (te->rest == NILexpr) {
            pat = IdVecPattern::create(te);
        } else {
            pat = IdAmperRestPattern::create(te);
        }
    } else {
        pat = ComplexPattern::create(te);
    }

    PROTECT(pat);
    Tuple* keys = Tuple::create(pat->numberOfKeys(), INVALID);
    pat->stuffKeys(keys, 0);
    PROTECT(keys);
    StdMeta* keymeta = StdMeta::create(keys, FIXNUM(1), RBLFALSE);

    void* loc = PALLOC1(sizeof(Template), keymeta);
    return new (loc) Template(keys, keymeta, pat);
}


Template* Template::create(Tuple* keys, Ob* keymeta, CompoundPattern* pat) {
    /*
     * This version of create should only be used when making the
     * NILtemplate.
     */

    void* loc = PALLOC3(sizeof(Template), keys, keymeta, pat);
    return new (loc) Template(keys, keymeta, pat);
}


Ob* Template::fail(Ob*) { return INVALID; }


Ob* Template::cloneTo(Ob* new_meta, Ob* new_parent) {
    if (this == NILtemplate) {
        return (Ob*)this;
    } else {
        return Ob::cloneTo(new_meta, new_parent);
    }
}


Pattern* TupleExpr::makePattern() {
    if (this == NILexpr) {
        return NILpattern;
    }

    int n = numberOfElements();

    while (n--) {
        if (!IS_SYM(elem(n))) {
            return ComplexPattern::create(this);
        }
    }

    if (rest == NILexpr) {
        return IdVecPattern::create(this);
    } else if (IS_SYM(rest)) {
        return IdAmperRestPattern::create(this);
    } else {
        return ComplexPattern::create(this);
    }
}


Template* TupleExpr::makeTemplate() {
    if (this == NILexpr) {
        return NILtemplate;
    } else if (rest != NILexpr && !IS_SYM(rest)) {
        return (Template*)INVALID;
    } else {
        return Template::create(this);
    }
}


MODULE_INIT(Pattern) {
    NILmeta = heap->tenure(StdMeta::create(NIL));
    NILpattern = (CompoundPattern*)heap->tenure(IdVecPattern::create(NILexpr));
    NILtemplate =
        (Template*)heap->tenure(Template::create(NIL, NILmeta, NILpattern));
}
