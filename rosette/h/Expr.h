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

#if !defined(_RBL_Expr_h)
#define _RBL_Expr_h

#include "rosette.h"
#include "Ob.h"


class Expr : public Ob {
   protected:
    Expr(int, Ob* = INVALID, Ob* = INVALID);

    virtual bool ConstantP();
    virtual Ob* unquote();
};


class BlockExpr : public Expr {
    STD_DECLS(BlockExpr);

   protected:
    BlockExpr(Tuple*, Ob* = RBLFALSE);

   public:
    Tuple* subExprs;
    Ob* implicit;

    static BlockExpr* create(Tuple*, Ob* = RBLFALSE);

    int numberOfSubExprs();

    virtual AttrNode* makeAttrNode(bool);
    virtual Ob* indexedSize();
    virtual Ob* nth(int);
    virtual Ob* setNth(int, Ob*);
    virtual Ob* subObject(int, int);
};


class FreeExpr : public Expr {
    STD_DECLS(FreeExpr);

   protected:
    FreeExpr(TupleExpr*, Ob*);

   public:
    TupleExpr* freeIds;
    Ob* body;

    static FreeExpr* create(TupleExpr*, Ob*);
    virtual AttrNode* makeAttrNode(bool);
};


class GotoExpr : public Expr {
    STD_DECLS(GotoExpr);

   protected:
    GotoExpr(Ob*);

   public:
    Ob* label;

    static GotoExpr* create(Ob*);
    virtual AttrNode* makeAttrNode(bool);
};


class IfExpr : public Expr {
    STD_DECLS(IfExpr);

   protected:
    IfExpr(Ob*, Ob*, Ob*);

   public:
    Ob* condition;
    Ob* trueBranch;
    Ob* falseBranch;

    static IfExpr* create(Ob*, Ob*, Ob* = INVALID);
    virtual AttrNode* makeAttrNode(bool);
};


class LabelExpr : public Expr {
    STD_DECLS(LabelExpr);

   protected:
    LabelExpr(Ob*, Ob*);

   public:
    Ob* label;
    Ob* body;

    static LabelExpr* create(Ob*, Ob*);
    virtual AttrNode* makeAttrNode(bool);
};


class LetExpr : public Expr {
    STD_DECLS(LetExpr);

   protected:
    LetExpr(TupleExpr*, Ob*);
    LetExpr(int, Ob*, Ob*, TupleExpr*, Ob*);

   public:
    TupleExpr* bindings;
    Ob* body;

    Ob* boundId(int);
    Ob* boundExpr(int);

    static LetExpr* create(TupleExpr*, Ob*);
    virtual AttrNode* makeAttrNode(bool);
};


class LetrecExpr : public LetExpr {
    STD_DECLS(LetrecExpr);

   protected:
    LetrecExpr(TupleExpr*, Ob*);

   public:
    static LetrecExpr* create(TupleExpr*, Ob*);
    virtual AttrNode* makeAttrNode(bool);
};


class MethodExpr : public Expr {
    STD_DECLS(MethodExpr);

   protected:
    MethodExpr(Ob*, Ob*, Ob*);
    MethodExpr(int, Ob*, Ob*, Ob*, Ob*, Ob*);

   public:
    Ob* identity;
    Ob* formals;
    Ob* body;

    static MethodExpr* create(Ob*, Ob*, Ob*);
    virtual AttrNode* makeAttrNode(bool);
};


class NullExpr : public Expr {
    STD_DECLS(NullExpr);

   protected:
    NullExpr();

   public:
    static NullExpr* create();
    virtual AttrNode* makeAttrNode(bool);
};


class ProcExpr : public MethodExpr {
    STD_DECLS(ProcExpr);

   protected:
    ProcExpr(Ob*, Ob*, Ob*);

   public:
    static ProcExpr* create(Ob*, Ob*, Ob*);
    virtual AttrNode* makeAttrNode(bool);
};


class QuoteExpr : public Expr {
    STD_DECLS(QuoteExpr);

   protected:
    QuoteExpr(Ob*);

   public:
    Ob* expr;

    static QuoteExpr* create(Ob*);
    virtual bool ConstantP();
    virtual AttrNode* makeAttrNode(bool);
    virtual Ob* unquote();
};


class ReflectiveMethodExpr : public MethodExpr {
    STD_DECLS(ReflectiveMethodExpr);

   protected:
    ReflectiveMethodExpr(Ob*, Ob*, Ob*);

   public:
    static ReflectiveMethodExpr* create(Ob*, Ob*, Ob*);
    virtual AttrNode* makeAttrNode(bool);
};


class RequestExpr : public Expr {
    STD_DECLS(RequestExpr);

   protected:
    RequestExpr(Ob*, TupleExpr*);
    RequestExpr(int, Ob*, Ob*, Ob*, TupleExpr*);

   public:
    Ob* target;
    TupleExpr* msg;

    static RequestExpr* create(Ob*, TupleExpr*);
    virtual AttrNode* makeAttrNode(bool);
};


class SendExpr : public RequestExpr {
    STD_DECLS(SendExpr);

   protected:
    SendExpr(Ob*, TupleExpr*);

   public:
    static SendExpr* create(Ob*, TupleExpr*);
    virtual AttrNode* makeAttrNode(bool);
};


class SeqExpr : public Expr {
    STD_DECLS(SeqExpr);

   protected:
    SeqExpr(Tuple*);

   public:
    Tuple* subExprs;

    static SeqExpr* create(Tuple*);
    int numberOfSubExprs();
    virtual AttrNode* makeAttrNode(bool);
    virtual Ob* indexedSize();
    virtual Ob* nth(int);
    virtual Ob* setNth(int, Ob*);
    virtual Ob* subObject(int, int);
};


class SetExpr : public Expr {
    STD_DECLS(SetExpr);

   protected:
    SetExpr(Ob*, Ob*);

   public:
    Ob* trgt;
    Ob* val;

    static SetExpr* create(Ob*, Ob*);
    virtual AttrNode* makeAttrNode(bool);
};


class TupleExpr : public Expr {
    STD_DECLS(TupleExpr);

   protected:
    TupleExpr();
    TupleExpr(int, Ob* = NILexpr);

   public:
    Ob* rest;

    static TupleExpr* create();
    static TupleExpr* create(Ob**, int, Ob* = NILexpr);
    static TupleExpr* create(int, Ob* = NILexpr);

    Ob*& elem(int n) {
        return _slot[n + 3];  // skip the meta, parent, and rest fields
    }

    int numberOfElements() {
        return (SIZE(this) - sizeof(TupleExpr)) / sizeof(Ob*);
    }

    bool allPairs();
    bool allSymbols();

    bool ConstantP();
    TupleExpr* cons(Ob*);
    TupleExpr* makeSlice(int, int);
    Pattern* makePattern();
    Template* makeTemplate();
    Ob* cloneTo(Ob*, Ob*);
    AttrNode* makeAttrNode(bool);
    Ob* unquote();
    Ob* indexedSize();
    Ob* nth(int);
    Ob* setNth(int, Ob*);
    Ob* subObject(int, int);
};


#endif
