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

#if !defined(_RBL_Compile_h)
#define _RBL_Compile_h

#include "rosette.h"

#include "BinaryOb.h"
#include "Code.h"
#include "Location.h"
#include "Labels.h"

#include <stdarg.h>
#include <setjmp.h>

/*
 * This disgusting #define is because of the change (in 2.0) in the
 * meaning of "protected".  The items declared protected really shouldn't
 * be visible to the rest of the outside world, but they must be visible
 * to other AttrNodes, since they are the heart and soul of the various
 * decisions that must be made during compilation.  Unfortunately, C++
 * doesn't permit a scoping distinction that allows distinct objects of
 * the same class (or derived classes) access to members while
 * prohibiting that access to objects of unrelated classes.
 * Consequently, I am forced to make these members public.  The protected
 * label is kept as a reminder.
 */
#define protected public

class CompilationUnit : public BinaryOb {
    STD_DECLS(CompilationUnit);

   protected:
    CompilationUnit(Ob*, AttrNode*, CodeBuf*, Tuple*, LabelTable*);

   public:
#ifdef SYSV4
    /* the bsd compat package may do sigsetjmp which uses more space */
    sigjmp_buf abortbuf;
#else
    jmp_buf abortbuf;
#endif
    Ob* info;
    AttrNode* graph;
    CodeBuf* codebuf;
    Tuple* litvec;
    LabelTable* labels;

    static const int MaximumLitVecSize = 0x7f;
    static const int LookupDeferMask = 0x80;

    static CompilationUnit* create(Ob*, Ob*, Ob*);

    int traversePtrs(PSOb__PSOb);
    int traversePtrs(SI__PSOb);
    void traversePtrs(V__PSOb);

    unsigned extendLitvec(Ob*);
    Code* compileExpr(Ob*, Ob* = TopEnv);
    Code* compileBody(Template*, Ob*, Ob* = TopEnv);
    void abort();
    void abort(const char*, ...);
    void warning(const char*, ...);
    void vwarning(const char*, const char*, va_list);

    void atTopLevel();
    Label newLabel();
    void setLabel(Label);
    void registerLabel(Label);
};


enum RtnCode { ImplicitRtn, TaggedRtn, UntaggedRtn };
static const bool CtxtAvailable = true;
static const bool ArgvecAvailable = true;

#define GET_ATTR(x, flag) GET_FLAG((x).word, flag)
#define SET_ATTR(x, flag, val) \
    (val ? SET_FLAG((x).word, flag) : REMOVE_FLAG((x).word, flag))


enum NodeFlag {

    /*
   * topLevel is set if this node is the top node of the attributed
   * graph.  It is useful for suppressing certain warnings that are a
   * nuisance for top-level expressions.
   */
    f_topLevel = BITS(Ob*) - (2 * BITS(uint8_t) + 6) + 1,
    /*
     * valueContext is true if the expression represented by this node is
     * in a position where it is expected to produce a value.
     */
    f_valueContext,

    /*
     * producesValue is a flag that indicates whether the expression
     * represented by this node will return a value to its surrounding
     * context.  This is important for making seq and block expressions
     * work correctly in the presence of asynchronous interactions.
     */

    f_producesValue,
    /*
     * simpleNode indicates whether the node describes a symbol or a
     * literal.
     */
    f_simpleNode,
    /*
     * inlineableNode is true if the expression can be evaluated in the
     * same ctxt as the enclosing expression, i.e, if it is not a
     * suspending sub-expression.  For example, this is true for request
     * expressions whose target is a symbol whose compile-time binding is
     * a primitive function such as fx+.  All simple sub-expressions
     * (i.e., symbols and literals) are by definition inlineable.
     */
    f_inlineableNode
};

class AttrNode : public BinaryOb {
    STD_DECLS(AttrNode);

   protected:
    /*
     * av_size is the argvec size required for the evaluation of this
     * expression.  It includes the requirements for all inlined
     * subexpressions.
     */

    uint8_t av_size;

    /*
     * outstanding is a count of the number suspending (not inlineable)
     * sub-expressions contained by this expression.
     */

    uint8_t outstanding;

    unsigned short word;

    Location dest;
    Label resume;
    bool deferredLookup;

    /*
     * The gc routines (traversPtrs and friends) use the address of the
     * member variable cu as the starting point for "significant"
     * pointers in an AttrNode, i.e., they interpret all words at cu and
     * beyond as potential pointers to be followed.  Any member variables
     * that are to be ignored during scavenges and gcs should be placed
     * in front of cu; any member variables (i.e., pOb values) that are
     * to be examined should be placed after cu.
     */

    CompilationUnit* cu;

    AttrNode(int, bool);

    int traversePtrs(PSOb__PSOb);
    int traversePtrs(SI__PSOb);
    void traversePtrs(V__PSOb);

    virtual void changeDest(Location&);
    virtual void emitDispatchCode(bool, bool, RtnCode, Label);
    virtual void emitResumeCode(RtnCode);

    unsigned getDestOffset();
    void emitAlloc(unsigned);
    void emitApplyPrim(unsigned, unsigned, bool, RtnCode, Label);
    void emitExtend(Template*);
    void emitLit(Ob*);
    void emitLookup(Ob*);
    void emitOpAndLabel(Opcode, Label);
    void emitOpAndLabel(Opcode, uint8_t, Label);
    void emitOpAndLabel(Opcode, Ob*);
    void emitOpAndLabel(Opcode, uint8_t, Ob*);
    void emitOutstanding();
    void emitPush(int);
    void emitStore(Label);
    void emitRtn(RtnCode, Label);
    void emitImplicitRtn(Label);
    void emitTaggedRtn(Label);
    void emitUntaggedRtn(Label);
    void emitXfer(Location);

    virtual int primNumber();

    void emitF0(Opcode, unsigned = 0);
    void emitF1(Opcode, unsigned, unsigned);
    void emitF2(Opcode, unsigned, unsigned);
    void emitF3(Opcode, unsigned, unsigned, unsigned);
    void emitF4(Opcode, unsigned, unsigned, unsigned, unsigned);
    void emitF5(Opcode, unsigned, unsigned, unsigned = 0);
    void emitF6(Opcode, unsigned);
    void emitF7(Opcode, unsigned, unsigned, unsigned, unsigned);
    void emitE0(unsigned, unsigned);
    void emitE1(unsigned);
    void emitE2(unsigned, unsigned);

    friend class CompilationUnit;

   public:
    virtual void initialize(Ob*, Ob*, Location, CompilationUnit*);
};


class ConstNode : public AttrNode {
    STD_DECLS(ConstNode);

   protected:
    Ob* val;

    ConstNode(Ob*, bool);

    void emitDispatchCode(bool, bool, RtnCode, Label);
    int primNumber();

   public:
    static ConstNode* create(Ob*, bool);

    void initialize(Ob*, Ob*, Location, CompilationUnit*);
};


class SymbolNode : public AttrNode {
    STD_DECLS(SymbolNode);

   protected:
    Location loc;
    Ob* sym;

    SymbolNode(Ob*, bool);

    void emitDispatchCode(bool, bool, RtnCode, Label);
    int primNumber();

    friend class SetNode;

   public:
    static SymbolNode* create(Ob*, bool);

    void initialize(Ob*, Ob*, Location, CompilationUnit*);
};


class FreeNode : public AttrNode {
    STD_DECLS(FreeNode);

   protected:
    FreeExpr* expr;
    AttrNode* body;

    FreeNode(FreeExpr*, bool);

    virtual void changeDest(Location&);
    virtual void emitDispatchCode(bool, bool, RtnCode, Label);
    virtual void emitResumeCode(RtnCode);
    virtual int primNumber();

   public:
    static FreeNode* create(FreeExpr*, bool);

    virtual void initialize(Ob*, Ob*, Location, CompilationUnit*);
};


class NullNode : public AttrNode {
    STD_DECLS(NullNode);

   protected:
    NullNode(bool);

    void emitDispatchCode(bool, bool, RtnCode, Label);

   public:
    static NullNode* create(bool);
};


class XferNode : public AttrNode {
    STD_DECLS(XferNode);

   protected:
    Ob* source;

    XferNode(int);

    void emitDispatchCode(bool, bool, RtnCode, Label);

   public:
    static XferNode* create(int);
};


struct ExprStack {
    Tuple* exprs;
    Ob* top;

    ExprStack();
};

class CompoundNode : public AttrNode {
    STD_DECLS(CompoundNode);

   protected:
    ExprStack simple;
    ExprStack inlined;
    ExprStack nested;

    CompoundNode(int, bool);

    void addTo(ExprStack*, AttrNode*);
    void analyze(AttrNode*);
    void rearrangeInlinedExprs();
    int determineFree(ArgNum*);
    void sortInlinedExprs();
    void fixInlinedConflicts(ArgNum*, int);
    void emitSimpleExprDispatchCode(RtnCode, Label);
    void emitInlinedExprDispatchCode(RtnCode, Label);
    virtual void emitNestedExprDispatchCode(RtnCode, Label);
    void emitNestedExprResumeCode(RtnCode);

    virtual int numberOfSubExprs();
    virtual void emitDispatchCode(bool, bool, RtnCode, Label);
    virtual void emitResumeCode(RtnCode);
    virtual void emitPrefix(bool, bool);
    virtual void emitWrapup(RtnCode, Label);
};


class BlockNode : public CompoundNode {
    STD_DECLS(BlockNode);

   protected:
    BlockExpr* expr;

    BlockNode(BlockExpr*, bool);

    virtual void changeDest(Location&);
    virtual int numberOfSubExprs();
    virtual void emitDispatchCode(bool, bool, RtnCode, Label);
    virtual void emitNestedExprDispatchCode(RtnCode, Label);
    virtual void emitResumeCode(RtnCode);

   public:
    static BlockNode* create(BlockExpr*, bool);

    virtual void initialize(Ob*, Ob*, Location, CompilationUnit*);
};


class RequestNode : public CompoundNode {
    STD_DECLS(RequestNode);

   protected:
    RequestExpr* expr;
    AttrNode* trgtNode;
    Ob* primTrgt;

    RequestNode(RequestExpr*, bool);
    RequestNode(int, RequestExpr*, bool);

    virtual int numberOfSubExprs();
    virtual void emitWrapup(RtnCode, Label);
    virtual void emitXmit(unsigned, bool, RtnCode, Label);

   public:
    static RequestNode* create(RequestExpr*, bool);

    virtual void initialize(Ob*, Ob*, Location, CompilationUnit*);
};


class SendNode : public RequestNode {
    STD_DECLS(SendNode);

   protected:
    SendNode(SendExpr*, bool);

   public:
    static SendNode* create(SendExpr*, bool);

    virtual void initialize(Ob*, Ob*, Location, CompilationUnit*);
};


class TupleNode : public CompoundNode {
    STD_DECLS(TupleNode);

   protected:
    TupleExpr* expr;

    TupleNode(TupleExpr*, bool);

    virtual int numberOfSubExprs();
    virtual void emitDispatchCode(bool, bool, RtnCode, Label);
    virtual void emitWrapup(RtnCode, Label);

   public:
    static TupleNode* create(TupleExpr*, bool);

    virtual void initialize(Ob*, Ob*, Location, CompilationUnit*);
};


class IfNode : public CompoundNode {
    STD_DECLS(IfNode);

   protected:
    IfExpr* expr;
    AttrNode* trueNode;
    AttrNode* falseNode;

    IfNode(IfExpr*, bool);

    virtual void changeDest(Location&);
    virtual int numberOfSubExprs();
    virtual void emitResumeCode(RtnCode);
    virtual void emitWrapup(RtnCode, Label);

   public:
    static IfNode* create(IfExpr*, bool);

    virtual void initialize(Ob*, Ob*, Location, CompilationUnit*);
};


class LetNode : public CompoundNode {
    STD_DECLS(LetNode);

   protected:
    LetExpr* expr;
    Template* templat;  // AT&T usurped the id "template".
    AttrNode* bodyNode;

    LetNode(LetExpr*, bool);
    LetNode(int, bool, LetExpr*);

    virtual void changeDest(Location&);

    virtual Ob* letEnv(Ob*);
    virtual Location ithLoc(int);
    virtual int numberOfSubExprs();
    virtual void emitResumeCode(RtnCode);
    virtual void emitWrapup(RtnCode, Label);

   public:
    static LetNode* create(LetExpr*, bool);

    virtual void initialize(Ob*, Ob*, Location, CompilationUnit*);
};


class LetrecNode : public LetNode {
    STD_DECLS(LetrecNode);

   protected:
    LetrecNode(LetrecExpr*, bool);

    virtual Ob* letEnv(Ob*);
    virtual Location ithLoc(int);
    virtual void emitDispatchCode(bool, bool, RtnCode, Label);
    virtual void emitWrapup(RtnCode, Label);

   public:
    static LetrecNode* create(LetrecExpr*, bool);
};


class MethodNode : public AttrNode {
    STD_DECLS(MethodNode);

   protected:
    MethodExpr* expr;
    Code* code;

    MethodNode(MethodExpr*, bool);
    MethodNode(int, MethodExpr*, bool);

    virtual Template* adjustFormals();
    virtual int constructor();
    virtual Code* compileBody(Ob*, Ob*);
    virtual void emitDispatchCode(bool, bool, RtnCode, Label);

   public:
    static MethodNode* create(MethodExpr*, bool);

    virtual void initialize(Ob*, Ob*, Location, CompilationUnit*);
};


class ReflectiveMethodNode : public MethodNode {
    STD_DECLS(ReflectiveMethodNode);

   protected:
    ReflectiveMethodNode(ReflectiveMethodExpr*, bool);

    virtual Template* adjustFormals();
    virtual int constructor();

   public:
    static ReflectiveMethodNode* create(ReflectiveMethodExpr*, bool);
};


class ProcNode : public MethodNode {
    STD_DECLS(ProcNode);

   protected:
    ProcNode(ProcExpr*, bool);

    virtual Template* adjustFormals();
    virtual int constructor();
    virtual void emitDispatchCode(bool, bool, RtnCode, Label);

   public:
    static ProcNode* create(ProcExpr*, bool);

    virtual void initialize(Ob*, Ob*, Location, CompilationUnit*);
};


class SeqNode : public CompoundNode {
    STD_DECLS(SeqNode);

   protected:
    AttrNode* first;
    AttrNode* second;

    SeqNode(AttrNode*, AttrNode*, bool);

    virtual void changeDest(Location&);
    virtual int numberOfSubExprs();
    virtual void emitResumeCode(RtnCode);
    virtual void emitWrapup(RtnCode, Label);

   public:
    static SeqNode* create(AttrNode*, AttrNode*, bool);

    virtual void initialize(Ob*, Ob*, Location, CompilationUnit*);
};


class SetNode : public CompoundNode {
    STD_DECLS(SetNode);

   protected:
    SetExpr* expr;
    SymbolNode* trgtNode;
    AttrNode* valNode;

    SetNode(SetExpr*, bool);

    virtual int numberOfSubExprs();
    virtual void emitResumeCode(RtnCode);
    virtual void emitWrapup(RtnCode, Label);

   public:
    static SetNode* create(SetExpr*, bool);

    virtual void initialize(Ob*, Ob*, Location, CompilationUnit*);
};


class GotoNode : public AttrNode {
    STD_DECLS(GotoNode);

   protected:
    enum CompilerFakery { MaximumCut = (1 << BITS(uint8_t)) - 1 };

    Ob* labelName;
    LabelNode* labelNode;
    Ob* ctEnv;

    GotoNode(GotoExpr*, bool);

    virtual void emitDispatchCode(bool, bool, RtnCode, Label);

   public:
    static GotoNode* create(GotoExpr*, bool);

    virtual void initialize(Ob*, Ob*, Location, CompilationUnit*);
};


class LabelNode : public CompoundNode {
    STD_DECLS(LabelNode);

   protected:
    LabelExpr* expr;
    AttrNode* bodyNode;
    Ob* label;
    Ob* ctEnv;

    LabelNode(LabelExpr*, bool);

    virtual void changeDest(Location&);
    virtual int numberOfSubExprs();
    virtual void emitDispatchCode(bool, bool, RtnCode, Label);
    virtual void emitResumeCode(RtnCode);
    virtual int primNumber();

    friend class LabelTable;
    friend class GotoNode;

   public:
    static LabelNode* create(LabelExpr*, bool);

    virtual void initialize(Ob*, Ob*, Location, CompilationUnit*);
};

#undef protected

#endif
