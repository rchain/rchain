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

#include "Compile.h"

#include "Code.h"
#include "Expr.h"
#include "Interrupt.h"
#include "Location.h"
#include "Ob.h"
#include "Pattern.h"
#include "Prim.h"
#include "Tuple.h"
#include "Vm.h"
#include "Export.h"

#include "BuiltinClass.h"
#include "ModuleInit.h"

#include <algorithm>

#include <assert.h>
#include <setjmp.h>
#include <stdarg.h>


#if defined(__GNUG__)

#define SETJMP _setjmp
#define LONGJMP _longjmp

/*
 * In the GNU library, these versions of setjmp and longjmp don't worry
 * about restoring the signal mask, which makes them *considerably*
 * faster that the usual versions (approx 25 us versus 290 us for the
 * usual one).
 */

/*
 * When you port this code to a new compiler or system, it is worth your
 * time to check and see if setjmp is a lot slower than _setjmp.  Since
 * SETJMP is called every time a CompilationUnit::compileExpr or
 * CompilationUnit::compileBody is called (i.e., a lot), this can have a
 * bearing on the Rosette compiler's performance.
 */
/*
extern "C" {
    int _setjmp (jmp_buf);
    void _longjmp (jmp_buf, int);
}
*/

#else

#define SETJMP setjmp
#define LONGJMP longjmp

#endif

extern int DeferLookupFlag;

extern Prim* tplNew;
extern Prim* tplConsStar;
extern Prim* tplConcat;
extern Tuple* rcons(Tuple*, pOb);


BUILTIN_CLASS(AttrNode) {}
BUILTIN_CLASS(ConstNode) {}
BUILTIN_CLASS(SymbolNode) {}
BUILTIN_CLASS(FreeNode) {}
BUILTIN_CLASS(NullNode) {}
BUILTIN_CLASS(XferNode) {}
BUILTIN_CLASS(CompoundNode) {}
BUILTIN_CLASS(BlockNode) {}
BUILTIN_CLASS(RequestNode) {}
BUILTIN_CLASS(SendNode) {}
BUILTIN_CLASS(TupleNode) {}
BUILTIN_CLASS(IfNode) {}
BUILTIN_CLASS(LetNode) {}
BUILTIN_CLASS(LetrecNode) {}
BUILTIN_CLASS(MethodNode) {}
BUILTIN_CLASS(ReflectiveMethodNode) {}
BUILTIN_CLASS(ProcNode) {}
BUILTIN_CLASS(SeqNode) {}
BUILTIN_CLASS(SetNode) {}
BUILTIN_CLASS(GotoNode) {}
BUILTIN_CLASS(LabelNode) {}


AttrNode::AttrNode(int sz, bool valueContext)
    : BinaryOb(sz, CLASS_META(AttrNode), CLASS_SBO(AttrNode)),
      av_size(0),
      outstanding(0),
      word(0),
      dest(LocRslt),
      resume(NoParticularLabel),
      deferredLookup(false),
      cu((CompilationUnit*)INVALID) {
    SET_ATTR(*this, f_valueContext, valueContext);
    SET_FLAG(word, f_producesValue);
}


void AttrNode::initialize(pOb, pOb, Location dest, CompilationUnit* cu) {
    if (interruptPending) {
        /*
         * Don't reset the interruptPending flag here, so that it will
         * still be set for the virtual machine to field and deal with.
         */
        cu->abort();
    }

    this->dest = dest;
    ASSIGN(this, cu, cu);
}


int AttrNode::traversePtrs(PSOb__PSOb f) {
    int sum = BinaryOb::traversePtrs(f);
    pOb* p = &slot(SLOT_NUM(AttrNode, cu));
    const pOb* end = endp();

    for (; p < (pOb*)end; p++) {
        sum += useIfPtr(p, f);
    }

    return sum;
}


int AttrNode::traversePtrs(SI__PSOb f) {
    int sum = BinaryOb::traversePtrs(f);
    pOb* p = &slot(SLOT_NUM(AttrNode, cu));
    const pOb* end = endp();

    for (; p < (pOb*)end; p++) {
        sum += useIfPtr(*p, f);
    }

    return sum;
}


void AttrNode::traversePtrs(V__PSOb f) {
    BinaryOb::traversePtrs(f);

    pOb* p = &slot(SLOT_NUM(AttrNode, cu));
    const pOb* end = endp();

    for (; p < (pOb*)end; p++) {
        useIfPtr(*p, f);
    }
}


void AttrNode::emitF0(Opcode opcode, unsigned op) {
    cu->codebuf->emitF0(opcode, op);
}


void AttrNode::emitF1(Opcode opcode, unsigned op0, unsigned op1) {
    cu->codebuf->emitF1(opcode, op0, op1);
}


void AttrNode::emitF2(Opcode opcode, unsigned op0, unsigned op1) {
    cu->codebuf->emitF2(opcode, op0, op1);
}


void AttrNode::emitF3(Opcode opcode, unsigned op0, unsigned op1, unsigned op2) {
    cu->codebuf->emitF3(opcode, op0, op1, op2);
}


void AttrNode::emitF4(Opcode opcode, unsigned unwind, unsigned next,
                      unsigned nargs, unsigned op0) {
    cu->codebuf->emitF4(opcode, unwind, next, nargs, op0);
}


void AttrNode::emitF5(Opcode opcode, unsigned unwind, unsigned next,
                      unsigned op0) {
    cu->codebuf->emitF5(opcode, unwind, next, op0);
}


void AttrNode::emitF6(Opcode opcode, unsigned pc) {
    cu->codebuf->emitF6(opcode, pc);
}


void AttrNode::emitF7(Opcode opcode, unsigned indirect, unsigned level,
                      unsigned offset, unsigned op0) {
    cu->codebuf->emitF7(opcode, indirect, level, offset, op0);
}


void AttrNode::emitE0(unsigned op0, unsigned op1) {
    cu->codebuf->emitE0(op0, op1);
}


void AttrNode::emitE1(unsigned op0) { cu->codebuf->emitE1(op0); }

void AttrNode::emitE2(unsigned op0, unsigned op1) {
    cu->codebuf->emitE2(op0, op1);
}

void AttrNode::changeDest(Location& newloc) { dest = newloc; }


void AttrNode::emitDispatchCode(bool, bool, RtnCode, Label) {
    NI("emitDispatchCode");
}


void AttrNode::emitResumeCode(RtnCode rtn) {}


unsigned AttrNode::getDestOffset() { return cu->extendLitvec(dest.atom); }


void AttrNode::emitAlloc(unsigned n) {
    if (n == 0) {
        return;
    } else if (n < 256) {
        emitF0(opAlloc, n);
    } else {
        cu->abort("argvec size (%d) too large for alloc instruction", n);
    }
}


void AttrNode::emitApplyPrim(unsigned primnum, unsigned nargs, bool unwind,
                             RtnCode rtn, Label next) {
    PROTECT_THIS(AttrNode);

    Opcode opcode;
    unsigned op;

    if (dest == LocLimbo) {
        SELF->emitF5(opApplyCmd, unwind, next == NoneRemaining, nargs);
        SELF->emitE2(primnum, 0);
    } else {
        switch (rtn) {
        case ImplicitRtn:

            switch (GET_GENERIC_TYPE(dest)) {
            case LT_CtxtRegister:
                opcode = opApplyPrimReg;
                op = GET_CTXTREG_INDEX(dest);
                break;

            case LT_ArgRegister:
                opcode = opApplyPrimArg;
                op = GET_ARGREG_INDEX(dest);
                break;

            default:
                opcode = opApplyPrimTag;
                op = cu->extendLitvec(dest.atom);
                break;
            }
            SELF->emitF5(opcode, unwind, next == NoneRemaining, nargs);
            SELF->emitE2(primnum, op);
            break;

        case UntaggedRtn:

            switch (GET_GENERIC_TYPE(dest)) {
            case LT_CtxtRegister:
                opcode = opApplyPrimReg;
                op = GET_CTXTREG_INDEX(dest);
                break;

            case LT_ArgRegister:
                opcode = opApplyPrimArg;
                op = GET_ARGREG_INDEX(dest);
                break;

            default:
                opcode = opApplyPrimTag;
                op = cu->extendLitvec(dest.atom);
                break;
            }
            SELF->emitF5(opcode, unwind, false, nargs);
            SELF->emitE2(primnum, op);
            SELF->emitUntaggedRtn(next);
            break;

        case TaggedRtn:

            SELF->emitF5(opApplyPrimReg, unwind, false, nargs);
            SELF->emitE2(primnum, CRN_Rslt);
            SELF->emitTaggedRtn(next);
            break;
        }
    }
}


void AttrNode::emitExtend(Template* templat) {
    PROTECT_THIS(AttrNode);
    unsigned offset = SELF->cu->extendLitvec(templat);
    SELF->emitF0(opExtend, offset);
}


void AttrNode::emitLit(pOb val) {
    int n = -1;
    const LocationType locType = (LocationType)GET_GENERIC_TYPE(dest);
    const int argno = GET_ARGREG_INDEX(dest);
    const int regno = GET_CTXTREG_INDEX(dest);


    for (int i = 0; i < 16; i++) {
        if (VirtualMachine::vmLiterals[i] == val) {
            n = i;
        }
    }

    if (n >= 0) {
        switch (locType) {
        case LT_ArgRegister:
            emitF2(opImmediateLitToArg, n, argno);
            return;
        case LT_CtxtRegister:
            emitF2(opImmediateLitToReg, n, regno);
            return;
        default: {
            PROTECT_THIS(AttrNode);
            unsigned offset = SELF->cu->extendLitvec(SELF->dest.atom);
            SELF->emitF2(opImmediateLitToReg, n, CRN_Rslt);
            SELF->emitF0(opXferRsltToDest, offset);
            return;
        }
        }
    }

    /*
     * If we get here, the literal that we need to emit is not one that
     * is specially recognized in the instruction set, and we need to use
     * the more general indirect literal instructions.  The exact
     * instruction (or sequence of instructions) chosen is a function of
     * size of the index into the litvec and the destination.
     */

    PROTECT_THIS(AttrNode);
    unsigned valOffset = SELF->cu->extendLitvec(val);

    if (locType == LT_CtxtRegister && regno == CRN_Rslt) {
        SELF->emitF0(opIndLitToRslt, valOffset);
        return;
    } else if (valOffset < 16) {
        if (locType == LT_ArgRegister && argno < 16) {
            SELF->emitF1(opIndLitToArg, argno, valOffset);
            return;
        } else if (locType == LT_CtxtRegister) {
            SELF->emitF1(opIndLitToReg, regno, valOffset);
            return;
        }
    }

    /*
     * If we get here, one or more of three conditions can hold:
     *
     * 	1. the litvec index is too big to fit in a compact
     * 	   encoding,
     *
     * 	2. the destination is an arg register whose index won't
     * 	   fit in the opIndLitToArg instruction, or
     *
     * 	3. the destination is some whacko location.
     */

    SELF->emitF0(opIndLitToRslt, valOffset);
    switch (locType) {
    case LT_ArgRegister:
        SELF->emitF0(opXferRsltToArg, argno);
        return;
    case LT_CtxtRegister:
        SELF->emitF0(opXferRsltToReg, regno);
        return;
    default: {
        unsigned offset = SELF->getDestOffset();
        SELF->emitF0(opXferRsltToDest, offset);
        return;
    }
    }
}


void AttrNode::emitLookup(pOb symbol) {
    const LocationType locType = (LocationType)GET_GENERIC_TYPE(dest);
    const int argno = GET_ARGREG_INDEX(dest);
    const int regno = GET_CTXTREG_INDEX(dest);

    PROTECT_THIS(AttrNode);

    unsigned litOffset = SELF->cu->extendLitvec(symbol);
    assert(litOffset <= CompilationUnit::MaximumLitVecSize);

    // If doing deferred symbol lookup add the defer bit.
    if (deferredLookup) {
        litOffset |= CompilationUnit::LookupDeferMask;
    }

    switch (locType) {
    case LT_CtxtRegister:
        SELF->emitF2(opLookupToReg, regno, litOffset);
        break;

    case LT_ArgRegister:
        if (argno < 16) {
            SELF->emitF2(opLookupToArg, argno, litOffset);
        } else {
            SELF->emitF2(opLookupToReg, CRN_Rslt, litOffset);
            SELF->emitF0(opXferRsltToArg, argno);
        }
        break;

    default: {
        unsigned destOffset = SELF->getDestOffset();
        SELF->emitF2(opLookupToReg, CRN_Rslt, litOffset);
        SELF->emitF0(opXferRsltToDest, destOffset);
        break;
    }
    }
}


void AttrNode::emitOpAndLabel(Opcode op, Label label) {
    PROTECT_THIS(AttrNode);
    SELF->cu->registerLabel(label);
    SELF->emitF6(op, 0);
}


void AttrNode::emitOpAndLabel(Opcode op, pOb label_name) {
    emitOpAndLabel(op, cu->labels->getLabel(label_name));
}


void AttrNode::emitPush(int nargs) {
    if (nargs == 0) {
        emitF0(opPush);
    } else {
        emitF0(opPushAlloc, nargs);
    }
}


void AttrNode::emitOutstanding() {
    if (outstanding > 0) {
        PROTECT_THIS(AttrNode);
        SELF->resume = SELF->cu->newLabel();
        SELF->emitOpAndLabel(opOutstanding, SELF->resume);
        SELF->emitE0(SELF->outstanding, 0);
    }
}


void AttrNode::emitRtn(RtnCode rtn, Label next) {
    switch (rtn) {
    case ImplicitRtn:
        emitImplicitRtn(next);
        break;

    case UntaggedRtn:
        emitUntaggedRtn(next);
        break;

    case TaggedRtn:
        emitTaggedRtn(next);
    }
}


void AttrNode::emitImplicitRtn(Label next) {
    if (next == NoneRemaining)
        emitF0(opNxt);
}


void AttrNode::emitTaggedRtn(Label next) {
    PROTECT_THIS(AttrNode);

    Opcode opcode;
    unsigned op;

    switch (GET_GENERIC_TYPE(SELF->dest)) {
    case LT_ArgRegister:
        opcode = opRtnArg;
        op = GET_ARGREG_INDEX(SELF->dest);
        break;
    case LT_CtxtRegister:
        opcode = opRtnReg;
        op = GET_CTXTREG_INDEX(SELF->dest);
        break;
    default:
        opcode = opRtnTag;
        op = SELF->getDestOffset();
        break;
    }

    /*
     * Since getDestOffset might cause a garbage collection (by growing
     * the litvec), we have to invoke the emit through the protected
     * value of SELF here.
     */

    SELF->emitF5(opcode, false, next == NoneRemaining, op);
}


void AttrNode::emitUntaggedRtn(Label next) {
    if (dest != LocLimbo) {
        emitF5(opRtn, false, next == NoneRemaining);
    }
}


void AttrNode::emitXfer(Location source) {
    if (source == dest) {
        return;
    }

    const LocationType destType = (LocationType)GET_GENERIC_TYPE(dest);
    const LocationType sourceType = (LocationType)GET_GENERIC_TYPE(source);

    PROTECT_THIS(AttrNode);

    if (sourceType == LT_GlobalVariable) {
        switch (destType) {
        case LT_CtxtRegister:
            SELF->emitF0(opXferGlobalToReg, GET_CTXTREG_INDEX(dest));
            SELF->emitE1(GET_GLOBALVAR_OFFSET(source));
            return;
        case LT_ArgRegister:
            SELF->emitF0(opXferGlobalToArg, GET_ARGREG_INDEX(dest));
            SELF->emitE1(GET_GLOBALVAR_OFFSET(source));
            return;
        default: {
            unsigned offset = SELF->getDestOffset();
            SELF->emitF0(opXferGlobalToReg, CRN_Rslt);
            SELF->emitF0(opXferRsltToDest, offset);
            return;
        }
        }
    } else if (sourceType == LT_LexVariable && GET_LEXVAR_LEVEL(source) < 8 &&
               GET_LEXVAR_OFFSET(source) < 16) {
        const unsigned ind = GET_LEXVAR_IND(source);
        const unsigned level = GET_LEXVAR_LEVEL(source);
        const unsigned offset = GET_LEXVAR_OFFSET(source);

        switch (destType) {
        case LT_CtxtRegister:
            SELF->emitF7(opXferLexToReg, ind, level, offset,
                         GET_CTXTREG_INDEX(SELF->dest));
            return;
        case LT_ArgRegister:
            if (GET_ARGREG_INDEX(dest) < 16) {
                SELF->emitF7(opXferLexToArg, ind, level, offset,
                             GET_ARGREG_INDEX(SELF->dest));
                return;
            } else {
                SELF->emitF7(opXferLexToReg, ind, level, offset, CRN_Rslt);
                SELF->emitF0(opXferRsltToArg, GET_ARGREG_INDEX(SELF->dest));
                return;
            }
        default: {
            unsigned destOffset = SELF->getDestOffset();
            SELF->emitF7(opXferLexToReg, ind, level, offset, CRN_Rslt);
            SELF->emitF0(opXferRsltToDest, destOffset);
            return;
        }
        }
    }

    else if (sourceType == LT_ArgRegister && destType == LT_ArgRegister)
        if (GET_ARGREG_INDEX(source) < 16 &&
            GET_ARGREG_INDEX(SELF->dest) < 16) {
            emitF1(opXferArgToArg, GET_ARGREG_INDEX(SELF->dest),
                   GET_ARGREG_INDEX(source));
            return;
        } else {
            SELF->emitF0(opXferArgToRslt, GET_ARGREG_INDEX(source));
            SELF->emitF0(opXferRsltToArg, GET_ARGREG_INDEX(SELF->dest));
            return;
        }

    else if (destType == LT_CtxtRegister &&
             GET_CTXTREG_INDEX(SELF->dest) == CRN_Rslt) {
        Opcode opcode;
        unsigned offset;
        switch (sourceType) {
        case LT_ArgRegister:
            opcode = opXferArgToRslt;
            offset = GET_ARGREG_INDEX(source);
            break;
        case LT_CtxtRegister:
            opcode = opXferRegToRslt;
            offset = GET_CTXTREG_INDEX(source);
            break;
        default:
            opcode = opXferSrcToRslt;
            offset = SELF->cu->extendLitvec(source.atom);
            break;
        }
        SELF->emitF0(opcode, offset);
        return;
    }

    unsigned sourceOffset = SELF->cu->extendLitvec(source.atom);
    unsigned destOffset = SELF->getDestOffset();
    SELF->emitF0(opXferSrcToRslt, sourceOffset);
    SELF->emitF0(opXferRsltToDest, destOffset);
}


int AttrNode::primNumber() { return -1; }


ConstNode::ConstNode(pOb lit, bool valueCtxt)
    : AttrNode(sizeof(ConstNode), valueCtxt), val(lit) {
    ConstNode::updateCnt();
}


ConstNode* ConstNode::create(pOb lit, bool valueCtxt) {
    void* loc = PALLOC1(sizeof(ConstNode), lit);
    return new (loc) ConstNode(lit, valueCtxt);
}


void ConstNode::initialize(pOb ctEnv, pOb freeEnv, Location dest,
                           CompilationUnit* cu) {
    AttrNode::initialize(ctEnv, freeEnv, dest, cu);
    SET_FLAG(word, f_inlineableNode);
    SET_FLAG(word, f_simpleNode);
}


void ConstNode::emitDispatchCode(bool ctxtAvailable, bool, RtnCode rtn,
                                 Label next) {
    assert(ctxtAvailable);

    PROTECT_THIS(ConstNode);
    Location temp = dest;

    if (rtn == TaggedRtn) {
        dest = LocRslt;
    }

    SELF->emitLit(SELF->val);
    SELF->dest = temp;
    SELF->emitRtn(rtn, next);
}


int ConstNode::primNumber() {
    Prim* prim = BASE(val)->InlineablePrimP();
    return (prim == INVALID) ? -1 : prim->primNumber();
}


SymbolNode::SymbolNode(pOb symbol, bool valueCtxt)
    : AttrNode(sizeof(SymbolNode), valueCtxt), loc(LocLimbo), sym(symbol) {
    SymbolNode::updateCnt();
}


SymbolNode* SymbolNode::create(pOb symbol, bool valueCtxt) {
    void* loc = PALLOC1(sizeof(SymbolNode), symbol);
    return new (loc) SymbolNode(symbol, valueCtxt);
}


void SymbolNode::initialize(pOb ctEnv, pOb freeEnv, Location dest,
                            CompilationUnit* cu) {
    AttrNode::initialize(ctEnv, freeEnv, dest, cu);
    SET_FLAG(word, f_inlineableNode);
    SET_FLAG(word, f_simpleNode);

    if (BASE(freeEnv)->lex(sym, 0) != LocLimbo) {
        /*
         * The symbol is free, and we should make no attempt to lex it.
         */
        loc = LocLimbo;
        return;
    }

    loc = BASE(ctEnv)->lex(sym, 0);

    if (loc != LocLimbo) {
        assert(GET_GENERIC_TYPE(loc) == LT_LexVariable);
        return;
    }

    loc = GlobalEnv->lex(sym, 0);

    if (loc != LocLimbo) {
        assert(GET_GENERIC_TYPE(loc) == LT_LexVariable);
        /*
         * If the level is non-zero, then the binding was actually found
         * in some ancestor of GlobalEnv (e.g., Top-SBO), and we don't
         * want to mark it as a global reference.
         */
        if (GET_LEXVAR_LEVEL(loc) == 0) {
            loc = GlobalVar(GET_LEXVAR_OFFSET(loc));

            // If this symbol is not a primitive, defer the lookup until runtime.
            // This is part of the effort to separate the compile and execute
            // phases. The ultimate goal being to validate the Roscala VM by cross
            // utilizing the compilers and VMs.
            if (DeferLookupFlag && primNumber() < 0) {
                loc = LocLimbo;
                deferredLookup = true;
            }
            return;
        }
    }

    loc = LocLimbo;
    cu->warning("no compile-time binding for '%s", SYMPTR(sym));
}


void SymbolNode::emitDispatchCode(bool ctxtAvailable, bool, RtnCode rtn,
                                  Label next) {
    assert(ctxtAvailable);

    Location temp = dest;
    PROTECT_THIS(SymbolNode);

    if (rtn == TaggedRtn) {
        SELF->dest = LocRslt;
    }

    if (SELF->loc == LocLimbo) {
        SELF->emitLookup(SELF->sym);
    } else {
        SELF->emitXfer(SELF->loc);
    }

    SELF->dest = temp;
    SELF->emitRtn(rtn, next);
}


int SymbolNode::primNumber() {
    if (GET_GENERIC_TYPE(loc) == LT_GlobalVariable) {
        pOb globalVal = GlobalEnv->entry(GET_GLOBALVAR_OFFSET(loc));
        Prim* prim = BASE(globalVal)->InlineablePrimP();
        return (prim == INVALID) ? -1 : prim->primNumber();
    } else {
        return -1;
    }
}


FreeNode::FreeNode(FreeExpr* expr, bool valueCtxt)
    : AttrNode(sizeof(FreeNode), valueCtxt),
      expr(expr),
      body((AttrNode*)INVALID) {
    FreeNode::updateCnt();
}


FreeNode* FreeNode::create(FreeExpr* expr, bool valueCtxt) {
    void* loc = PALLOC1(sizeof(FreeNode), expr);
    return new (loc) FreeNode(expr, valueCtxt);
}


void FreeNode::initialize(pOb ctEnv, pOb freeEnv, Location dest,
                          CompilationUnit* cu) {
    PROTECT_THIS(FreeNode);
    PROTECT(ctEnv);
    PROTECT(freeEnv);

    AttrNode::initialize(ctEnv, freeEnv, dest, cu);

    Template* templat = SELF->expr->freeIds->makeTemplate();
    pOb new_freeEnv = BASE(freeEnv)->extendWith(templat->keymeta);
    PROTECT(new_freeEnv);

    AttrNode* node =
        BASE(SELF->expr->body)->makeAttrNode(GET_ATTR(*SELF, f_valueContext));
    ASSIGN(SELF, body, node);
    node->initialize(ctEnv, new_freeEnv, SELF->dest, SELF->cu);

    SELF->av_size = SELF->body->av_size;
    SELF->outstanding = SELF->body->outstanding;
    SET_ATTR(*SELF, f_inlineableNode,
             GET_ATTR(*(SELF->body), f_inlineableNode));
    SET_ATTR(*SELF, f_simpleNode, GET_ATTR(*(SELF->body), f_simpleNode));
    SET_ATTR(*SELF, f_producesValue, GET_ATTR(*(SELF->body), f_producesValue));
}


void FreeNode::changeDest(Location& new_loc) { body->changeDest(new_loc); }


void FreeNode::emitDispatchCode(bool ctxtAvailable, bool argvecAvailable,
                                RtnCode rtn, Label next) {
    body->emitDispatchCode(ctxtAvailable, argvecAvailable, rtn, next);
}


void FreeNode::emitResumeCode(RtnCode rtn) { body->emitResumeCode(rtn); }


int FreeNode::primNumber() { return body->primNumber(); }


NullNode::NullNode(bool valueCtxt) : AttrNode(sizeof(NullNode), valueCtxt) {
    SET_ATTR(*this, f_inlineableNode, true);
    SET_ATTR(*this, f_simpleNode, true);
    SET_ATTR(*this, f_producesValue, false);
    NullNode::updateCnt();
}


NullNode* NullNode::create(bool valueCtxt) {
    void* loc = PALLOC(sizeof(NullNode));
    return new (loc) NullNode(valueCtxt);
}


void NullNode::emitDispatchCode(bool, bool, RtnCode, Label next) {
    if (next == NoneRemaining) {
        emitF0(opNxt);
    }
}


XferNode::XferNode(int source)
    : AttrNode(sizeof(XferNode), true), source(FIXNUM(source)) {
    XferNode::updateCnt();
}


XferNode* XferNode::create(int source) {
    void* loc = PALLOC(sizeof(XferNode));
    return new (loc) XferNode(source);
}


void XferNode::emitDispatchCode(bool, bool, RtnCode, Label next) {
    /*
     * This code is only invoked in special circumstances (moving
     * temporary values to their ultimate locations) in which we know:
     *
     * 	ctxtAvailable == true
     * 	argvecAvailable == true
     * 	rtn == ImplicitRtn
     */

    PROTECT_THIS(XferNode);
    SELF->emitXfer(ArgReg(FIXVAL(SELF->source)));
    if (next == NoneRemaining) {
        SELF->emitF0(opNxt);
    }
}


ExprStack::ExprStack() : exprs(NIL), top(FIXNUM(0)) {}


CompoundNode::CompoundNode(int sz, bool ctxt) : AttrNode(sz, ctxt) {}


void CompoundNode::addTo(ExprStack* exprstack, AttrNode* node) {
    int n = FIXVAL(exprstack->top);
    if (n == 0) {
        PROTECT_THIS(CompoundNode);
        PROTECT(node);
        int offset = (char*)exprstack - (char*)SELF;
        Tuple* t = Tuple::create(SELF->numberOfSubExprs(), NIV);
        exprstack = (ExprStack*)((char*)SELF + offset);
        SELF->checkStore(exprstack->exprs = t);
    }
    ASSIGN(exprstack->exprs, elem(n), node);
    FIXNUM_INC(exprstack->top);
}


void CompoundNode::analyze(AttrNode* node) {
    if (GET_ATTR(*node, f_simpleNode)) {
        addTo(&simple, node);
    } else if (GET_ATTR(*node, f_inlineableNode)) {
        av_size = std::max(av_size, node->av_size);
        addTo(&inlined, node);
    } else {
        outstanding++;
        addTo(&nested, node);
    }
}


void CompoundNode::rearrangeInlinedExprs() {
    if (inlined.exprs == NIL)
        return;

    ArgNum freeArgs[MaxArgs];
    int top = determineFree(freeArgs);

    sortInlinedExprs();
    fixInlinedConflicts(freeArgs, top);
}


int CompoundNode::determineFree(ArgNum* free) {
    int freeTop = 0;
    int nextNode = FIXVAL(inlined.top);
    int arg = 0;

    while (nextNode--) {
        AttrNode* node = (AttrNode*)inlined.exprs->elem(nextNode);
        if (GET_GENERIC_TYPE(node->dest) == LT_ArgRegister) {
            int nextDest = GET_ARGREG_INDEX(node->dest);
            while (arg < nextDest)
                free[freeTop++] = arg++;
            arg++;
        }
    }

    while (arg < av_size) {
        free[freeTop++] = arg++;
    }

    return freeTop;
}


void CompoundNode::sortInlinedExprs() {
    int N = FIXVAL(inlined.top);

    for (int i = 0; i < N; i++) {
        AttrNode* ni = (AttrNode*)inlined.exprs->elem(i);
        if (GET_GENERIC_TYPE(ni->dest) == LT_ArgRegister) {
            int N_i = ni->av_size;
            int dest_i = GET_ARGREG_INDEX(ni->dest);
            int highest_i = std::max(N_i - 1, dest_i);
            for (int j = i + 1; j < N; j++) {
                AttrNode* nj = (AttrNode*)inlined.exprs->elem(j);
                int N_j = nj->av_size;
                int dest_j = GET_ARGREG_INDEX(nj->dest);
                int highest_j = std::max(N_j - 1, dest_j);
                if (GET_GENERIC_TYPE(nj->dest) != LT_ArgRegister ||
                    highest_j > highest_i ||
                    (highest_j == highest_i &&
                     (N_i > dest_i || N_j >= dest_i))) {
                    inlined.exprs->elem(i) = nj;
                    inlined.exprs->elem(j) = ni;
                    ni = nj;
                    N_i = N_j;
                    dest_i = dest_j;
                    highest_i = highest_j;
                }
            }
        }
    }
}


void CompoundNode::fixInlinedConflicts(ArgNum* free, int freeTop) {
    int N = FIXVAL(inlined.top);

    PROTECT_THIS(CompoundNode);
    AttrNode* ni = (AttrNode*)inlined.exprs->elem(0);
    AttrNode* nj = (AttrNode*)INVALID;
    PROTECT(ni);
    PROTECT(nj);

    int j = 1, highest_j, nextTemp = SELF->av_size;

    while (j < N && GET_GENERIC_TYPE(ni->dest) != LT_ArgRegister) {
        ni = (AttrNode*)SELF->inlined.exprs->elem(j++);
    }

    for (; j < N; j++) {
        nj = (AttrNode*)SELF->inlined.exprs->elem(j);
        // NB(leaf): Cast the 2nd argument, which is unsigned, to avoid type
        // error.
        int argreg_index_nj = (int)GET_ARGREG_INDEX(nj->dest);
        int argreg_index_ni = (int)GET_ARGREG_INDEX(ni->dest);
        highest_j = std::max(nj->av_size - 1, argreg_index_nj);

        if (argreg_index_ni <= highest_j) {
            int tempReg = (freeTop > 0 && free[freeTop - 1] > highest_j)
                              ? free[--freeTop]
                              : nextTemp++;
            Location tempDest = ArgReg(tempReg);
            XferNode* tempXfer = XferNode::create(tempReg);
            tempXfer->initialize(TopEnv, TopEnv, ni->dest, SELF->cu);
            ni->changeDest(tempDest);
            SELF->addTo(&SELF->simple, tempXfer);
        }
        ni = nj;
    }

    SELF->av_size = nextTemp;
}


void CompoundNode::emitPrefix(bool ctxtAvailable, bool argvecAvailable) {
    if (!GET_FLAG(word, f_inlineableNode) && !ctxtAvailable) {
        emitPush(av_size);
    } else if (!argvecAvailable) {
        emitAlloc(av_size);
    }
}


void CompoundNode::emitDispatchCode(bool ctxtAvailable, bool argvecAvailable,
                                    RtnCode rtn, Label exit) {
    PROTECT_THIS(CompoundNode);

    bool needPush = !GET_FLAG(word, f_inlineableNode) && !ctxtAvailable;
    bool needPop = needPush && exit != NoneRemaining;
    bool willWrapup = outstanding == 0;
    bool nestedCode = nested.top != FIXNUM(0);  // outstanding > 0;
    bool inlinedCode = inlined.top != FIXNUM(0);
    bool simpleCode = simple.top != FIXNUM(0);

    Label nestedExit = exit, inlinedExit = exit, simpleExit = exit;

    SELF->emitPrefix(ctxtAvailable, argvecAvailable);

    if (inlinedCode) {
        if (simpleCode || nestedCode || willWrapup || needPop)
            inlinedExit = SELF->cu->newLabel();
        SELF->emitInlinedExprDispatchCode(ImplicitRtn, inlinedExit);
    }

    if (simpleCode) {
        if (inlinedCode)
            SELF->cu->setLabel(inlinedExit);
        if (nestedCode || willWrapup || needPop)
            simpleExit = SELF->cu->newLabel();
        SELF->emitSimpleExprDispatchCode(ImplicitRtn, simpleExit);
    }

    if (nestedCode) {
        if (simpleCode) {
            SELF->cu->setLabel(simpleExit);
        } else if (inlinedCode) {
            SELF->cu->setLabel(inlinedExit);
        }

        if (willWrapup || needPop) {
            nestedExit = SELF->cu->newLabel();
        }

        SELF->emitNestedExprDispatchCode(TaggedRtn, nestedExit);
    }

    if (willWrapup || needPop) {
        if (nestedCode) {
            SELF->cu->setLabel(nestedExit);
        } else if (simpleCode) {
            SELF->cu->setLabel(simpleExit);
        } else if (inlinedCode) {
            SELF->cu->setLabel(inlinedExit);
        }

        if (needPop) {
            if (willWrapup) {
                Label finish = SELF->cu->newLabel();
                SELF->emitWrapup(rtn, finish);
                SELF->cu->setLabel(finish);
            }

            SELF->emitF0(opPop);

        } else {
            SELF->emitWrapup(rtn, exit);
        }
    }
}


void CompoundNode::emitSimpleExprDispatchCode(RtnCode rtn, Label exit) {
    Tuple* exprs = simple.exprs;
    int nexprs = FIXVAL(simple.top);

    if (nexprs > 0) {
        PROTECT(exprs);

        while (nexprs--) {
            /*
             * Because all of these expressions are known to be simple,
             * only the last needs to examine a real label.
             */
            AttrNode* node = (AttrNode*)exprs->elem(nexprs);
            node->emitDispatchCode(CtxtAvailable, ArgvecAvailable, rtn,
                                   nexprs == 0 ? exit : NoParticularLabel);
        }
    }
}


void CompoundNode::emitInlinedExprDispatchCode(RtnCode rtn, Label exit) {
    Tuple* exprs = inlined.exprs;
    int nexprs = FIXVAL(inlined.top);

    if (nexprs > 0) {
        PROTECT_THIS(CompoundNode);
        PROTECT(exprs);

        for (int i = 0; i < nexprs; i++) {
            bool last = i == nexprs - 1;
            Label next = last ? exit : SELF->cu->newLabel();
            AttrNode* node = (AttrNode*)exprs->elem(i);
            node->emitDispatchCode(CtxtAvailable, ArgvecAvailable, rtn, next);
            if (!last) {
                SELF->cu->setLabel(next);
            }
        }
    }
}


void CompoundNode::emitNestedExprDispatchCode(RtnCode rtn, Label exit) {
    Tuple* exprs = nested.exprs;
    int nexprs = FIXVAL(nested.top);

    if (nexprs > 0) {
        PROTECT_THIS(CompoundNode);
        PROTECT(exprs);

        SELF->emitOutstanding();

        for (int i = 0; i < nexprs; i++) {
            bool last = i == nexprs - 1;
            Label nextExpr = last ? exit : SELF->cu->newLabel();
            AttrNode* node = (AttrNode*)exprs->elem(i);
            node->emitDispatchCode(!CtxtAvailable, !ArgvecAvailable, rtn,
                                   nextExpr);
            if (!last) {
                SELF->cu->setLabel(nextExpr);
            }
        }
    }
}


void CompoundNode::emitNestedExprResumeCode(RtnCode rtn) {
    int nexprs = FIXVAL(nested.top);

    if (nexprs > 0) {
        Tuple* exprs = nested.exprs;
        PROTECT(exprs);

        for (int i = 0; i < nexprs; i++) {
            AttrNode* node = (AttrNode*)exprs->elem(i);
            node->emitResumeCode(rtn);
        }
    }
}


int CompoundNode::numberOfSubExprs() {
    NI("numberOfSubExprs");
    return 0;
}


void CompoundNode::emitResumeCode(RtnCode rtn) {
    if (outstanding > 0) {
        PROTECT_THIS(CompoundNode);
        SELF->emitNestedExprResumeCode(TaggedRtn);
        SELF->cu->setLabel(SELF->resume);
        SELF->emitWrapup(rtn, NoneRemaining);
    }
}


void CompoundNode::emitWrapup(RtnCode, Label) { NI("emitWrapup"); }


BlockNode::BlockNode(BlockExpr* be, bool valueCtxt)
    : CompoundNode(sizeof(BlockNode), valueCtxt), expr(be) {
    BlockNode::updateCnt();
}


BlockNode* BlockNode::create(BlockExpr* be, bool valueCtxt) {
    void* loc = PALLOC1(sizeof(BlockNode), be);
    return new (loc) BlockNode(be, valueCtxt);
}


void BlockNode::initialize(pOb ctEnv, pOb freeEnv, Location dest,
                           CompilationUnit* cu) {
    PROTECT_THIS(BlockNode);
    PROTECT(ctEnv);
    PROTECT(freeEnv);
    int nexprs = expr->numberOfSubExprs();
    int valueProducers = 0;

    CompoundNode::initialize(ctEnv, freeEnv, dest, cu);

    for (int i = 0; i < nexprs; i++) {
        AttrNode* node =
            BASE(SELF->expr->subExprs->elem(i))->makeAttrNode(false);
        PROTECT(node);
        node->initialize(ctEnv, freeEnv, dest, SELF->cu);
        SELF->analyze(node);
        valueProducers += (GET_ATTR(*node, f_producesValue) ? 1 : 0);
    }

    SET_ATTR(*SELF, f_producesValue, (valueProducers > 0));

    if (GET_ATTR(*SELF, f_valueContext)) {
        switch (valueProducers) {
        case 0: {
            if (!GET_ATTR(*SELF, f_topLevel)) {
                const char* modifier =
                    BOOLVAL(SELF->expr->implicit) ? "implicit " : "";
                SELF->cu->warning(
                    "no value returned from %sblock in value-expecting "
                    "position",
                    modifier);
            }
            AttrNode* node = ConstNode::create(NIV, true);
            PROTECT(node);
            node->initialize(ctEnv, freeEnv, dest, SELF->cu);
            SELF->analyze(node);
        }
        case 1:
            break;
        default:
            if (!GET_ATTR(*SELF, f_topLevel)) {
                const char* modifier =
                    BOOLVAL(SELF->expr->implicit) ? "an implicit" : "a";
                SELF->cu->warning(
                    "more than one result may be returned from %s block "
                    "expression",
                    modifier);
                break;
            }
        }
    }
}


void BlockNode::changeDest(Location& newloc) {
    int i = 0;
    for (i = FIXVAL(simple.top); i--;) {
        AttrNode* node = (AttrNode*)simple.exprs->elem(i);
        node->changeDest(newloc);
    }
    for (i = FIXVAL(inlined.top); i--;) {
        AttrNode* node = (AttrNode*)inlined.exprs->elem(i);
        node->changeDest(newloc);
    }
    for (i = FIXVAL(nested.top); i--;) {
        AttrNode* node = (AttrNode*)nested.exprs->elem(i);
        node->changeDest(newloc);
    }
}


int BlockNode::numberOfSubExprs() {
    /*
     * We need to provide a cushion of one sub-expression in case we need
     * to generate a NIV expr for a block in a value-requiring position.
     */

    return expr->numberOfSubExprs() + (GET_FLAG(word, f_valueContext));
}


void BlockNode::emitDispatchCode(bool ctxtAvailable, bool argvecAvailable,
                                 RtnCode rtn, Label exit) {
    PROTECT_THIS(BlockNode);

    bool needPush = !GET_FLAG(word, f_inlineableNode) && !ctxtAvailable;
    bool needPop = needPush && exit != NoneRemaining;
    bool nestedCode = nested.top != FIXNUM(0);  // outstanding > 0;
    bool inlinedCode = inlined.top != FIXNUM(0);
    bool simpleCode = simple.top != FIXNUM(0);

    Label nestedExit = exit, inlinedExit = exit, simpleExit = exit;

    SELF->emitPrefix(ctxtAvailable, argvecAvailable);

    if (inlinedCode) {
        if (simpleCode || nestedCode || needPop)
            inlinedExit = SELF->cu->newLabel();
        SELF->emitInlinedExprDispatchCode(rtn, inlinedExit);
    }

    if (simpleCode) {
        if (inlinedCode)
            SELF->cu->setLabel(inlinedExit);
        if (nestedCode || needPop)
            simpleExit = SELF->cu->newLabel();
        SELF->emitSimpleExprDispatchCode(rtn, simpleExit);
    }

    if (nestedCode) {
        if (simpleCode)
            SELF->cu->setLabel(simpleExit);
        else if (inlinedCode)
            SELF->cu->setLabel(inlinedExit);
        if (needPop)
            nestedExit = SELF->cu->newLabel();
        SELF->emitNestedExprDispatchCode(rtn, nestedExit);
    }

    if (needPop) {
        if (nestedCode) {
            SELF->cu->setLabel(nestedExit);
        } else if (simpleCode) {
            SELF->cu->setLabel(simpleExit);
        } else if (inlinedCode) {
            SELF->cu->setLabel(inlinedExit);
        }

        if (needPop) {
            Label finish = SELF->cu->newLabel();
            SELF->emitF0(opNxt);
            SELF->cu->setLabel(finish);
        } else {
            SELF->emitF0(opNxt);
        }
    }
}


void BlockNode::emitNestedExprDispatchCode(RtnCode rtn, Label) {
    Tuple* exprs = nested.exprs;
    int nexprs = FIXVAL(nested.top);

    if (nexprs > 0) {
        PROTECT_THIS(BlockNode);
        PROTECT(exprs);

        for (int i = 0; i < nexprs; i++) {
            Label nextStrand = 0;
            bool last = i == nexprs - 1;
            if (!last) {
                nextStrand = SELF->cu->newLabel();
                SELF->emitOpAndLabel(opFork, nextStrand);
            }

            /*
             * A ctxt is always available for each strand: the first
             * strand runs out of the initiating ctxt, while subsequent
             * ones have the ctxt provided by the fork instruction.  The
             * first strand is also free to try to use the existing
             * argvec.
             */

            AttrNode* node = (AttrNode*)exprs->elem(i);
            node->emitDispatchCode(CtxtAvailable,
                                   i == 0 && node->av_size <= SELF->av_size,
                                   rtn, NoneRemaining);
            if (!last)
                SELF->cu->setLabel(nextStrand);
        }
    }
}


void BlockNode::emitResumeCode(RtnCode rtn) {
    int nexprs = FIXVAL(nested.top);
    if (nexprs > 0) {
        PROTECT_THIS(BlockNode);
        for (int i = 0; i < nexprs; i++) {
            AttrNode* node = (AttrNode*)SELF->nested.exprs->elem(i);
            node->emitResumeCode(rtn);
        }
    }
}


RequestNode::RequestNode(int sz, RequestExpr* expr, bool valueCtxt)
    : CompoundNode(sz, valueCtxt),
      expr(expr),
      trgtNode((AttrNode*)INVALID),
      primTrgt(FIXNUM(-1)) {}


RequestNode::RequestNode(RequestExpr* expr, bool valueCtxt)
    : CompoundNode(sizeof(RequestNode), valueCtxt),
      expr(expr),
      trgtNode((AttrNode*)INVALID),
      primTrgt(FIXNUM(-1)) {
    RequestNode::updateCnt();
}


RequestNode* RequestNode::create(RequestExpr* expr, bool valueCtxt) {
    void* loc = PALLOC1(sizeof(RequestNode), expr);
    return new (loc) RequestNode(expr, valueCtxt);
}


void RequestNode::initialize(pOb ctEnv, pOb freeEnv, Location dest,
                             CompilationUnit* cu) {
    int nargs = expr->msg->numberOfElements();
    bool hasRestArg = expr->msg->rest != NILexpr;
    AttrNode* node = (AttrNode*)INVALID;
    PROTECT_THIS(RequestNode);
    PROTECT(node);
    PROTECT(ctEnv);
    PROTECT(freeEnv);
    PROTECT(cu);

    SELF->av_size = nargs + hasRestArg;
    CompoundNode::initialize(ctEnv, freeEnv, dest, cu);

    node = BASE(SELF->expr->target)->makeAttrNode(true);
    ASSIGN(SELF, trgtNode, node);
    node->initialize(ctEnv, freeEnv, LocTrgt, SELF->cu);
    ASSIGN(SELF, primTrgt, FIXNUM(node->primNumber()));

    /*
     * We throw the evaluation of the target expression in with the
     * evaluation of the arguments, and let that mechanism worry about
     * scheduling the order, pushing and popping ctxts, etc.  Notice that
     * if the target can be inlined it is stored as the first inline
     * expression to be evaluated; this allows it to clobber any arg
     * registers that it needs, and since its result will be put in the
     * trgt register, no following inlined arg evaluation can clobber the
     * result.
     */

    if (SELF->primTrgt == FIXNUM(-1))
        SELF->analyze(node);

    if (hasRestArg) {
        /*
         * We also throw the evaluation of the rest expression in with
         * the evaluation of the arguments, and let that mechanism worry
         * about scheduling the order, pushing and popping ctxts, etc.
         */
        node = BASE(SELF->expr->msg->rest)->makeAttrNode(true);
        node->initialize(ctEnv, freeEnv, ArgReg(nargs), SELF->cu);
        SELF->analyze(node);
    }

    for (int i = nargs; i--;) {
        if (i > MaxArgs)
            cu->abort("cannot pass more than %d arguments", MaxArgs);
        node = BASE(SELF->expr->msg->elem(i))->makeAttrNode(true);
        node->initialize(ctEnv, freeEnv, ArgReg(i), SELF->cu);
        SELF->analyze(node);
    }

    SELF->rearrangeInlinedExprs();
    SET_ATTR(*SELF, f_inlineableNode,
             (SELF->outstanding == 0 && (SELF->primTrgt != FIXNUM(-1))));
    SET_ATTR(*SELF, f_producesValue, (SELF->dest != LocLimbo));
}


int RequestNode::numberOfSubExprs() {
    return 1 + expr->msg->numberOfElements() + (expr->msg->rest != NILexpr);
}


void RequestNode::emitWrapup(RtnCode rtn, Label next) {
    unsigned nargs = expr->msg->numberOfElements();
    bool unwind = expr->msg->rest != NILexpr;

    if (primTrgt != FIXNUM(-1))
        emitApplyPrim(FIXVAL(primTrgt), nargs, unwind, rtn, next);
    else
        emitXmit(nargs, unwind, rtn, next);
}


void RequestNode::emitXmit(unsigned nargs, bool unwind, RtnCode rtn,
                           Label next) {
    bool nxt = next == NoneRemaining;

    switch (rtn) {
    case ImplicitRtn:
        assert(rtn != ImplicitRtn);
        break;

    case TaggedRtn:
        switch (GET_GENERIC_TYPE(dest)) {
        case LT_CtxtRegister:
            if (nargs < 16)
                emitF4(opXmitReg, unwind, nxt, nargs, GET_CTXTREG_INDEX(dest));
            else {
                PROTECT_THIS(RequestNode);
                SELF->emitF5(opXmitRegXtnd, unwind, nxt, nargs);
                SELF->emitE0(GET_CTXTREG_INDEX(dest), 0);
            }
            break;

        case LT_ArgRegister:
            if (GET_ARGREG_INDEX(dest) < 16 && nargs < 16)
                emitF4(opXmitArg, unwind, nxt, nargs, GET_ARGREG_INDEX(dest));
            else {
                PROTECT_THIS(RequestNode);
                SELF->emitF5(opXmitArgXtnd, unwind, nxt, nargs);
                SELF->emitE0(GET_ARGREG_INDEX(dest), 0);
            }
            break;

        case LT_Limbo:
            emitF5(opSend, unwind, nxt, nargs);
            break;

        default: {
            PROTECT_THIS(RequestNode);
            unsigned destOffset = SELF->getDestOffset();
            if (destOffset < 16 && nargs < 16)
                SELF->emitF4(opXmitTag, unwind, nxt, nargs, destOffset);
            else {
                SELF->emitF5(opXmitTagXtnd, unwind, nxt, nargs);
                SELF->emitE0(destOffset, 0);
            }
            break;
        }
        }
        break;

    case UntaggedRtn:
        emitF5((dest == LocLimbo ? opSend : opXmit), unwind, nxt, nargs);
        break;
    }
}


SendNode::SendNode(SendExpr* se, bool valueCtxt)
    : RequestNode(sizeof(SendNode), se, valueCtxt) {
    SendNode::updateCnt();
}


SendNode* SendNode::create(SendExpr* se, bool valueCtxt) {
    void* loc = PALLOC1(sizeof(SendNode), se);
    return new (loc) SendNode(se, valueCtxt);
}


void SendNode::initialize(pOb ctEnv, pOb freeEnv, Location,
                          CompilationUnit* cu) {
    RequestNode::initialize(ctEnv, freeEnv, LocLimbo, cu);
}


TupleNode::TupleNode(TupleExpr* te, bool valueCtxt)
    : CompoundNode(sizeof(TupleNode), valueCtxt), expr(te) {
    TupleNode::updateCnt();
}


TupleNode* TupleNode::create(TupleExpr* te, bool valueCtxt) {
    void* loc = PALLOC1(sizeof(TupleNode), te);
    return new (loc) TupleNode(te, valueCtxt);
}


void TupleNode::initialize(pOb ctEnv, pOb freeEnv, Location dest,
                           CompilationUnit* cu) {
    int nelems = expr->numberOfElements();
    bool restExpr = expr->rest != NILexpr;
    AttrNode* node = (AttrNode*)INVALID;
    PROTECT_THIS(TupleNode);
    PROTECT(node);
    PROTECT(ctEnv);
    PROTECT(freeEnv);

    SELF->av_size = nelems + restExpr;
    CompoundNode::initialize(ctEnv, freeEnv, dest, cu);

    if (restExpr) {
        node = BASE(SELF->expr->rest)->makeAttrNode(true);
        node->initialize(ctEnv, freeEnv, ArgReg(nelems), SELF->cu);
        SELF->analyze(node);
    }

    for (int i = nelems; i--;) {
        node = BASE(SELF->expr->elem(i))->makeAttrNode(true);
        node->initialize(ctEnv, freeEnv, ArgReg(i), SELF->cu);
        SELF->analyze(node);
    }

    SELF->rearrangeInlinedExprs();
}


int TupleNode::numberOfSubExprs() {
    return expr->numberOfElements() + (expr->rest != NILexpr);
}


void TupleNode::emitDispatchCode(bool ctxtAvailable, bool, RtnCode rtn,
                                 Label exit) {
    CompoundNode::emitDispatchCode(ctxtAvailable, !ArgvecAvailable, rtn, exit);
}


void TupleNode::emitWrapup(RtnCode rtn, Label next) {
    int nelems = expr->numberOfElements();

    if (expr->rest == NILexpr) {
        PROTECT_THIS(TupleNode);
        Location temp = SELF->dest;

        if (av_size > nelems) {
            SELF->dest = ArgReg(nelems);
            SELF->emitLit(NIL);
            SELF->dest = temp;
            SELF->emitApplyPrim(tplConsStar->primNumber(), nelems + 1, false,
                                rtn, next);
        } else {
            if (rtn == TaggedRtn)
                SELF->dest = LocRslt;

            SELF->emitXfer(CtxtReg(CRN_Argvec));
            SELF->dest = temp;
            SELF->emitRtn(rtn, next);
        }
    } else {
        emitApplyPrim(tplConsStar->primNumber(), nelems + 1, false, rtn, next);
    }
}


IfNode::IfNode(IfExpr* ie, bool valueCtxt)
    : CompoundNode(sizeof(IfNode), valueCtxt),
      expr(ie),
      trueNode((AttrNode*)INVALID),
      falseNode((AttrNode*)INVALID) {
    IfNode::updateCnt();
}


IfNode* IfNode::create(IfExpr* ie, bool valueCtxt) {
    void* loc = PALLOC1(sizeof(IfNode), ie);
    return new (loc) IfNode(ie, valueCtxt);
}


void IfNode::initialize(pOb ctEnv, pOb freeEnv, Location dest,
                        CompilationUnit* cu) {
    AttrNode* node = (AttrNode*)INVALID;
    PROTECT_THIS(IfNode);
    PROTECT(node);
    PROTECT(ctEnv);
    PROTECT(freeEnv);

    CompoundNode::initialize(ctEnv, freeEnv, dest, cu);

    node = BASE(SELF->expr->condition)->makeAttrNode(true);
    node->initialize(ctEnv, freeEnv, LocRslt, SELF->cu);
    SELF->analyze(node);
    bool condInlineable = GET_ATTR(*node, f_inlineableNode);

    node = BASE(SELF->expr->trueBranch)
               ->makeAttrNode(GET_ATTR(*SELF, f_valueContext));
    ASSIGN(SELF, trueNode, node);
    node->initialize(ctEnv, freeEnv, dest, SELF->cu);

    node = BASE(SELF->expr->falseBranch)
               ->makeAttrNode(GET_ATTR(*SELF, f_valueContext));
    ASSIGN(SELF, falseNode, node);
    node->initialize(ctEnv, freeEnv, dest, SELF->cu);

    SET_ATTR(*SELF, f_inlineableNode,
             (condInlineable && GET_ATTR(*(SELF->trueNode), f_inlineableNode) &&
              GET_ATTR(*(SELF->falseNode), f_inlineableNode)));

    if (condInlineable)
        SELF->av_size = std::max(
            SELF->av_size,
            std::max(SELF->trueNode->av_size, SELF->falseNode->av_size));

    SET_ATTR(*SELF, f_producesValue,
             GET_ATTR(*(SELF->trueNode), f_producesValue));
    /* what was this here for ????
            && GET_ATTR(*(SELF->falseNode),f_producesValue);
    */

    /*
     * Actually, it is probably the case that
     * SELF->trueNode->producesValue and SELF->falseNode->producesValue
     * are both true whenever GET_ATTR(*SELF,f_valueContext) is, because
     * makeAttrNode() is designed to build value-producing nodes when
     * required, possibly replacing the original expression with a
     * value-producing one (e.g., replacing the null expression in a
     * one-armed if expression with a niv expression).
     */
}


void IfNode::changeDest(Location& newloc) {
    AttrNode::changeDest(newloc);
    trueNode->changeDest(newloc);
    falseNode->changeDest(newloc);
}


int IfNode::numberOfSubExprs() { return 1; }


void IfNode::emitResumeCode(RtnCode rtn) {
    PROTECT_THIS(IfNode);
    SELF->CompoundNode::emitResumeCode(rtn);
    SELF->trueNode->emitResumeCode(rtn);
    SELF->falseNode->emitResumeCode(rtn);
}


void IfNode::emitWrapup(RtnCode rtn, Label next) {
    PROTECT_THIS(IfNode);

    bool condInlined = SELF->nested.top == FIXNUM(0);

    Label startFalseBranch = SELF->cu->newLabel();
    SELF->emitOpAndLabel(opJmpFalse, startFalseBranch);
    SELF->trueNode->emitDispatchCode(CtxtAvailable, condInlined, rtn, next);

    /*
     * This generates a superfluous jmp in the case that the false branch
     * is empty; the correct way to fix it, as with a number of other
     * problems, is to introduce an optimization phase that fixes up
     * problems like this.  The quick fix is too ugly to include.
     */

    if (next != NoneRemaining)
        SELF->emitOpAndLabel(opJmp, next);

    SELF->cu->setLabel(startFalseBranch);
    SELF->falseNode->emitDispatchCode(CtxtAvailable, condInlined, rtn, next);
}


LetNode::LetNode(int sz, bool ctxt, LetExpr* le)
    : CompoundNode(sz, ctxt),
      expr(le),
      templat((Template*)INVALID),
      bodyNode((AttrNode*)INVALID) {}


LetNode::LetNode(LetExpr* le, bool valueCtxt)
    : CompoundNode(sizeof(LetNode), valueCtxt),
      expr(le),
      templat((Template*)INVALID),
      bodyNode((AttrNode*)INVALID) {
    LetNode::updateCnt();
}


LetNode* LetNode::create(LetExpr* le, bool valueCtxt) {
    void* loc = PALLOC1(sizeof(LetNode), le);
    return new (loc) LetNode(le, valueCtxt);
}


void LetNode::initialize(pOb ctEnv, pOb freeEnv, Location dest,
                         CompilationUnit* cu) {
    int nexprs = expr->bindings->numberOfElements();
    AttrNode* node = (AttrNode*)INVALID;
    PROTECT_THIS(LetNode);
    PROTECT(node);
    PROTECT(ctEnv);
    PROTECT(freeEnv);

    SELF->av_size = nexprs;
    CompoundNode::initialize(ctEnv, freeEnv, dest, cu);

    {
        TupleExpr* boundIds = nexprs == 0 ? NILexpr : TupleExpr::create(nexprs);
        PROTECT(boundIds);
        for (int i = nexprs; i--;)
            boundIds->elem(i) = SELF->expr->boundId(i);
        Template* tmpl = boundIds->makeTemplate();
        ASSIGN(SELF, templat, tmpl);
    }

    pOb letEnv = SELF->letEnv(ctEnv);
    PROTECT(letEnv);

    for (int i = nexprs; i--;) {
        node = BASE(SELF->expr->boundExpr(i))->makeAttrNode(true);
        node->initialize(letEnv, freeEnv, SELF->ithLoc(i), SELF->cu);
        SELF->analyze(node);
    }

    SELF->rearrangeInlinedExprs();

    node =
        BASE(SELF->expr->body)->makeAttrNode(GET_ATTR(*SELF, f_valueContext));
    ASSIGN(SELF, bodyNode, node);
    pOb new_ctEnv = BASE(ctEnv)->extendWith(SELF->templat->keymeta);
    node->initialize(new_ctEnv, freeEnv, dest, SELF->cu);

    SET_ATTR(*SELF, f_producesValue, GET_ATTR(*node, f_producesValue));
}


void LetNode::changeDest(Location& newloc) {
    AttrNode::changeDest(newloc);
    bodyNode->changeDest(newloc);
}


pOb LetNode::letEnv(pOb ctenv) { return ctenv; }


Location LetNode::ithLoc(int i) { return ArgReg(i); }


int LetNode::numberOfSubExprs() { return expr->bindings->numberOfElements(); }


void LetNode::emitResumeCode(RtnCode rtn) {
    PROTECT_THIS(LetNode);
    SELF->CompoundNode::emitResumeCode(rtn);
    SELF->bodyNode->emitResumeCode(rtn);
}


void LetNode::emitWrapup(RtnCode rtn, Label next) {
    PROTECT_THIS(LetNode);
    SELF->emitF0(opNargs, SELF->expr->bindings->numberOfElements());
    SELF->emitExtend(SELF->templat);
    SELF->bodyNode->emitDispatchCode(CtxtAvailable, !ArgvecAvailable, rtn,
                                     next);
}


LetrecNode::LetrecNode(LetrecExpr* le, bool valueCtxt)
    : LetNode(sizeof(LetrecNode), valueCtxt, le) {
    LetrecNode::updateCnt();
}


LetrecNode* LetrecNode::create(LetrecExpr* le, bool valueCtxt) {
    void* loc = PALLOC1(sizeof(LetrecNode), le);
    return new (loc) LetrecNode(le, valueCtxt);
}


pOb LetrecNode::letEnv(pOb ctenv) {
    return BASE(ctenv)->extendWith(templat->keymeta);
}


Location LetrecNode::ithLoc(int i) { return LexVar(0, i); }


void LetrecNode::emitDispatchCode(bool ctxtAvailable, bool argvecAvailable,
                                  RtnCode rtn, Label next) {
    int nargs = expr->bindings->numberOfElements();
    PROTECT_THIS(LetrecNode);

    SELF->emitPrefix(ctxtAvailable, argvecAvailable);
    SELF->emitF0(opNargs, nargs);
    SELF->emitExtend(SELF->templat);
    SELF->CompoundNode::emitDispatchCode(CtxtAvailable, !ArgvecAvailable, rtn,
                                         next);
}


void LetrecNode::emitWrapup(RtnCode rtn, Label next) {
    bodyNode->emitDispatchCode(CtxtAvailable, !ArgvecAvailable, rtn, next);
}


MethodNode::MethodNode(int sz, MethodExpr* me, bool valueCtxt)
    : AttrNode(sz, valueCtxt) {
    this->expr = me;
    this->code = (Code*)INVALID;
}


MethodNode::MethodNode(MethodExpr* me, bool valueCtxt)
    : AttrNode(sizeof(MethodNode), valueCtxt), expr(me), code((Code*)INVALID) {
    MethodNode::updateCnt();
}


MethodNode* MethodNode::create(MethodExpr* me, bool valueCtxt) {
    void* loc = PALLOC1(sizeof(MethodNode), me);
    return new (loc) MethodNode(me, valueCtxt);
}


void MethodNode::initialize(pOb ctEnv, pOb freeEnv, Location dest,
                            CompilationUnit* cu) {
    PROTECT_THIS(MethodNode);

    AttrNode::initialize(ctEnv, freeEnv, dest, cu);
    av_size = 3;
    SET_FLAG(word, f_inlineableNode);

    Code* code = compileBody(ctEnv, freeEnv);
    if (code == INVALID)
        SELF->cu->abort();
    ASSIGN(SELF, code, code);
}


void MethodNode::emitDispatchCode(bool ctxtAvailable, bool argvecAvailable,
                                  RtnCode rtn, Label next) {
    PROTECT_THIS(MethodNode);

    AttrNode* node = NIL->makeAttrNode(true);
    PROTECT(node);

    node->initialize(TopEnv, TopEnv, LocRslt, SELF->cu);

    if (!ctxtAvailable)
        SELF->emitPush(SELF->av_size);
    else if (!argvecAvailable)
        SELF->emitAlloc(SELF->av_size);

    node->dest = ArgReg(0);
    node->emitLit(SELF->code);

    node->dest = ArgReg(1);
    node->emitLit(SELF->expr->identity);

    node->dest = ArgReg(2);
    node->emitLit(SELF->expr);

    SELF->emitApplyPrim(constructor(), SELF->av_size, false, rtn, next);
}


Template* MethodNode::adjustFormals() {
    /*
     * We make a "fake" formals template here that adjusts for the
     * presence of the receiver in the runtime argvec.  This allows the
     * virtual machine to avoid any adjustment of that argvec when
     * invoking a method.
     */
    PROTECT_THIS(MethodNode);
    TupleExpr* formals = (TupleExpr*)SELF->expr->formals;
    if (!IS_A(formals, TupleExpr))
        SELF->cu->abort("invalid formal parameter template");
    TupleExpr* adjustedFormals = formals->cons(SYMBOL("#self"));
    Template* templat = adjustedFormals->makeTemplate();
    if (templat == INVALID)
        SELF->cu->abort("invalid formal parameter template");
    return templat;
}


int MethodNode::constructor() {
    extern Prim* makeMethod;
    return makeMethod->primNumber();
}


Code* MethodNode::compileBody(pOb ctEnv, pOb freeEnv) {
    PROTECT_THIS(MethodNode);
    PROTECT(ctEnv);
    PROTECT(freeEnv);
    Template* templat = SELF->adjustFormals();
    PROTECT(templat);
    CompilationUnit* bodycu =
        CompilationUnit::create(SELF->expr->body, SELF->cu->info, SELF->expr);
    return bodycu->compileBody(templat, ctEnv, freeEnv);
}


ReflectiveMethodNode::ReflectiveMethodNode(ReflectiveMethodExpr* rme,
                                           bool valueCtxt)
    : MethodNode(sizeof(ReflectiveMethodNode), rme, valueCtxt) {
    ReflectiveMethodNode::updateCnt();
}


ReflectiveMethodNode* ReflectiveMethodNode::create(ReflectiveMethodExpr* rme,
                                                   bool valueCtxt) {
    void* loc = PALLOC1(sizeof(ReflectiveMethodNode), rme);
    return new (loc) ReflectiveMethodNode(rme, valueCtxt);
}


Template* ReflectiveMethodNode::adjustFormals() {
    PROTECT_THIS(ReflectiveMethodNode);
    Template* templat = SELF->expr->formals->makeTemplate();
    if (templat == INVALID)
        SELF->cu->abort("invalid formal parameter template");
    return templat;
}


int ReflectiveMethodNode::constructor() {
    extern Prim* makeReflectiveMethod;
    return makeReflectiveMethod->primNumber();
}


ProcNode::ProcNode(ProcExpr* pe, bool valueCtxt)
    : MethodNode(sizeof(ProcNode), pe, valueCtxt) {
    ProcNode::updateCnt();
}


ProcNode* ProcNode::create(ProcExpr* pe, bool valueCtxt) {
    void* loc = PALLOC1(sizeof(ProcNode), pe);
    return new (loc) ProcNode(pe, valueCtxt);
}


void ProcNode::initialize(pOb ctEnv, pOb freeEnv, Location dest,
                          CompilationUnit* cu) {
    PROTECT_THIS(ProcNode);
    AttrNode::initialize(ctEnv, freeEnv, dest, cu);
    av_size = 4;
    SET_FLAG(word, f_inlineableNode);

    Code* code = compileBody(ctEnv, freeEnv);
    if (code == INVALID)
        SELF->cu->abort();
    ASSIGN(SELF, code, code);
}


Template* ProcNode::adjustFormals() {
    PROTECT_THIS(ProcNode);
    Template* templat = (BASE(SELF->expr->formals))->makeTemplate();
    if (templat == INVALID)
        SELF->cu->abort("invalid formal parameter template");
    return templat;
}


int ProcNode::constructor() {
    extern Prim* makeProc;
    return makeProc->primNumber();
}


void ProcNode::emitDispatchCode(bool ctxtAvailable, bool argvecAvailable,
                                RtnCode rtn, Label next) {
    PROTECT_THIS(ProcNode);

    AttrNode* node = NIL->makeAttrNode(true);
    PROTECT(node);

    node->initialize(TopEnv, TopEnv, LocRslt, SELF->cu);

    if (!ctxtAvailable)
        SELF->emitPush(SELF->av_size);
    else if (!argvecAvailable)
        SELF->emitAlloc(SELF->av_size);

    node->dest = ArgReg(0);
    node->emitXfer(CtxtReg(CRN_Env));

    node->dest = ArgReg(1);
    node->emitLit(SELF->code);

    node->dest = ArgReg(2);
    node->emitLit(SELF->expr->identity);

    node->dest = ArgReg(3);
    node->emitLit(SELF->expr);

    SELF->emitApplyPrim(constructor(), SELF->av_size, false, rtn, next);
}


SeqNode::SeqNode(AttrNode* first, AttrNode* second, bool valueCtxt)
    : CompoundNode(sizeof(SeqNode), valueCtxt), first(first), second(second) {
    SeqNode::updateCnt();
}


SeqNode* SeqNode::create(AttrNode* first, AttrNode* second, bool valueCtxt) {
    void* loc = PALLOC2(sizeof(SeqNode), first, second);
    return new (loc) SeqNode(first, second, valueCtxt);
}


void SeqNode::initialize(pOb ctEnv, pOb freeEnv, Location dest,
                         CompilationUnit* cu) {
    PROTECT_THIS(SeqNode);
    PROTECT(ctEnv);
    PROTECT(freeEnv);

    CompoundNode::initialize(ctEnv, freeEnv, dest, cu);

    SELF->first->initialize(ctEnv, freeEnv, LocRslt, SELF->cu);
    SELF->analyze(SELF->first);
    SELF->second->initialize(ctEnv, freeEnv, dest, SELF->cu);

    SELF->av_size = std::max(SELF->first->av_size, SELF->second->av_size);
    SET_ATTR(*SELF, f_inlineableNode,
             GET_ATTR(*(SELF->first), f_inlineableNode) &&
                 GET_ATTR(*(SELF->second), f_inlineableNode));
}


void SeqNode::changeDest(Location& newloc) { second->changeDest(newloc); }


int SeqNode::numberOfSubExprs() { return 1; }


void SeqNode::emitResumeCode(RtnCode rtn) {
    PROTECT_THIS(SeqNode);
    SELF->CompoundNode::emitResumeCode(rtn);
    SELF->second->emitResumeCode(rtn);
}


void SeqNode::emitWrapup(RtnCode rtn, Label next) {
    second->emitDispatchCode(CtxtAvailable, ArgvecAvailable, rtn, next);
}


SetNode::SetNode(SetExpr* se, bool valueCtxt)
    : CompoundNode(sizeof(SetNode), valueCtxt),
      expr(se),
      trgtNode((SymbolNode*)INVALID),
      valNode((AttrNode*)INVALID) {
    SetNode::updateCnt();
}


SetNode* SetNode::create(SetExpr* se, bool valueCtxt) {
    void* loc = PALLOC1(sizeof(SetNode), se);
    return new (loc) SetNode(se, valueCtxt);
}


void SetNode::initialize(pOb ctEnv, pOb freeEnv, Location dest,
                         CompilationUnit* cu) {
    PROTECT_THIS(SetNode);
    PROTECT(ctEnv);
    PROTECT(freeEnv);

    CompoundNode::initialize(ctEnv, freeEnv, dest, cu);

    SymbolNode* sym = (SymbolNode*)BASE(expr->trgt)->makeAttrNode(true);
    PROTECT(sym);
    sym->initialize(ctEnv, freeEnv, ArgReg(0), SELF->cu);
    ASSIGN(SELF, trgtNode, sym);

    if (sym->loc == LocLimbo)
        cu->abort("can't set! free variables");

    AttrNode* vn = BASE(SELF->expr->val)->makeAttrNode(true);
    ASSIGN(SELF, valNode, vn);
    SELF->valNode->initialize(ctEnv, freeEnv, sym->loc, SELF->cu);
    SELF->analyze(SELF->valNode);

    SET_ATTR(*SELF, f_inlineableNode,
             GET_ATTR(*(SELF->valNode), f_inlineableNode));
    if (GET_ATTR(*SELF, f_inlineableNode)) {
        int n = SELF->valNode->av_size;
        SELF->av_size = (sym->loc == LocRslt ? std::max(2, n) : n);
    }
}


int SetNode::numberOfSubExprs() { return 1; }


void SetNode::emitResumeCode(RtnCode rtn) {
    PROTECT_THIS(SetNode);
    SELF->CompoundNode::emitResumeCode(rtn);
    SELF->valNode->emitResumeCode(rtn);
}


void SetNode::emitWrapup(RtnCode rtn, Label exit) {
    PROTECT_THIS(SetNode);
    Location temp = dest;

    if (rtn == TaggedRtn)
        dest = LocRslt;
    SELF->emitXfer(SELF->valNode->dest);
    SELF->dest = temp;
    SELF->emitRtn(rtn, exit);
}


GotoNode::GotoNode(GotoExpr* ge, bool valueCtxt)
    : AttrNode(sizeof(GotoNode), valueCtxt),
      labelName(ge->label),
      labelNode((LabelNode*)INVALID),
      ctEnv(INVALID) {
    GotoNode::updateCnt();
}


GotoNode* GotoNode::create(GotoExpr* ge, bool valueCtxt) {
    void* loc = PALLOC1(sizeof(GotoNode), ge);
    return new (loc) GotoNode(ge, valueCtxt);
}


void GotoNode::initialize(pOb ctEnv, pOb freeEnv, Location dest,
                          CompilationUnit* cu) {
    AttrNode::initialize(ctEnv, freeEnv, dest, cu);
    SET_FLAG(word, f_inlineableNode);
    SET_FLAG(word, f_simpleNode);
    ASSIGN(this, ctEnv, ctEnv);
    ASSIGN(this, labelNode, cu->labels->getLabelNode(labelName));
    if (labelNode == INVALID)
        cu->abort("unknown goto label '%s'", SYMPTR(labelName));
}


void GotoNode::emitDispatchCode(bool ctxtAvailable, bool argvecAvailable,
                                RtnCode, Label) {
    assert(ctxtAvailable);

    pOb env = ctEnv;
    int n = 0;
    for (; env != labelNode->ctEnv; (n++, env = BASE(env)->parent()))
        if (env == TopEnv)
            cu->abort("attempt to jump to label '%s' not in an enclosing scope",
                      SYMPTR(labelName));
        else if (n > MaximumCut)
            cu->abort(
                "attempt to cut back more than %d lexical levels in jump to "
                "label '%s'",
                MaximumCut, SYMPTR(labelName));

    PROTECT_THIS(GotoNode);

    if (!argvecAvailable)
        SELF->emitAlloc(SELF->av_size);

    if (n != 0) {
        SELF->emitOpAndLabel(opJmpCut, SELF->labelName);
        SELF->emitE0(n, 0);
    } else {
        SELF->emitOpAndLabel(opJmp, SELF->labelName);
    }
}


LabelNode::LabelNode(LabelExpr* le, bool valueCtxt)
    : CompoundNode(sizeof(LabelNode), valueCtxt),
      expr(le),
      bodyNode((AttrNode*)INVALID),
      label(INVALID),
      ctEnv(INVALID) {
    LabelNode::updateCnt();
}


LabelNode* LabelNode::create(LabelExpr* le, bool valueCtxt) {
    void* loc = PALLOC1(sizeof(LabelNode), le);
    return new (loc) LabelNode(le, valueCtxt);
}


void LabelNode::initialize(pOb ctEnv, pOb freeEnv, Location dest,
                           CompilationUnit* cu) {
    PROTECT_THIS(LabelNode);
    PROTECT(ctEnv);
    PROTECT(freeEnv);

    CompoundNode::initialize(ctEnv, freeEnv, dest, cu);

    ASSIGN(SELF, ctEnv, ctEnv);
    SELF->label = FIXNUM(SELF->cu->labels->newExternalLabel(SELF));

    AttrNode* node =
        BASE(SELF->expr->body)->makeAttrNode(GET_ATTR(*SELF, f_valueContext));
    PROTECT(node);
    node->initialize(ctEnv, freeEnv, dest, SELF->cu);
    ASSIGN(SELF, bodyNode, node);
    SELF->analyze(node);

    SELF->av_size = SELF->bodyNode->av_size;
    SELF->outstanding = SELF->bodyNode->outstanding;
    SET_ATTR(*SELF, f_inlineableNode,
             GET_ATTR(*(SELF->bodyNode), f_inlineableNode));
    SET_ATTR(*SELF, f_simpleNode, GET_ATTR(*(SELF->bodyNode), f_simpleNode));
    SET_ATTR(*SELF, f_producesValue,
             GET_ATTR(*(SELF->bodyNode), f_producesValue));
}


void LabelNode::changeDest(Location& newloc) { bodyNode->changeDest(newloc); }


int LabelNode::numberOfSubExprs() { return 1; }


void LabelNode::emitDispatchCode(bool ctxtAvailable, bool argvecAvailable,
                                 RtnCode rtn, Label next) {
    PROTECT_THIS(LabelNode);
    if (!ctxtAvailable)
        SELF->emitPush(0);
    SELF->cu->setLabel((Label)FIXVAL(SELF->label));
    SELF->bodyNode->emitDispatchCode(CtxtAvailable, argvecAvailable, rtn, next);
}


void LabelNode::emitResumeCode(RtnCode rtn) { bodyNode->emitResumeCode(rtn); }


int LabelNode::primNumber() { return bodyNode->primNumber(); }


BUILTIN_CLASS(CompilationUnit) {}


CompilationUnit::CompilationUnit(pOb info, AttrNode* graph, CodeBuf* codebuf,
                                 Tuple* litvec, LabelTable* labels)
    : BinaryOb(sizeof(CompilationUnit), CLASS_META(CompilationUnit),
               CLASS_SBO(CompilationUnit)),
      info(info),
      graph(graph),
      codebuf(codebuf),
      litvec(litvec),
      labels(labels) {
    CompilationUnit::updateCnt();
}


CompilationUnit* CompilationUnit::create(pOb expr, pOb info, pOb source) {
    PROTECT(info);
    PROTECT(source);
    AttrNode* graph = BASE(expr)->makeAttrNode(true);
    PROTECT(graph);
    CodeBuf* codebuf = CodeBuf::create();
    PROTECT(codebuf);
    Tuple* litvec = Tuple::create(1, source);
    PROTECT(litvec);
    LabelTable* labels = LabelTable::create();
    void* loc = PALLOC1(sizeof(CompilationUnit), labels);
    return new (loc) CompilationUnit(info, graph, codebuf, litvec, labels);
}


int CompilationUnit::traversePtrs(PSOb__PSOb f) {
    int sum = BinaryOb::traversePtrs(f);
    sum += useIfPtr(&info, f);
    sum += useIfPtr(&graph, f);
    sum += useIfPtr(&codebuf, f);
    sum += useIfPtr(&litvec, f);
    sum += useIfPtr(&labels, f);
    return sum;
}


int CompilationUnit::traversePtrs(SI__PSOb f) {
    int sum = BinaryOb::traversePtrs(f);
    sum += useIfPtr(info, f);
    sum += useIfPtr(graph, f);
    sum += useIfPtr(codebuf, f);
    sum += useIfPtr(litvec, f);
    sum += useIfPtr(labels, f);
    return sum;
}


void CompilationUnit::traversePtrs(V__PSOb f) {
    BinaryOb::traversePtrs(f);
    useIfPtr(info, f);
    useIfPtr(graph, f);
    useIfPtr(codebuf, f);
    useIfPtr(litvec, f);
    useIfPtr(labels, f);
}


unsigned CompilationUnit::extendLitvec(pOb val) {
    int litOffset = litvec->numberOfElements();

    /*
     * Look to see if the literal is already in the litvec.  This only
     * does an 'EQ' test (i.e., pointer equality), but it is enough to
     * avoid duplication of symbols and large integers, characters, etc.
     */

    for (int i = litOffset; i--;)
        if (val == litvec->elem(i))
            return i;
    if (litOffset > MaximumLitVecSize)
        abort("too many literals for one code object");

    PROTECT_THIS(CompilationUnit);
    Tuple* newlitvec = (Tuple*)SELF->litvec->rcons(val);
    ASSIGN(SELF, litvec, newlitvec);

    return litOffset;
}


Code* CompilationUnit::compileExpr(pOb ctEnv, pOb freeEnv) {
    PROTECT_THIS(CompilationUnit);
    PROTECT(ctEnv);
    PROTECT(freeEnv);

    /*
     * The chain of ProtectedItem nodes is not corrupted by the use of
     * SETJMP and LONGJMP here for only one reason: because this
     * routine itself PROTECTs some values, and because the the
     * destructors for ProtectedItems clean up in a specific way, the
     * ProtectedItem chain will be cleaned up from its (possibly)
     * temporarily invalid state upon exit from this routine.  If values
     * have been PROTECTed in the nested calls between this routine and
     * the point at which LONGJMP is invoked, the chain of ProtectedItem
     * nodes will contain (invalid) pointers into the abandoned part of
     * the stack when we jump back into this routine (this is because
     * automatic destructors are *not* invoked when exiting a routine via
     * a LONGJMP).  However, the ProtectedItem destructors in this
     * routine will "lop off" the invalid links and restore things to a
     * valid state upon exit.  Just make sure that you don't ever do
     * anything to trigger a garbage collection during the period between
     * the return from the longjump and the exit from the routine.
     */

    if (SETJMP(SELF->abortbuf))
        return (Code*)INVALID;
    else
        SELF->graph->initialize(ctEnv, freeEnv, LocRslt, SELF);

    SELF->graph->emitDispatchCode(CtxtAvailable, !ArgvecAvailable, UntaggedRtn,
                                  NoneRemaining);
    SELF->graph->emitResumeCode(UntaggedRtn);
    SELF->labels->resolveLabels(SELF->codebuf);

    Code * cp = Code::create(SELF->codebuf, SELF->litvec);

    return cp;
}

Code* CompilationUnit::compileBody(Template* templat, pOb ctEnv, pOb freeEnv) {
    PROTECT_THIS(CompilationUnit);
    PROTECT(templat);
    PROTECT(ctEnv);
    PROTECT(freeEnv);

    pOb new_ctEnv = BASE(ctEnv)->extendWith(templat->keymeta);
    PROTECT(new_ctEnv);

    /*
     * See the note in CompilationUnit::compileExpr about interactions
     * between PROTECT and setjmp/longjmp.
     */

    if (SETJMP(SELF->abortbuf))
        return (Code*)INVALID;
    else
        SELF->graph->initialize(new_ctEnv, freeEnv, LocRslt, SELF);

    SELF->graph->emitExtend(templat);
    SELF->graph->emitDispatchCode(CtxtAvailable, !ArgvecAvailable, UntaggedRtn,
                                  NoneRemaining);
    SELF->graph->emitResumeCode(UntaggedRtn);
    SELF->labels->resolveLabels(SELF->codebuf);

    Code * cp = Code::create(SELF->codebuf, SELF->litvec);

    return cp;
}


void CompilationUnit::abort() { LONGJMP(abortbuf, 1); }


void CompilationUnit::abort(const char* msg, ...) {
    va_list args;
    va_start(args, msg);
    vwarning("error", msg, args);
    va_end(args);
    abort();
}


void CompilationUnit::warning(const char* msg, ...) {
    va_list args;
    va_start(args, msg);
    vwarning("warning", msg, args);
    va_end(args);
}


void CompilationUnit::vwarning(const char* severity, const char* fmt,
                               va_list args) {
    fprintf(stderr, "*** %s: ", severity);
    vfprintf(stderr, fmt, args);
    if (info != NIV)
        fprintf(stderr, " in %s", BASE(info)->asCstring());
    putc('\n', stderr);
}


void CompilationUnit::atTopLevel() { SET_ATTR(*graph, f_topLevel, true); }


Label CompilationUnit::newLabel() { return labels->newLabel(); }


void CompilationUnit::setLabel(Label label) {
    labels->bindLabel(label, codebuf->size());
}


void CompilationUnit::registerLabel(Label label) {
    assert(label != NoParticularLabel);
    assert(label != NoneRemaining);
    int addrslot = codebuf->size();
    labels->addFixup(addrslot, label);
}


AttrNode* BlockExpr::makeAttrNode(bool valueCtxt) {
    return BlockNode::create(this, valueCtxt);
}


AttrNode* RequestExpr::makeAttrNode(bool valueCtxt) {
    return RequestNode::create(this, valueCtxt);
}


AttrNode* SendExpr::makeAttrNode(bool valueCtxt) {
    if (valueCtxt) {
        /*
         * This technique of dealing with send's in value-expecting
         * contexts relies on two existing properties of the compiler and
         * virtual machine:
         *
         * 	1. The compiler will add a clause to the newly-created
         * 	   block expression that returns a #niv to the parent
         * 	   ctxt before the send is issued.
         *
         * 	2. The virtual machine schedules new strands in stack
         * 	   order.  This is important in those cases where the
         * 	   send expression contains sub-expressions that must
         * 	   themselves be scheduled for evaluation; those
         * 	   sub-expressions must all "complete" before the
         * 	   surrounding ctxt for this send expression is
         * 	   notified of the send's completion.
         */

        Tuple* subExprs = Tuple::create(1, this);
        BlockExpr* blockExpr = BlockExpr::create(subExprs, RBLTRUE);
        return blockExpr->makeAttrNode(true);
    } else {
        return SendNode::create(this, false);
    }
}


AttrNode* TupleExpr::makeAttrNode(bool valueCtxt) {
    if (ConstantP())
        return ConstNode::create(unquote(), valueCtxt);

    int nelems = numberOfElements();
    int tailarg = rest != NILexpr;

    if (nelems + tailarg <= MaxArgs)
        if (!tailarg)
            return TupleNode::create(this, valueCtxt);
        else {
            PROTECT_THIS(TupleExpr);
            TupleExpr* te = TupleExpr::create(nelems + 1);
            te->elem(nelems) = SELF->rest;
            for (int i = nelems; i--;)
                te->elem(i) = SELF->elem(i);
            RequestExpr* re = RequestExpr::create(tplConsStar, te);
            return re->makeAttrNode(valueCtxt);
        }

    else {
        TupleExpr* te = (TupleExpr*)INVALID;
        PROTECT_THIS(TupleExpr);
        PROTECT(te);
        if (nelems < MaxArgs * (MaxArgs - 1)) {
            int nslices = nelems / MaxArgs;
            int nrest = nelems % MaxArgs;
            int semislice = nrest != 0;
            te = TupleExpr::create(nslices + semislice + tailarg);
            for (int i = 0; i < nslices; i++) {
                int offset = i * MaxArgs;
                TupleExpr* temp = SELF->makeSlice(offset, MaxArgs);
                ASSIGN(te, elem(i), temp);
            }

            if (semislice) {
                TupleExpr* temp = SELF->makeSlice(nelems - nrest, nrest);
                ASSIGN(te, elem(nslices), temp);
            }

            if (tailarg) {
                ASSIGN(te, elem(nslices + semislice), SELF->rest);
            }
        } else {
            /*
             * The expression is too big to represent as one concat of
             * MaxArgs-1 tupleexprs (each of maximum length MaxArgs), so
             * we have to break it down recursively.
             */

            int nSlices = MaxArgs - tailarg;
            te = TupleExpr::create(nSlices);
            int sliceSize = nelems / nSlices;
            int excessSize = nelems % nSlices;

            TupleExpr* temp = SELF->makeSlice(0, sliceSize + excessSize);
            ASSIGN(te, elem(0), temp);

            int offset = sliceSize + excessSize;
            for (int i = 1; i < nSlices; (offset += sliceSize, i++)) {
                TupleExpr* temp = SELF->makeSlice(offset, sliceSize);
                ASSIGN(te, elem(i), temp);
            }

            if (tailarg) {
                ASSIGN(te, elem(nSlices), SELF->rest);
            }
        }

        RequestExpr* re = RequestExpr::create(tplConcat, te);
        return re->makeAttrNode(valueCtxt);
    }
}


AttrNode* IfExpr::makeAttrNode(bool valueCtxt) {
    return IfNode::create(this, valueCtxt);
}


AttrNode* LetExpr::makeAttrNode(bool valueCtxt) {
    return LetNode::create(this, valueCtxt);
}


AttrNode* LetrecExpr::makeAttrNode(bool valueCtxt) {
    return LetrecNode::create(this, valueCtxt);
}


AttrNode* QuoteExpr::makeAttrNode(bool valueCtxt) {
    return ConstNode::create(expr, valueCtxt);
}


AttrNode* MethodExpr::makeAttrNode(bool valueCtxt) {
    return MethodNode::create(this, valueCtxt);
}


AttrNode* ReflectiveMethodExpr::makeAttrNode(bool valueCtxt) {
    return ReflectiveMethodNode::create(this, valueCtxt);
}


AttrNode* FreeExpr::makeAttrNode(bool valueCtxt) {
    return FreeNode::create(this, valueCtxt);
}


AttrNode* NullExpr::makeAttrNode(bool valueCtxt) {
    if (valueCtxt)
        return ConstNode::create(NIV, valueCtxt);
    else
        return NullNode::create(valueCtxt);
}


AttrNode* ProcExpr::makeAttrNode(bool valueCtxt) {
    return ProcNode::create(this, valueCtxt);
}


AttrNode* SeqExpr::makeAttrNode(bool valueCtxt) {
    int nexprs = numberOfSubExprs();

    switch (nexprs) {
    case 0:
        if (valueCtxt) {
            return ConstNode::create(NIV, valueCtxt);
        } else {
            return NullNode::create(valueCtxt);
        }

    case 1:
        return BASE(subExprs->elem(0))->makeAttrNode(valueCtxt);

    default: {
        AttrNode* tail = (AttrNode*)INVALID;
        PROTECT_THIS(SeqExpr);
        PROTECT(tail);
        tail = BASE(SELF->subExprs->elem(--nexprs))->makeAttrNode(valueCtxt);
        while (nexprs--) {
            AttrNode* head =
                BASE(SELF->subExprs->elem(nexprs))->makeAttrNode(true);
            tail = SeqNode::create(head, tail, valueCtxt);
        }
        return tail;
    }
    }
}


AttrNode* SetExpr::makeAttrNode(bool valueCtxt) {
    return SetNode::create(this, valueCtxt);
}


AttrNode* GotoExpr::makeAttrNode(bool valueCtxt) {
    return GotoNode::create(this, valueCtxt);
}


AttrNode* LabelExpr::makeAttrNode(bool valueCtxt) {
    return LabelNode::create(this, valueCtxt);
}


MODULE_INIT(Compile) {
    /*
     * Define the positions in litvecs at which interesting information
     * is found.  This is a half-hearted attempt to make the Rosette code
     * a little less sensitive to implementation changes.
     */
    Define("source-offset", FIXNUM(0));
    Define("formals-offset", FIXNUM(1));
}
