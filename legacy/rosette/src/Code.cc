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

#include "Code.h"
#include "Compile.h"

#include "Ctxt.h"
#include "Location.h"
#include "Prim.h"
#include "RBLstring.h"
#include "Ob.h"
#include "Number.h"

#include "BuiltinClass.h"
#include "ModuleInit.h"

#include "CommandLine.h"

#include <memory.h>

BUILTIN_CLASS(CodeBuf) {
    OB_FIELD("codevec", CodeBuf, codevec);
    OB_FIELD("pc", CodeBuf, pc);
}


CodeBuf::CodeBuf()
    : Ob(sizeof(CodeBuf), CLASS_META(CodeBuf), CLASS_SBO(CodeBuf)) {
    this->codevec = (CodeVec*)INVALID;
    this->pc = FIXNUM(0);
    CodeBuf::updateCnt();
}


CodeBuf* CodeBuf::create() {
    void* loc = PALLOC(sizeof(CodeBuf));
    return new (loc) CodeBuf();
}

void CodeBuf::deposit(Instr i) {
    int int_pc = FIXVAL(pc);
    Instr* p;

    if (codevec != INVALID && codevec->numberOfWords() > int_pc) {
        FIXNUM_INC(pc);
        p = codevec->absolutize(int_pc);
    } else {
        PROTECT_THIS(CodeBuf);
        SELF->growCodevec(DefaultCodeVecSize);
        FIXNUM_INC(SELF->pc);
        p = SELF->codevec->absolutize(int_pc);
    }

    *p = i;
}


void CodeBuf::growCodevec(int sz) {
    PROTECT_THIS(CodeBuf);

    if (codevec == INVALID) {
        CodeVec* newcodevec = CodeVec::create(sz);
        ASSIGN(SELF, codevec, newcodevec);
    } else {
        CodeVec* newcodevec =
            CodeVec::create(SELF->codevec->numberOfWords() + sz);
        memcpy(&newcodevec->instr(0), &SELF->codevec->instr(0),
               FIXVAL(SELF->pc) * sizeof(Instr));
        ASSIGN(SELF, codevec, newcodevec);
    }
}


void CodeBuf::emitF0(Opcode opcode, unsigned op) {
    Instr i;
    SET_OP_f0_opcode(i, opcode);
    SET_OP_f0_op0(i, op);
    deposit(i);
}


void CodeBuf::emitF1(Opcode opcode, unsigned op0, unsigned op1) {
    Instr i;
    SET_OP_f0_opcode(i, opcode);
    SET_OP_f1_op0(i, op0);
    SET_OP_f1_op1(i, op1);
    deposit(i);
}


void CodeBuf::emitF2(Opcode opcode, unsigned op0, unsigned op1) {
    Instr i;
    SET_OP_f0_opcode(i, opcode);
    SET_OP_f2_op0(i, op0);
    SET_OP_f2_op1(i, op1);
    deposit(i);
}


void CodeBuf::emitF3(Opcode opcode, unsigned op0, unsigned op1, unsigned op2) {
    Instr i;
    SET_OP_f0_opcode(i, opcode);
    SET_OP_f3_op0(i, op0);
    SET_OP_f3_op1(i, op1);
    SET_OP_f3_op2(i, op2);
    deposit(i);
}


void CodeBuf::emitF4(Opcode opcode, unsigned unwind, unsigned next,
                     unsigned nargs, unsigned op0) {
    Instr i;
    SET_OP_f0_opcode(i, opcode);
    SET_OP_f4_unwind(i, unwind);
    SET_OP_f4_next(i, next);
    SET_OP_f4_nargs(i, nargs);
    SET_OP_f4_op0(i, op0);
    deposit(i);
}


void CodeBuf::emitF5(Opcode opcode, unsigned unwind, unsigned next,
                     unsigned op0) {
    Instr i;
    SET_OP_f0_opcode(i, opcode);
    SET_OP_f5_unwind(i, unwind);
    SET_OP_f5_next(i, next);
    SET_OP_f5_op0(i, op0);
    deposit(i);
}


void CodeBuf::emitF6(Opcode opcode, unsigned pc) {
    Instr i;
    SET_OP_f0_opcode(i, opcode);
    SET_OP_f6_pc(i, pc);
    deposit(i);
}


void CodeBuf::emitF7(Opcode opcode, unsigned indirect, unsigned level,
                     unsigned offset, unsigned op0) {
    Instr i;
    SET_OP_f0_opcode(i, opcode);
    SET_OP_f7_indirect(i, indirect);
    SET_OP_f7_level(i, level);
    SET_OP_f7_offset(i, offset);
    SET_OP_f7_op0(i, op0);
    deposit(i);
}


void CodeBuf::emitE0(unsigned op0, unsigned op1) {
    Instr i;
    SET_OP_e0_op0(i, op0);
    SET_OP_e0_op1(i, op1);
    deposit(i);
}


void CodeBuf::emitE1(unsigned op0) {
    Instr i;
    SET_OP_e1_op0(i, op0);
    deposit(i);
}

void CodeBuf::emitE2(unsigned op0, unsigned op1) {
    Instr i;
    Instr j;
    PROTECT_THIS(CodeBuf);
    SET_OP_e0_op0(i, (op0 >= 255) ? 255 : op0);
    SET_OP_e0_op1(i, op1);
    SELF->deposit(i);
    if (op0 >= 255) {
        SET_OP_e1_op0(j, op0);
        SELF->deposit(j);
    }
}

void CodeBuf::patchAddress(int wordAddress, uint16_t realAddress) {
    Instr* pc = codevec->absolutize(wordAddress);
    SET_OP_f6_pc((*pc), realAddress);
}


void CodeBuf::clear() { pc = FIXNUM(0); }


CodeVec* CodeBuf::finish() {
    if (codevec == INVALID || pc == 0) {
        suicide("trying to finish up an empty code vector");
    }

    int int_pc = FIXVAL(pc);
    PROTECT_THIS(CodeBuf);

    CodeVec* newcodevec = CodeVec::create(int_pc);
    memcpy(&newcodevec->instr(0), &SELF->codevec->instr(0),
           int_pc * sizeof(Instr));

    return newcodevec;
}


BUILTIN_CLASS(CodeVec) {}


CodeVec::CodeVec(int numberOfInstrs)
    : Word16Vec(sizeof(CodeVec) + align(numberOfInstrs * sizeof(Instr)),
                CLASS_META(CodeVec), CLASS_SBO(CodeVec), numberOfInstrs) {
    CodeVec::updateCnt();
}


CodeVec* CodeVec::create(int numberOfInstrs) {
    void* loc = PALLOC(sizeof(CodeVec) + align(numberOfInstrs * sizeof(Instr)));
    return new (loc) CodeVec(numberOfInstrs);
}


Instr* CodeVec::dumpInstr(Instr* pc, char* buf, Code* code) {
    static const char* const immediateLitStrings[] = {
        "0", "1", "2", "3", "4", "5", "6", "7", "#t", "#f", "nil", "#niv"};

    int index;
    bool defer;
    Location dest;
    Instr insn = *pc++;

    switch (OP_f0_opcode(insn)) {
    case opHalt:
        strcpy(buf, "halt");
        goto noDest;

    case opPush:
        strcpy(buf, "push");
        goto noDest;

    case opPop:
        strcpy(buf, "pop");
        goto noDest;

    case opNargs:
        sprintf(buf, "nargs %d", (int)OP_f0_op0(insn));
        goto noDest;

    case opAlloc:
        sprintf(buf, "alloc %d", (int)OP_f0_op0(insn));
        goto noDest;

    case opPushAlloc:
        sprintf(buf, "push/alloc %d", (int)OP_f0_op0(insn));
        goto noDest;

    case opExtend:
        sprintf(buf, "extend %d", (int)OP_f0_op0(insn));
        goto noDest;

    case opOutstanding | 0:
    case opOutstanding | 1:
    case opOutstanding | 2:
    case opOutstanding | 3:
        sprintf(buf, "outstanding %d,%d", (int)OP_f6_pc(insn),
                (int)OP_e0_op0((*pc++)));
        goto noDest;

    case opFork | 0:
    case opFork | 1:
    case opFork | 2:
    case opFork | 3:
        sprintf(buf, "fork %d", (int)OP_f6_pc(insn));
        goto noDest;


    case opXmitTag | NextOff | UnwindOff:
    case opXmitTag | NextOff | UnwindOn:
    case opXmitTag | NextOn | UnwindOff:
    case opXmitTag | NextOn | UnwindOn:
        sprintf(buf, "xmit%s%s %d,", OP_f4_unwind(insn) ? "/unwind" : "",
                OP_f4_next(insn) ? "/nxt" : "", OP_f4_nargs(insn));
        dest.atom = code->lit(OP_f4_op0(insn));
        goto formatDest;

    case opXmitArg | NextOff | UnwindOff:
    case opXmitArg | NextOff | UnwindOn:
    case opXmitArg | NextOn | UnwindOff:
    case opXmitArg | NextOn | UnwindOn:
        sprintf(buf, "xmit%s%s %d,arg[%d]", OP_f4_unwind(insn) ? "/unwind" : "",
                OP_f4_next(insn) ? "/nxt" : "", (int)OP_f4_nargs(insn),
                (int)OP_f4_op0(insn));
        goto noDest;

    case opXmitReg | NextOff | UnwindOff:
    case opXmitReg | NextOff | UnwindOn:
    case opXmitReg | NextOn | UnwindOff:
    case opXmitReg | NextOn | UnwindOn:
        sprintf(buf, "xmit%s%s %d,", OP_f4_unwind(insn) ? "/unwind" : "",
                OP_f4_next(insn) ? "/nxt" : "", (int)OP_f4_nargs(insn));
        dest = CtxtReg((CtxtRegName)OP_f4_op0(insn));
        goto formatDest;

    case opXmit | NextOff | UnwindOff:
    case opXmit | NextOff | UnwindOn:
    case opXmit | NextOn | UnwindOff:
    case opXmit | NextOn | UnwindOn:
        sprintf(buf, "xmit%s%s %d", OP_f5_unwind(insn) ? "/unwind" : "",
                OP_f5_next(insn) ? "/nxt" : "", (int)OP_f5_op0(insn));
        goto noDest;


    case opXmitTagXtnd | NextOff | UnwindOff:
    case opXmitTagXtnd | NextOff | UnwindOn:
    case opXmitTagXtnd | NextOn | UnwindOff:
    case opXmitTagXtnd | NextOn | UnwindOn:
        sprintf(buf, "xmit%s%s %d,", OP_f5_unwind(insn) ? "/unwind" : "",
                OP_f5_next(insn) ? "/nxt" : "", OP_f5_op0(insn));
        dest.atom = code->lit(OP_e0_op0((*pc++)));
        goto formatDest;

    case opXmitArgXtnd | NextOff | UnwindOff:
    case opXmitArgXtnd | NextOff | UnwindOn:
    case opXmitArgXtnd | NextOn | UnwindOff:
    case opXmitArgXtnd | NextOn | UnwindOn:
        sprintf(buf, "xmit%s%s %d,arg[%d]", OP_f5_unwind(insn) ? "/unwind" : "",
                OP_f5_next(insn) ? "/nxt" : "", (int)OP_f5_op0(insn),
                (int)OP_e0_op0((*pc++)));
        goto noDest;

    case opXmitRegXtnd | NextOff | UnwindOff:
    case opXmitRegXtnd | NextOff | UnwindOn:
    case opXmitRegXtnd | NextOn | UnwindOff:
    case opXmitRegXtnd | NextOn | UnwindOn:
        sprintf(buf, "xmit%s%s %d,", OP_f5_unwind(insn) ? "/unwind" : "",
                OP_f5_next(insn) ? "/nxt" : "", (int)OP_f5_op0(insn));
        dest = CtxtReg((CtxtRegName)(OP_e0_op0((*pc++))));
        goto formatDest;

    case opSend | NextOff | UnwindOff:
    case opSend | NextOff | UnwindOn:
    case opSend | NextOn | UnwindOff:
    case opSend | NextOn | UnwindOn:
        sprintf(buf, "send%s%s %d", OP_f5_unwind(insn) ? "/unwind" : "",
                OP_f5_next(insn) ? "/nxt" : "", (int)OP_f5_op0(insn));
        goto noDest;

    case opApplyPrimTag | NextOff | UnwindOff:
    case opApplyPrimTag | NextOff | UnwindOn:
    case opApplyPrimTag | NextOn | UnwindOff:
    case opApplyPrimTag | NextOn | UnwindOn: {
        uint16_t extension = (*pc++).word;
        unsigned prim_num = WORD_OP_e0_op0(extension);
        if (prim_num == 255) {
            prim_num = OP_e1_op0((*pc++));
        }

        sprintf(buf, "%s%s%s %d,", SYMPTR(Prim::nthPrim(prim_num)->id),
                OP_f5_unwind(insn) ? "/unwind" : "",
                OP_f5_next(insn) ? "/nxt" : "", (int)OP_f5_op0(insn));
        dest.atom = code->lit(WORD_OP_e0_op1(extension));
        goto formatDest;
    }


    case opApplyPrimArg | NextOff | UnwindOff:
    case opApplyPrimArg | NextOff | UnwindOn:
    case opApplyPrimArg | NextOn | UnwindOff:
    case opApplyPrimArg | NextOn | UnwindOn: {
        uint16_t extension = (*pc++).word;
        unsigned prim_num = WORD_OP_e0_op0(extension);
        if (prim_num == 255) {
            prim_num = OP_e1_op0((*pc++));
        }

        sprintf(buf, "%s%s%s %d,arg[%d]", SYMPTR(Prim::nthPrim(prim_num)->id),
                OP_f5_unwind(insn) ? "/unwind" : "",
                OP_f5_next(insn) ? "/nxt" : "", (int)OP_f5_op0(insn),
                (int)WORD_OP_e0_op1(extension));
        goto noDest;
    }


    case opApplyPrimReg | NextOff | UnwindOff:
    case opApplyPrimReg | NextOff | UnwindOn:
    case opApplyPrimReg | NextOn | UnwindOff:
    case opApplyPrimReg | NextOn | UnwindOn: {
        uint16_t extension = (*pc++).word;
        unsigned prim_num = WORD_OP_e0_op0(extension);
        if (prim_num == 255) {
            prim_num = OP_e1_op0((*pc++));
        }

        sprintf(buf, "%s%s%s %d,", SYMPTR(Prim::nthPrim(prim_num)->id),
                OP_f5_unwind(insn) ? "/unwind" : "",
                OP_f5_next(insn) ? "/nxt" : "", (int)OP_f5_op0(insn));
        dest = CtxtReg((CtxtRegName)WORD_OP_e0_op1(extension));
        goto formatDest;
    }

    case opApplyCmd | NextOff | UnwindOff:
    case opApplyCmd | NextOff | UnwindOn:
    case opApplyCmd | NextOn | UnwindOff:
    case opApplyCmd | NextOn | UnwindOn: {
        uint16_t extension = (*pc++).word;
        unsigned prim_num = WORD_OP_e0_op0(extension);
        if (prim_num == 255) {
            prim_num = OP_e1_op0((*pc++));
        }

        sprintf(buf, "%s%s%s %d", SYMPTR(Prim::nthPrim(prim_num)->id),
                OP_f5_unwind(insn) ? "/unwind" : "",
                OP_f5_next(insn) ? "/nxt" : "", (int)OP_f5_op0(insn));
        goto noDest;
    }


    case opRtnTag | NextOff:
    case opRtnTag | NextOn:
        sprintf(buf, "rtn%s ", OP_f5_next(insn) ? "/nxt" : "");
        dest.atom = code->lit(OP_f5_op0(insn));
        goto formatDest;

    case opRtnArg | NextOff:
    case opRtnArg | NextOn:
        sprintf(buf, "rtn%s arg[%d]", OP_f5_next(insn) ? "/nxt" : "",
                OP_f5_op0(insn));
        goto noDest;

    case opRtnReg | NextOff:
    case opRtnReg | NextOn:
        sprintf(buf, "rtn%s ", OP_f5_next(insn) ? "/nxt" : "");
        dest = CtxtReg((CtxtRegName)OP_f5_op0(insn));
        goto formatDest;

    case opRtn | NextOff:
    case opRtn | NextOn:
        sprintf(buf, "rtn%s", OP_f5_next(insn) ? "/nxt" : "");
        goto noDest;

    case opUpcallRtn | NextOff:
    case opUpcallRtn | NextOn:
        sprintf(buf, "upcall-rtn%s ", OP_f5_next(insn) ? "/nxt" : "");
        dest.atom = code->lit(OP_f5_op0(insn));
        goto formatDest;

    case opUpcallResume:
        strcpy(buf, "upcall-resume");
        goto noDest;

    case opNxt:
        strcpy(buf, "nxt");
        goto noDest;


    case opJmp | 0:
    case opJmp | 1:
    case opJmp | 2:
    case opJmp | 3:
        sprintf(buf, "jmp %d", (int)OP_f6_pc(insn));
        goto noDest;

    case opJmpFalse | 0:
    case opJmpFalse | 1:
    case opJmpFalse | 2:
    case opJmpFalse | 3:
        sprintf(buf, "jf %d", (int)OP_f6_pc(insn));
        goto noDest;

    case opJmpCut | 0:
    case opJmpCut | 1:
    case opJmpCut | 2:
    case opJmpCut | 3:
        sprintf(buf, "jmp/cut %d,%d", (int)OP_e0_op0((*pc++)),
                (int)OP_f6_pc(insn));
        goto noDest;


    case opLookupToArg | 0x0:
    case opLookupToArg | 0x1:
    case opLookupToArg | 0x2:
    case opLookupToArg | 0x3:
    case opLookupToArg | 0x4:
    case opLookupToArg | 0x5:
    case opLookupToArg | 0x6:
    case opLookupToArg | 0x7:
    case opLookupToArg | 0x8:
    case opLookupToArg | 0x9:
    case opLookupToArg | 0xa:
    case opLookupToArg | 0xb:
    case opLookupToArg | 0xc:
    case opLookupToArg | 0xd:
    case opLookupToArg | 0xe:
    case opLookupToArg | 0xf:
        index = OP_f2_op1(insn);
        defer = ((index & CompilationUnit::LookupDeferMask) != 0);
        index &= ~CompilationUnit::LookupDeferMask; 
        sprintf(buf, "lookup%s %d,arg[%d]", defer ? "(defer)" : "", index,
                (int)OP_f2_op0(insn));
        goto noDest;

    case opLookupToReg | 0x0:
    case opLookupToReg | 0x1:
    case opLookupToReg | 0x2:
    case opLookupToReg | 0x3:
    case opLookupToReg | 0x4:
    case opLookupToReg | 0x5:
    case opLookupToReg | 0x6:
    case opLookupToReg | 0x7:
    case opLookupToReg | 0x8:
    case opLookupToReg | 0x9:
    case opLookupToReg | 0xa:
    case opLookupToReg | 0xb:
    case opLookupToReg | 0xc:
    case opLookupToReg | 0xd:
    case opLookupToReg | 0xe:
    case opLookupToReg | 0xf:
        index = OP_f2_op1(insn);
        defer = ((index & CompilationUnit::LookupDeferMask) != 0);
        index &= ~CompilationUnit::LookupDeferMask; 
        sprintf(buf, "lookup%s %d,", defer ? "(defer)" : "", index);
        dest = CtxtReg((CtxtRegName)OP_f2_op0(insn));
        goto formatDest;


    case opXferLexToArg | 0:
    case opXferLexToArg | 1:
    case opXferLexToArg | 2:
    case opXferLexToArg | 3:
    case opXferLexToArg | 4:
    case opXferLexToArg | 5:
    case opXferLexToArg | 6:
    case opXferLexToArg | 7:
    case opXferLexToArg | IndirectOn | 0:
    case opXferLexToArg | IndirectOn | 1:
    case opXferLexToArg | IndirectOn | 2:
    case opXferLexToArg | IndirectOn | 3:
    case opXferLexToArg | IndirectOn | 4:
    case opXferLexToArg | IndirectOn | 5:
    case opXferLexToArg | IndirectOn | 6:
    case opXferLexToArg | IndirectOn | 7:
        sprintf(buf, (OP_f7_indirect(insn) ? "xfer lex[%d,(%d)],arg[%d]"
                                           : "xfer lex[%d,%d],arg[%d]"),
                (int)OP_f7_level(insn), (int)OP_f7_offset(insn),
                (int)OP_f7_op0(insn));
        goto noDest;

    case opXferLexToReg | 0:
    case opXferLexToReg | 1:
    case opXferLexToReg | 2:
    case opXferLexToReg | 3:
    case opXferLexToReg | 4:
    case opXferLexToReg | 5:
    case opXferLexToReg | 6:
    case opXferLexToReg | 7:
    case opXferLexToReg | IndirectOn | 0:
    case opXferLexToReg | IndirectOn | 1:
    case opXferLexToReg | IndirectOn | 2:
    case opXferLexToReg | IndirectOn | 3:
    case opXferLexToReg | IndirectOn | 4:
    case opXferLexToReg | IndirectOn | 5:
    case opXferLexToReg | IndirectOn | 6:
    case opXferLexToReg | IndirectOn | 7:
        sprintf(buf, (OP_f7_indirect(insn) ? "xfer lex[%d,(%d)],"
                                           : "xfer lex[%d,%d],"),
                (int)OP_f7_level(insn), (int)OP_f7_offset(insn));
        dest = CtxtReg((CtxtRegName)OP_f7_op0(insn));
        goto formatDest;

    case opXferGlobalToArg:
        sprintf(buf, "xfer global[%s],arg[%d]",
                BASE(GlobalEnv->entryKey(OP_e1_op0((*pc++))))->asCstring(),
                (int)OP_f0_op0(insn));
        goto noDest;

    case opXferGlobalToReg:
        sprintf(buf, "xfer global[%s],",
                BASE(GlobalEnv->entryKey(OP_e1_op0((*pc++))))->asCstring());
        dest = CtxtReg((CtxtRegName)OP_f0_op0(insn));
        goto formatDest;

    case opXferArgToArg:
        sprintf(buf, "xfer arg[%d],arg[%d]", (int)OP_f1_op1(insn),
                (int)OP_f1_op0(insn));
        goto noDest;

    case opXferRsltToArg:
        sprintf(buf, "xfer rslt,arg[%d]", (int)OP_f0_op0(insn));
        goto noDest;

    case opXferArgToRslt:
        sprintf(buf, "xfer arg[%d],rslt", (int)OP_f0_op0(insn));
        goto noDest;

    case opXferRsltToReg:
        strcpy(buf, "xfer rslt,");
        dest = CtxtReg((CtxtRegName)OP_f0_op0(insn));
        goto formatDest;

    case opXferRegToRslt:
        strcpy(buf, "xfer ");
        printRep(CtxtReg((CtxtRegName)OP_f0_op0(insn)), &buf[strlen(buf)]);
        strcat(buf, ",rslt");
        goto noDest;

    case opXferRsltToDest:
        strcpy(buf, "xfer rslt,");
        dest.atom = code->lit(OP_f0_op0(insn));
        goto formatDest;

    case opXferSrcToRslt:
        strcpy(buf, "xfer ");
        dest.atom = code->lit(OP_f0_op0(insn));
        printRep(dest, &buf[strlen(buf)]);
        strcat(buf, ",rslt");
        goto noDest;


    case opIndLitToArg:
        sprintf(buf, "liti %d,arg[%d]", (int)OP_f1_op1(insn),
                (int)OP_f1_op0(insn));
        goto noDest;

    case opIndLitToReg:
        sprintf(buf, "liti %d,", (int)OP_f1_op1(insn));
        dest = CtxtReg((CtxtRegName)OP_f1_op0(insn));
        goto formatDest;

    case opIndLitToRslt:
        sprintf(buf, "liti %d,rslt", (int)OP_f0_op0(insn));
        goto noDest;

    case opImmediateLitToArg | 0x0:
    case opImmediateLitToArg | 0x1:
    case opImmediateLitToArg | 0x2:
    case opImmediateLitToArg | 0x3:
    case opImmediateLitToArg | 0x4:
    case opImmediateLitToArg | 0x5:
    case opImmediateLitToArg | 0x6:
    case opImmediateLitToArg | 0x7:
    case opImmediateLitToArg | 0x8:
    case opImmediateLitToArg | 0x9:
    case opImmediateLitToArg | 0xa:
    case opImmediateLitToArg | 0xb:
        sprintf(buf, "lit %s,arg[%d]", immediateLitStrings[OP_f2_op0(insn)],
                (int)OP_f2_op1(insn));
        goto noDest;

    case opImmediateLitToReg | 0x0:
    case opImmediateLitToReg | 0x1:
    case opImmediateLitToReg | 0x2:
    case opImmediateLitToReg | 0x3:
    case opImmediateLitToReg | 0x4:
    case opImmediateLitToReg | 0x5:
    case opImmediateLitToReg | 0x6:
    case opImmediateLitToReg | 0x7:
    case opImmediateLitToReg | 0x8:
    case opImmediateLitToReg | 0x9:
    case opImmediateLitToReg | 0xa:
    case opImmediateLitToReg | 0xb:
        sprintf(buf, "lit %s,", immediateLitStrings[OP_f2_op0(insn)]);
        dest = CtxtReg((CtxtRegName)OP_f2_op1(insn));
        goto formatDest;

    default:
        sprintf(buf, "illegal 0x%.4x", (int)insn.word);
        goto noDest;

    }  // end switch (opcode)

formatDest:
    printRep(dest, &buf[strlen(buf)]);

noDest:
    return pc;
}


void CodeVec::dumpOn(FILE* f, Code* code) {
    Instr* pc = absolutize(0);
    Instr* const start = pc;
    Instr* const end = absolutize(numberOfWords());
    char outputBuffer[128];

    while (pc < end) {
        sprintf(outputBuffer, "%4d:\t", (int)(pc - start));
        /*
         * outputBuffer[6] (i.e., just past the address) is where
         * dumpInstr should start putting down its result.
         */
        pc = dumpInstr(pc, &outputBuffer[6], code);
        fprintf(f, "%s\n", outputBuffer);
    }
}


BUILTIN_CLASS(Code) {
    OB_FIELD("codevec", Code, codevec);
    OB_FIELD("litvec", Code, litvec);
}


Code::Code(CodeVec* codevec, Tuple* litvec)
    : Ob(sizeof(Code), CLASS_META(Code), CLASS_SBO(Code)) {
    this->codevec = codevec;
    this->litvec = litvec;
    Code::updateCnt();
}

Code* Code::create(CodeBuf* codebuf, Tuple* litvec) {
    PROTECT(litvec);
    CodeVec* codevec = codebuf->finish();
    void* loc = PALLOC1(sizeof(Code), codevec);
    return new (loc) Code(codevec, litvec);
}


Instr* Code::dumpInstr(Instr* pc, char* buf) {
    sprintf(buf, "%4d:\t", relativize(pc));
    return codevec->dumpInstr(pc, buf + 6, this);
}


void Code::dumpOn(FILE* f) {
    if (litvec != NIL) {
        fprintf(f, "litvec:\n");
        for (int i = 0; i < litvec->numberOfElements(); i++) {
            fprintf(f, "%4d:\t", i);
            BASE(litvec->elem(i))->printQuotedOn(f);
            fprintf(f, " (%llu)\n", BASE(litvec->elem(i))->objectId);
        }
    }
    fprintf(f, "codevec:\n");
    codevec->dumpOn(f, this);
}


Ob* Code::associatedSource() {
    if (litvec == NIL) {
        return SYMBOL("***source unavailable***");
    }

    return lit(0);
}

char* opcodeStrings[MaxOpcodes];

MODULE_INIT(Code) {
    extern int RestoringImage;

    if (!RestoringImage) {
        for (int i = 0; i < MaxOpcodes; i++) {
            opcodeStrings[i] = NULL;
        }

        opcodeStrings[opHalt] = "halt";
        opcodeStrings[opPush] = "push";
        opcodeStrings[opPop] = "pop";
        opcodeStrings[opNargs] = "nargs";
        opcodeStrings[opAlloc] = "alloc";
        opcodeStrings[opPushAlloc] = "push/alloc";
        opcodeStrings[opExtend] = "extend";

        opcodeStrings[opOutstanding | 0] = "outstanding";
        opcodeStrings[opOutstanding | 1] = "outstanding";
        opcodeStrings[opOutstanding | 2] = "outstanding";
        opcodeStrings[opOutstanding | 3] = "outstanding";
        opcodeStrings[opFork | 0] = "fork";
        opcodeStrings[opFork | 1] = "fork";
        opcodeStrings[opFork | 2] = "fork";
        opcodeStrings[opFork | 3] = "fork";

        opcodeStrings[opXmitTag | NextOff | UnwindOff] = "xmit/tag";
        opcodeStrings[opXmitTag | NextOff | UnwindOn] = "xmit/tag/unwind";
        opcodeStrings[opXmitTag | NextOn | UnwindOff] = "xmit/tag/nxt";
        opcodeStrings[opXmitTag | NextOn | UnwindOn] = "xmit/tag/unwind/nxt";
        opcodeStrings[opXmitArg | NextOff | UnwindOff] = "xmit/arg";
        opcodeStrings[opXmitArg | NextOff | UnwindOn] = "xmit/arg/unwind";
        opcodeStrings[opXmitArg | NextOn | UnwindOff] = "xmit/arg/nxt";
        opcodeStrings[opXmitArg | NextOn | UnwindOn] = "xmit/arg/unwind/nxt";
        opcodeStrings[opXmitReg | NextOff | UnwindOff] = "xmit/reg";
        opcodeStrings[opXmitReg | NextOff | UnwindOn] = "xmit/reg/unwind";
        opcodeStrings[opXmitReg | NextOn | UnwindOff] = "xmit/reg/nxt";
        opcodeStrings[opXmitReg | NextOn | UnwindOn] = "xmit/reg/unwind/nxt";
        opcodeStrings[opXmit | NextOff | UnwindOff] = "xmit";
        opcodeStrings[opXmit | NextOff | UnwindOn] = "xmit/unwind";
        opcodeStrings[opXmit | NextOn | UnwindOff] = "xmit/nxt";
        opcodeStrings[opXmit | NextOn | UnwindOn] = "xmit/unwind/nxt";
        opcodeStrings[opXmitTagXtnd | NextOff | UnwindOff] = "xmit/tag/xtnd";
        opcodeStrings[opXmitTagXtnd | NextOff | UnwindOn] =
            "xmit/tag/unwind/xtnd";
        opcodeStrings[opXmitTagXtnd | NextOn | UnwindOff] = "xmit/tag/nxt/xtnd";
        opcodeStrings[opXmitTagXtnd | NextOn | UnwindOn] =
            "xmit/tag/unwind/nxt/xtnd";
        opcodeStrings[opXmitArgXtnd | NextOff | UnwindOff] = "xmit/arg/xtnd";
        opcodeStrings[opXmitArgXtnd | NextOff | UnwindOn] =
            "xmit/arg/unwind/xtnd";
        opcodeStrings[opXmitArgXtnd | NextOn | UnwindOff] = "xmit/arg/nxt/xtnd";
        opcodeStrings[opXmitArgXtnd | NextOn | UnwindOn] =
            "xmit/arg/unwind/nxt/xtnd";
        opcodeStrings[opXmitRegXtnd | NextOff | UnwindOff] = "xmit/reg/xtnd";
        opcodeStrings[opXmitRegXtnd | NextOff | UnwindOn] =
            "xmit/reg/unwind/xtnd";
        opcodeStrings[opXmitRegXtnd | NextOn | UnwindOff] = "xmit/reg/nxt/xtnd";
        opcodeStrings[opXmitRegXtnd | NextOn | UnwindOn] =
            "xmit/reg/unwind/nxt/xtnd";
        opcodeStrings[opSend | NextOff | UnwindOff] = "send/reg";
        opcodeStrings[opSend | NextOff | UnwindOn] = "send/unwind";
        opcodeStrings[opSend | NextOn | UnwindOff] = "send/nxt";
        opcodeStrings[opSend | NextOn | UnwindOn] = "send/unwind/nxt";

        opcodeStrings[opApplyPrimTag | NextOff | UnwindOff] = "applyprim/tag";
        opcodeStrings[opApplyPrimTag | NextOff | UnwindOn] =
            "applyprim/tag/unwind";
        opcodeStrings[opApplyPrimTag | NextOn | UnwindOff] =
            "applyprim/tag/nxt";
        opcodeStrings[opApplyPrimTag | NextOn | UnwindOn] =
            "applyprim/tag/unwind/nxt";
        opcodeStrings[opApplyPrimArg | NextOff | UnwindOff] = "applyprim/arg";
        opcodeStrings[opApplyPrimArg | NextOff | UnwindOn] =
            "applyprim/arg/unwind";
        opcodeStrings[opApplyPrimArg | NextOn | UnwindOff] =
            "applyprim/arg/nxt";
        opcodeStrings[opApplyPrimArg | NextOn | UnwindOn] =
            "applyprim/arg/unwind/nxt";
        opcodeStrings[opApplyPrimReg | NextOff | UnwindOff] = "applyprim/reg";
        opcodeStrings[opApplyPrimReg | NextOff | UnwindOn] =
            "applyprim/reg/unwind";
        opcodeStrings[opApplyPrimReg | NextOn | UnwindOff] =
            "applyprim/reg/nxt";
        opcodeStrings[opApplyPrimReg | NextOn | UnwindOn] =
            "applyprim/reg/unwind/nxt";

        opcodeStrings[opApplyCmd | NextOff | UnwindOff] = "applycmd";
        opcodeStrings[opApplyCmd | NextOff | UnwindOn] = "applycmd/unwind";
        opcodeStrings[opApplyCmd | NextOn | UnwindOff] = "applycmd/nxt";
        opcodeStrings[opApplyCmd | NextOn | UnwindOn] = "applycmd/unwind/nxt";

        opcodeStrings[opRtnTag | NextOff] = "rtn/tag";
        opcodeStrings[opRtnTag | NextOn] = "rtn/tag/nxt";
        opcodeStrings[opRtnArg | NextOff] = "rtn/arg";
        opcodeStrings[opRtnArg | NextOn] = "rtn/arg/nxt";
        opcodeStrings[opRtnReg | NextOff] = "rtn/reg";
        opcodeStrings[opRtnReg | NextOn] = "rtn/reg/nxt";
        opcodeStrings[opRtn | NextOff] = "rtn";
        opcodeStrings[opRtn | NextOn] = "rtn/nxt";
        opcodeStrings[opUpcallRtn | NextOff] = "upcall-rtn";
        opcodeStrings[opUpcallRtn | NextOn] = "upcall-rtn/nxt";
        opcodeStrings[opUpcallResume] = "upcall resume";
        opcodeStrings[opNxt] = "nxt";

        opcodeStrings[opJmp | 0] = "jmp";
        opcodeStrings[opJmp | 1] = "jmp";
        opcodeStrings[opJmp | 2] = "jmp";
        opcodeStrings[opJmp | 3] = "jmp";
        opcodeStrings[opJmpFalse | 0] = "jf";
        opcodeStrings[opJmpFalse | 1] = "jf";
        opcodeStrings[opJmpFalse | 2] = "jf";
        opcodeStrings[opJmpFalse | 3] = "jf";
        opcodeStrings[opJmpCut | 0] = "jmp/cut";
        opcodeStrings[opJmpCut | 1] = "jmp/cut";
        opcodeStrings[opJmpCut | 2] = "jmp/cut";
        opcodeStrings[opJmpCut | 3] = "jmp/cut";

        opcodeStrings[opLookupToArg | 0x0] = "lookup->arg";
        opcodeStrings[opLookupToArg | 0x1] = "lookup->arg";
        opcodeStrings[opLookupToArg | 0x2] = "lookup->arg";
        opcodeStrings[opLookupToArg | 0x3] = "lookup->arg";
        opcodeStrings[opLookupToArg | 0x4] = "lookup->arg";
        opcodeStrings[opLookupToArg | 0x5] = "lookup->arg";
        opcodeStrings[opLookupToArg | 0x6] = "lookup->arg";
        opcodeStrings[opLookupToArg | 0x7] = "lookup->arg";
        opcodeStrings[opLookupToArg | 0x8] = "lookup->arg";
        opcodeStrings[opLookupToArg | 0x9] = "lookup->arg";
        opcodeStrings[opLookupToArg | 0xa] = "lookup->arg";
        opcodeStrings[opLookupToArg | 0xb] = "lookup->arg";
        opcodeStrings[opLookupToArg | 0xc] = "lookup->arg";
        opcodeStrings[opLookupToArg | 0xd] = "lookup->arg";
        opcodeStrings[opLookupToArg | 0xe] = "lookup->arg";
        opcodeStrings[opLookupToArg | 0xf] = "lookup->arg";
        opcodeStrings[opLookupToReg | 0x0] = "lookup->reg";
        opcodeStrings[opLookupToReg | 0x1] = "lookup->reg";
        opcodeStrings[opLookupToReg | 0x2] = "lookup->reg";
        opcodeStrings[opLookupToReg | 0x3] = "lookup->reg";
        opcodeStrings[opLookupToReg | 0x4] = "lookup->reg";
        opcodeStrings[opLookupToReg | 0x5] = "lookup->reg";
        opcodeStrings[opLookupToReg | 0x6] = "lookup->reg";
        opcodeStrings[opLookupToReg | 0x7] = "lookup->reg";
        opcodeStrings[opLookupToReg | 0x8] = "lookup->reg";
        opcodeStrings[opLookupToReg | 0x9] = "lookup->reg";
        opcodeStrings[opLookupToReg | 0xa] = "lookup->reg";
        opcodeStrings[opLookupToReg | 0xb] = "lookup->reg";
        opcodeStrings[opLookupToReg | 0xc] = "lookup->reg";
        opcodeStrings[opLookupToReg | 0xd] = "lookup->reg";
        opcodeStrings[opLookupToReg | 0xe] = "lookup->reg";
        opcodeStrings[opLookupToReg | 0xf] = "lookup->reg";

        opcodeStrings[opXferLexToArg | IndirectOff | 0] = "lex[0,]->arg";
        opcodeStrings[opXferLexToArg | IndirectOff | 1] = "lex[1,]->arg";
        opcodeStrings[opXferLexToArg | IndirectOff | 2] = "lex[2,]->arg";
        opcodeStrings[opXferLexToArg | IndirectOff | 3] = "lex[3,]->arg";
        opcodeStrings[opXferLexToArg | IndirectOff | 4] = "lex[4,]->arg";
        opcodeStrings[opXferLexToArg | IndirectOff | 5] = "lex[5,]->arg";
        opcodeStrings[opXferLexToArg | IndirectOff | 6] = "lex[6,]->arg";
        opcodeStrings[opXferLexToArg | IndirectOff | 7] = "lex[7,]->arg";
        opcodeStrings[opXferLexToArg | IndirectOn | 0] = "lex[0,()]->arg";
        opcodeStrings[opXferLexToArg | IndirectOn | 1] = "lex[1,()]->arg";
        opcodeStrings[opXferLexToArg | IndirectOn | 2] = "lex[2,()]->arg";
        opcodeStrings[opXferLexToArg | IndirectOn | 3] = "lex[3,()]->arg";
        opcodeStrings[opXferLexToArg | IndirectOn | 4] = "lex[4,()]->arg";
        opcodeStrings[opXferLexToArg | IndirectOn | 5] = "lex[5,()]->arg";
        opcodeStrings[opXferLexToArg | IndirectOn | 6] = "lex[6,()]->arg";
        opcodeStrings[opXferLexToArg | IndirectOn | 7] = "lex[7,()]->arg";

        opcodeStrings[opXferLexToReg | IndirectOff | 0] = "lex[0,]->reg";
        opcodeStrings[opXferLexToReg | IndirectOff | 1] = "lex[1,]->reg";
        opcodeStrings[opXferLexToReg | IndirectOff | 2] = "lex[2,]->reg";
        opcodeStrings[opXferLexToReg | IndirectOff | 3] = "lex[3,]->reg";
        opcodeStrings[opXferLexToReg | IndirectOff | 4] = "lex[4,]->reg";
        opcodeStrings[opXferLexToReg | IndirectOff | 5] = "lex[5,]->reg";
        opcodeStrings[opXferLexToReg | IndirectOff | 6] = "lex[6,]->reg";
        opcodeStrings[opXferLexToReg | IndirectOff | 7] = "lex[7,]->reg";
        opcodeStrings[opXferLexToReg | IndirectOn | 0] = "lex[0,()]->reg";
        opcodeStrings[opXferLexToReg | IndirectOn | 1] = "lex[1,()]->reg";
        opcodeStrings[opXferLexToReg | IndirectOn | 2] = "lex[2,()]->reg";
        opcodeStrings[opXferLexToReg | IndirectOn | 3] = "lex[3,()]->reg";
        opcodeStrings[opXferLexToReg | IndirectOn | 4] = "lex[4,()]->reg";
        opcodeStrings[opXferLexToReg | IndirectOn | 5] = "lex[5,()]->reg";
        opcodeStrings[opXferLexToReg | IndirectOn | 6] = "lex[6,()]->reg";
        opcodeStrings[opXferLexToReg | IndirectOn | 7] = "lex[7,()]->reg";

        opcodeStrings[opXferGlobalToArg] = "global->arg";
        opcodeStrings[opXferGlobalToReg] = "global->reg";
        opcodeStrings[opXferArgToArg] = "arg->arg";
        opcodeStrings[opXferRsltToArg] = "rslt->arg";
        opcodeStrings[opXferArgToRslt] = "arg->rslt";
        opcodeStrings[opXferRsltToReg] = "rslt->reg";
        opcodeStrings[opXferRegToRslt] = "reg->rslt";
        opcodeStrings[opXferRsltToDest] = "rslt->dest";
        opcodeStrings[opXferSrcToRslt] = "src->rslt";

        opcodeStrings[opIndLitToArg] = "liti->arg";
        opcodeStrings[opIndLitToReg] = "liti->reg";
        opcodeStrings[opIndLitToRslt] = "liti->rslt";

        opcodeStrings[opImmediateLitToArg | 0x0] = "lit 0/arg";
        opcodeStrings[opImmediateLitToArg | 0x1] = "lit 1/arg";
        opcodeStrings[opImmediateLitToArg | 0x2] = "lit 2/arg";
        opcodeStrings[opImmediateLitToArg | 0x3] = "lit 3/arg";
        opcodeStrings[opImmediateLitToArg | 0x4] = "lit 4/arg";
        opcodeStrings[opImmediateLitToArg | 0x5] = "lit 5/arg";
        opcodeStrings[opImmediateLitToArg | 0x6] = "lit 6/arg";
        opcodeStrings[opImmediateLitToArg | 0x7] = "lit 7/arg";
        opcodeStrings[opImmediateLitToArg | 0x8] = "lit #t/arg";
        opcodeStrings[opImmediateLitToArg | 0x9] = "lit #f/arg";
        opcodeStrings[opImmediateLitToArg | 0xa] = "lit nil/arg";
        opcodeStrings[opImmediateLitToArg | 0xb] = "lit #niv/arg";

        opcodeStrings[opImmediateLitToReg | 0x0] = "lit 0/reg";
        opcodeStrings[opImmediateLitToReg | 0x1] = "lit 1/reg";
        opcodeStrings[opImmediateLitToReg | 0x2] = "lit 2/reg";
        opcodeStrings[opImmediateLitToReg | 0x3] = "lit 3/reg";
        opcodeStrings[opImmediateLitToReg | 0x4] = "lit 4/reg";
        opcodeStrings[opImmediateLitToReg | 0x5] = "lit 5/reg";
        opcodeStrings[opImmediateLitToReg | 0x6] = "lit 6/reg";
        opcodeStrings[opImmediateLitToReg | 0x7] = "lit 7/reg";
        opcodeStrings[opImmediateLitToReg | 0x8] = "lit #t/reg";
        opcodeStrings[opImmediateLitToReg | 0x9] = "lit #f/reg";
        opcodeStrings[opImmediateLitToReg | 0xa] = "lit nil/reg";
        opcodeStrings[opImmediateLitToReg | 0xb] = "lit #niv/reg";

    }
}


DEF("code-dump", codeDump, 1, 1) {
    CHECK(0, Code, code);
    code->dumpOn(stdout);
    return NIV;
}


DEF("opcode->string", opcodeString, 1, 1) {
    CHECK_FIXNUM(0, opcode);

    if (0 <= opcode && opcode < MaxOpcodes) {
        char* str = opcodeStrings[opcode];
        if (str) {
            return RBLstring::create(str);
        } else {
            char buf[32];
            sprintf(buf, "unknown:0x%2x", opcode);
            return RBLstring::create(buf);
        }
    } else {
        return PRIM_ERROR("invalid opcode");
    }
}


DEF("primNumber", prim_primNumber__Prim, 1, 1) {
    CHECK(0, Prim, prim);
    return FIXNUM(prim->primNumber());
}
