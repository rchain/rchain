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

#if !defined(_RBL_Code_h)
#define _RBL_Code_h

#include "rosette.h"
#include "BinaryOb.h"
#include "Ob.h"
#include "Opcode.h"
#include "Tuple.h"

static const int DefaultCodeVecSize = 32;


class CodeBuf : public Ob {
    STD_DECLS(CodeBuf);

   protected:

   public:
    void deposit(Instr);
    void growCodevec(int = DefaultCodeVecSize);
    CodeBuf();

    CodeVec* codevec;
    Ob* pc;

    static CodeBuf* create();

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

    void patchAddress(int, uint16_t);

    int size() { return FIXVAL(pc); }
    void clear();
    CodeVec* finish();
};


class CodeVec : public Word16Vec {
    STD_DECLS(CodeVec);

   public:
    CodeVec(int);

    static CodeVec* create(int);

    Instr* dumpInstr(Instr*, char*, Code*);
    void dumpOn(FILE*, Code*);

    Instr& instr(int n) {
        Instr* p = (Instr*)&word(0);
        return p[n];
    }

    int relativize(Instr* pc) { return pc - &instr(0); }
    Instr* absolutize(int pc) { return &instr(pc); }
};


class Code : public Ob {
    STD_DECLS(Code);

   public:
    Code(CodeVec*, Tuple*);

    CodeVec* codevec;
    Tuple* litvec;

    static Code* create(CodeBuf*, Tuple*);

    Instr* dumpInstr(Instr*, char*);
    void dumpOn(FILE*);
    void ExportCode();

    int relativize(Instr* pc) { return codevec->relativize(pc); }
    Instr* absolutize(int pc) { return codevec->absolutize(pc); }
    Ob* lit(int n) { return (litvec->elem(n)); }

    Ob* associatedSource();
};

#endif
