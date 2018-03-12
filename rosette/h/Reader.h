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

#if !defined(_RBL_Reader_h)
#define _RBL_Reader_h 1

#include "rosette.h"
#include "BinaryOb.h"
#include "ObStk.h"

#define OPTIMIZE_ATOMS

class ReadMacro;
class ReaderFrame;
class ReadTable;
class FrameStk;
class Reader;


class FrameStk {
    int topframe;
    int nexttop;
    char* stk;
    int stksize;

    FrameStk();
    ~FrameStk();

    void* alloc(int);
    int link(int);
    ReaderFrame* top();
    void pop();
    int empty();
    void reset();

    friend class Reader;
};


enum ReaderMode {
    START,
    CONTINUE,
    STOP,
#if defined(OPTIMIZE_ATOMS)
    GROK_ATOM
#endif
};


#undef ftop

class Reader : public BinaryOb {
    STD_DECLS(Reader);

   protected:
    char* buf;
    int bufsize;
    int bufp;
    char errorEncountered;
    enum { NOT_WAITING, WAITING_FOR_EXPR, WAITING_FOR_CHAR } waitingOnIO;
    uint16_t filler_up_please;
    FrameStk fstk;
    ObStk ostk;

    Reader(ReadTable*, FILE*);

    Ob* resumeExpr();
    Ob* resumeCh();
    Ob* suspendReader();
    Ob* finish(Ob*);
    void growBuffer();
    ReaderMode acceptEscChar(int, int);

   public:
    virtual ~Reader();

    static Reader* create(FILE*);

    ReadTable* rt;
    ReaderMode mode;
    FILE* file;
    int digitSeen;

    void buffer(int);
    void resetBuffer();
    char* finalizeBuffer();
    Ob* finalizeAtom();

    Ob* readExpr();
    Ob* readCh();
    Ob* resume();
    Ob* error(const char*, ...);
    void resetState();

    void opush(Ob*);
    Ob*& otop(int);
    Ob* opop();
    void odel(int);

    void* falloc(int);
    int flink(int);
    ReaderFrame* ftop();
    void fpop();

    ReaderMode accept(int, int = false);
    ReaderMode receiveOb(Ob*);
    ReaderMode receiveChar(int);
    ReaderMode receiveTerminator(int);
    ReaderMode receiveDot(int);

    virtual Ob* cloneTo(Ob*, Ob*);
    virtual int traversePtrs(PSOb__PSOb);
    virtual int traversePtrs(SI__PSOb);
    virtual void traversePtrs(V__PSOb);
};


extern Reader* StdinReader;

#endif
