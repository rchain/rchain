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

#if !defined(_RBL_Parser_h)
#define _RBL_Parser_h 1

#include "rosette.h"
#include "BinaryOb.h"
#include "ObStk.h"

#define OPTIMIZE_ATOMS

class ParseMacro;
class ParserFrame;
class ParseTable;
class PFrameStk;
class Parser;


class PFrameStk {
    int topframe;
    int nexttop;
    char* stk;
    int stksize;

    PFrameStk();
    ~PFrameStk();

    void* alloc(int);
    int link(int);
    ParserFrame* top();
    void pop();
    int empty();
    void reset();

    friend class Parser;
};


enum ParserMode {
    START,
    CONTINUE,
    STOP,
#if defined(OPTIMIZE_ATOMS)
    GROK_ATOM,
#endif
};

enum WaitMode {
    NOT_WAITING,
    WAITING_FOR_EXPR,
};


class Parser : public BinaryOb {
    STD_DECLS(Parser);

   protected:
    Parser(ParseTable*);

    Ob* resumeExpr();
    Ob* suspendParser();
    Ob* finish(Ob*);
    void growBuffer();
    ParserMode acceptEscChar(int, int);

   public:
    virtual ~Parser();

    static Parser* create();

    RBLstring* inbuf;
    int inp;
    char* buf;
    int bufsize;
    int bufp;
    int errorEncountered;
    WaitMode waitingOnIO;
    ParseTable* rt;
    ParserMode mode;
    int digitSeen;
    PFrameStk fstk;
    ObStk ostk;

    void buffer(int);
    void resetBuffer();
    char* finalizeBuffer();
    Ob* finalizeAtom();

    Ob* readExpr();
    Ob* resume(RBLstring*);
    Ob* error(const char*, ...);
    void resetState();

    void opush(Ob*);
    Ob*& otop(int);
    Ob* opop();
    void odel(int);

    void* falloc(int);
    int flink(int);
    ParserFrame* ftop();
    void fpop();

    ParserMode accept(int, int = false);
    ParserMode receiveOb(Ob*);
    ParserMode receiveChar(int);
    ParserMode receiveTerminator(int);
    ParserMode receiveDot(int);

    virtual Ob* cloneTo(Ob*, Ob*);
    virtual int traversePtrs(PSOb__PSOb);
    virtual int traversePtrs(SI__PSOb);
    virtual void traversePtrs(V__PSOb);
};


/* extern Parser* StdinParser; */

#endif
