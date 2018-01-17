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

#include "Parser.h"
#include "Expr.h"
#include "Number.h"
#include "Ob.h"
#include "ObStk.h"
#include "Prim.h"
#include "RBLstring.h"
#include "BuiltinClass.h"

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <signal.h>
#include <errno.h>
#include <assert.h>
#include <memory.h>
#include <stdarg.h>

#define OPTIMIZE_ATOMS


class ParserFrame {
   protected:
    ParserFrame(Parser*, int);

    void* operator new(size_t);
    void* operator new(size_t, void*);

    int link;

    friend class SpecialPFrame;
    friend class CommentParseMacro;
    friend class PFrameStk;
    friend class StringParseMacro;
    friend class _SpecialParseMacro;
    friend class QuoteParseMacro;
    friend class ListParseMacro;
    friend class Parser;

   public:
    virtual ParserMode process(int, Parser*);
    virtual ParserMode receiveOb(Ob*, Parser*);
    virtual ParserMode receiveChar(int, Parser*);
    virtual ParserMode receiveTerminator(int, Parser*);
    virtual ParserMode receiveDot(int, Parser*);
    virtual ParserMode receiveEof(Parser*);
};

void debug_builtinprim(char* s) { /* printf("listing %s\n", s); */
}


void* ParserFrame::operator new(size_t s) {
    suicide("operator new not allowed for Rosette objects\n");
    return NULL;
}

void* ParserFrame::operator new(size_t, void* p) { return p; }


class ParseMacro {
   public:
    ParseMacro();

    virtual ParserMode start(int, Parser*) = 0;
};


static const int NCHARS = 128;
static const int _DELIMITER = 0x1;


class ParseTable {
   protected:
    ParseMacro* tbl[NCHARS];
    char attributes[NCHARS];

    friend class Parser;
    friend class ListParseMacro;

   public:
    ParseTable();

    int isDelimiter(int);
};


/*
 * These routines are hit upon heavily enough to merit their designation
 * as inline.  They included here at the front of the file so that they
 * will be visible to the various ParseMacro and ParserFrame routines.
 */


int ParseTable::isDelimiter(int c) { return attributes[c] & _DELIMITER; }
void* Parser::falloc(int sz) { return fstk.alloc(sz); }

int PFrameStk::link(int sz) {
    /*
     * We will store ParserFrame link fields as relative offsets so that
     * we don't have so much work to do when we resize (and move) a
     * PFrameStk.
     */
    int tmp = topframe;
    topframe = nexttop;
    nexttop += sz;
    return tmp;
}


ParserFrame* PFrameStk::top() { return (ParserFrame*)&stk[topframe]; }
int Parser::flink(int sz) { return fstk.link(sz); }
ParserFrame* Parser::ftop() { return fstk.top(); }
void Parser::fpop() { fstk.pop(); }

void Parser::buffer(int c) {
    if (bufp >= bufsize) {
        growBuffer();
    }

    buf[bufp++] = c;
}


ParserMode Parser::accept(int c, int gapsPermitted) {
    if (c == EOF) {
        return STOP;
    } else if (c == '\\') {
        return acceptEscChar(c, gapsPermitted);
    } else {
        buffer(c);
#if defined(OPTIMIZE_ATOMS)
        return (mode == GROK_ATOM) ? GROK_ATOM : CONTINUE;
#else
        return CONTINUE;
#endif
    }
}


PFrameStk::PFrameStk() : topframe(-1), nexttop(0), stk(NULL), stksize(0) {}


PFrameStk::~PFrameStk() {
    if (stk)
        delete stk;
}


void* PFrameStk::alloc(int sz) {
    if (nexttop + sz > stksize) {
        char* newstk = new char[stksize + 256];
        if (stk) {
            memcpy(newstk, stk, nexttop * sizeof(char));
            delete stk;
        }

        stk = newstk;
        stksize += 256;
    }

    return (void*)&stk[nexttop];
}


void PFrameStk::pop() {
    nexttop = topframe;
    topframe = top()->link;
}


int PFrameStk::empty() { return topframe == -1; }


void PFrameStk::reset() {
    topframe = -1;
    nexttop = 0;
}


/* Parser continuation frames */

/*
 * The general protocol for building a new frame is to allocate room for
 * it (initiated by ParserFrame::operator new) and then to link it as
 * part of the frame initialization.
 */


ParserFrame::ParserFrame(Parser* r, int sz) : link(r->flink(sz)) {}

ParserMode ParserFrame::process(int, Parser*) {
    suicide("ParserFrame::process is abstract");
    return STOP;
}


ParserMode ParserFrame::receiveOb(Ob*, Parser*) {
    suicide("ParserFrame::receiveOb is abstract");
    return STOP;
}


ParserMode ParserFrame::receiveChar(int, Parser*) {
    suicide("ParserFrame::receiveChar is abstract");
    return STOP;
}


ParserMode ParserFrame::receiveTerminator(int c, Parser* r) {
    (void)r->error("unexpected closing '%c'", c);
    return STOP;
}


ParserMode ParserFrame::receiveDot(int, Parser*) {
    suicide("ParserFrame::receiveDot is abstract");
    return STOP;
}


ParserMode ParserFrame::receiveEof(Parser* r) {
    (void)r->error("unexpected eof");
    r->opush(RBLEOF);
    return STOP;
}


/*
 * CommentFrames record the fact that we are scanning comments, waiting
 * for a newline or EOF.  When we encounter an end-of-comment situation,
 * we delete the CommentFrame, and resume with whatever was happening
 * before.
 */

class CommentFrame : public ParserFrame {
   public:
    CommentFrame(Parser*);
    virtual ParserMode process(int, Parser*);
};


CommentFrame::CommentFrame(Parser* r) : ParserFrame(r, sizeof(CommentFrame)) {}


ParserMode CommentFrame::process(int c, Parser* r) {
    if (c == EOF || c == '\n') {
        r->fpop();
        return START;
    } else {
        return CONTINUE;
    }
}


/*
 * EscCharFrames record the progress of reading escaped characters in
 * atom, strings, and special tokens.  The usual sorts of things are
 * allowed, such as \n, \f, \023 (octal values require exactly 3 digits),
 * and \xa4 (hex values require exactly 4 digits).  Furthermore, strings
 * permit a "gap" (borrowed from the Haskell lexical conventions), which
 * is everything between an escaped newline and a subsequent escape char;
 * everything in the gap is discarded, permitting multi-line string
 * constants without destroying source layout.  For example,
 *
 * 	"This is a multi-line \
 * 	\string."
 *
 * is exactly equivalent to
 *
 * 	"This is a multi-line string."
 *
 * The "base" member variable encodes important state information about
 * the frame.  If base is
 *
 * 	0	We are scanning an ordinary escape sequence
 *
 * 	8	We are scanning an octal sequence; nchars tells how
 * 		many characters we have seen so far, and val is the
 * 		running value of the sequence
 *
 * 	16	Similar to 8, but for a hex sequence
 *
 * 	-1	We are scanning a string gap
 */


class EscCharFrame : public ParserFrame {
   protected:
    int gapPermitted;
    int nchars;
    int base;
    int val;

   public:
    EscCharFrame(Parser*, int = 0);
    virtual ParserMode process(int, Parser*);
};


EscCharFrame::EscCharFrame(Parser* r, int gp)
    : ParserFrame(r, sizeof(EscCharFrame)),
      gapPermitted(gp),
      nchars(0),
      base(0),
      val(0) {}


ParserMode EscCharFrame::process(int c, Parser* r) {
    if (base == 0) {
        switch (c) {
        case 'n':
            c = '\n';
            break;
        case 'f':
            c = '\f';
            break;
        case 'r':
            c = '\r';
            break;
        case 't':
            c = '\t';
            break;
        case 'b':
            c = '\b';
            break;
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
            base = 8;
            val = c - '0';
            nchars = 1;
            break;
        case 'x':
            base = 16;
            val = 0;
            nchars = 0;
            break;
        case '\n':
            if (gapPermitted)
                base = -1;
            break;
        default:
            break;
        }

        if (base == 0) {
            return r->receiveChar(c);
        } else {
            return CONTINUE;
        }
    } else if (base == 8) {
        if ('0' <= c && c < '8') {
            val = 8 * val + (c - '0');
        } else {
            /*
             * Pretend the digit was a zero.
             */
            val *= 8;
            (void)r->error("invalid octal digit ('%c')", c);
        }

        if (++nchars == 3) {
            return r->receiveChar(val);
        } else {
            return CONTINUE;
        }
    } else if (base == 16) {
        if (isxdigit(c)) {
            val =
                16 * val +
                (isdigit(c) ? c - '0' : ((c - (isupper(c) ? 'A' : 'a')) + 10));
        } else {
            val *= 16;
            (void)r->error("invalid hex digit ('%c')", c);
        }

        if (++nchars == 2) {
            return r->receiveChar(val);
        } else {
            return CONTINUE;
        }
    } else if (base == -1) {
        if (c == '\\') {
            /*
             * We have just bumped into the gap terminator; eat it, drop
             * this frame, and go on as if nothing had happened.
             */
            r->fpop();
        }

        return CONTINUE;
    } else {
        suicide("unknown base (%d) in EscCharFrame", base);
        return STOP;
    }
}


/*
 * AtomFrames record the progress of the tokenization of a symbol or
 * number.
 */


class AtomFrame : public ParserFrame {
   protected:
    AtomFrame(Parser*, int);

   public:
    AtomFrame(Parser*);
    virtual ParserMode process(int, Parser*);
    virtual ParserMode receiveChar(int, Parser*);
};


AtomFrame::AtomFrame(Parser* r, int sz) : ParserFrame(r, sz) {}


AtomFrame::AtomFrame(Parser* r) : ParserFrame(r, sizeof(AtomFrame)) {}


ParserMode AtomFrame::process(int c, Parser* r) {
    if (r->rt->isDelimiter(c)) {
        r->inp--; /* ungetc */
        PROTECT(r);
        Ob* v = r->finalizeAtom();
        return r->receiveOb(v);
    }

    return r->accept(c);
}


ParserMode AtomFrame::receiveChar(int c, Parser* r) {
    r->buffer(c);
    return CONTINUE;
}


/*
 * StringFrames are similar enough to AtomFrames that we make them a
 * subclass, primarily to inherit the accept routine.  The only useful
 * difference when reading a string is that the string delimiter is the
 * only terminator, instead of any delimiter in the case of an atom.
 */


class StringFrame : public AtomFrame {
    int delimiter;

   public:
    StringFrame(Parser*, int);
    virtual ParserMode process(int, Parser*);
};


StringFrame::StringFrame(Parser* r, int d)
    : AtomFrame(r, sizeof(StringFrame)), delimiter(d) {}


ParserMode StringFrame::process(int c, Parser* r) {
    if (c == delimiter) {
        PROTECT(r);
        RBLstring* str = RBLstring::create(r->finalizeBuffer());
        return r->receiveOb(str);
    } else {
        /*
         * Remember to tell the acceptance routine that "gaps" are
         * permitted in strings.
         */
        return r->accept(c, true);
    }
}


/*
 * SpecialPFrames are used to catch special symbols.  In the case of the
 * standard Parser, a special symbol is introduced by a '#', and if
 * followed by a letter it is assumed to introduce a "reserved word", as
 * with #niv, #t, or #f, for example.  If followed by a '\\', it is
 * assumed to introduce a character, which can be further described by
 * the usual escape notation; for example, #\\n denotes the newline
 * character.
 */


class SpecialPFrame : public ParserFrame {
   private:
    enum { SP_INIT, SP_EXPECTING_CHAR } state;
    Ob* checkSym(char*);

   public:
    SpecialPFrame(Parser*);
    virtual ParserMode process(int, Parser*);
    virtual ParserMode receiveOb(Ob*, Parser*);
    virtual ParserMode receiveChar(int, Parser*);
};


SpecialPFrame::SpecialPFrame(Parser* r)
    : ParserFrame(r, sizeof(SpecialPFrame)), state(SP_INIT) {}


ParserMode SpecialPFrame::process(int c, Parser* r)

{
    switch (state) {
    case SP_INIT:
        if (c == '\\') {
            state = SP_EXPECTING_CHAR;
            return CONTINUE;
        } else if (isalpha(c)) {
            new (r->falloc(sizeof(AtomFrame))) AtomFrame(r);
            r->resetBuffer();
            return r->accept(c);
        } else {
            (void)r->error("unknown special #%c", c);
            return START;
        }

    case SP_EXPECTING_CHAR:
        if (c == '\\') {
            new (r->falloc(sizeof(EscCharFrame))) EscCharFrame(r);
            return CONTINUE;
        } else {
            return r->receiveOb(RBLCHAR(c));
        }

    default:
        suicide("unexpected case in SpecialPFrame::process");
        return STOP;
    }
}


Ob* SpecialPFrame::checkSym(char* sym) {
    /*
     * It won't do to make this table-driven, because the values to be
     * returned are not necessarily valid at table-initialization time.
     */
    if (strcmp(sym, "t") == 0)
        return RBLTRUE;
    if (strcmp(sym, "f") == 0)
        return RBLFALSE;
    if (strcmp(sym, "niv") == 0)
        return NIV;
    if (strcmp(sym, "absent") == 0)
        return ABSENT;
    return INVALID;
}


ParserMode SpecialPFrame::receiveOb(Ob* v, Parser* r) {
    assert(IS_SYM(v));
    Ob* result = checkSym(SYMPTR(v));
    if (result == INVALID) {
        (void)r->error("unrecognized special (#%s)", SYMPTR(v));
        r->fpop();
        return START;
    }

    return r->receiveOb(result);
}


ParserMode SpecialPFrame::receiveChar(int c, Parser* r) {
    return r->receiveOb(RBLCHAR(c));
}


/*
 * QuoteFrames record the fact that we are reading a quoted expression.
 * When the completed subexpression is returned to the QuoteFrame, we
 * enclose it in a QuoteExpr and return that to the next frame.
 */


class QuoteFrame : public ParserFrame {
   public:
    QuoteFrame(Parser*);
    virtual ParserMode receiveOb(Ob*, Parser*);
};


QuoteFrame::QuoteFrame(Parser* r) : ParserFrame(r, sizeof(QuoteFrame)) {}


ParserMode QuoteFrame::receiveOb(Ob* v, Parser* r) {
    PROTECT(r);
    Ob* result = QuoteExpr::create(v);
    return r->receiveOb(result);
}


/*
 * There is one TopFrame per FrameStack; its purpose in life is to catch
 * the ultimate expression that was read in and to set up the Parser to
 * return it to the waiting world.
 */


class TopFrame : public ParserFrame {
   public:
    TopFrame(Parser*);
    virtual ParserMode receiveOb(Ob*, Parser*);
    virtual ParserMode receiveEof(Parser*);
};


TopFrame::TopFrame(Parser* r) : ParserFrame(r, sizeof(TopFrame)) {}


ParserMode TopFrame::receiveOb(Ob* result, Parser* r) {
    r->fpop();
    r->opush(result);
    return STOP;
}


ParserMode TopFrame::receiveEof(Parser* r) { return receiveOb(RBLEOF, r); }


/*
 * ListFrames are used to gather the components of a bracketed list.  To
 * do so, each frame needs to keep track of the number of subexpressions
 * that have been collected so far; these subexpressions are retained on
 * the Parsers ostk, so that when the closing token is read, the top of
 * the ostk holds the last subexpression in the list.
 */


typedef Ob* (*FINALIZER)(Parser*, Ob**, int, Ob*);


class ListFrame : public ParserFrame {
   protected:
    int nexprs;
    int dotState;
    int dotChar;
    int closingChar;
    FINALIZER finalizer;

   public:
    ListFrame(Parser*, int, int, FINALIZER);
    virtual ParserMode receiveOb(Ob*, Parser*);
    virtual ParserMode receiveTerminator(int, Parser*);
    virtual ParserMode receiveDot(int, Parser*);
};


ListFrame::ListFrame(Parser* r, int dc, int cc, FINALIZER f)
    : ParserFrame(r, sizeof(ListFrame)),
      nexprs(0),
      dotState(0),
      dotChar(dc),
      closingChar(cc),
      finalizer(f) {}


ParserMode ListFrame::receiveOb(Ob* subexpr, Parser* r) {
    if (dotState == 0) {
        ++nexprs;
        r->opush(subexpr);
    } else if (dotState == 1) {
        ++dotState;
        r->opush(subexpr);
    } else {
        /*
         * If we get here, we have found more than one expression
         * following the dot character, which is an error.  The way this
         * is written now, we will simply ignore the excess expressions.
         */
        r->error("more than one expression following '%c'", dotChar);
        /*
         * Don't return here; keep eating things in hopes of finding the
         * terminator.
         */
    }
    return START;
}


ParserMode ListFrame::receiveTerminator(int c, Parser* r) {
    if (c != closingChar) {
        (void)r->error("unexpected closing '%c'", c);
    }

    Ob* rest = NILexpr;
    if (dotState == 1) {
        (void)r->error("no expression following '%c'", dotChar);
    } else if (dotState > 1) {
        rest = r->opop();
    }

    PROTECT(r);
    Ob* result = (*finalizer)(r, &r->otop(nexprs), nexprs, rest);
    r->odel(nexprs);
    return r->receiveOb(result);
}


ParserMode ListFrame::receiveDot(int c, Parser* r) {
    if (c != dotChar) {
        r->error("received '%c' when expecting '%c'", c, dotChar);
    }

    if (dotState != 0) {
        r->error("too many '%c's", c);
    }

    dotState = 1;
    return START;
}


/* Read macros */

ParseMacro::ParseMacro() {}


class WhitespaceParseMacro : public ParseMacro {
   public:
    virtual ParserMode start(int, Parser*);
};


ParserMode WhitespaceParseMacro::start(int, Parser*) { return START; }


class CommentParseMacro : public ParseMacro {
   public:
    virtual ParserMode start(int, Parser*);
};


ParserMode CommentParseMacro::start(int, Parser* r) {
    new (r->falloc(sizeof(CommentFrame))) CommentFrame(r);
    return CONTINUE;
}


class AtomParseMacro : public ParseMacro {
   public:
    virtual ParserMode start(int, Parser*);
};


ParserMode AtomParseMacro::start(int c, Parser* r) {
#if defined(OPTIMIZE_ATOMS)
    /*
     * The GROK_ATOM optimization takes advantage of the fact that we can
     * only be reading one atom at a time, unlike, for example, lists.
     * We exploit that by not actually allocating an AtomFrame unless
     * some extraordinary event (incomplete io, escape char in the atom)
     * forces us to do so, and instead use GROK_ATOM mode to record the
     * fact that there is an implicit AtomFrame on the top of the frame
     * stack.
     */
    r->mode = GROK_ATOM;
#else
    new (r->falloc(sizeof(AtomParseMacro))) AtomFrame(r);
#endif
    r->digitSeen = isdigit(c);
    r->resetBuffer();
    return r->accept(c);
}


/* StringParseMacro */


class StringParseMacro : public ParseMacro {
   public:
    virtual ParserMode start(int, Parser*);
};


ParserMode StringParseMacro::start(int c, Parser* r) {
    new (r->falloc(sizeof(StringFrame))) StringFrame(r, c);
    r->resetBuffer();
    return CONTINUE;
}


/* SpecialParseMacro */

class _SpecialParseMacro : public ParseMacro {
   public:
    virtual ParserMode start(int, Parser*);
};


ParserMode _SpecialParseMacro::start(int, Parser* r) {
    new (r->falloc(sizeof(SpecialPFrame))) SpecialPFrame(r);
    return CONTINUE;
}


/* QuoteParseMacro */

class QuoteParseMacro : public ParseMacro {
   public:
    virtual ParserMode start(int, Parser*);
};


ParserMode QuoteParseMacro::start(int, Parser* r) {
    new (r->falloc(sizeof(QuoteFrame))) QuoteFrame(r);
    return START;
}


/* DotParseMacro */

class DotParseMacro : public ParseMacro {
   public:
    virtual ParserMode start(int, Parser*);
};


ParserMode DotParseMacro::start(int c, Parser* r) { return r->receiveDot(c); }


/* ListTerminatorParseMacro */

class ListTerminatorParseMacro : public ParseMacro {
   public:
    virtual ParserMode start(int, Parser*);
};


ParserMode ListTerminatorParseMacro::start(int c, Parser* r) {
    return r->receiveTerminator(c);
}


/* ListParseMacro */

class ListParseMacro : public ParseMacro {
   protected:
    int startChar;
    int dotChar;
    int closingChar;
    FINALIZER finalizer;

   public:
    ListParseMacro(int, int, int, FINALIZER);
    virtual ParserMode start(int, Parser*);
    void install(ParseTable*);
};


ListParseMacro::ListParseMacro(int sc, int dc, int cc, FINALIZER f)
    : ParseMacro(), startChar(sc), dotChar(dc), closingChar(cc), finalizer(f) {}


ParserMode ListParseMacro::start(int, Parser* r) {
    new (r->falloc(sizeof(ListFrame)))
        ListFrame(r, dotChar, closingChar, finalizer);
    return START;
}


static Ob* msgFinalizer(Parser* r, Ob** stk, int n, Ob* rest) {
    if (n == 0 && rest == NILexpr) {
        return NILexpr;
    } else {
        return TupleExpr::create(stk, n, rest);
    }
}


static FINALIZER findSpecialPForm(Ob* symbol);


static Ob* rqstFinalizer(Parser* r, Ob** stk, int n, Ob* rest) {
    if (n == 0) {
        return r->error("no target in request expr");
    }

    FINALIZER fn = findSpecialPForm(stk[0]);

    if (fn) {
        return (*fn)(r, stk, n, rest);
    } else {
        TupleExpr* msg = (TupleExpr*)msgFinalizer(r, stk + 1, n - 1, rest);
        return RequestExpr::create(stk[0], msg);
    }
}


static Ob* sendFinalizer(Parser* r, Ob** stk, int n, Ob* rest) {
    if (n < 2) {
        return r->error("no target in send expr");
    }

    return (SendExpr::create(
        stk[1], (TupleExpr*)msgFinalizer(r, stk + 2, n - 2, rest)));
}


static Ob* ifFinalizer(Parser* r, Ob** stk, int n, Ob*) {
    switch (n) {
    case 4:
        return IfExpr::create(stk[1], stk[2], stk[3]);
    case 3:
        return IfExpr::create(stk[1], stk[2]);
    default:
        return r->error("wrong number of branches for if expr");
    }
}


static Ob* blockHelper(Ob** stk, int n, bool implicit = true) {
    if (n == 1) {
        return stk[0];
    } else {
        Tuple* subExprs = Tuple::create(stk, n);
        return BlockExpr::create(subExprs, RBLBOOL(implicit));
    }
}


static Ob* blockFinalizer(Parser* r, Ob** stk, int n, Ob* rest) {
    if (n < 2 || rest != NILexpr) {
        return r->error("improper syntax for block expression");
    }

    return blockHelper(stk + 1, n - 1, false);
}


static Ob* freeFinalizer(Parser* r, Ob** stk, int n, Ob* rest) {
    if (n < 3 || rest != NILexpr) {
        return r->error("improper syntax for free expression");
    }

    TupleExpr* p = (TupleExpr*)stk[1];

    if (!IS_A(p, TupleExpr) || !p->allSymbols()) {
        return r->error("improper syntax for free expression");
    } else {
        PROTECT(p);
        Ob* block = blockHelper(stk + 2, n - 2);
        return FreeExpr::create(p, block);
    }
}


static Ob* methodFinalizer(Parser* r, Ob** stk, int n, Ob* rest) {
    if (n < 3 || rest != NILexpr) {
        return r->error("improper syntax for method expression");
    }

    Ob* formals = stk[1];
    PROTECT(formals);
    Ob* block = blockHelper(stk + 2, n - 2);
    return MethodExpr::create(NIV, formals, block);
}


static Ob* procFinalizer(Parser* r, Ob** stk, int n, Ob* rest) {
    if (n < 3 || rest != NILexpr) {
        return r->error("improper syntax for proc expression");
    }

    Ob* formals = stk[1];
    PROTECT(formals);
    Ob* block = blockHelper(stk + 2, n - 2);
    return ProcExpr::create(NIV, formals, block);
}


static Ob* namedProcFinalizer(Parser* r, Ob** stk, int n, Ob* rest) {
    if (n < 4 || rest != NILexpr) {
        return r->error("improper syntax for named-proc expression");
    }

    Ob* formals = stk[2];
    PROTECT(formals);
    Ob* block = blockHelper(stk + 3, n - 3);
    return ProcExpr::create(stk[1], formals, block);
}


static Ob* letFinalizer(Parser* r, Ob** stk, int n, Ob* rest) {
    if (n < 3 || rest != NILexpr) {
        return r->error("improper syntax for let expression");
    }

    TupleExpr* p = (TupleExpr*)stk[1];

    if (!IS_A(p, TupleExpr) || !p->allPairs()) {
        return r->error("improper syntax for let expression");
    } else {
        PROTECT(p);
        Ob* block = blockHelper(stk + 2, n - 2);
        return LetExpr::create(p, block);
    }
}


static Ob* letstarFinalizer(Parser* r, Ob** stk, int n, Ob* rest) {
    if (n < 3 || rest != NILexpr) {
        return r->error("improper syntax for let* expression");
    }

    TupleExpr* p = (TupleExpr*)stk[1];

    if (!IS_A(p, TupleExpr) || !p->allPairs()) {
        return r->error("improper syntax for let* expression");
    }

    PROTECT(p);
    Ob* body = blockHelper(stk + 2, n - 2);
    PROTECT(body);

    for (int i = p->numberOfElements(); i--;) {
        TupleExpr* t = TupleExpr::create(1);
        t->elem(0) = p->elem(i);
        body = LetExpr::create(t, body);
    }

    return body;
}


static Ob* letrecFinalizer(Parser* r, Ob** stk, int n, Ob* rest) {
    if (n < 3 || rest != NILexpr) {
        return r->error("improper syntax for letrec expression");
    }

    TupleExpr* p = (TupleExpr*)stk[1];

    if (!IS_A(p, TupleExpr) || !p->allPairs()) {
        return r->error("improper syntax for letrec expression");
    } else {
        PROTECT(p);
        Ob* block = blockHelper(stk + 2, n - 2);
        return LetrecExpr::create(p, block);
    }
}


static Ob* seqFinalizer(Parser* r, Ob** stk, int n, Ob* rest) {
    if (n < 2 || rest != NILexpr) {
        return r->error("improper syntax for seq expression");
    }

    if (n == 2) {
        return stk[1];
    } else {
        Tuple* subExprs = Tuple::create(stk + 1, n - 1);
        return SeqExpr::create(subExprs);
    }
}


static Ob* setFinalizer(Parser* r, Ob** stk, int n, Ob* rest) {
    if (n != 3 || rest != NILexpr || !IS_SYM(stk[1])) {
        return r->error("improper syntax for set! expression");
    }

    return SetExpr::create(stk[1], stk[2]);
}


static Ob* gotoFinalizer(Parser* r, Ob** stk, int n, Ob* rest) {
    if (n != 2 || rest != NILexpr || !IS_SYM(stk[1])) {
        return r->error("improper syntax for goto expression");
    }

    return GotoExpr::create(stk[1]);
}


static Ob* labelFinalizer(Parser* r, Ob** stk, int n, Ob* rest) {
    Ob* label;

    if (n < 3 || rest != NILexpr || !IS_SYM(label = stk[n - 2])) {
        return r->error("improper syntax for label expression");
    }

    Ob* block = blockHelper(stk + 2, n - 2);
    return LabelExpr::create(label, block);
}


FINALIZER
findSpecialPForm(Ob* symbol) {
    extern Ob* Qif;
    extern Ob* Qmethod;
    extern Ob* Qproc;
    extern Ob* Qnamedproc;
    extern Ob* Qlet;
    extern Ob* Qletstar;
    extern Ob* Qletrec;
    extern Ob* Qsend;
    extern Ob* Qblock;
    extern Ob* Qseq;
    extern Ob* Qsetbang;
    extern Ob* Qgoto;
    extern Ob* Qlabel;
    extern Ob* Qfree;

    if (symbol == Qif)
        return ifFinalizer;
    if (symbol == Qmethod)
        return methodFinalizer;
    if (symbol == Qproc)
        return procFinalizer;
    if (symbol == Qnamedproc)
        return namedProcFinalizer;
    if (symbol == Qlet)
        return letFinalizer;
    if (symbol == Qletstar)
        return letstarFinalizer;
    if (symbol == Qletrec)
        return letrecFinalizer;
    if (symbol == Qsend)
        return sendFinalizer;
    if (symbol == Qblock)
        return blockFinalizer;
    if (symbol == Qseq)
        return seqFinalizer;
    if (symbol == Qsetbang)
        return setFinalizer;
    if (symbol == Qgoto)
        return gotoFinalizer;
    if (symbol == Qlabel)
        return labelFinalizer;
    if (symbol == Qfree)
        return freeFinalizer;
    return 0;
}


static WhitespaceParseMacro _WRM;
static CommentParseMacro _CRM;
static AtomParseMacro _ARM;
static DotParseMacro _DRM;
static ListTerminatorParseMacro _LTRM;
static ListParseMacro _TeRM('[', '&', ']', &msgFinalizer);
static ListParseMacro _ReRM('(', '&', ')', &rqstFinalizer);
static StringParseMacro _SRM;
static _SpecialParseMacro _SpRM;
static QuoteParseMacro _QRM;


void ListParseMacro::install(ParseTable* rt) {
    rt->tbl[startChar] = this;
    rt->tbl[dotChar] = &_DRM;
    rt->tbl[closingChar] = &_LTRM;
    rt->attributes[startChar] |= _DELIMITER;
    rt->attributes[closingChar] |= _DELIMITER;
}


static ParseTable StdParseTable;


ParseTable::ParseTable() {
    for (int i = 0; i < NCHARS; i++) {
        if (isspace(i)) {
            tbl[i] = &_WRM;
            attributes[i] |= _DELIMITER;
        } else {
            tbl[i] = &_ARM;
        }
    }

    tbl[';'] = &_CRM;
    attributes[';'] |= _DELIMITER;
    tbl['\"'] = &_SRM;
    attributes['\"'] |= _DELIMITER;
    tbl['#'] = &_SpRM;
    attributes['#'] |= _DELIMITER;
    tbl['\''] = &_QRM;

    _TeRM.install(this);
    _ReRM.install(this);
}


BUILTIN_CLASS(Parser) {
    BIT_FIELD("buf", Parser, buf, BITS(char*));
    BIT_FIELD("bufsize", Parser, bufsize, BITS(int));
    BIT_FIELD("bufp", Parser, bufp, BITS(int));
    BIT_FIELD("err?", Parser, errorEncountered, BITS(int));
    BIT_FIELD("wait", Parser, waitingOnIO, BITS(WaitMode));
    BIT_FIELD("ptbl", Parser, rt, BITS(ParseTable*));
    BIT_FIELD("mode", Parser, mode, BITS(ParserMode));
    OB_FIELD("inbuf", Parser, inbuf);
    BIT_FIELD("inp", Parser, inp, BITS(int));
    BIT_FIELD("digit?", Parser, digitSeen, BITS(int));
}


Parser::Parser(ParseTable* rt)
    : BinaryOb(align(sizeof(Parser)), CLASS_META(Parser), CLASS_SBO(Parser)),
      inbuf((RBLstring*)NIV),
      inp(0),
      buf(NULL),
      bufsize(0),
      bufp(0),
      errorEncountered(false),
      waitingOnIO(NOT_WAITING),
      rt(rt),
      mode(START),
      fstk(),
      ostk() {
    heap->registerForeignOb(this);
    Parser::updateCnt();
}


Parser::~Parser() { inbuf = (RBLstring*)NIV; }


Parser* Parser::create() {
    void* loc = PALLOC(align(sizeof(Parser)));
    return new (loc) Parser(&StdParseTable);
}


Ob* Parser::readExpr() {
    switch (waitingOnIO) {
    case NOT_WAITING:
        new (falloc(sizeof(TopFrame))) TopFrame(this);
        mode = START;
        waitingOnIO = WAITING_FOR_EXPR;
        if (inbuf == (RBLstring*)NIV) {
            return suspendParser();
        } else {
            return resumeExpr();
        }
    default:
        return finish(error("parser already waiting on readExpr"));
    }
}


Ob* Parser::resume(RBLstring* str) {
    switch (waitingOnIO) {
    case NOT_WAITING:
        return finish(error("parser not waiting on resume"));
    case WAITING_FOR_EXPR:
        inbuf = str;
        inp = 0;
        return resumeExpr();
    default:
        suicide("invalid value for Parser::waitingOnIO (= %d)", waitingOnIO);
        return NIV;
    }
}

Ob* Parser::resumeExpr() {
    /*
     * We have to protect the Parser because the calls to finalizeAtom
     * (as well as the calls to receiveOb) may cause scavenges to occur.
     * We try to minimize the damage by using an unprotected version of
     * SELF as much as possible within the loop.
     */

    PROTECT_THIS(Parser);

    ParserMode nextMode = SELF->mode;

    for (;;) {
        /*
         * The variable "my" always gets initialized at the top of this
         * loop, and is never changed, so the optimizer ought to do a
         * good job of minimizing the cost of protection from the
         * scavenger.  However, we must be careful that
         * scavenge-provoking operations always occur at the end of the
         * loop, or to use SELF in the event that we need to do something
         * between the end of the operation and the end of the loop.
         */

        /*
         * For this same reason, we have to employ a temporary "nextMode"
         * to capture the next mode of the Parser; assigning into
         * "my->mode" won't necessarily work because "my" might be
         * invalidated by a scavenge during a call to "process" or
         * "receiveWhatever".
         */

        Parser* const my = SELF;
        my->mode = nextMode;

        if (inp >= (inbuf->numberOfBytes() - 1)) {
            inp = 0;
            inbuf = (RBLstring*)NIV;
            return my->suspendParser();
        }

        int c = inbuf->byte(inp);
        inp++;

        switch (nextMode) {
        case START:
            if (c == EOF) {
                nextMode = my->ftop()->receiveEof(my);
            } else {
                nextMode = my->rt->tbl[c]->start(c, my);
            }
            break;

        case CONTINUE:
            if (c == EOF) {
                nextMode = my->ftop()->receiveEof(my);
            } else {
                nextMode = my->ftop()->process(c, my);
            }
            break;

#if defined(OPTIMIZE_ATOMS)
        case GROK_ATOM:
            /*
             * In this case, there is an implicit AtomFrame on the fstk;
             * in the event that nothing out-of-the-ordinary happens, we
             * can just build up the symbol and finalize it without ever
             * needing to materialize the frame.  If something "odd" does
             * happen, such as bumping into an escape character or
             * running out of input, we will materialize the frame and
             * continue on from there.
             */
            if (my->rt->isDelimiter(c)) {
                inp--; /* ungetc */
                       /*
                        * We can't use Parser::receiveOb here because the
                        * AtomFrame is implicit, and receiveOb insists on
                        * popping a frame.
                        */
                Ob* v = my->finalizeAtom();
                /*
                 * Because finalizeAtom *might* cause a scavenge (if
                 * we're unlucky enough to be reading a floating-point
                 * number at just the right moment), it's not safe to use
                 * "my" after this point.
                 */
                nextMode = SELF->ftop()->receiveOb(v, SELF);
            } else {
                my->digitSeen |= isdigit(c);
                nextMode = my->accept(c);
            }

            break;
#endif

        default:
            suicide("unanticipated case in Parser::readExpr");
            break;
        }

        if (nextMode == STOP) {
            return SELF->finish(SELF->opop());
        }
    }
}


Ob* Parser::error(const char* fmt, ...) {
    /*
     * This really ought to be changed to be less dogmatic about writing
     * on stderr, and use the Rosette error reporting mechanism instead.
     */
    fprintf(stderr, "***parse error: ");
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fputc('\n', stderr);

    errorEncountered = true;
    return READ_ERROR;
}


void Parser::resetState() {
    waitingOnIO = NOT_WAITING;
    errorEncountered = false;
    mode = START;
    inbuf = (RBLstring*)NIV;
    inp = 0;
    fstk.reset();
    ostk.reset();
}


Ob* Parser::suspendParser() {
    /*
     * Everything is already nicely packaged and waiting; we simply set
     * the lock and give up control.
     */
    if (errorEncountered) {
        errorEncountered = false;
        waitingOnIO = NOT_WAITING;
        return READ_ERROR;
    } else {
        /*
         * Leave waitingOnIO unchanged, so that we will re-enter with the
         * proper state.
         */
        return INCOMPLETE_IO;
    }
}


Ob* Parser::finish(Ob* v) {
    waitingOnIO = NOT_WAITING;
    if (errorEncountered) {
        resetState();
        return READ_ERROR;
    } else {
        assert(ostk.empty());
        assert(fstk.empty());
        return v;
    }
}


void Parser::opush(Ob* v) {
    /*
     * Be careful to check whether we are pushing a young element onto an
     * ostk owned by an old Parser.
     */
    ostk.push(v);
    checkStore(v);
}


Ob*& Parser::otop(int n) { return ostk.top(n); }
Ob* Parser::opop() { return ostk.pop(); }
void Parser::odel(int n) { ostk.del(n); }

ParserMode Parser::receiveOb(Ob* v) {
    fpop();
    return ftop()->receiveOb(v, this);
}


ParserMode Parser::receiveChar(int c) {
    fpop();
    return ftop()->receiveChar(c, this);
}


ParserMode Parser::receiveDot(int c) { return ftop()->receiveDot(c, this); }


ParserMode Parser::receiveTerminator(int c) {
    return ftop()->receiveTerminator(c, this);
}


ParserMode Parser::acceptEscChar(int c, int gapsPermitted) {
#if defined(OPTIMIZE_ATOMS)
    if (mode == GROK_ATOM) {
        /*
         * Make explicit the AtomFrame that is implicit in the GROK_ATOM
         * state.
         */
        new (falloc(sizeof(AtomFrame))) AtomFrame(this);
    }
#endif
    new (falloc(sizeof(EscCharFrame))) EscCharFrame(this, gapsPermitted);
    return CONTINUE;
}


void Parser::growBuffer() {
    char* newbuf = new char[bufsize + 128];
    if (buf) {
        memcpy(newbuf, buf, bufsize * sizeof(char));
        delete buf;
    }
    buf = newbuf;
    bufsize += 128;
}


void Parser::resetBuffer() {
    if (buf) {
        buf[0] = '\0';
    }
    bufp = 0;
}


char* Parser::finalizeBuffer() {
    buffer('\0');
    return buf;
}


Ob* Parser::finalizeAtom() {
    char* sym = finalizeBuffer();
    char* delimiter = NULL;

    if (!digitSeen) {
        return SYMBOL(sym);
    }

    long n = strtol(sym, &delimiter, 0);
    if ((n != 0 || delimiter != sym) && (*delimiter == '\0')) {
        return FIXNUM(n);
    }

    Rfloat d = strtod(sym, &delimiter);
    if ((d != 0.0 || delimiter != sym) && (*delimiter == '\0')) {
        return Float::create(d);
    }

    return SYMBOL(sym);
}


Ob* Parser::cloneTo(Ob*, Ob*) {
    warning("can't clone Parsers");
    return this;
}


int Parser::traversePtrs(PSOb__PSOb f) {
    return (BinaryOb::traversePtrs(f) + ostk.traversePtrs(f) +
            useIfPtr(&inbuf, f));
}


int Parser::traversePtrs(SI__PSOb f) {
    return (BinaryOb::traversePtrs(f) + ostk.traversePtrs(f) +
            useIfPtr(inbuf, f));
}


void Parser::traversePtrs(V__PSOb f) {
    BinaryOb::traversePtrs(f);
    ostk.traversePtrs(f);
    useIfPtr(inbuf, f);
}

DEF("parser-new", makeParser, 1, 1) { return Parser::create(); }

DEF("parser-parse", parserParse, 1, 1) {
    CHECK(0, Parser, parser);
    return parser->readExpr();
}

DEF("parser-resume", parserResume, 2, 2) {
    CHECK(0, Parser, parser);
    CHECK(1, RBLstring, str);
    return parser->resume(str);
}

DEF("parser-reset", parserReset, 1, 1) {
    CHECK(0, Parser, parser);
    parser->resetState();
    return NIV;
}
