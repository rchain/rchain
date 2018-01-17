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

#include "Reader.h"
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

#define QuoteFrame QuoteFrame_Reader
#define TopFrame TopFrame_Reader
#define StringFrame StringFrame_Reader
#define AtomFrame AtomFrame_Reader
#define ListFrame ListFrame_Reader
#define CommentFrame CommentFrame_Reader
#define EscCharFrame EscCharFrame_Reader


class ReaderFrame {
   protected:
    ReaderFrame(Reader*, int);

    void* operator new(size_t);
    void* operator new(size_t, void*);

    int link;

    friend class FrameStk;
    friend class SpecialFrame;
    friend class CommentReadMacro;
    friend class StringReadMacro;
    friend class _SpecialReadMacro;
    friend class QuoteReadMacro;
    friend class ListReadMacro;
    friend class Reader;

   public:
    virtual ReaderMode process(int, Reader*);
    virtual ReaderMode receiveOb(Ob*, Reader*);
    virtual ReaderMode receiveChar(int, Reader*);
    virtual ReaderMode receiveTerminator(int, Reader*);
    virtual ReaderMode receiveDot(int, Reader*);
    virtual ReaderMode receiveEof(Reader*);
};


class ReadMacro {
   public:
    ReadMacro();

    virtual ReaderMode start(int, Reader*) = 0;
};


static const int NCHARS = 256;
static const int _DELIMITER = 0x1;


class ReadTable {
   protected:
    ReadMacro* tbl[NCHARS];
    char attributes[NCHARS];

    friend class Reader;
    friend class ListReadMacro;

   public:
    ReadTable();

    int isDelimiter(int);
};


/*
 * These routines are hit upon heavily enough to merit their designation
 * as inline.  They included here at the front of the file so that they
 * will be visible to the various ReadMacro and ReaderFrame routines.
 */


int ReadTable::isDelimiter(int c) { return attributes[c] & _DELIMITER; }
void* Reader::falloc(int sz) { return fstk.alloc(sz); }


int FrameStk::link(int sz) {
    /*
     * We will store ReaderFrame link fields as relative offsets so that
     * we don't have so much work to do when we resize (and move) a
     * FrameStk.
     */
    int tmp = topframe;
    topframe = nexttop;
    nexttop += sz;
    return tmp;
}


int Reader::flink(int sz) { return fstk.link(sz); }
ReaderFrame* FrameStk::top() { return (ReaderFrame*)&stk[topframe]; }
ReaderFrame* Reader::ftop() { return fstk.top(); }
void Reader::fpop() { fstk.pop(); }


void Reader::buffer(int c) {
    if (bufp >= bufsize)
        growBuffer();
    buf[bufp++] = c;
}


ReaderMode Reader::accept(int c, int gapsPermitted) {
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


FrameStk::FrameStk() : topframe(-1), nexttop(0), stk(NULL), stksize(0) {}


FrameStk::~FrameStk() {
    if (stk)
        delete stk;
}


void* ReaderFrame::operator new(size_t) {
    suicide("operator new not allowed for Rosette objects\n");
    return NULL;
}

void* ReaderFrame::operator new(size_t, void* p) { return p; }


void* FrameStk::alloc(int sz) {
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


void FrameStk::pop() {
    nexttop = topframe;
    topframe = top()->link;
}


int FrameStk::empty() { return topframe == -1; }


void FrameStk::reset() {
    topframe = -1;
    nexttop = 0;
}


/* Reader continuation frames */

/*
 * The general protocol for building a new frame is to allocate room for
 * it (initiated by ReaderFrame::operator new) and then to link it as
 * part of the frame initialization.
 */


ReaderFrame::ReaderFrame(Reader* r, int sz) : link(r->flink(sz)) {}

ReaderMode ReaderFrame::process(int, Reader*) {
    suicide("ReaderFrame::process is abstract");
    return STOP;
}


ReaderMode ReaderFrame::receiveOb(Ob*, Reader*) {
    suicide("ReaderFrame::receiveOb is abstract");
    return STOP;
}


ReaderMode ReaderFrame::receiveChar(int, Reader*) {
    suicide("ReaderFrame::receiveChar is abstract");
    return STOP;
}


ReaderMode ReaderFrame::receiveTerminator(int c, Reader* r) {
    (void)r->error("unexpected closing '%c'", c);
    return STOP;
}


ReaderMode ReaderFrame::receiveDot(int, Reader*) {
    suicide("ReaderFrame::receiveDot is abstract");
    return STOP;
}


ReaderMode ReaderFrame::receiveEof(Reader* r) {
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

class CommentFrame : public ReaderFrame {
   public:
    CommentFrame(Reader*);
    virtual ReaderMode process(int, Reader*);
};


CommentFrame::CommentFrame(Reader* r) : ReaderFrame(r, sizeof(CommentFrame)) {}


ReaderMode CommentFrame::process(int c, Reader* r) {
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


class EscCharFrame : public ReaderFrame {
   protected:
    int gapPermitted;
    int nchars;
    int base;
    int val;

   public:
    EscCharFrame(Reader*, int = 0);
    virtual ReaderMode process(int, Reader*);
};


EscCharFrame::EscCharFrame(Reader* r, int gp)
    : ReaderFrame(r, sizeof(EscCharFrame)),
      gapPermitted(gp),
      nchars(0),
      base(0),
      val(0) {}


ReaderMode EscCharFrame::process(int c, Reader* r) {
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
            val = 16 * val +
                  (isdigit(c) ? c - '0' : 10 + (c - (isupper(c) ? 'A' : 'a')));
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


class AtomFrame : public ReaderFrame {
   protected:
    AtomFrame(Reader*, int);

   public:
    AtomFrame(Reader*);
    virtual ReaderMode process(int, Reader*);
    virtual ReaderMode receiveChar(int, Reader*);
};


AtomFrame::AtomFrame(Reader* r, int sz) : ReaderFrame(r, sz) {}


AtomFrame::AtomFrame(Reader* r) : ReaderFrame(r, sizeof(AtomFrame)) {}


ReaderMode AtomFrame::process(int c, Reader* r) {
    if (r->rt->isDelimiter(c)) {
        ungetc(c, r->file);
        PROTECT(r);
        Ob* v = r->finalizeAtom();
        return r->receiveOb(v);
    } else {
        return r->accept(c);
    }
}


ReaderMode AtomFrame::receiveChar(int c, Reader* r) {
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
    StringFrame(Reader*, int);
    virtual ReaderMode process(int, Reader*);
};


StringFrame::StringFrame(Reader* r, int d)
    : AtomFrame(r, sizeof(StringFrame)), delimiter(d) {}


ReaderMode StringFrame::process(int c, Reader* r) {
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
 * SpecialFrames are used to catch special symbols.  In the case of the
 * standard reader, a special symbol is introduced by a '#', and if
 * followed by a letter it is assumed to introduce a "reserved word", as
 * with #niv, #t, or #f, for example.  If followed by a '\\', it is
 * assumed to introduce a character, which can be further described by
 * the usual escape notation; for example, #\\n denotes the newline
 * character.
 */


class SpecialFrame : public ReaderFrame {
   private:
    enum { SP_INIT, SP_EXPECTING_CHAR } state;
    Ob* checkSym(char*);

   public:
    SpecialFrame(Reader*);
    virtual ReaderMode process(int, Reader*);
    virtual ReaderMode receiveOb(Ob*, Reader*);
    virtual ReaderMode receiveChar(int, Reader*);
};


SpecialFrame::SpecialFrame(Reader* r)
    : ReaderFrame(r, sizeof(SpecialFrame)), state(SP_INIT) {}


ReaderMode SpecialFrame::process(int c, Reader* r) {
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
        suicide("unexpected case in SpecialFrame::process");
        return STOP;
    }
}


Ob* SpecialFrame::checkSym(char* sym) {
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


ReaderMode SpecialFrame::receiveOb(Ob* v, Reader* r) {
    assert(IS_SYM(v));
    Ob* result = checkSym(SYMPTR(v));
    if (result == INVALID) {
        (void)r->error("unrecognized special (#%s)", SYMPTR(v));
        r->fpop();
        return START;
    }

    return r->receiveOb(result);
}


ReaderMode SpecialFrame::receiveChar(int c, Reader* r) {
    return r->receiveOb(RBLCHAR(c));
}


/*
 * QuoteFrames record the fact that we are reading a quoted expression.
 * When the completed subexpression is returned to the QuoteFrame, we
 * enclose it in a QuoteExpr and return that to the next frame.
 */


class QuoteFrame : public ReaderFrame {
   public:
    QuoteFrame(Reader*);
    virtual ReaderMode receiveOb(Ob*, Reader*);
};


QuoteFrame::QuoteFrame(Reader* r) : ReaderFrame(r, sizeof(QuoteFrame)) {}


ReaderMode QuoteFrame::receiveOb(Ob* v, Reader* r) {
    PROTECT(r);
    Ob* result = QuoteExpr::create(v);
    return r->receiveOb(result);
}


/*
 * There is one TopFrame per FrameStack; its purpose in life is to catch
 * the ultimate expression that was read in and to set up the reader to
 * return it to the waiting world.
 */


class TopFrame : public ReaderFrame {
   public:
    TopFrame(Reader*);
    virtual ReaderMode receiveOb(Ob*, Reader*);
    virtual ReaderMode receiveEof(Reader*);
};


TopFrame::TopFrame(Reader* r) : ReaderFrame(r, sizeof(TopFrame)) {}


ReaderMode TopFrame::receiveOb(Ob* result, Reader* r) {
    r->fpop();
    r->opush(result);
    return STOP;
}


ReaderMode TopFrame::receiveEof(Reader* r) { return receiveOb(RBLEOF, r); }


/*
 * ListFrames are used to gather the components of a bracketed list.  To
 * do so, each frame needs to keep track of the number of subexpressions
 * that have been collected so far; these subexpressions are retained on
 * the readers ostk, so that when the closing token is read, the top of
 * the ostk holds the last subexpression in the list.
 */


typedef Ob* (*FINALIZER)(Reader*, Ob**, int, Ob*);


class ListFrame : public ReaderFrame {
   protected:
    int nexprs;
    int dotState;
    int dotChar;
    int closingChar;
    FINALIZER finalizer;

   public:
    ListFrame(Reader*, int, int, FINALIZER);
    virtual ReaderMode receiveOb(Ob*, Reader*);
    virtual ReaderMode receiveTerminator(int, Reader*);
    virtual ReaderMode receiveDot(int, Reader*);
};


ListFrame::ListFrame(Reader* r, int dc, int cc, FINALIZER f)
    : ReaderFrame(r, sizeof(ListFrame)),
      nexprs(0),
      dotState(0),
      dotChar(dc),
      closingChar(cc),
      finalizer(f) {}


ReaderMode ListFrame::receiveOb(Ob* subexpr, Reader* r) {
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


ReaderMode ListFrame::receiveTerminator(int c, Reader* r) {
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


ReaderMode ListFrame::receiveDot(int c, Reader* r) {
    if (c != dotChar) {
        (void)r->error("received '%c' when expecting '%c'", c, dotChar);
    }

    if (dotState != 0) {
        (void)r->error("too many '%c's", c);
    }

    dotState = 1;
    return START;
}


/* Read macros */

ReadMacro::ReadMacro() {}


class WhitespaceReadMacro : public ReadMacro {
   public:
    virtual ReaderMode start(int, Reader*);
};


ReaderMode WhitespaceReadMacro::start(int, Reader*) { return START; }


class CommentReadMacro : public ReadMacro {
   public:
    virtual ReaderMode start(int, Reader*);
};


ReaderMode CommentReadMacro::start(int, Reader* r) {
    new (r->falloc(sizeof(CommentFrame))) CommentFrame(r);
    return CONTINUE;
}


class AtomReadMacro : public ReadMacro {
   public:
    virtual ReaderMode start(int, Reader*);
};


ReaderMode AtomReadMacro::start(int c, Reader* r) {
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
    new (r->falloc(sizeof(AtomReadMacro))) AtomFrame(r);
#endif
    r->digitSeen = isdigit(c);
    r->resetBuffer();
    return r->accept(c);
}


/* StringReadMacro */


class StringReadMacro : public ReadMacro {
   public:
    virtual ReaderMode start(int, Reader*);
};


ReaderMode StringReadMacro::start(int c, Reader* r) {
    new (r->falloc(sizeof(StringFrame))) StringFrame(r, c);
    r->resetBuffer();
    return CONTINUE;
}


/* SpecialReadMacro */

class _SpecialReadMacro : public ReadMacro {
   public:
    virtual ReaderMode start(int, Reader*);
};


ReaderMode _SpecialReadMacro::start(int, Reader* r) {
    new (r->falloc(sizeof(SpecialFrame))) SpecialFrame(r);
    return CONTINUE;
}


/* QuoteReadMacro */

class QuoteReadMacro : public ReadMacro {
   public:
    virtual ReaderMode start(int, Reader*);
};


ReaderMode QuoteReadMacro::start(int, Reader* r) {
    new (r->falloc(sizeof(QuoteFrame))) QuoteFrame(r);
    return START;
}


/* DotReadMacro */

class DotReadMacro : public ReadMacro {
   public:
    virtual ReaderMode start(int, Reader*);
};


ReaderMode DotReadMacro::start(int c, Reader* r) { return r->receiveDot(c); }


/* ListTerminatorReadMacro */

class ListTerminatorReadMacro : public ReadMacro {
   public:
    virtual ReaderMode start(int, Reader*);
};


ReaderMode ListTerminatorReadMacro::start(int c, Reader* r) {
    return r->receiveTerminator(c);
}


/* ListReadMacro */

class ListReadMacro : public ReadMacro {
   protected:
    int startChar;
    int dotChar;
    int closingChar;
    FINALIZER finalizer;

   public:
    ListReadMacro(int, int, int, FINALIZER);
    virtual ReaderMode start(int, Reader*);
    void install(ReadTable*);
};


ListReadMacro::ListReadMacro(int sc, int dc, int cc, FINALIZER f)
    : ReadMacro(), startChar(sc), dotChar(dc), closingChar(cc), finalizer(f) {}


ReaderMode ListReadMacro::start(int, Reader* r) {
    new (r->falloc(sizeof(ListFrame)))
        ListFrame(r, dotChar, closingChar, finalizer);
    return START;
}


static Ob* msgFinalizer(Reader* r, Ob** stk, int n, Ob* rest) {
    if (n == 0 && rest == NILexpr) {
        return NILexpr;
    } else {
        return TupleExpr::create(stk, n, rest);
    }
}


static FINALIZER findSpecialForm(Ob* symbol);


static Ob* rqstFinalizer(Reader* r, Ob** stk, int n, Ob* rest) {
    if (n == 0) {
        return r->error("no target in request expr");
    }

    FINALIZER fn = findSpecialForm(stk[0]);

    if (fn) {
        return (*fn)(r, stk, n, rest);
    } else {
        TupleExpr* msg = (TupleExpr*)msgFinalizer(r, stk + 1, n - 1, rest);
        return RequestExpr::create(stk[0], msg);
    }
}


static Ob* sendFinalizer(Reader* r, Ob** stk, int n, Ob* rest) {
    if (n < 2) {
        return r->error("no target in send expr");
    }

    return (SendExpr::create(
        stk[1], (TupleExpr*)msgFinalizer(r, stk + 2, n - 2, rest)));
}


static Ob* ifFinalizer(Reader* r, Ob** stk, int n, Ob*) {
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


static Ob* blockFinalizer(Reader* r, Ob** stk, int n, Ob* rest) {
    if (n < 2 || rest != NILexpr) {
        return r->error("improper syntax for block expression");
    }

    return blockHelper(stk + 1, n - 1, false);
}


static Ob* freeFinalizer(Reader* r, Ob** stk, int n, Ob* rest) {
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


static Ob* methodFinalizer(Reader* r, Ob** stk, int n, Ob* rest) {
    if (n < 3 || rest != NILexpr) {
        return r->error("improper syntax for method expression");
    }

    Ob* formals = stk[1];
    PROTECT(formals);
    Ob* block = blockHelper(stk + 2, n - 2);
    return MethodExpr::create(NIV, formals, block);
}


static Ob* procFinalizer(Reader* r, Ob** stk, int n, Ob* rest) {
    if (n < 3 || rest != NILexpr) {
        return r->error("improper syntax for proc expression");
    }

    Ob* formals = stk[1];
    PROTECT(formals);
    Ob* block = blockHelper(stk + 2, n - 2);
    return ProcExpr::create(NIV, formals, block);
}


static Ob* namedProcFinalizer(Reader* r, Ob** stk, int n, Ob* rest) {
    if (n < 4 || rest != NILexpr) {
        return r->error("improper syntax for named-proc expression");
    }

    Ob* formals = stk[2];
    PROTECT(formals);
    Ob* block = blockHelper(stk + 3, n - 3);
    return ProcExpr::create(stk[1], formals, block);
}


static Ob* letFinalizer(Reader* r, Ob** stk, int n, Ob* rest) {
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


static Ob* letstarFinalizer(Reader* r, Ob** stk, int n, Ob* rest) {
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


static Ob* letrecFinalizer(Reader* r, Ob** stk, int n, Ob* rest) {
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


static Ob* seqFinalizer(Reader* r, Ob** stk, int n, Ob* rest) {
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


static Ob* setFinalizer(Reader* r, Ob** stk, int n, Ob* rest) {
    if (n != 3 || rest != NILexpr || !IS_SYM(stk[1])) {
        return r->error("improper syntax for set! expression");
    }

    return SetExpr::create(stk[1], stk[2]);
}


static Ob* gotoFinalizer(Reader* r, Ob** stk, int n, Ob* rest) {
    if (n != 2 || rest != NILexpr || !IS_SYM(stk[1])) {
        return r->error("improper syntax for goto expression");
    }

    return GotoExpr::create(stk[1]);
}


static Ob* labelFinalizer(Reader* r, Ob** stk, int n, Ob* rest) {
    Ob* label;

    if (n < 3 || rest != NILexpr || !IS_SYM(label = stk[n - 2])) {
        return r->error("improper syntax for label expression");
    }

    Ob* block = blockHelper(stk + 2, n - 2);
    return LabelExpr::create(label, block);
}


FINALIZER
findSpecialForm(Ob* symbol) {
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


static WhitespaceReadMacro _WRM;
static CommentReadMacro _CRM;
static AtomReadMacro _ARM;
static DotReadMacro _DRM;
static ListTerminatorReadMacro _LTRM;
static ListReadMacro _TeRM('[', '&', ']', &msgFinalizer);
static ListReadMacro _ReRM('(', '&', ')', &rqstFinalizer);
static StringReadMacro _SRM;
static _SpecialReadMacro _SpRM;
static QuoteReadMacro _QRM;


void ListReadMacro::install(ReadTable* rt) {
    rt->tbl[startChar] = this;
    rt->tbl[dotChar] = &_DRM;
    rt->tbl[closingChar] = &_LTRM;
    rt->attributes[startChar] |= _DELIMITER;
    rt->attributes[closingChar] |= _DELIMITER;
}


static ReadTable StdReadTable;


ReadTable::ReadTable() {
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


BUILTIN_CLASS(Reader) {}


Reader::Reader(ReadTable* rt, FILE* f)
    : BinaryOb(align(sizeof(Reader)), CLASS_META(Reader), CLASS_SBO(Reader)),
      buf(NULL),
      bufsize(0),
      bufp(0),
      errorEncountered(false),
      waitingOnIO(NOT_WAITING),
      fstk(),
      ostk(),
      rt(rt),
      mode(START),
      file(f) {
    heap->registerForeignOb(this);
    Reader::updateCnt();
}


Reader::~Reader() {
    if (file && file != stdin) {
        fclose(file);
    }
}


Reader* Reader::create(FILE* f) {
    void* loc = PALLOC(align(sizeof(Reader)));
    return new (loc) Reader(&StdReadTable, f);
}


Ob* Reader::readCh() {
    if (waitingOnIO != NOT_WAITING) {
        return finish(
            error("IO synchronization problem with reader for file %d",
                  fileno(file)));
    } else {
        waitingOnIO = WAITING_FOR_CHAR;
        return resumeCh();
    }
}


Ob* Reader::readExpr() {
    if (waitingOnIO != NOT_WAITING) {
        return finish(
            error("IO synchronization problem with reader for file %d",
                  fileno(file)));
    } else {
        new (falloc(sizeof(TopFrame))) TopFrame(this);
        mode = START;
        waitingOnIO = WAITING_FOR_EXPR;
        return resumeExpr();
    }
}


Ob* Reader::resume() {
    switch (waitingOnIO) {
    case NOT_WAITING:
        return finish(
            error("IO synchronization problem with reader for file %d",
                  fileno(file)));
    case WAITING_FOR_EXPR:
        return resumeExpr();
    case WAITING_FOR_CHAR:
        return resumeCh();
    default:
        suicide("invalid value for Reader::waitingOnIO (= %d)", waitingOnIO);
        return NIV;
    }
}


Ob* Reader::resumeCh() {
    errno = 0;
#ifndef MIPS_SGI_SYSV
    int c = getc(file);
#else
    int c = fgetc(file);
#endif

    if (c == EOF) {
        if (EWOULDBLOCK == errno) {
            return suspendReader();
        } else {
            return finish(RBLEOF);
        }
    } else {
        return finish(RBLCHAR(c));
    }
}


Ob* Reader::resumeExpr() {
    /*
     * We have to protect the reader because the calls to finalizeAtom
     * (as well as the calls to receiveOb) may cause scavenges to occur.
     * We try to minimize the damage by using an unprotected version of
     * SELF as much as possible within the loop.
     */

    PROTECT_THIS(Reader);

    ReaderMode nextMode = SELF->mode;

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
         * to capture the next mode of the reader; assigning into
         * "my->mode" won't necessarily work because "my" might be
         * invalidated by a scavenge during a call to "process" or
         * "receiveWhatever".
         */

        Reader* const my = SELF;
        my->mode = nextMode;

        errno = 0;
#ifndef MIPS_SGI_SYSV
        int c = getc(my->file);
#else
        int c = fgetc(my->file);
#endif

        if (EOF == c && EWOULDBLOCK == errno) {
            return my->suspendReader();
        }

        switch (nextMode) {
        case START:
            if (EOF == c) {
                nextMode = my->ftop()->receiveEof(my);
            } else {
                nextMode = my->rt->tbl[c]->start(c, my);
            }
            break;

        case CONTINUE:
            if (EOF == c) {
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
                ungetc(c, my->file);
                /*
                 * We can't use Reader::receiveOb here because the
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
            suicide("unanticipated case in Reader::readExpr");
            break;
        }

        if (nextMode == STOP) {
            return SELF->finish(SELF->opop());
        }
    }
}


Ob* Reader::error(const char* fmt, ...) {
    /*
     * This really ought to be changed to be less dogmatic about writing
     * on stderr, and use the Rosette error reporting mechanism instead.
     */
    fprintf(stderr, "***read error: ");
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fputc('\n', stderr);

    errorEncountered = true;
    return READ_ERROR;
}


void Reader::resetState() {
    waitingOnIO = NOT_WAITING;
    errorEncountered = false;
    mode = START;
    fstk.reset();
    ostk.reset();
}


Ob* Reader::suspendReader() {
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


Ob* Reader::finish(Ob* v) {
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


void Reader::opush(Ob* v) {
    /*
     * Be careful to check whether we are pushing a young element onto an
     * ostk owned by an old Reader.
     */
    ostk.push(v);
    checkStore(v);
}


Ob*& Reader::otop(int n) { return ostk.top(n); }
Ob* Reader::opop() { return ostk.pop(); }
void Reader::odel(int n) { ostk.del(n); }


ReaderMode Reader::receiveOb(Ob* v) {
    fpop();
    return ftop()->receiveOb(v, this);
}


ReaderMode Reader::receiveChar(int c) {
    fpop();
    return ftop()->receiveChar(c, this);
}


ReaderMode Reader::receiveDot(int c) { return ftop()->receiveDot(c, this); }


ReaderMode Reader::receiveTerminator(int c) {
    return ftop()->receiveTerminator(c, this);
}


ReaderMode Reader::acceptEscChar(int c, int gapsPermitted) {
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


void Reader::growBuffer() {
    char* newbuf = new char[bufsize + 128];
    if (buf) {
        memcpy(newbuf, buf, bufsize * sizeof(char));
        delete buf;
    }
    buf = newbuf;
    bufsize += 128;
}


void Reader::resetBuffer() {
    if (buf) {
        buf[0] = '\0';
    }
    bufp = 0;
}


char* Reader::finalizeBuffer() {
    buffer('\0');
    return buf;
}


Ob* Reader::finalizeAtom() {
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


Ob* Reader::cloneTo(Ob*, Ob*) {
    warning("can't clone Readers");
    return this;
}


int Reader::traversePtrs(PSOb__PSOb f) {
    return BinaryOb::traversePtrs(f) + ostk.traversePtrs(f);
}


int Reader::traversePtrs(SI__PSOb f) {
    return BinaryOb::traversePtrs(f) + ostk.traversePtrs(f);
}


void Reader::traversePtrs(V__PSOb f) {
    BinaryOb::traversePtrs(f);
    ostk.traversePtrs(f);
}
