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

#include "RblAtom.h"
#include "Compile.h"
#include "Ctxt.h"
#include "Ob.h"
#include "Pattern.h"
#include "Prim.h"
#include "RBLstring.h"
#include "Tuple.h"
#include "StreamUtils.h"
#include "BuiltinClass.h"
#include "ModuleInit.h"

#include <assert.h>


Ob* Qsend;
Ob* Qif;
Ob* Qmethod;
Ob* Qproc;
Ob* Qnamedproc;
Ob* Qblock;
Ob* Qlet;
Ob* Qletstar;
Ob* Qletrec;
Ob* Qseq;
Ob* Qsetbang;
Ob* Qgoto;
Ob* Qlabel;
Ob* Qfree;

Ob* Qmeta;
Ob* Qparent;
Ob* Qmap;
Ob* Qid;
Ob* Qcode;
Ob* Qsource;
Ob* Qenv;
Ob* Qlitvec;
Ob* Qcodevec;

Ob* Qanon;
Ob* Qunnamed;


RblAtom::RblAtom(int sz, Ob* meta, Ob* sbo) : Ob(sz, meta, sbo) {
    atom = INVALID;
}

Ob* RblAtom::self() { return atom; }
Ob* RblAtom::cloneTo(Ob*, Ob*) { return atom; }
Ob* RblAtom::update(bool, Ctxt*) { return atom; }


BUILTIN_CLASS(Symbol) {}

Symbol::Symbol()
    : RblAtom(sizeof(Symbol), CLASS_META(Symbol), CLASS_SBO(Symbol)) {
    Symbol::updateCnt();
}

Symbol* Symbol::create() {
    void* loc = PALLOC(sizeof(Symbol));
    return new (loc) Symbol();
}

bool Symbol::ConstantP() { return false; }

void Symbol::printOn(FILE* f) {
    char* str = SYMPTR(atom);
    char c;
    do {
        c = *str++;
        if ('\0' == c) {  // End of string
            break;
        }

        if (c == '\\') {
            fputc('\\', f);
            fputc('\\', f);
        } else if (isprint(c) && !isspace(c)) {
            fputc(c, f);
        } else {
            fputc('\\', f);
            switch (c) {
            case ' ':
                fputc(' ', f);
                break;
            case '\n':
                fputc('n', f);
                break;
            case '\f':
                fputc('f', f);
                break;
            case '\t':
                fputc('t', f);
                break;
            case '\r':
                fputc('r', f);
                break;
            default:
                fprintf(f, "x%.2x", (int)c);
                break;
            }
        }
    } while (true);
}

void Symbol::printQuotedOn(FILE* f) {
    putc('\'', f);
    printOn(f);
}

void Symbol::displayOn(FILE* f) { fputs(SYMPTR(atom), f); }
const char* Symbol::asCstring() { return SYMPTR(atom); }
char* Symbol::asPathname() { return SYMPTR(atom); }
Pattern* Symbol::makePattern() { return IdPattern::create(atom); }

AttrNode* Symbol::makeAttrNode(bool valueCtxt) {
    return SymbolNode::create(atom, valueCtxt);
}

Ob* Symbol::unquote() {
    NI("unquote");
    return INVALID;
}


BUILTIN_CLASS(RblBool) {}


RblBool::RblBool()
    : RblAtom(sizeof(RblBool), CLASS_META(RblBool), CLASS_SBO(RblBool)) {
    RblBool::updateCnt();
}

RblBool* RblBool::create() {
    void* loc = PALLOC(sizeof(RblBool));
    return new (loc) RblBool();
}

const char* RblBool::asCstring() { return BOOLVAL(atom) ? "#t" : "#f"; }

convertArgReturnPair RblBool::convertActualArg(Ctxt* ctxt, Ob* obj) {
    if (typep(obj)) {
        cnvArgRetPair.val = (uint32_t)BOOLVAL(obj);
        cnvArgRetPair.failp = 0;
    } else {
        cnvArgRetPair.val = (uint32_t)-1;
        cnvArgRetPair.failp = 1;
    }

    return cnvArgRetPair;
}


BUILTIN_CLASS(Char) {}

Char::Char() : RblAtom(sizeof(Char), CLASS_META(Char), CLASS_SBO(Char)) {
    Char::updateCnt();
}

Char* Char::create() {
    void* loc = PALLOC(sizeof(Char));
    return new (loc) Char();
}

void Char::printOn(FILE* f) {
    fputc('#', f);
    fputc('\\', f);
    putSafeChar(CHARVAL(atom), f, '\\');
}

void Char::displayOn(FILE* f) { putc(CHARVAL(atom), f); }
int Base::nClasses = 0;
char Ob::stringbuf[256] = {0};

const char* Char::asCstring() {
    Ob::stringbuf[0] = CHARVAL(atom);
    Ob::stringbuf[1] = '\0';
    return Ob::stringbuf;
}


BUILTIN_CLASS(Fixnum) {}

Fixnum::Fixnum()
    : RblAtom(sizeof(Fixnum), CLASS_META(Fixnum), CLASS_SBO(Fixnum)) {
    Fixnum::updateCnt();
}

Fixnum* Fixnum::create() {
    void* loc = PALLOC(sizeof(Fixnum));
    return new (loc) Fixnum();
}

char Fixnum::format[FixnumFormatSize] = "%d";


const char* Fixnum::asCstring() {
    sprintf(Ob::stringbuf, Fixnum::format, FIXVAL(atom));
    return Ob::stringbuf;
}

convertArgReturnPair Fixnum::convertActualArg(Ctxt* ctxt, Ob* obj) {
    if (typep(obj)) {
        cnvArgRetPair.val = FIXVAL(obj);
        cnvArgRetPair.failp = 0;
    } else {
        cnvArgRetPair.val = (uint32_t)-1;
        cnvArgRetPair.failp = 0;
    }
    return cnvArgRetPair;
}

Ob* Fixnum::convertActualRslt(Ctxt*, uint32_t obj) { return FIXNUM(obj); }


DEF("fxFormat:", fxFormat, 1, 1) {
    CHECK(0, RBLstring, fmt);
    PROTECT(__CTXT__);
    RBLstring* result = RBLstring::create(Fixnum::format);
    strncpy(Fixnum::format, (char*)&fmt->byte(0), FixnumFormatSize - 1);
    Fixnum::format[FixnumFormatSize - 1] = '\0';
    return result;
}


BUILTIN_CLASS(Niv) {}

Niv::Niv() : RblAtom(sizeof(Niv), CLASS_META(Niv), CLASS_SBO(Niv)) {
    this->atom = NIV;
    Niv::updateCnt();
}

Niv* Niv::create() {
    void* loc = PALLOC(sizeof(Niv));
    return new (loc) Niv();
}

const char* Niv::asCstring() { return "#niv"; }

Ob* Niv::invoke(Ctxt* c) {
    /* return NIV; */
    return Ob::invoke(c);
}


BUILTIN_CLASS(Sysval) {}

Sysval::Sysval()
    : RblAtom(sizeof(Sysval), CLASS_META(Sysval), CLASS_SBO(Sysval)) {
    Sysval::updateCnt();
}

Sysval* Sysval::create() {
    void* loc = PALLOC(sizeof(Sysval));
    return new (loc) Sysval();
}

const char* Sysval::asCstring() {
    switch (ESCVAL(atom)) {
    case syscodeInvalid:
        return "#inv";
    case syscodeUpcall:
        return "#upcall";
    case syscodeSuspended:
        return "#suspended";
    case syscodeInterrupt:
        return "#interrupt";
    case syscodeSleep:
        return "#sleep";
    case syscodeDeadThread:
        return "#deadThread";
    }

    suicide("unrecognized syscode value -- %d", ESCVAL(atom));
    return NULL;
}


BUILTIN_CLASS(ExpandedLocation) {}

ExpandedLocation::ExpandedLocation()
    : RblAtom(sizeof(ExpandedLocation), CLASS_META(ExpandedLocation),
              CLASS_SBO(ExpandedLocation)) {
    ExpandedLocation::updateCnt();
}

ExpandedLocation* ExpandedLocation::create() {
    void* loc = PALLOC(sizeof(ExpandedLocation));
    return new (loc) ExpandedLocation();
}

const char* ExpandedLocation::asCstring() {
    Location loc;
    loc.atom = atom;
    strcpy(Ob::stringbuf, "{Loc ");
    printRep(loc, &Ob::stringbuf[strlen(Ob::stringbuf)]);
    strcat(Ob::stringbuf, "}");
    return Ob::stringbuf;
}


Symbol* prototypicalSym;
RblBool* prototypicalBool;
Char* prototypicalChar;
Fixnum* prototypicalFixnum;
Niv* prototypicalNiv;
Sysval* prototypicalSysval;
ExpandedLocation* prototypicalExpandedLocation;


void tagError(Ob* v, ObTag t) {
    suicide("tag mismatch: expected tag = 0x%x, got data = 0x%x", t, v);
}


Ob* decodeAtom(Ob* v) {
    switch (TAG(v)) {
    case OTsym:
        prototypicalSym->atom = v;
        return prototypicalSym;

    case OTfixnum:
        prototypicalFixnum->atom = v;
        return prototypicalFixnum;

    case OTesc:
        switch (ESCTAG(v)) {
        case OTbool:
            prototypicalBool->atom = v;
            return prototypicalBool;

        case OTchar:
            prototypicalChar->atom = v;
            return prototypicalChar;

        case OTniv:
            return prototypicalNiv;

        case OTsysval:
            prototypicalSysval->atom = v;
            return prototypicalSysval;

        case OTlocation:
            prototypicalExpandedLocation->atom = v;
            return prototypicalExpandedLocation;

        default:
            suicide("decodeAtom -- unrecognized escape tag %d", ESCTAG(v));
        }
    }

    return INVALID;
}


MODULE_INIT(RblAtom) {
    Qsend = SYMBOL("send");
    Qif = SYMBOL("if");
    Qmethod = SYMBOL("method");
    Qproc = SYMBOL("proc");
    Qnamedproc = SYMBOL("named-proc");
    Qblock = SYMBOL("block");
    Qlet = SYMBOL("let");
    Qletstar = SYMBOL("let*");
    Qletrec = SYMBOL("letrec");
    Qseq = SYMBOL("seq");
    Qsetbang = SYMBOL("set!");
    Qgoto = SYMBOL("goto");
    Qlabel = SYMBOL("label");
    Qfree = SYMBOL("free");
    Qmeta = SYMBOL("meta");
    Qparent = SYMBOL("parent");
    Qmap = SYMBOL("map");
    Qid = SYMBOL("id");
    Qcode = SYMBOL("code");
    Qsource = SYMBOL("source");
    Qenv = SYMBOL("env");
    Qlitvec = SYMBOL("litvec");
    Qcodevec = SYMBOL("codevec");

    Qanon = SYMBOL("anonymous");
    Qunnamed = SYMBOL("unnamed-class");

    prototypicalSym = (Symbol*)heap->tenure(Symbol::create());
    prototypicalBool = (RblBool*)heap->tenure(RblBool::create());
    prototypicalChar = (Char*)heap->tenure(Char::create());
    prototypicalFixnum = (Fixnum*)heap->tenure(Fixnum::create());
    prototypicalNiv = (Niv*)heap->tenure(Niv::create());
    prototypicalSysval = (Sysval*)heap->tenure(Sysval::create());
    prototypicalExpandedLocation =
        (ExpandedLocation*)heap->tenure(ExpandedLocation::create());

    Define("Sysval", INVALID);
}


DEF("ch<", charLt, 2, 2) {
    if (!IS(OTchar, ARG(0))) {
        return PRIM_MISMATCH(0, "Char");
    }

    if (!IS(OTchar, ARG(1))) {
        return RBLFALSE;
    }

    return RBLBOOL((int)ARG(0) < (int)ARG(1));
}


DEF("ch<=", charLe, 2, 2) {
    if (!IS(OTchar, ARG(0))) {
        return PRIM_MISMATCH(0, "Char");
    }

    if (!IS(OTchar, ARG(1))) {
        return RBLFALSE;
    }

    return RBLBOOL((int)ARG(0) <= (int)ARG(1));
}

DEF("ch=", charEq, 2, 2) {
    if (!IS(OTchar, ARG(0))) {
        return PRIM_MISMATCH(0, "Char");
    }

    if (!IS(OTchar, ARG(1))) {
        return RBLFALSE;
    }

    return RBLBOOL((int)ARG(0) == (int)ARG(1));
}

DEF("ch!=", charNe, 2, 2) {
    if (!IS(OTchar, ARG(0))) {
        return PRIM_MISMATCH(0, "Char");
    }

    if (!IS(OTchar, ARG(1))) {
        return RBLFALSE;
    }

    return RBLBOOL((int)ARG(0) != (int)ARG(1));
}


DEF("ch>=", charGe, 2, 2) {
    if (!IS(OTchar, ARG(0))) {
        return PRIM_MISMATCH(0, "Char");
    }

    if (!IS(OTchar, ARG(1))) {
        return RBLFALSE;
    }

    return RBLBOOL((int)ARG(0) >= (int)ARG(1));
}


DEF("ch>", charGt, 2, 2) {
    if (!IS(OTchar, ARG(0))) {
        return PRIM_MISMATCH(0, "Char");
    }

    if (!IS(OTchar, ARG(1))) {
        return RBLFALSE;
    }

    return RBLBOOL((int)ARG(0) > (int)ARG(1));
}


DEF("ch->fx", charToFixnum, 1, 1) {
    if (!IS(OTchar, ARG(0))) {
        return PRIM_MISMATCH(0, "Char");
    }

    return FIXNUM(CHARVAL(ARG(0)));
}
