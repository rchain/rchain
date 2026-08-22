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

#include "rosette.h"
#include "Code.h"
#include "Ctxt.h"
#include "Location.h"
#include "Addr.h"
#include "ModuleInit.h"

#define SPANSIZE(n) ((n) == 0 ? (1 << BitFieldSpanSize) : (n))
#define SPANSIZE00(n) ((n) == 0 ? (1 << BitField00SpanSize) : (n))

int operator==(Location loc1, Location loc2) { return loc1.atom == loc2.atom; }
int operator!=(Location loc1, Location loc2) { return loc1.atom != loc2.atom; }


#ifdef MAP_BACK_ADDRESS
uint32_t nontrivial_pre_fixnum_to_addr(int x) {
    uint32_t y = x;
    if (x < END_SMALL_ADDR) {
        return y;
    }

    if (x >= END_SMALL_ADDR) {
        return (y + DBEGIN - END_SMALL_ADDR);
    }

    printf("error");
    return 0;
}

int nontrivial_addr_to_pre_fixnum(Ob* x) {
    if (x < (void*)END_SMALL_ADDR) {
        return (int)x;
    }

    if (x >= (void*)DBEGIN) {
        return (int)((unsigned int)x - DBEGIN + END_SMALL_ADDR);
    }

    printf("error");
    return 0;
}
#endif


bool store(Location loc, Ctxt* k, Ob* val) {
    switch (GET_GENERIC_TYPE(loc)) {
    case LT_CtxtRegister:
        ASSIGN(k, reg(GET_CTXTREG_INDEX(loc)), val);
        return false;

    case LT_ArgRegister:
        if ((int)GET_ARGREG_INDEX(loc) >= k->argvec->numberOfElements()) {
            return true;
        } else {
            ASSIGN(k->argvec, elem(GET_ARGREG_INDEX(loc)), val);
            return false;
        }

    case LT_LexVariable:
        return BASE(k->env)->setLex(GET_LEXVAR_IND(loc), GET_LEXVAR_LEVEL(loc),
                                    GET_LEXVAR_OFFSET(loc), val) == INVALID;

    case LT_AddrVariable:
        return BASE(k->env)->setAddr(GET_ADDRVAR_IND(loc),
                                     GET_ADDRVAR_LEVEL(loc),
                                     GET_ADDRVAR_OFFSET(loc), val) == INVALID;

    case LT_GlobalVariable: {
        Ob* p = GlobalEnv->container();
        int n = GET_GLOBALVAR_OFFSET(loc);
        return n < p->numberOfSlots() && (ASSIGN(p, slot(n), val), true);
    }

    case LT_BitField:
        return !IS_FIXNUM(val) ||
               BASE(k->env)->setField(
                   GET_BITFIELD_IND(loc), GET_BITFIELD_LEVEL(loc),
                   GET_BITFIELD_OFFSET(loc), SPANSIZE(GET_BITFIELD_SPAN(loc)),
                   (unsigned long)(FIXVAL(val))) == INVALID;

    case LT_BitField00:
        return !IS_FIXNUM(val) ||
               BASE(k->env)->setField(0, 0, GET_BITFIELD00_OFFSET(loc),
                                      SPANSIZE00(GET_BITFIELD00_SPAN(loc)),
                                      (unsigned long)(FIXVAL(val))) == INVALID;
    case LT_Limbo:
    default:
        return true;
    }
}


Ob* fetch(Location loc, Ctxt* k) {
    switch (GET_GENERIC_TYPE(loc)) {
    case LT_CtxtRegister:
        return (GET_CTXTREG_INDEX(loc) < NumberOfCtxtRegs
                    ? k->reg(GET_CTXTREG_INDEX(loc))
                    : INVALID);

    case LT_ArgRegister:
        return ((int)GET_ARGREG_INDEX(loc) < k->argvec->numberOfElements()
                    ? k->argvec->elem(GET_ARGREG_INDEX(loc))
                    : INVALID);

    case LT_LexVariable:
        return BASE(k->env)->getLex(GET_LEXVAR_IND(loc), GET_LEXVAR_LEVEL(loc),
                                    GET_LEXVAR_OFFSET(loc));

    case LT_AddrVariable:
        return BASE(k->env)->getAddr(GET_ADDRVAR_IND(loc),
                                     GET_ADDRVAR_LEVEL(loc),
                                     GET_ADDRVAR_OFFSET(loc));

    case LT_GlobalVariable: {
        Ob* p = GlobalEnv->container();
        return ((int)GET_GLOBALVAR_OFFSET(loc) < p->numberOfSlots()
                    ? p->slot(GET_GLOBALVAR_OFFSET(loc))
                    : INVALID);
    }

    case LT_BitField:
        return BASE(k->env)->getField(
            GET_BITFIELD_IND(loc), GET_BITFIELD_LEVEL(loc),
            GET_BITFIELD_OFFSET(loc), SPANSIZE(GET_BITFIELD_SPAN(loc)),
            GET_BITFIELD_SIGN(loc));

    case LT_BitField00:
        return BASE(k->env)->getField(0, 0, GET_BITFIELD00_OFFSET(loc),
                                      SPANSIZE00(GET_BITFIELD00_SPAN(loc)),
                                      GET_BITFIELD00_SIGN(loc));

    case LT_Limbo:
    default:
        return INVALID;
    }
}


void printRep(Location loc, char* buf) {
    /*
     * These names must be laid out in the same order as the registers in
     * a Ctxt structure.
     */

    static char* names[] = {"rslt", "trgt", "argvec",   "env",  "code",
                            "ctxt", "self", "self-env", "rcvr", "monitor"};

    switch (GET_GENERIC_TYPE(loc)) {
    case LT_CtxtRegister:
        if (0 <= GET_CTXTREG_INDEX(loc) &&
            GET_CTXTREG_INDEX(loc) < NumberOfCtxtRegs)
            strcpy(buf, names[GET_CTXTREG_INDEX(loc)]);
        else
            sprintf(buf, "unknown ctxt register 0x%x",
                    (int)GET_CTXTREG_INDEX(loc));
        break;

    case LT_ArgRegister:
        sprintf(buf, "arg[%d]", (int)GET_ARGREG_INDEX(loc));
        break;

    case LT_LexVariable:
        sprintf(buf, GET_LEXVAR_IND(loc) ? "lex[%d,(%d)]" : "lex[%d,%d]",
                (int)GET_LEXVAR_LEVEL(loc), (int)GET_LEXVAR_OFFSET(loc));
        break;

    case LT_AddrVariable:
        sprintf(buf, GET_ADDRVAR_IND(loc) ? "addr[%d,(%d)]" : "addr[%d,%d]",
                (int)GET_ADDRVAR_LEVEL(loc), (int)GET_ADDRVAR_OFFSET(loc));
        break;

    case LT_GlobalVariable:
        sprintf(buf, "global[%d]", GET_GLOBALVAR_OFFSET(loc));
        break;

    case LT_BitField:
        sprintf(buf,
                GET_BITFIELD_IND(loc) ? "%sfld[%d,(%d),%d]" : "%sfld[%d,%d,%d]",
                GET_BITFIELD_SIGN(loc) ? "s" : "u",
                (int)GET_BITFIELD_LEVEL(loc), (int)GET_BITFIELD_OFFSET(loc),
                (int)SPANSIZE(GET_BITFIELD_SPAN(loc)));
        break;

    case LT_BitField00:
        sprintf(buf, "%sfld[%d,%d]", GET_BITFIELD00_SIGN(loc) ? "s" : "u",
                (int)GET_BITFIELD00_OFFSET(loc),
                (int)SPANSIZE(GET_BITFIELD00_SPAN(loc)));
        break;

    case LT_Limbo:
        strcpy(buf, "limbo");
        break;

    default:
        suicide("printRep(Location, char*)");
    }
}


Ob* valWrt(Location loc, Ob* v) {
    switch (GET_GENERIC_TYPE(loc)) {
    case LT_LexVariable:
        return BASE(v)->getLex(GET_LEXVAR_IND(loc), GET_LEXVAR_LEVEL(loc),
                               GET_LEXVAR_OFFSET(loc));
    case LT_AddrVariable:
        return BASE(v)->getAddr(GET_ADDRVAR_IND(loc), GET_ADDRVAR_LEVEL(loc),
                                GET_ADDRVAR_OFFSET(loc));
    case LT_BitField:
        return BASE(v)->getField(GET_BITFIELD_IND(loc), GET_BITFIELD_LEVEL(loc),
                                 GET_BITFIELD_OFFSET(loc),
                                 SPANSIZE(GET_BITFIELD_SPAN(loc)),
                                 GET_BITFIELD_SIGN(loc));
    case LT_BitField00:
        return BASE(v)->getField(0, 0, GET_BITFIELD00_OFFSET(loc),
                                 SPANSIZE(GET_BITFIELD00_SPAN(loc)),
                                 GET_BITFIELD00_SIGN(loc));
    case LT_GlobalVariable:
        return BASE(GlobalEnv)->getLex(true, 0, GET_GLOBALVAR_OFFSET(loc));
    case LT_Limbo:
        return ABSENT;
    default:
        suicide("valWrt(Location, Ob*)");
    }
    return INVALID;
}


Ob* setValWrt(Location loc, Ob* v, Ob* val) {
    switch (GET_GENERIC_TYPE(loc)) {
    case LT_LexVariable:
        return BASE(v)->setLex(GET_LEXVAR_IND(loc), GET_LEXVAR_LEVEL(loc),
                               GET_LEXVAR_OFFSET(loc), val);
    case LT_AddrVariable:
        return BASE(v)->setAddr(GET_ADDRVAR_IND(loc), GET_ADDRVAR_LEVEL(loc),
                                GET_ADDRVAR_OFFSET(loc), val);
    case LT_BitField:
        if (IS_FIXNUM(val)) {
            uint32_t bits = (uint32_t)FIXVAL(val);
            return BASE(v)->setField(GET_BITFIELD_IND(loc),
                                     GET_BITFIELD_LEVEL(loc),
                                     GET_BITFIELD_OFFSET(loc),
                                     SPANSIZE(GET_BITFIELD_SPAN(loc)), bits);
        } else {
            warning("Location::setValWrt arg not fixnum");
            return INVALID;
        }
    case LT_BitField00:
        if (IS_FIXNUM(val)) {
            uint32_t bits = (uint32_t)FIXVAL(val);
            return BASE(v)->setField(0, 0, GET_BITFIELD00_OFFSET(loc),
                                     SPANSIZE(GET_BITFIELD00_SPAN(loc)), bits);
        } else {
            warning("Location::setValWrt arg not fixnum");
            return INVALID;
        }
    case LT_GlobalVariable:
        return BASE(GlobalEnv)->setLex(true, 0, GET_GLOBALVAR_OFFSET(loc), val);
    default:
        suicide("Location::setValWrt");
    }
    return INVALID;
}


void adjustLevel(Location loc, int adjustment) {
    unsigned int tem;
    switch (GET_GENERIC_TYPE(loc)) {
    case LT_LexVariable:
        tem = GET_LEXVAR_LEVEL(loc) + adjustment;
        SET_LEXVAR_LEVEL(loc, tem);
        break;
    case LT_AddrVariable:
        tem = GET_ADDRVAR_LEVEL(loc) + adjustment;
        SET_ADDRVAR_LEVEL(loc, tem);
        break;
    case LT_BitField:
        tem = GET_BITFIELD_LEVEL(loc) + adjustment;
        SET_BITFIELD_LEVEL(loc, tem);
        break;
    default:
        suicide("Location::adjustLevel");
    }
}


Location CtxtReg(CtxtRegName n) {
    if (n >= NumberOfCtxtRegs) {
        suicide("invalid ctxt register (%d)", (int)n);
    }

    Location loc;
    loc.locfields = 0;
    SET_CTXTREG_INDEX(loc, n);
    SET_CTXTREG_TYPE(loc, LT_CtxtRegister);
    SET_GENERIC_TAG(loc, OTlocation);
    return loc;
}


Location ArgReg(int n) {
    if (n > MaxArgs) {
        suicide("invalid arg register index (%d)", n);
    }

    Location loc;
    loc.locfields = 0;
    SET_ARGREG_INDEX(loc, n);
    SET_ARGREG_TYPE(loc, LT_ArgRegister);
    SET_GENERIC_TAG(loc, OTlocation);
    return loc;
}


Location LexVar(int level, int offset, int indirect) {
    if (level >= (1 << LexLevelSize) || offset >= (1 << LexOffsetSize)) {
        suicide(indirect ? "%s (lex[%d,(%d)])" : "%s (lex[%d,%d])",
                "unrepresentable location", level, offset);
    }

    Location loc;
    loc.locfields = 0;
    SET_LEXVAR_IND(loc, (indirect != 0));
    SET_LEXVAR_LEVEL(loc, level);
    SET_LEXVAR_OFFSET(loc, offset);
    SET_LEXVAR_TYPE(loc, LT_LexVariable);
    SET_GENERIC_TAG(loc, OTlocation);
    return loc;
}


Location AddrVar(int level, int offset, int indirect) {
    if (level >= (1 << AddrLevelSize) || offset >= (1 << AddrOffsetSize)) {
        suicide(indirect ? "%s (addr[%d,(%d)])" : "%s (addr[%d,%d])",
                "unrepresentable location", level, offset);
    }

    Location loc;
    loc.locfields = 0;
    SET_ADDRVAR_IND(loc, (indirect != 0));
    SET_ADDRVAR_LEVEL(loc, level);
    SET_ADDRVAR_OFFSET(loc, offset);
    SET_ADDRVAR_TYPE(loc, LT_AddrVariable);
    SET_GENERIC_TAG(loc, OTlocation);
    return loc;
}


Location GlobalVar(int n) {
    if (n >= (1 << GlobalOffsetSize)) {
        suicide("unrepresentable location (global[%d])", n);
    }

    Location loc;
    loc.locfields = 0;
    SET_GLOBALVAR_OFFSET(loc, n);
    SET_GLOBALVAR_TYPE(loc, LT_GlobalVariable);
    SET_GENERIC_TAG(loc, OTlocation);
    return loc;
}


Location BitField(int level, int offset, int span, int indirect, int sign) {
    if (level >= (1 << BitFieldLevelSize) ||
        offset >= (1 << BitFieldOffsetSize) || span > (1 << BitFieldSpanSize)) {
        suicide(indirect ? "%s (%sfld[%d,(%d),%d])" : "%s (%sfld[%d,%d,%d])",
                "unrepresentable location", sign ? "s" : "u", level, offset,
                span);
    }

    Location loc;
    loc.locfields = 0;
    SET_BITFIELD_IND(loc, (indirect != 0));
    SET_BITFIELD_SIGN(loc, (sign != 0));
    SET_BITFIELD_LEVEL(loc, level);
    SET_BITFIELD_OFFSET(loc, offset);
    SET_BITFIELD_SPAN(loc, (span == (1 << BitFieldSpanSize) ? 0 : span));
    SET_BITFIELD_TYPE(loc, LT_BitField);
    SET_GENERIC_TAG(loc, OTlocation);
    return loc;
}

Location BitField00(int offset, int span, int sign) {
    if (offset >= (1 << BitField00OffsetSize) ||
        span > (1 << BitField00SpanSize)) {
        suicide("%s (%sfld[%d,%d])", "unrepresentable location",
                sign ? "s" : "u", offset, span);
    }

    Location loc;
    loc.locfields = 0;
    SET_BITFIELD00_SIGN(loc, (sign != 0));
    SET_BITFIELD00_OFFSET(loc, offset);
    SET_BITFIELD00_SPAN(loc, (span == (1 << BitField00SpanSize) ? 0 : span));
    SET_BITFIELD00_TYPE(loc, LT_BitField00);
    SET_BITFIELD00_TAG(loc, OTlocation);
    return loc;
}

static Location Limbo() {
    Location loc;
    loc.locfields = 0;
    SET_GENERIC_TYPE(loc, LT_Limbo);
    SET_GENERIC_TAG(loc, OTlocation);
    return loc;
}


Location LocLimbo = Limbo();
Location LocRslt = CtxtReg(CRN_Rslt);
Location LocTrgt = CtxtReg(CRN_Trgt);

static int private_totally_ours_LocSize = sizeof(Location);
static int private_totally_ours_ObStarSize = sizeof(Ob*);


MODULE_INIT(Location) {
    assert(private_totally_ours_LocSize == private_totally_ours_ObStarSize);
    // assert(sizeof(Location) == sizeof(Ob*));
}
