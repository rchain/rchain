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

#include "Number.h"
#include "Ctxt.h"
#include "Interrupt.h"
#include "Prim.h"
#include "RBLstring.h"
#include "Tuple.h"

#include "BuiltinClass.h"

#include <algorithm>
#include <ctype.h>
#include <math.h>


Ob* checkFxResult(Prim* prim, Ctxt* ctxt, int answer) {
    if (arithmeticException) {
        arithmeticException = 0;
        return prim->runtimeError(ctxt, "arithmetic exception");
    } else {
        return FIXNUM(answer);
    }
}


DEF("fx+", fxPlus, 0, MaxArgs) {
    short int n = NARGS;
    int accum = 0;

    while (n--) {
        CHECK_FIXNUM(n, m);
        accum += m;
    }

    return FIXNUM(accum);
}


DEF("fx-", fxMinus, 1, 2) {
    int result = 0;

    switch (NARGS) {
    case 1: {
        CHECK_FIXNUM(0, n);
        result = -n;
        break;
    }

    case 2: {
        CHECK_FIXNUM(0, m);
        CHECK_FIXNUM(1, n);
        result = m - n;
        break;
    }
    }

    return FIXNUM(result);
}


DEF("fx*", fxTimes, 0, MaxArgs) {
    short int nargs = NARGS;
    int accum = 1;

    while (nargs--) {
        CHECK_FIXNUM(nargs, n);
        accum *= n;
    }

    return FIXNUM(accum);
}


DEF("fx/", fxDiv, 2, 2) {
    CHECK_FIXNUM(0, m);
    CHECK_FIXNUM(1, n);
    return checkFxResult(fxDiv, __CTXT__, m / n);
}


DEF("fx%", fxMod, 2, 2) {
    CHECK_FIXNUM(0, m);
    CHECK_FIXNUM(1, n);
    return checkFxResult(fxMod, __CTXT__, m % n);
}


DEF("fx<", fxLt, 2, 2) {
    CHECK_FIXNUM(0, m);
    if (!IS_FIXNUM(ARG(1))) {
        return RBLFALSE;
    }
    int n = FIXVAL(ARG(1));
    return RBLBOOL(m < n);
}


DEF("fx<=", fxLe, 2, 2) {
    CHECK_FIXNUM(0, m);
    if (!IS_FIXNUM(ARG(1))) {
        return RBLFALSE;
    }
    int n = FIXVAL(ARG(1));
    return RBLBOOL(m <= n);
}


DEF("fx>", fxGt, 2, 2) {
    CHECK_FIXNUM(0, m);
    if (!IS_FIXNUM(ARG(1))) {
        return RBLFALSE;
    }
    int n = FIXVAL(ARG(1));
    return RBLBOOL(m > n);
}


DEF("fx>=", fxGe, 2, 2) {
    CHECK_FIXNUM(0, m);
    if (!IS_FIXNUM(ARG(1))) {
        return RBLFALSE;
    }
    int n = FIXVAL(ARG(1));
    return RBLBOOL(m >= n);
}


DEF("fx=", fxEq, 2, 2) {
    CHECK_FIXNUM(0, m);
    if (!IS_FIXNUM(ARG(1))) {
        return RBLFALSE;
    }
    int n = FIXVAL(ARG(1));
    return RBLBOOL(m == n);
}


DEF("fx!=", fxNe, 2, 2) {
    CHECK_FIXNUM(0, m);
    if (!IS_FIXNUM(ARG(1))) {
        return RBLFALSE;
    }
    int n = FIXVAL(ARG(1));
    return RBLBOOL(m != n);
}


DEF("fx-min", fxMin, 1, MaxArgs) {
    CHECK_FIXNUM(0, result);
    for (int next = 1; next < NARGS; next++) {
        CHECK_FIXNUM(next, n);
        result = std::min(result, n);
    }
    return FIXNUM(result);
}


DEF("fx-max", fxMax, 1, MaxArgs) {
    CHECK_FIXNUM(0, result);
    for (int next = 1; next < NARGS; next++) {
        CHECK_FIXNUM(next, n);
        result = std::max(result, n);
    }
    return FIXNUM(result);
}


DEF("fx-abs", fxAbs, 1, 1) {
    CHECK_FIXNUM(0, n);
    return FIXNUM(n < 0 ? -n : n);
}


DEF("fx-expt", fxExpt, 2, 2) {
    CHECK_FIXNUM(0, m);
    CHECK_FIXNUM(1, n);
    return FIXNUM(pow(m, n));
}


DEF("fx-lg", fxLg, 1, 1) {
    CHECK_FIXNUM(0, m);
    double logn = log((double)m);
    double log2 = log((double)2.0);
    return FIXNUM((int)ceil(logn / log2));
}


DEF("fx-lgf", fxLgf, 1, 1) {
    CHECK_FIXNUM(0, m);
    double logn = log((double)m);
    double log2 = log((double)2.0);
    return FIXNUM((int)floor(logn / log2));
}


DEF("fx-logand", fxLogand, 0, MaxArgs) {
    int result = ~0;
    for (int next = 0; next < NARGS; next++) {
        CHECK_FIXNUM(next, n);
        result &= n;
    }
    return FIXNUM(result);
}


DEF("fx-logor", fxLogor, 0, MaxArgs) {
    int result = 0;
    for (int next = 0; next < NARGS; next++) {
        CHECK_FIXNUM(next, n);
        result |= n;
    }
    return FIXNUM(result);
}


DEF("fx-logxor", fxLogxor, 0, MaxArgs) {
    int result = 0;
    for (int next = 0; next < NARGS; next++) {
        CHECK_FIXNUM(next, n);
        result = (result & ~n) | (~result & n);
    }
    return FIXNUM(result);
}


DEF("fx-lognot", fxLognot, 1, 1) {
    CHECK_FIXNUM(0, n);
    return FIXNUM(~n);
}


DEF("fx-mdiv", fxMdiv, 0, MaxArgs) {
    int commonbits = ~0;
    for (int next = 0; next < NARGS; next++) {
        CHECK_FIXNUM(next, n);
        commonbits &= n;
    }
    return FIXNUM(commonbits != 0);
}


DEF("fx-cdiv", fxCdiv, 2, 2) {
    CHECK_FIXNUM(0, m);
    CHECK_FIXNUM(1, n);
    int result = (int)ceil((double)m / (double)n);
    if (arithmeticException) {
        arithmeticException = 0;
        return PRIM_ERROR("division exception");
    } else {
        return FIXNUM(result);
    }
}


DEF("fx->fl", fxToFl, 1, 1) {
    CHECK_FIXNUM(0, n);
    return Float::create((Rfloat)n);
}


DEF("fx->ch", fxToChar, 1, 1) {
    CHECK_FIXNUM(0, n);
    return RBLCHAR(n);
}


DEF("fx-asl", fxAsl, 2, 2) {
    CHECK_FIXNUM(0, m);
    CHECK_FIXNUM(1, n);
    return FIXNUM(m << n);
}


DEF("fx-asr", fxAsr, 2, 2) {
    CHECK_FIXNUM(0, m);
    CHECK_FIXNUM(1, n);
    return FIXNUM(m >> n);
}


DEF("fx-lsl", fxLsl, 2, 2) {
    CHECK_FIXNUM(0, m);
    CHECK_FIXNUM(1, n);
    return FIXNUM(m << n);
}


DEF("fx-lsr", fxLsr, 2, 2) {
    CHECK_FIXNUM(0, m);
    CHECK_FIXNUM(1, n);
    u_int32_t k = m;
    return FIXNUM(k >> n);
}


BUILTIN_CLASS(Float) {}


Float::Float(Rfloat v)
    : BinaryOb(sizeof(Float), CLASS_META(Float), CLASS_SBO(Float)), val(v) {
    Float::updateCnt();
}


Float* Float::create(Rfloat v) {
    void* loc = PALLOC(sizeof(Float));
    return new (loc) Float(v);
}


const char* Float::asCstring() {
    sprintf(Ob::stringbuf, Float::format, val);
    return Ob::stringbuf;
}


char Float::format[FloatFormatSize] = "%g";


Ob* checkFlResult(Prim* prim, Ctxt* ctxt, Rfloat answer) {
    if (arithmeticException) {
        arithmeticException = 0;
        return prim->runtimeError(ctxt, "arithmetic exception");
    } else {
        return Float::create(answer);
    }
}


#define FLOATVAL(x) ((Float*)(x))->val


DEF("fl+", flPlus, 0, MaxArgs) {
    short int n = NARGS;
    Rfloat accum = 0.0;

    while (n--) {
        CHECK(n, Float, m);
        accum += FLOATVAL(m);
    }

    return checkFlResult(flPlus, __CTXT__, accum);
}


DEF("fl-", flMinus, 1, 2) {
    Rfloat result = 0;

    switch (NARGS) {
    case 1: {
        CHECK(0, Float, n);
        result = -FLOATVAL(n);
        break;
    }

    case 2: {
        CHECK(0, Float, m);
        CHECK(1, Float, n);
        result = FLOATVAL(m) - FLOATVAL(n);
        break;
    }
    }

    return checkFlResult(flMinus, __CTXT__, result);
}


DEF("fl*", flTimes, 0, MaxArgs) {
    short int n = NARGS;
    Rfloat accum = 1;

    while (n--) {
        CHECK(n, Float, m);
        accum *= FLOATVAL(m);
    }

    return checkFlResult(flTimes, __CTXT__, accum);
}


DEF("fl/", flDiv, 2, 2) {
    CHECK(0, Float, m);
    CHECK(1, Float, n);
    return checkFlResult(flDiv, __CTXT__, FLOATVAL(m) / FLOATVAL(n));
}


DEF("fl<", flLt, 2, 2) {
    CHECK(0, Float, m);
    CHECK(1, Float, n);
    return RBLBOOL(FLOATVAL(m) < FLOATVAL(n));
}


DEF("fl<=", flLe, 2, 2) {
    CHECK(0, Float, m);
    CHECK(1, Float, n);
    return RBLBOOL(FLOATVAL(m) <= FLOATVAL(n));
}


DEF("fl>", flGt, 2, 2) {
    CHECK(0, Float, m);
    CHECK(1, Float, n);
    return RBLBOOL(FLOATVAL(m) > FLOATVAL(n));
}


DEF("fl>=", flGe, 2, 2) {
    CHECK(0, Float, m);
    CHECK(1, Float, n);
    return RBLBOOL(FLOATVAL(m) >= FLOATVAL(n));
}


DEF("fl=", flEq, 2, 2) {
    CHECK(0, Float, m);
    CHECK(1, Float, n);
    return RBLBOOL(FLOATVAL(m) == FLOATVAL(n));
}


DEF("fl!=", flNe, 2, 2) {
    CHECK(0, Float, m);
    CHECK(1, Float, n);
    return RBLBOOL(FLOATVAL(m) != FLOATVAL(n));
}


DEF("fl-min", flMin, 1, MaxArgs) {
    CHECK(0, Float, n);
    Rfloat result = FLOATVAL(n);
    for (int next = 1; next < NARGS; next++) {
        CHECK(next, Float, n);
        Rfloat r = FLOATVAL(n);
        if (r < result) {
            result = r;
        }
    }
    return Float::create(result);
}


DEF("fl-max", flMax, 1, MaxArgs) {
    CHECK(0, Float, n);
    Rfloat result = FLOATVAL(n);
    for (int next = 1; next < NARGS; next++) {
        CHECK(next, Float, n);
        Rfloat r = FLOATVAL(n);
        if (r > result) {
            result = r;
        }
    }
    return Float::create(result);
}


DEF("fl-abs", flAbs, 1, 1) {
    CHECK(0, Float, n);
    return Float::create(fabs(FLOATVAL(n)));
}


DEF("fl-exp", flExp, 1, 1) {
    CHECK(0, Float, n);
    return checkFlResult(flExp, __CTXT__, exp(FLOATVAL(n)));
}


DEF("fl-expt", flExpt, 2, 2) {
    CHECK(0, Float, m);
    CHECK(1, Float, n);
    return checkFlResult(flExpt, __CTXT__, pow(FLOATVAL(m), FLOATVAL(n)));
}


DEF("fl-log", flLog, 1, 1) {
    CHECK(0, Float, n);
    return Float::create(log(FLOATVAL(n)));
}


DEF("fl-log10", flLog10, 1, 1) {
    CHECK(0, Float, n);
    return Float::create(log10(FLOATVAL(n)));
}


DEF("fl-floor", flFloor, 1, 1) {
    CHECK(0, Float, n);
    return Float::create(floor(FLOATVAL(n)));
}


DEF("fl-ceil", flCeil, 1, 1) {
    CHECK(0, Float, n);
    return Float::create(ceil(FLOATVAL(n)));
}


DEF("fl-atan", flAtan, 1, 1) {
    CHECK(0, Float, n);
    return Float::create(atan(FLOATVAL(n)));
}


DEF("fl-cos", flCos, 1, 1) {
    CHECK(0, Float, n);
    return Float::create(cos(FLOATVAL(n)));
}


DEF("fl-sin", flSin, 1, 1) {
    CHECK(0, Float, n);
    return Float::create(sin(FLOATVAL(n)));
}


DEF("fl->fx", flToFx, 1, 1) {
    CHECK(0, Float, n);
    return FIXNUM((int)floor(FLOATVAL(n)));
}


DEF("flFormat:", flFormat, 1, 1) {
    CHECK(0, RBLstring, fmt);
    PROTECT(__CTXT__);
    RBLstring* result = RBLstring::create(Float::format);
    strncpy(Float::format, (char*)&fmt->byte(0), FloatFormatSize - 1);
    Float::format[FloatFormatSize - 1] = '\0';
    return result;
}
