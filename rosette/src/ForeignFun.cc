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

#include "ForeignFun.h"
#include "RblAtom.h"
#include "BinaryOb.h"
#include "Dynload.h"
#include "Meta.h"
#include "Number.h"
#include "Prim.h"
#include "RBLstring.h"
#include "Tuple.h"

#include "BuiltinClass.h"

// abstract class

BUILTIN_CLASS(AbstractForeignFunction) {
    OB_FIELD("native-name", AbstractForeignFunction, Cname);
    OB_FIELD("arg-converters", AbstractForeignFunction, argConverters);
    OB_FIELD("rslt-converter", AbstractForeignFunction, rsltConverter);
    OB_FIELD("native-addr", AbstractForeignFunction, Caddr);
}


AbstractForeignFunction::AbstractForeignFunction(Ob* Cname,
                                                 Tuple* argConverters,
                                                 Ob* rsltConverter, void* Caddr)
    : Ob(sizeof(ForeignFunction), CLASS_META(AbstractForeignFunction),
         CLASS_SBO(AbstractForeignFunction)),
      Cname(Cname),
      argConverters(argConverters),
      rsltConverter(rsltConverter),
      Caddr(FIXNUM((int)Caddr)) {
    AbstractForeignFunction::updateCnt();
}

AbstractForeignFunction::AbstractForeignFunction(Ob* Cname,
                                                 Tuple* argConverters,
                                                 Ob* rsltConverter, void* Caddr,
                                                 int s, Ob* m, Ob* p)
    : Ob(s, m, p),
      Cname(Cname),
      argConverters(argConverters),
      rsltConverter(rsltConverter),
      Caddr(FIXNUM((int)Caddr)) {
    AbstractForeignFunction::updateCnt();
}

int FF_ERRNO = 0;

/****************************************************************************/
/***                                                                      ***/
/***                              std interface                           ***/
/***                                                                      ***/
/****************************************************************************/

#ifdef STD_FOREIGN_FUNCTION_STYLE
#define FF_STYLE STDForeignFunction

BUILTIN_CLASS(ForeignFunction) {
    OB_FIELD("native-name", ForeignFunction, Cname);
    OB_FIELD("arg-converters", ForeignFunction, argConverters);
    OB_FIELD("rslt-converter", ForeignFunction, rsltConverter);
    OB_FIELD("native-addr", ForeignFunction, Caddr);
}

ForeignFunction::ForeignFunction(Ob* Cname, Tuple* argConverters,
                                 Ob* rsltConverter, void* Caddr)
    : AbstractForeignFunction(
          Cname, argConverters, rsltConverter, Caddr, sizeof(ForeignFunction),
          CLASS_META(ForeignFunction), CLASS_SBO(ForeignFunction)) {
    // ForeignFunction is already calling it's version of updateCnt.
    // Should we call this???
    ForeignFunction::updateCnt();
}

ForeignFunction* ForeignFunction::create(Ob* Cname, Tuple* argConverters,
                                         Ob* rsltConverter, void* Caddr) {
    void* loc =
        PALLOC3(sizeof(ForeignFunction), Cname, argConverters, rsltConverter);
    return new (loc)
        ForeignFunction(Cname, argConverters, rsltConverter, Caddr);
}

Ob* ForeignFunction::typecheckActuals(Ctxt* ctxt) {
    ForeignFunction* const __PRIM__ = this;
    Ctxt* const __CTXT__ = ctxt;

    Ob* result = RBLTRUE;

    const int n = argConverters->numberOfElements();
    for (int argpos = 0; argpos < n; argpos++) {
        if (RBLTRUE != BASE(argConverters->elem(argpos))->typep(ARG(argpos))) {
            return (PRIM_ERROR("unknown argument type"));
        }
    }
    return result;
}

convertArgReturnPair ForeignFunction::convertActual(Ctxt* ctxt, int argpos) {
    Ctxt* const __CTXT__ = ctxt;
    Ob* argCnv = argConverters->elem(argpos);
    Ob* arg = ARG(argpos);
    return BASE(argCnv)->convertActualArg(ctxt, arg);
}

Ob* ForeignFunction::convertResult(Ctxt* ctxt, long rslt) {
    if (NIV == rsltConverter) {
        return NIV;
    }

    return BASE(rsltConverter)->convertActualRslt(ctxt, rslt);
}

#define CNVARG(i) this->convertActual(ctxt, i)

Ob* ForeignFunction::dispatch(Ctxt* ctxt) {
    if (debugging_level) {
        printf("\tforeign fn %s\n", BASE(Cname)->asCstring());
    }

    /*
     * Check all of the arguments for type conformance, and compute how
     * much space will be required to pass them.  (We also give
     * definitions of
     * "__CTXT__" and "__PRIM__" to keep the CHECK macros
     * happy.)
     */
    PROTECT(ctxt);
    ForeignFunction* const __PRIM__ = this;
    Ctxt* const __CTXT__ = ctxt;
    uint32_t x[32];
    const int n = argConverters->numberOfElements();
    long res;
    Incantation the_real_fn = (Incantation)(FIXVAL(Caddr));

    if (n != NARGS) {
        return PRIM_MISMATCH(n, n);
    }

    Ob* result = NIV;

    {
        int m = NARGS;
        int i;
        for (i = 0; i < m; i++) {
            convertArgReturnPair u;
            u = CNVARG(i);
            if (u.failp) {
                result = runtimeError(ctxt, "type mismatch ");
                ctxt->ret(result);
                return result;
            } else {
                x[i] = u.val;
            }
        }

#define CAS(i) \
    case i:    \
        res = (*the_real_fn)
#define BR break
        switch (m) {
            CAS(0)();
            BR;
            CAS(1)(x[0]);
            BR;
            CAS(2)(x[0], x[1]);
            BR;
            CAS(3)(x[0], x[1], x[2]);
            BR;
            CAS(4)(x[0], x[1], x[2], x[3]);
            BR;
            CAS(5)(x[0], x[1], x[2], x[3], x[4]);
            BR;
            CAS(6)(x[0], x[1], x[2], x[3], x[4], x[5]);
            BR;
            CAS(7)(x[0], x[1], x[2], x[3], x[4], x[5], x[6]);
            BR;
            CAS(8)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7]);
            BR;
            CAS(9)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8]);
            BR;
            CAS(10)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9]);
            BR;
            CAS(11)
            (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10]);
            BR;
            CAS(12)
            (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
             x[11]);
            BR;
            CAS(13)
            (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
             x[11], x[12]);
            BR;
            CAS(14)
            (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
             x[11], x[12], x[13]);
            BR;
            CAS(15)
            (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
             x[11], x[12], x[13], x[14]);
            BR;
            CAS(16)
            (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
             x[11], x[12], x[13], x[14], x[15]);
            BR;
            CAS(17)
            (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
             x[11], x[12], x[13], x[14], x[15], x[16]);
            BR;
            CAS(18)
            (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
             x[11], x[12], x[13], x[14], x[15], x[16], x[17]);
            BR;
            CAS(19)
            (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
             x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18]);
            BR;
            CAS(20)
            (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
             x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19]);
            BR;
            CAS(21)
            (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
             x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19],
             x[20]);
            BR;
            CAS(22)
            (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
             x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19],
             x[20], x[21]);
            BR;
            CAS(23)
            (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
             x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19],
             x[20], x[21], x[22]);
            BR;
            CAS(24)
            (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
             x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19],
             x[20], x[21], x[22], x[23]);
            BR;
            CAS(25)
            (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
             x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19],
             x[20], x[21], x[22], x[23], x[24]);
            BR;
            CAS(26)
            (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
             x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19],
             x[20], x[21], x[22], x[23], x[24], x[25]);
            BR;
            CAS(27)
            (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
             x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19],
             x[20], x[21], x[22], x[23], x[24], x[25], x[26]);
            BR;
            CAS(28)
            (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
             x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19],
             x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27]);
            BR;
            CAS(29)
            (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
             x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19],
             x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28]);
            BR;
            CAS(30)
            (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
             x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19],
             x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28],
             x[29]);
            BR;
            CAS(31)
            (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
             x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19],
             x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28],
             x[29], x[30]);
            BR;
            CAS(32)
            (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
             x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19],
             x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28],
             x[29], x[30], x[31]);
            BR;
        default: {
            runtimeError(ctxt, "Exceeded arg limit");
            ctxt->ret(result);
            return result;
        }
        }
        result = convertResult(ctxt, res);
        ctxt->ret(result);
        return result;
    }
}

/****************************************************************************/
/***                                                                      ***/
/***                              sun interface                           ***/
/***                                                                      ***/
/****************************************************************************/

#else
#define FF_STYLE ASMForeignFunction

#if defined(mc68020)
typedef unsigned long LONG_ARG;
typedef unsigned long INT_ARG;
typedef unsigned long SHORT_ARG;
typedef unsigned long CHAR_ARG;
typedef float FLOAT_ARG;
typedef double DOUBLE_ARG;
typedef void* PTR_ARG;
#elif defined(sparc)
typedef unsigned long LONG_ARG;
typedef unsigned long INT_ARG;
typedef unsigned long SHORT_ARG;
typedef unsigned long CHAR_ARG;
typedef float FLOAT_ARG;
typedef double DOUBLE_ARG;
typedef void* PTR_ARG;
#endif

typedef LONG_ARG (*single_fun)(...);
typedef DOUBLE_ARG (*double_fun)(...);

BUILTIN_CLASS(ForeignFunction) {
    OB_FIELD("native-name", ForeignFunction, Cname);
    OB_FIELD("arg-converters", ForeignFunction, argConverters);
    OB_FIELD("rslt-converter", ForeignFunction, rsltConverter);
    OB_FIELD("native-addr", ForeignFunction, Caddr);
}

ForeignFunction::ForeignFunction(Ob* Cname, Tuple* argConverters,
                                 Ob* rsltConverter, void* Caddr)
    : AbstractForeignFunction(
          Cname, argConverters, rsltConverter, Caddr, sizeof(ForeignFunction),
          CLASS_META(ForeignFunction), CLASS_SBO(ForeignFunction)) {
    // ForeignFunction is already calling it's version of updateCnt.
    // Should we call this???
    ForeignFunction::updateCnt();
}

ForeignFunction* ForeignFunction::create(Ob* Cname, Tuple* argConverters,
                                         Ob* rsltConverter, void* Caddr) {
    void* loc =
        PALLOC3(sizeof(ForeignFunction), Cname, argConverters, rsltConverter);
    return new (loc)
        ForeignFunction(Cname, argConverters, rsltConverter, Caddr);
}

#if defined(sun) && (defined(mc68020) || defined(sparc))
Ob* ForeignFunction::dispatch(Ctxt* ctxt) {
    /*
     * This code is rife with assumptions about data representation and
     * procedure
     * calling conventions.  In particular, it is assumed that
     *
     * 1. A long is sufficient to hold all builtin types (other than double),
     * including pointer to function.
     *
     * 2. A union of two longs exactly covers a double, and that passing the
     * first
     * long and then the second long passes the bits in the same order as if the
     * double where passed directly.
     *
     * 3. On Sun4's, functions returning 32-bit results do return the result in
     * %o0,
     * while functions returning double results do so in %f0. (These assumptions
     * are actually made in the code in ff-helper.s.)  On Sun3's, single results
     * are assumed to be returned in d0, while doubles are returned in f0.
     *
     * The code to deal with foreign functions that return structs (not pointers
     * to
     * structs, but structs themselves) hasn't been implemented.  The Sun4
     * architecture specifies some funky protocol that requires the caller to
     * pass a hidden parameter to the area in which to write the result, and to
     * add an unimplemented instruction (following the call instruction) that
     * specifies the size of that area.  This seems to make it impossible to
     * write a generic invocation routine without resorting to self-modifying
     * code (since the size information encoded in the unimplemented instruction
     * could vary with each call.)  The whole arrangement seems to be
     * sufficiently infrequent that we have chosen to ignore it for the time
     * being.
     *
     *
     *
     * These typedefs indicate the kind of bucket that arguments of various
     * types
     * are passed in.  For example, on Sun3's characters are passed as unsigned
     * short integers, while on Sun4's they are passed as unsigned longs.
     *
     * Argsize[type] indicates the number of sizeof() units required to pass an
     * argument of type "type".
     */

    static unsigned char argsize[nArgConverters] = {
        sizeof(LONG_ARG),   /* AC_FixnumToUnsignedLong   */
        sizeof(LONG_ARG),   /* AC_FixnumToLong	     */
        sizeof(INT_ARG),    /* AC_FixnumToInt	     */
        sizeof(SHORT_ARG),  /* AC_FixnumToUnsignedShort  */
        sizeof(SHORT_ARG),  /* AC_FixnumToShort	     */
        sizeof(CHAR_ARG),   /* AC_FixnumToUnsignedChar   */
        sizeof(CHAR_ARG),   /* AC_FixnumToChar	     */
        sizeof(FLOAT_ARG),  /* AC_FixnumToFloat	     */
        sizeof(DOUBLE_ARG), /* AC_FixnumToDouble	     */
        sizeof(FLOAT_ARG),  /* AC_FloatToFloat	     */
        sizeof(DOUBLE_ARG), /* AC_FloatToDouble	     */
        sizeof(INT_ARG),    /* AC_BoolToInt		     */
        sizeof(PTR_ARG),    /* AC_StringToCharStar	     */
        sizeof(PTR_ARG),    /* AC_ByteVecToVoidStar	     */
        sizeof(PTR_ARG),    /* AC_Rosette		     */
        sizeof(PTR_ARG),    /* AC_FixnumToVoidStar	     */
    };

    if (debugging_level) {
        printf("\tforeign fn %s\n", BASE(Cname)->asCstring());
    }

    /**
     * Check all of the arguments for type conformance, and compute how much
     * space will be required to pass them.  (We also give definitions of
     * "__CTXT__" and "__PRIM__" to keep the CHECK macros happy.)
     */

    ForeignFunction* const __PRIM__ = this;
    Ctxt* const __CTXT__ = ctxt;

    const int n = argConverters->numberOfElements();
    int i = 0;
    int nChars = 0;

    if (n != NARGS) {
        return PRIM_MISMATCH(n, n);
    }

    for (i = 0; i < n; i++) {
        switch ((ArgConverter)FIXVAL(argConverters->elem(i))) {
        case AC_FixnumToUnsignedLong:
        case AC_FixnumToLong:
        case AC_FixnumToInt:
        case AC_FixnumToUnsignedShort:
        case AC_FixnumToShort:
        case AC_FixnumToUnsignedChar:
        case AC_FixnumToChar:
        case AC_FixnumToFloat:
        case AC_FixnumToDouble:
        case AC_FixnumToVoidStar:
            CHECK_FIXNUM(i, dummyint);
            break;
        case AC_FloatToFloat:
        case AC_FloatToDouble:
            CHECK(i, Float, dummyfloat);
            break;
        case AC_BoolToInt:
            CHECK(i, RblBool, dummyBool);
            break;
        case AC_StringToCharStar:
            CHECK(i, RBLstring, dummystr);
            break;
        case AC_ByteVecToVoidStar:
            /* pray a lot */
            break;
        case AC_Rosette:
            break;
        case nArgConverters:
        default:
            return runtimeError(ctxt, "unknown argument type");
        }
        nChars += argsize[FIXVAL(argConverters->elem(i))];
    }

    static char* marshallingArea = 0;
    static int marshallingSize = 0;

    if (marshallingSize < nChars) {
        if (marshallingArea) {
            free(marshallingArea);
        }

        marshallingArea = new char[nChars];
        marshallingSize = nChars;
    }

    /**
     * By now, all of the arguments are known to conform, and we know how
     * much space each argument requires, so we lay them all down in the
     * marshalling area.
     */

    char* argp = marshallingArea;

    for (i = 0; i < n; i++) {
        switch ((ArgConverter)FIXVAL(argConverters->elem(i))) {
        case AC_FixnumToUnsignedLong: {
            LONG_ARG* p = (LONG_ARG*)argp;
            *p = (LONG_ARG)(unsigned long)FIXVAL(ARG(i));
            break;
        }
        case AC_FixnumToLong: {
            LONG_ARG* p = (LONG_ARG*)argp;
            *p = (LONG_ARG)(long)FIXVAL(ARG(i));
            break;
        }
        case AC_FixnumToInt: {
            INT_ARG* p = (INT_ARG*)argp;
            *p = (INT_ARG)(int)FIXVAL(ARG(i));
            break;
        }
        case AC_FixnumToUnsignedShort: {
            SHORT_ARG* p = (SHORT_ARG*)argp;
            *p = (SHORT_ARG)(unsigned short)FIXVAL(ARG(i));
            break;
        }
        case AC_FixnumToShort: {
            SHORT_ARG* p = (SHORT_ARG*)argp;
            *p = (SHORT_ARG)(short)FIXVAL(ARG(i));
            break;
        }
        case AC_FixnumToUnsignedChar: {
            CHAR_ARG* p = (CHAR_ARG*)argp;
            *p = (CHAR_ARG)(unsigned char)FIXVAL(ARG(i));
            break;
        }
        case AC_FixnumToChar: {
            CHAR_ARG* p = (CHAR_ARG*)argp;
            *p = (CHAR_ARG)(char)FIXVAL(ARG(i));
            break;
        }
        case AC_FixnumToFloat: {
            FLOAT_ARG* p = (FLOAT_ARG*)argp;
            *p = (FLOAT_ARG)(float)FIXVAL(ARG(i));
            break;
        }
        case AC_FixnumToDouble: {
            /* Watch out for alignment problems here. */
            LONG_ARG* p = (LONG_ARG*)argp;
            union {
                double f;
                LONG_ARG ul[2];
            } tmp;
            tmp.f = (double)FIXVAL(ARG(i));
            p[0] = tmp.ul[0];
            p[1] = tmp.ul[1];
            break;
        }
        case AC_FloatToFloat: {
            FLOAT_ARG* p = (FLOAT_ARG*)argp;
            *p = (FLOAT_ARG)(float)((Float*)ARG(i))->val;
            break;
        }
        case AC_FloatToDouble: {
            LONG_ARG* p = (LONG_ARG*)argp;
            union {
                double f;
                LONG_ARG ul[2];
            } tmp;
            tmp.f = (double)((Float*)ARG(i))->val;
            p[0] = tmp.ul[0];
            p[1] = tmp.ul[1];
            break;
        }
        case AC_BoolToInt: {
            INT_ARG* p = (INT_ARG*)argp;
            *p = (INT_ARG)(int)BOOLVAL(ARG(i));
            break;
        }
        case AC_StringToCharStar: {
            PTR_ARG* p = (PTR_ARG*)argp;
            *p = (PTR_ARG) & ((RBLstring*)ARG(i))->byte(0);
            break;
        }
        case AC_ByteVecToVoidStar: {
            PTR_ARG* p = (PTR_ARG*)argp;
            *p = (PTR_ARG) & ((ByteVec*)ARG(i))->byte(0);
            break;
        }
        case AC_Rosette: {
            PTR_ARG* p = (PTR_ARG*)argp;
            *p = (PTR_ARG)ARG(i);
            break;
        }
        case AC_FixnumToVoidStar: {
            PTR_ARG* p = (PTR_ARG*)argp;
            *p = (PTR_ARG)(void*)FIXVAL(ARG(i));
            break;
        }
        case nArgConverters:
            /*
             * To silence the compiler while still allowing it to make
             * sure that we have covered all cases.
             */
            break;
        }

        argp += argsize[FIXVAL(argConverters->elem(i))];
    }

    Ob* result = NIV;

    if ((RsltConverter)FIXVAL(rsltConverter) == RC_Double) {
        extern double ff_double_helper(double_fun, char*, int);
        double_fun fn = (double_fun)FIXVAL(Caddr);
        result = Float::create(ff_double_helper(fn, marshallingArea, nChars));
    } else {
        extern long ff_single_helper(single_fun, char*, int);
        single_fun fn = (single_fun)FIXVAL(Caddr);
        LONG_ARG rslt = ff_single_helper(fn, marshallingArea, nChars);

        switch ((RsltConverter)FIXVAL(rsltConverter)) {
        case RC_Void:
            result = NIV;
            break;

        case RC_UnsignedLong:
        case RC_Long:
        case RC_Int:
        case RC_UnsignedShort:
        case RC_Short:
        case RC_UnsignedChar:
        case RC_Char:
            result = FIXNUM(rslt);
            break;

        case RC_Float:
            result = Float::create((double)rslt);
            break;

        case RC_Double:
            suicide("unreachable case in ForeignFunction::dispatch");
            break;

        case RC_CharStar:
            result = RBLstring::create((char*)rslt);
            break;

        case RC_Rosette:
            result = (Ob*)rslt;
            break;

        case RC_VoidStar:
            if (rslt & 0xc0000000L) {
                return PRIM_ERROR("unrepresentable address");
            } else {
                result = FIXNUM(rslt);
            }
            break;

        case RC_ByteVec:
        case nRsltConverters:
        default:
            return PRIM_ERROR("unknown result type");
        }
    }

    FF_ERRNO = errno;
    ctxt->ret(result);
    return result;
}

#else
UNIMPLEMENTED(Ob*, ForeignFunction, dispatch, (Ctxt * ctxt));
#endif
#endif

Ob* AbstractForeignFunction::invoke(Ctxt* ctxt) { return dispatch(ctxt); }

Ob* ForeignFunction::invoke(Ctxt* ctxt) { return dispatch(ctxt); }


DEF("unix-load", unixLoad, 1, 3) {
    const char* libStr = "";
    const char* otherStr = "";
    const char* path = BASE(ARG(0))->asPathname();

    if (!path) {
        return PRIM_MISMATCH(0, "String or Symbol");
    }

    switch (NARGS) {
    case 3:
        otherStr = BASE(ARG(2))->asPathname();
        if (!otherStr)
            return PRIM_MISMATCH(2, "String or Symbol");

    case 2:
        libStr = BASE(ARG(1))->asPathname();
        if (!libStr)
            return PRIM_MISMATCH(1, "String");
    }

    return NIV;
}


DEF("wizard-load", unixWizardLoad, 1, 1) {
    /*
     * DON'T USE THIS UNLESS YOU REALLY KNOW WHAT YOU ARE DOING.
     *
     * This takes an sprintf-style string that will be passed to the load
     * mechanism unaltered.  That mechanism expects to find three sprintf
     * directives in the string: a %s field into which it will put the
     * name of the current relocation file, a %lx field into which it
     * will put the computed load point, and a %s field into which it
     * will put the name of the output file that the loader produces.
     * Other than that, you can make the string say anything you want.
     * This is useful for experimenting with other loaders, strange
     * library ordering problems, etc.
     */

    const char* cmd = BASE(ARG(0))->asPathname();
    if (!cmd) {
        return PRIM_MISMATCH(0, "String or Symbol");
    }

    return NIV;
}


DEF("unix-resolve", unixResolve, 1, 1) {
    const char* name = BASE(ARG(0))->asPathname();
    if (!name) {
        return PRIM_MISMATCH(0, "String or Symbol");
    }
    // NB(leaf): No dynamic loading support.
    return ABSENT;
}


DEF("ff-new", ffNew, 3, 3) {
    CHECK_SYM_NOVAR(0);
    CHECK_NOVAR(1, Tuple);

    char buf[BUFSIZ];
    // NB(leaf): No dynamic loading support.
    return PRIM_ERROR(buf);
}

DEF("ff-create", ffCreate, 4, 4) {
    CHECK_SYM(0, Cname);
    CHECK_FIXNUM(1, addr);
    CHECK(2, Tuple, argConverters);

    return ForeignFunction::create(Cname, argConverters, ARG(3), (void*)addr);
}
