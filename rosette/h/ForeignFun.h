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

#if !defined(_RBL_Foreign_Fun_h)
#define _RBL_Foreign_Fun_h

#include "rosette.h"
#include "Ob.h"

#define STD_FOREIGN_FUNCTION_STYLE 1
//#define ASM_FOREIGN_FUNCTION_STYLE 1

class AbstractForeignFunction : public Ob {
    STD_DECLS(AbstractForeignFunction);

   protected:
    AbstractForeignFunction(Ob*, Tuple*, Ob*, void*);
    AbstractForeignFunction(Ob*, Tuple*, Ob*, void*, int, Ob*, Ob*);

   public:
    enum ArgConverter {
        AC_FixnumToUnsignedLong = 0,
        AC_FixnumToLong = 1,
        AC_FixnumToInt = 2,
        AC_FixnumToUnsignedShort = 3,
        AC_FixnumToShort = 4,
        AC_FixnumToUnsignedChar = 5,
        AC_FixnumToChar = 6,
        AC_FixnumToFloat = 7,
        AC_FixnumToDouble = 8,
        AC_FloatToFloat = 9,
        AC_FloatToDouble = 10,
        AC_BoolToInt = 11,
        AC_StringToCharStar = 12,
        AC_ByteVecToVoidStar = 13,
        AC_Rosette = 14,
        AC_FixnumToVoidStar = 15,
        nArgConverters = 16
    };

    enum RsltConverter {
        RC_Void = 0,
        RC_UnsignedLong = 1,
        RC_Long = 2,
        RC_Int = 3,
        RC_UnsignedShort = 4,
        RC_Short = 5,
        RC_UnsignedChar = 6,
        RC_Char = 7,
        RC_Float = 8,
        RC_Double = 9,
        RC_CharStar = 10,
        RC_ByteVec = 11,
        RC_Rosette = 12,
        RC_VoidStar = 13,
        nRsltConverters = 14
    };

    Ob* Cname;
    Tuple* argConverters;
    Ob* rsltConverter;
    Ob* Caddr;

    // static AbstractForeignFunction*	create (Ob*, Tuple*, Ob*, void*);
    virtual Ob* dispatch(Ctxt*) = 0;
    virtual Ob* invoke(Ctxt*);
};

#ifdef STD_FOREIGN_FUNCTION_STYLE
#define FF_STYLE STDForeignFunction

typedef long (*Incantation)(...);

class ForeignFunction : public AbstractForeignFunction {
    STD_DECLS(ForeignFunction);

   protected:
    ForeignFunction(Ob* ob1, Tuple* tup, Ob* ob2, void* vd);

   public:
    static ForeignFunction* create(Ob*, Tuple*, Ob*, void*);
    virtual Ob* typecheckActuals(Ctxt*);
    virtual convertArgReturnPair convertActual(Ctxt*, int);
    virtual Ob* convertResult(Ctxt*, long);
    virtual Ob* dispatch(Ctxt*);
    virtual Ob* invoke(Ctxt*);
};

#else
#define FF_STYLE ASMForeignFunction

class ForeignFunction : public AbstractForeignFunction {
    STD_DECLS(ForeignFunction);

   protected:
    ForeignFunction(Ob* ob1, Tuple* tup, Ob* ob2, void* vd);

   public:
    static ForeignFunction* create(Ob*, Tuple*, Ob*, void*);
    virtual Ob* dispatch(Ctxt*);
};

#endif

#endif
