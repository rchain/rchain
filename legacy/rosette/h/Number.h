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

#if !defined(_RBL_Numbers_h)
#define _RBL_Numbers_h

#include "rosette.h"
#include "BinaryOb.h"

#ifdef USE_SHORT_FLOAT
typedef float Rfloat;
#else
typedef double Rfloat;
#endif


static const int FloatFormatSize = 16;

class Float : public BinaryOb {
    STD_DECLS(Float);

   protected:
    static char format[FloatFormatSize];
    friend BUILTIN_PRIM(flFormat);

    Float(Rfloat);

    Ob* checkResult(Prim*, Ctxt*, Rfloat);

   public:
    Rfloat val;

    static Float* create(Rfloat);
    const char* asCstring();
};

#endif
