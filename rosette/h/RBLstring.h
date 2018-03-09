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

#if !defined(_RBL_String_h)
#define _RBL_String_h

#include "rosette.h"
#include "BinaryOb.h"

class RBLstring : public ByteVec {
    STD_DECLS(RBLstring);

   public:
    RBLstring(int, char = ' ');
    RBLstring(int, char*);

    static RBLstring* create(int, char = ' ');
    static RBLstring* create(char*);
    static RBLstring* create(int, char*);

    void printOn(FILE*);
    void displayOn(FILE*);
    const char* asCstring();
    char* asPathname();
    Ob* indexedSize();
    Ob* nth(int);
    Ob* setNth(int, Ob*);
    Ob* subObject(int, int);

    virtual convertArgReturnPair convertActualArg(Ctxt*, Ob*);
    virtual Ob* convertActualRslt(Ctxt*, uint32_t);
};

#endif
