/* Mode: -*- C++ -*- */
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

/*
 * $Header$
 *
 * $Log$
 *
 @EC */

#if !defined(_RBL_Ctxt_h)
#define _RBL_Ctxt_h 1

#ifdef __GNUG__
#pragma interface
#endif

#include "rosette.h"

#include "Ob.h"
#include "Location.h"
#include "Tuple.h"

class Ctxt : public MboxOb {
    STD_DECLS(Ctxt);

   protected:
    int traversePtrs(PSOb__PSOb);
    int traversePtrs(SI__PSOb);
    void traversePtrs(V__PSOb);

    void inlineRcv(Ob*, Location);

    Ctxt(Tuple*, Ctxt*);
    Ctxt(Code*, Tuple*, Ctxt*, int = 0);
    Ctxt(Ob*, Tuple*);
    Ctxt(int, Ob*, Ob*, Ob*, Code*, Tuple*, Ctxt*, Location);

   public:
    Location tag;
    Byte nargs;
    Byte outstanding;
    Word16 pc;

    // Additions to or deletions from the following list must
    // be reflected in the definition of NumberOfCtxtRegs.

    Ob* rslt;          // reg[0]
    Ob* trgt;          // reg[1]
    Tuple* argvec;     // reg[2]
    Ob* env;           // reg[3]
    Code* code;        // reg[4]
    Ctxt* ctxt;        // reg[5]
    Ob* self2;         // reg[6]
    Ob* selfEnv;       // reg[7]
    Ob* rcvr;          // reg[8]
    Monitor* monitor;  // reg[9]

    static Ctxt* create(Tuple*, Ctxt*);
    static Ctxt* create(Code*, Tuple*, Ctxt*, int = 0);
    static Ctxt* create(Ob*, Tuple*);

    pOb& reg(int);  // for indirect access to the registers
    pOb& arg(int);

    virtual bool applyK(Ob*, Location);
    bool rcv(Ob*, Location);
    bool ret(Ob*);
    void scheduleStrand();
    void prepare();
    Ob* missingBindingError(Ob*);
    Ob* vmError();
};

inline pOb& Ctxt::reg(int n) {
    pOb* p = (pOb*)&rslt;
    return p[n];
}

inline pOb& Ctxt::arg(int n) { return argvec->elem(n); }

inline bool Ctxt::ret(Ob* result) {
    return (this->tag != LocLimbo && applyK(result, this->tag));
}


class UpcallCtxt : public Ctxt {
    STD_DECLS(UpcallCtxt);

   protected:
    UpcallCtxt(Code*, Tuple*, Ctxt*, Location);

   public:
    static UpcallCtxt* create(Code*, Tuple*, Ctxt*, Location);

    virtual bool applyK(Ob*, Location);
};

#endif
