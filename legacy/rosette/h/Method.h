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

#if !defined(_RBL_Method_h)
#define _RBL_Method_h

#include "rosette.h"
#include "Ob.h"

class StdMthd : public Ob {
    STD_DECLS(StdMthd);

   protected:
    StdMthd(Code*, Ob*, Ob*);
    StdMthd(int, Ob*, Ob*, Code*, Ob*, Ob*);

   public:
    Code* code;
    Ob* id;
    Ob* source;

    static StdMthd* create(Code*, Ob* = Qanon, Ob* = NIV);
    virtual Ob* dispatch(Ctxt*);
    virtual Ob* invoke(Ctxt*);
};


class ReflectiveMthd : public StdMthd {
    STD_DECLS(ReflectiveMthd);

   protected:
    ReflectiveMthd(Code*, Ob* = Qanon, Ob* = NIV);

   public:
    static ReflectiveMthd* create(Code*, Ob* = Qanon, Ob* = NIV);
    virtual Ob* dispatch(Ctxt*);
    virtual Ob* invoke(Ctxt*);
};

#endif
