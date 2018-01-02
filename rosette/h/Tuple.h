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

#if !defined(_RBL_Tuple_h)
#define _RBL_Tuple_h

#include "rosette.h"
#include "Ob.h"

class Tuple : public Ob {
    STD_DECLS(Tuple);

   protected:
    Tuple(int, Ob*);
    Tuple(Ob**, int);
    Tuple(int, Tuple*, int, int, Ob* = INVALID);
    Tuple(int, int, Tuple*);
    Tuple(Tuple*, Tuple*);
    Tuple(Tuple*, int, Tuple*);
    Tuple(Tuple*);

   public:
    static Tuple* create();
    static Tuple* create(int, Ob*);
    static Tuple* create(Ob**, int);
    static Tuple* create(int, Tuple*, int, int, Ob* = INVALID);
    static Tuple* create(int, int, Tuple*);
    static Tuple* create(Tuple*, Tuple*);
    static Tuple* create(Tuple*, int);
    static Tuple* create(Tuple*);

    StdExtension* becomeExtension(Ob*, Ob*);

    Tuple* makeSlice(int, int);
    Tuple* makeTail(int);
    Ob* cloneTo(Ob*, Ob*);
    Ob* indexedSize();
    Ob* nth(int);
    Ob* setNth(int, Ob*);
    Ob* subObject(int, int);
    bool accepts(Ctxt*);
    bool matches(Ctxt*);
    bool matches(Tuple*);

    bool typeMatcher(Tuple*, pOb);
    bool elemsCoveredByp(pOb, int = 0);

    Ob*& elem(int n) {
        return _slot[n + 2];  // Skip the meta and parent fields.
    }

    int numberOfElements() {
        return ((SIZE(this) - sizeof(Tuple)) / sizeof(Ob*));
    }
};


extern Tuple* cons(Ob*, Tuple*);
extern Tuple* consstar(Tuple*, int, Tuple*);
extern Tuple* rcons(Tuple*, Ob*);
extern Tuple* concat(Tuple*, Tuple*);

#endif
