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

#if !defined(_RBL_Stack_h)
#define _RBL_Stack_h

#ifdef __GNUG__
#pragma interface
#endif

#include "rosette.h"

#include "Ob.h"

class RblStack : public Ob {
    STD_DECLS(RblStack);

   protected:
    RblStack(Tuple*);

   public:
    Ob* nElems;
    Tuple* elems;

    static RblStack* create();

    Ob* cloneTo(Ob*, Ob*);
    int depth();
    bool isEmpty();
    void push(Ob*);
    Ob* pop();
    Ob* top();
    void reset();
    Ob* indexedSize();
    Ob* nth(int);
    Ob* setNth(int, Ob*);
    Ob* subObject(int, int);
};


inline int RblStack::depth() { return FIXVAL(nElems); }
inline bool RblStack::isEmpty() { return nElems == FIXNUM(0); }

#endif
