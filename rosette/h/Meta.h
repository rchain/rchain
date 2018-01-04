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

#if !defined(_RBL_Meta_h)
#define _RBL_Meta_h

#include "Ob.h"

class IndexedMeta : public StdMeta {
    STD_DECLS(IndexedMeta);

   protected:
    IndexedMeta(StdMeta*, int);

    friend class StdMeta;

   public:
    virtual Tuple* keys(Ob*);
    virtual Location keyLoc(Ob*, pOb = ABSENT);
    virtual Tuple* locContour(Ob*);
};

static const int INDEXEDMETA_START_INDEXED_PART_SLOT = BUILTIN_STDMETA_SLOTS;

static const int BUILTIN_INDEXEDMETA_SLOTS = BUILTIN_STDMETA_SLOTS + 1;

#endif
