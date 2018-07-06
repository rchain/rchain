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

#if !defined(_RBL_Table_h)
#define _RBL_Table_h

#include "rosette.h"
#include "BinaryOb.h"

static const int DefaultTableSize = 4;

typedef int (*RblTableHitFn)(pOb, pOb);
int RblTableDefaultHitFn(pOb key1, pOb key2);

class RblTable : public BinaryOb {
    STD_DECLS(RblTable);

   protected:
    /*
     * These entries will be overlaid on the elements of a tuple.
     */

    struct Entry {
        pOb key;
        pOb val;
    };

    void (RblTable::*addFn)(pOb, pOb);
    Entry* (RblTable::*lookupFn)(pOb);
    void (RblTable::*checkSizeFn)();

    static int maxMaxTableSize;

    int maxEntries;
    int numberOfEntries;
    int numberOfDeletedHashEntries;
    bool gcSensitiveKeys;
    bool registered;

    RblTable(int, Tuple*);
    RblTable(int, Tuple*, RblTableHitFn);

    Entry& entry(int);

    int hash(pOb);
    void hashify();
    void rehashAfterScavenge();
    void rehashAfterScavengeFrom(int);
    void rehashCompletely();
    void rehashCompletelyFrom(int);

    void hashResize();
    void hashAdd(pOb, pOb);
    Entry* hashLookup(pOb);
    void hashCheckSize();

    void linearResize();
    void linearAdd(pOb, pOb);
    Entry* linearLookup(pOb);
    void linearCheckSize();

    void addEntry(pOb, pOb);

    friend class TblExtension;

   public:
    Tuple* tbl;
    RblTableHitFn hitFn;

    static RblTable* create(RblTableHitFn, int = DefaultTableSize);
    static RblTable* create(int = DefaultTableSize);
    static RblTable* create(Tuple*);

    virtual int traversePtrs(PSOb__PSOb);
    virtual int traversePtrs(SI__PSOb);
    virtual void traversePtrs(V__PSOb);
    virtual bool gcFixup();
    virtual bool scavengeFixup();

    virtual pOb cloneTo(pOb, pOb);
    virtual Tuple* dumpKeys();
    virtual Tuple* dumpPairs();
    virtual int nPairs();
    virtual pOb getKey(pOb key);
    virtual pOb addKey(pOb key, pOb val);
};

#endif
