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

#include "Table.h"

#include "Ctxt.h"
#include "Prim.h"
#include "Ob.h"
#include "Tuple.h"

#include "BuiltinClass.h"

#include <memory.h>


BUILTIN_CLASS(RblTable) { OB_FIELD("tbl", RblTable, tbl); };

int RblTable::maxMaxTableSize = 8189;
static const int HashStride = 37;
static const int HashThreshold = 16;
int RblTableDefaultHitFn(pOb key1, pOb key2) { return (key1 == key2); }


RblTable::RblTable(int max, Tuple* tbl)
    : BinaryOb(sizeof(RblTable), CLASS_META(RblTable), CLASS_SBO(RblTable)),
      maxEntries(max),
      numberOfEntries(0),
      numberOfDeletedHashEntries(0),
      gcSensitiveKeys(false),
      registered(false),
      tbl(tbl),
      hitFn(&RblTableDefaultHitFn) {
    assert(max % HashStride != 0);
    if (max > HashThreshold) {
        addFn = &RblTable::hashAdd;
        lookupFn = &RblTable::hashLookup;
        checkSizeFn = &RblTable::hashCheckSize;
    } else {
        addFn = &RblTable::linearAdd;
        lookupFn = &RblTable::linearLookup;
        checkSizeFn = &RblTable::linearCheckSize;
    }
    RblTable::updateCnt();
}


RblTable* RblTable::create(int max) {
    Tuple* tmp = Tuple::create(max * (sizeof(Entry) / sizeof(pOb)), ABSENT);
    void* loc = PALLOC1(sizeof(RblTable), tmp);
    return new (loc) RblTable(max, tmp);
}

RblTable* RblTable::create(Tuple * tup) {
    void* loc = PALLOC1(sizeof(RblTable), tup);
    return new (loc) RblTable(SIZE(tup), tup);
}

RblTable::RblTable(int max, Tuple* tbl, RblTableHitFn rtabhfn)
    : BinaryOb(sizeof(RblTable), CLASS_META(RblTable), CLASS_SBO(RblTable)),
      maxEntries(max),
      numberOfEntries(0),
      numberOfDeletedHashEntries(0),
      gcSensitiveKeys(false),
      registered(false),
      tbl(tbl),
      hitFn(rtabhfn) {
    assert(max % HashStride != 0);
    if (max > HashThreshold) {
        addFn = &RblTable::hashAdd;
        lookupFn = &RblTable::hashLookup;
        checkSizeFn = &RblTable::hashCheckSize;
    } else {
        addFn = &RblTable::linearAdd;
        lookupFn = &RblTable::linearLookup;
        checkSizeFn = &RblTable::linearCheckSize;
    }

    RblTable::updateCnt();
}


RblTable* RblTable::create(RblTableHitFn rtabhfn, int max) {
    Tuple* tmp = Tuple::create(max * (sizeof(Entry) / sizeof(pOb)), ABSENT);
    void* loc = PALLOC1(sizeof(RblTable), tmp);
    return new (loc) RblTable(max, tmp, rtabhfn);
}

RblTable::Entry& RblTable::entry(int n) {
    Entry* p = (Entry*)&tbl->elem(0);
    return p[n];
}


void RblTable::linearResize() {
    /*
     * This routine copies the entire contents of the current table into
     * the front half of the new (twice as large) table, so that it can
     * be used by hashResize as well.  If it only copied the first
     * numberOfEntries entries, it might will omit some entries in a
     * hashified table.
     */
    PROTECT_THIS(RblTable);
    int newSz = 2 * maxEntries;

    if (newSz > RblTable::maxMaxTableSize) {
        newSz = RblTable::maxMaxTableSize;
    }

    Tuple* newTbl = Tuple::create(newSz * 2, ABSENT);
    memcpy(&newTbl->elem(0), &SELF->entry(0), maxEntries * sizeof(Entry));

    SELF->maxEntries = newSz;  // Don't do this until no GCs can occur.
    ASSIGN(SELF, tbl, newTbl);
}

void RblTable::hashResize() {
    PROTECT_THIS(RblTable);
    SELF->linearResize();
    SELF->rehashCompletely();
}


/*
 * The rehashing procedures use recursion and the C++ runtime stack for
 * temporary storage of the table's keys and values while recomputing
 * hash locations.  Hashify is used to convert a linear lookup table into
 * a hash lookup table.  To do so, it needs to temporarily remove all
 * entries from the table and then hash them into their proper locations.
 * RehashCompletely is called after a scavenge (if there are any
 * gcSensitive keys) or after growing a table.  In the case of a
 * scavenge, any pointer-valued key that changes because of the scavenge
 * must be rehashed into the table; a complication arises from the fact
 * that the scavenger might change a key from sensitive to insensitive
 * (i.e., it moves the object that the key points to from new space to
 * old space) without the table's ever knowing it.  Consequently, it is
 * not sufficient to merely look for gcSensitive keys in the fixup phase
 * -- we have to rehash the table completely.  Furthermore, because a
 * key's location in a table is a function of the table size, we need to
 * recompute all of the locations after growing.
 */


int RblTable::hash(pOb key) { return ((int)PTR(key) >> TagSize); }


void RblTable::hashify() {
    addFn = &RblTable::hashAdd;
    lookupFn = &RblTable::hashLookup;
    checkSizeFn = &RblTable::hashCheckSize;
    rehashCompletely();
}


void RblTable::rehashCompletely() {
    assert(SIZE(tbl) == sizeof(Tuple) + maxEntries * sizeof(Entry));
    gcSensitiveKeys = false;
    numberOfEntries = 0;
    numberOfDeletedHashEntries = 0;
    rehashCompletelyFrom(0);
}


void RblTable::rehashCompletelyFrom(int n) {
    for (; n < maxEntries; n++) {
        /*
         * Put the key and value in stack-allocated automatics for
         * safekeeping while we reorganize the table.
         */
        pOb key = entry(n).key;
        pOb val = entry(n).val;

        entry(n).key = ABSENT;
        entry(n).val = ABSENT;

        if (key != ABSENT && val != ABSENT) {
            rehashCompletelyFrom(n + 1);
            addEntry(key, val);
            return;
        }
    }
}


RblTable::Entry* RblTable::linearLookup(pOb key) {
    for (int i = 0; i < maxEntries; i++) {
        if ((*hitFn)(entry(i).key, key)) {
            return (entry(i).val == ABSENT ? 0 : &entry(i));
        }
    }

    return 0;
}


RblTable::Entry* RblTable::hashLookup(pOb key) {
    int numberOfProbes = maxEntries;
    int probe = hash(key) % maxEntries;

    while (numberOfProbes--) {
        Entry* p = &entry(probe);
        if ((*hitFn)(p->key, key)) {
            return p;
        } else if (p->key != ABSENT) {
            probe = (probe + HashStride) % maxEntries;
        } else {
            break;
        }
    }

    return 0;
}


void RblTable::linearAdd(pOb key, pOb val) {
    for (int i = 0; i < numberOfEntries; i++) {
        Entry* p = &entry(i);
        if ((*hitFn)(p->key, key)) {
            if (val == ABSENT) {
                /*
                 * Delete this entry by compacting the table.  This
                 * maintains the invariant that a linear table will never
                 * have a key with an associated ABSENT value.  (That
                 * invariant is *not* maintained by hash tables.)
                 */
                for (int j = i + 1; j < numberOfEntries; j++) {
                    entry(j - 1) = entry(j);
                }

                numberOfEntries--;
                entry(numberOfEntries).key = ABSENT;
                entry(numberOfEntries).val = ABSENT;
                return;
            } else {
                tbl->checkStore(p->val = val);
                return;
            }
        }
    }

    if (val == ABSENT) {
        return;
    }

    tbl->checkStore(entry(numberOfEntries).key = key);
    tbl->checkStore(entry(numberOfEntries).val = val);
    numberOfEntries++;
}


// See https://en.wikipedia.org/wiki/Linear_probing
void RblTable::hashAdd(pOb key, pOb val) {
    int numberOfProbes = maxEntries;
    int probe = hash(key) % maxEntries;

    for (; numberOfProbes--; probe = (probe + HashStride) % maxEntries) {
        Entry* p = &entry(probe);
        if (p->key != ABSENT && !((*hitFn)(p->key, key))) {
            continue;
        }

        if (val == ABSENT && p->val != ABSENT) {
            p->val = ABSENT;
            numberOfEntries--;
            numberOfDeletedHashEntries++;
            return;
        }

        if (!gcSensitiveKeys && IS_PTR(key) && key->gcSensitive()) {
            gcSensitiveKeys = true;
            if (!registered) {
                registered = true;
                heap->registerGCAgenda(this);
            }
        }

        numberOfEntries += (p->key == ABSENT);
        tbl->checkStore(p->key = key);
        tbl->checkStore(p->val = val);
        return;
    }

    /*
     * We should never get here, because the tables should get resized
     * before they ever get full.
     */

    suicide("%s::hashAdd -- out of room in hash table", typestring());
}


void RblTable::linearCheckSize() {
    if (numberOfEntries >= HashThreshold) {
        hashify();
        hashCheckSize();
    } else if (numberOfEntries >= maxEntries) {
        linearResize();
    }
}

// We need to keep track of the number of deleted entries separately
// because until the table is rehashed, they still occupy space in the hash
// table.
void RblTable::hashCheckSize() {
    if (4 * (numberOfEntries + numberOfDeletedHashEntries) >
        3 * maxEntries) {  // i.e., the table is
        // 75% full
        hashResize();
    }
}


void RblTable::addEntry(pOb key, pOb val) { (this->*addFn)(key, val); }


int RblTable::traversePtrs(PSOb__PSOb f) {
    return BinaryOb::traversePtrs(f) + useIfPtr(&tbl, f);
}


int RblTable::traversePtrs(SI__PSOb f) {
    return BinaryOb::traversePtrs(f) + useIfPtr(tbl, f);
}


void RblTable::traversePtrs(V__PSOb f) {
    BinaryOb::traversePtrs(f);
    useIfPtr(tbl, f);
}


bool RblTable::gcFixup() {
    if (gcSensitiveKeys) {
        return true;
    } else {
        return (registered = false);
    }
}


bool RblTable::scavengeFixup() {
    rehashCompletely();
    if (gcSensitiveKeys) {
        return true;
    } else {
        return (registered = false);
    }
}


pOb RblTable::cloneTo(pOb new_meta, pOb new_parent) {
    PROTECT_THIS(RblTable);
    Tuple* new_tbl = (Tuple*)tbl->clone();
    PROTECT(new_tbl);
    RblTable* new_table = (RblTable*)SELF->Ob::cloneTo(new_meta, new_parent);
    new_table->tbl = new_tbl;
    if (new_table->gcSensitiveKeys) {
        heap->registerGCAgenda(new_table);
    }

    return new_table;
}


Tuple* RblTable::dumpKeys() {
    PROTECT_THIS(RblTable);
    Tuple* result = Tuple::create(numberOfEntries, NIV);

    if (SELF->addFn == &RblTable::linearAdd) {
        for (int i = numberOfEntries; i--;) {
            result->elem(i) = SELF->entry(i).key;
        }
    } else {
        int i = 0;
        for (int j = maxEntries; j--;) {
            pOb key = SELF->entry(j).key;
            pOb val = SELF->entry(j).val;
            if (key != ABSENT && val != ABSENT) {
                result->elem(i++) = key;
            }
        }

        assert(i == SELF->numberOfEntries);
    }

    return result;
}


Tuple* RblTable::dumpPairs() {
    PROTECT_THIS(RblTable);
    Tuple* result = Tuple::create(2 * numberOfEntries, NIV);

    if (SELF->addFn == &RblTable::linearAdd) {
        memcpy(&result->elem(0), &SELF->entry(0),
               SELF->numberOfEntries * sizeof(Entry));
    } else {
        int i = 0;
        for (int j = maxEntries; j--;) {
            pOb key = SELF->entry(j).key;
            pOb val = SELF->entry(j).val;
            if (key != ABSENT && val != ABSENT) {
                result->elem(i++) = key;
                result->elem(i++) = val;
            }
        }
        assert(i == 2 * SELF->numberOfEntries);
    }

    return result;
}


int RblTable::nPairs() { return numberOfEntries; }


pOb RblTable::getKey(pOb key) {
    Entry* keyloc = (this->*lookupFn)(key);
    return (keyloc == 0 ? ABSENT : keyloc->val);
}


pOb RblTable::addKey(pOb key, pOb val) {
    PROTECT_THIS(RblTable);
    PROTECT(key);
    PROTECT(val);
    if (numberOfEntries >= RblTable::maxMaxTableSize) {
        return DEADTHREAD;
    } else {
        (SELF->*checkSizeFn)();
        SELF->addEntry(key, val);
        return key;
    }
}


DEF("tbl-add", tblAdd, 3, 3) {
    CHECK(0, RblTable, tbl);
    pOb result = tbl->addKey(ARG(1), ARG(2));
    if (result == DEADTHREAD) {
        return tbl->runtimeError(__CTXT__, "RblTable max size reached.");
    } else {
        return result;
    }
}


DEF("tbl-get", tblGet, 2, 2) {
    CHECK(0, RblTable, tbl);
    return tbl->getKey(ARG(1));
}


DEF("tbl-del", tblDel, 2, 2) {
    CHECK(0, RblTable, tbl);
    return tbl->addKey(ARG(1), ABSENT);
}
