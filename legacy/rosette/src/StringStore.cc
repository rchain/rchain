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

#include "rosette.h"
#include <string.h>


static inline unsigned long max(unsigned long m, unsigned long n) {
    return (m > n) ? m : n;
}


static unsigned long hash(const char* key) {
    /*
     * This is the hashpjw function described in Aho, Sethi, and Ullman
     * (Red Dragon) on p. 436.
     */
    const char* p;
    unsigned h = 0, g;
    for (p = key; *p != '\0'; p++) {
        h = (h << 4) + *p;
        g = h & 0xf0000000;
        if (0 != g) {
            h = h ^ (g >> 24);
            h = h ^ g;
        }
    }
    return h;
}


/*
 * This approach to symbol table (string table, really) management uses a
 * collection of large character buffers to hold the actual strings.  The
 * strings are deposited in the character buffer on 4-byte boundaries,
 * and are pointed to by Bucket nodes.  The RosetteStringTable itself
 * consists of an array of Bucket chains, indexed by the hash value of
 * candidate strings, and a list of character buffers (StringChunk's).
 * Interning a symbol guarantees that the symbol is stored only once in
 * the buffers, and that every subsequent request for an internment of
 * that symbol will return the same address.  This is the key to making
 * symbol equality testing inexpensive for the Rosette virtual machine:
 * it becomes a simple pointer equality test.
 */


/*
 * A Bucket consists of a pointer into a StringChunk (pointing at the
 * beginning of the string associated with the Bucket), and a link to
 * other Buckets that hashed to the same slot in the RosetteStringTable.
 */


class Bucket {
    friend class RosetteStringTable;

    const char* key;
    Bucket* next;

    Bucket(const char*, Bucket*);
    ~Bucket();

    const char* searchFor(const char*);
};


Bucket::Bucket(const char* k, Bucket* n) : key(k), next(n) {}


Bucket::~Bucket() {
    if (next) {
        delete next;
    }
}


const char* Bucket::searchFor(const char* k) {
    for (const Bucket* bp = this; bp; bp = bp->next) {
        if (strcmp(k, bp->key) == 0) {
            return bp->key;
        }
    }
    return 0;
}


/*
 * A StringChunk is a buffer area for holding C strings.  It consists of
 * pointers to the beginning of the buffer and the next allocatable
 * location in the buffer, a count of the remaining characters in the
 * buffer, and a link to other previously allocated StringChunks.
 */

static const unsigned long DefaultChunkSize = 8192;


class StringChunk {
    friend class RosetteStringTable;

    StringChunk* next;
    char* bp;
    char* buffer;
    unsigned long remaining;

    StringChunk(StringChunk*, unsigned long);
    ~StringChunk();

    char* align(char*);
    char* deposit(const char*);
    int hasRoom(unsigned long);
};


StringChunk::StringChunk(StringChunk* sp, unsigned long size) : next(sp) {
    unsigned long n = max(DefaultChunkSize, size);
    buffer = new char[n];
    /*
     * This is probably unnecessary, since new should return pointers
     * that are "maximally" aligned.  Nonetheless, it won't hurt.
     */
    bp = align(buffer);
    remaining = n - (bp - buffer);
}


StringChunk::~StringChunk() {
    delete buffer;
    if (next) {
        delete next;
    }
}


char* StringChunk::align(char* p) {
    /*
     * Return a pointer that is aligned on the nearest 4-byte boundary
     * that is greater than or equal to the argument.
     */
    return (char*)(((unsigned long)p + 3) & ~3);
}


char* StringChunk::deposit(const char* sym) {
    char* result = bp;
    char* p = bp;

    while (true) {
        char c = *sym++;
        *p++ = c;
        if (0 == c) {
            break;
        }
    }

    bp = align(p);
    remaining -= bp - result;
    return result;
}


int StringChunk::hasRoom(unsigned long size) { return remaining >= size; }


/*
 * A RosetteStringTable consists of a chain of StringChunks and an array
 * of Bucket pointers.  When interning a string, we hash the string to
 * obtain a candidate Bucket, and then chase the Bucket chain (using
 * Bucket::searchFor) until we either find a a match or exhaust the
 * chain.  If there was no match, we add the string to the current
 * StringChunk (if there is enough room in the chunk to do so), or
 * allocate a new chunk and deposit it in there.
 */


static const unsigned long DefaultRosetteStringTableSize = 513;


class RosetteStringTable {
    StringChunk* chunk;
    Bucket** bucket;
    unsigned long nbuckets;

   public:
    RosetteStringTable(unsigned long = DefaultRosetteStringTableSize);
    ~RosetteStringTable();

    const char* intern(const char* const);
};


RosetteStringTable::RosetteStringTable(unsigned long n) {
    /*
     * This is a rather hideous trick to avoid reinitializing
     * statically-allocated RosetteStringTable's during the restart of a
     * dumped image.  It works by relying on the value of chunk being 0
     * in an honest-to-goodness virgin RosetteStringTable, and non-zero
     * in a RosetteStringTable that has been previously initialized.
     */

    if (!chunk) {
        chunk = new StringChunk(0, 0);
        bucket = new Bucket*[n];
        nbuckets = n;

        while (n--) {
            bucket[n] = 0;
        }
    }
}


RosetteStringTable::~RosetteStringTable() {
    delete bucket;
    delete chunk;
}


const char* RosetteStringTable::intern(const char* const sym) {
    unsigned long j = hash(sym);
    unsigned long i = j % nbuckets;
    const char* key = bucket[i] ? bucket[i]->searchFor(sym) : 0;

    if (key)
        return key;

    int size = strlen(sym) + 1;
    if (!chunk->hasRoom(size)) {
        chunk = new StringChunk(chunk, size);
    }

    key = chunk->deposit(sym);
    bucket[i] = new Bucket(key, bucket[i]);

    return key;
};


RosetteStringTable symtab;


const char* intern(const char* sym) { return symtab.intern(sym); }
