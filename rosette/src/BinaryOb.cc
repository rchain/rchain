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
 @EC */

#ifdef __GNUG__
#pragma implementation
#endif

#include "BinaryOb.h"

#include "CommandLine.h"
#include "Ctxt.h"
#include "Prim.h"
#include "Tuple.h"

#include "BuiltinClass.h"

#include <memory.h>


void undersizedObError(EMPTY) {
    suicide("Attempt to allocate an object smaller than the minimum allowed!");
}


inline static int min(int m, int n) { return m < n ? m : n; }


BUILTIN_CLASS(ByteVec) {}
BUILTIN_CLASS(Word16Vec) {}
BUILTIN_CLASS(Word32Vec) {}


int BinaryOb::traversePtrs(PSOb__PSOb f) {
    return useIfPtr(&meta(), f) + useIfPtr(&parent(), f);
}


int BinaryOb::traversePtrs(SI__PSOb f) {
    return useIfPtr(meta(), f) + useIfPtr(parent(), f);
}


void BinaryOb::traversePtrs(V__PSOb f) {
    useIfPtr(meta(), f);
    useIfPtr(parent(), f);
}


ByteVec::ByteVec(int sz)
    : BinaryOb(sizeof(ByteVec) + align(sz * sizeof(Byte)), CLASS_META(ByteVec),
               CLASS_SBO(ByteVec)),
      byteCount(sz) {
    memset((char*)&this->byte(0), 0, align(sz * sizeof(Byte)));
    ByteVec::updateCnt();
}


ByteVec::ByteVec(ByteVec* old, int newsize)
    : BinaryOb(sizeof(ByteVec) + align(newsize * sizeof(Byte)), old->meta(),
               old->parent()),
      byteCount(newsize) {
    int oldsize = old->numberOfBytes();
    memcpy((char*)&this->byte(0), (char*)&old->byte(0),
           min(oldsize, newsize) * sizeof(Byte));
    if (newsize > oldsize)
        memset(&this->byte(0), 0, (newsize - oldsize) * sizeof(Byte));
    ByteVec::updateCnt();
}


ByteVec* ByteVec::create(int n) {
    void* loc = PALLOC(sizeof(ByteVec) + align(n * sizeof(Byte)));
    return NEW(loc) ByteVec(n);
}


ByteVec* ByteVec::create(ByteVec* old, int newsize) {
    void* loc = PALLOC1(sizeof(ByteVec) + align(newsize * sizeof(Byte)), old);
    return NEW(loc) ByteVec(old, newsize);
}


void ByteVec::reset(EMPTY) { memset((char*)&byte(0), 0, numberOfBytes()); }


unsigned long ByteVec::sum(EMPTY) {
    unsigned long total = 0;
    for (int i = numberOfBytes(); i--;)
        total += (unsigned long)byte(i);
    return total;
}


Ob* ByteVec::indexedSize(EMPTY) { return FIXNUM(numberOfBytes()); }


Ob* ByteVec::nth(int n) { return FIXNUM(byte(n)); }


Ob* ByteVec::setNth(int n, Ob* v) {
    byte(n) = FIXVAL(v);
    return this;
}


Ob* ByteVec::subObject(int start, int n) {
    PROTECT_THIS(ByteVec);
    ByteVec* result = ByteVec::create(n);
    memcpy((char*)&result->byte(0), (char*)&SELF->byte(start),
           n * sizeof(Byte));
    return result;
}


Word16Vec::Word16Vec(int n)
    : BinaryOb(sizeof(Word16Vec) + align(n * sizeof(Word16)),
               CLASS_META(Word16Vec), CLASS_SBO(Word16Vec)),
      wordCount(n) {
    while (n--)
        this->word(n) = 0;
    Word16Vec::updateCnt();
}


Word16Vec::Word16Vec(Ob* meta, Ob* parent, int n)
    : BinaryOb(sizeof(Word16Vec) + align(n * sizeof(Word16)), meta, parent),
      wordCount(n) {
    while (n--)
        this->word(n) = 0;
    Word16Vec::updateCnt();
}


Word16Vec::Word16Vec(Word16Vec* old, int newsize)
    : BinaryOb(sizeof(Word16Vec) + align(newsize * sizeof(Word16)), old->meta(),
               old->parent()),
      wordCount(newsize) {
    int oldsize = old->numberOfWords();
    memcpy(&this->word(0), &old->word(0),
           min(oldsize, newsize) * sizeof(Word16));
    if (newsize > oldsize)
        memset(&this->word(oldsize), 0, (newsize - oldsize) * sizeof(Word16));
    Word16Vec::updateCnt();
}


Word16Vec* Word16Vec::create(int n) {
    void* loc = PALLOC(sizeof(Word16Vec) + align(n * sizeof(Word16)));
    return NEW(loc) Word16Vec(n);
}


Word16Vec* Word16Vec::create(Ob* meta, Ob* parent, int n) {
    void* loc =
        PALLOC2(sizeof(Word16Vec) + align(n * sizeof(Word16)), meta, parent);
    return NEW(loc) Word16Vec(meta, parent, n);
}


Word16Vec* Word16Vec::create(Word16Vec* old, int newsize) {
    void* loc =
        PALLOC1(sizeof(Word16Vec) + align(newsize * sizeof(Word16)), old);
    return NEW(loc) Word16Vec(old, newsize);
}


void Word16Vec::reset(EMPTY) {
    for (int i = numberOfWords(); i--;)
        word(i) = 0;
}


unsigned long Word16Vec::sum(EMPTY) {
    unsigned long total = 0;
    for (int i = numberOfWords(); i--;)
        total += (unsigned long)word(i);
    return total;
}


Ob* Word16Vec::indexedSize(EMPTY) { return FIXNUM(numberOfWords()); }


Ob* Word16Vec::nth(int n) { return FIXNUM(word(n)); }


Ob* Word16Vec::setNth(int n, Ob* v) {
    word(n) = FIXVAL(v);
    return this;
}


Ob* Word16Vec::subObject(int start, int n) {
    PROTECT_THIS(Word16Vec);
    Word16Vec* result = Word16Vec::create(n);
    memcpy(&result->word(0), &SELF->word(start), n * sizeof(Word16));
    return result;
}


Word32Vec::Word32Vec(int n)
    : BinaryOb(sizeof(Word32Vec) + n * sizeof(Word32), CLASS_META(Word32Vec),
               CLASS_SBO(Word32Vec)) {
    while (n--)
        this->word(n) = 0;
    Word32Vec::updateCnt();
}


Word32Vec::Word32Vec(Word32Vec* old, int newsize)
    : BinaryOb(sizeof(Word32Vec) + newsize * sizeof(Word32), old->meta(),
               old->parent()) {
    int oldsize = old->numberOfWords();
    memcpy(&this->word(0), &old->word(0),
           min(oldsize, newsize) * sizeof(Word32));
    if (newsize > oldsize)
        memset(&this->word(oldsize), 0, (newsize - oldsize) * sizeof(Word32));
    Word32Vec::updateCnt();
}


Word32Vec* Word32Vec::create(int n) {
    void* loc = PALLOC(sizeof(Word32Vec) + n * sizeof(Word32));
    return NEW(loc) Word32Vec(n);
}


Word32Vec* Word32Vec::create(Word32Vec* old, int newsize) {
    void* loc = PALLOC1(sizeof(Word32Vec) + newsize * sizeof(Word32), old);
    return NEW(loc) Word32Vec(old, newsize);
}


void Word32Vec::reset(EMPTY) {
    for (int i = numberOfWords(); i--;)
        word(i) = 0;
}


unsigned long Word32Vec::sum(EMPTY) {
    unsigned long total = 0;
    for (int i = numberOfWords(); i--;)
        total += (unsigned long)word(i);
    return total;
}


Ob* Word32Vec::indexedSize(EMPTY) { return FIXNUM(numberOfWords()); }


Ob* Word32Vec::nth(int n) { return FIXNUM(word(n)); }


Ob* Word32Vec::setNth(int n, Ob* v) {
    word(n) = FIXVAL(v);
    return this;
}


Ob* Word32Vec::subObject(int start, int n) {
    PROTECT_THIS(Word32Vec);
    Word32Vec* result = Word32Vec::create(n);
    memcpy(&result->word(0), &SELF->word(start), n * sizeof(Word32));
    return result;
}


DEF("bytevec-new", byteVecNew, 1, 1) {
    CHECK_FIXNUM(0, n);
    return ByteVec::create(n);
}


DEF("word16vec-new", word16VecNew, 1, 1) {
    CHECK_FIXNUM(0, n);
    return Word16Vec::create(n);
}


DEF("word32vec-new", word32VecNew, 1, 1) {
    CHECK_FIXNUM(0, n);
    return Word32Vec::create(n);
}
