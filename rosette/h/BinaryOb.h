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

#if !defined(_RBL_BinaryOb_h)
#define _RBL_BinaryOb_h

#ifdef __GNUG__
#pragma interface
#endif

#include "Ob.h"

/*
 * BinaryOb's are assumed to contain *NO* Ob*'s in their bodies, and no
 * extra scavenging is done on them.  Marking is similar: no attempt is
 * made to perform recursive marking on the components of a BinaryOb.
 * This is a useful base class for objects that simply store binary
 * information, such as floating-point numbers.
 */

class BinaryOb : public Ob {
   protected:
    BinaryOb(int, pOb, pOb);
    BinaryOb(InPlace_Constructor*, pOb, pOb);

   public:
    virtual int traversePtrs(PSOb__PSOb);
    virtual int traversePtrs(SI__PSOb);
    virtual void traversePtrs(V__PSOb);
};

inline BinaryOb::BinaryOb(int sz, pOb meta, pOb parent)
    : Ob(sz, meta, parent) {}

inline BinaryOb::BinaryOb(InPlace_Constructor* ipc, pOb meta, pOb parent)
    : Ob(ipc, meta, parent) {}


class ByteVec : public BinaryOb {
    STD_DECLS(ByteVec);

   protected:
    int byteCount;

    /*
     * If there are any member variables added after byteCount, be sure
     * to modify the code for byte(), since it uses the address of
     * byteCount to determine the end of the fixed part of the bytevec.
     */

    ByteVec(int, pOb, pOb, int);
    ByteVec(int);
    ByteVec(ByteVec*, int);

   public:
    static ByteVec* create(int);
    static ByteVec* create(ByteVec*, int);

    uint8_t& byte(int);
    int numberOfBytes(EMPTY);
    void reset(EMPTY);
    uint32_t sum(EMPTY);

    virtual Ob* indexedSize(EMPTY);
    virtual Ob* nth(int);
    virtual Ob* setNth(int, Ob*);
    virtual Ob* subObject(int, int);
};

inline ByteVec::ByteVec(int sz, pOb meta, pOb parent, int numberOfBytes)
    : BinaryOb(sz, meta, parent) {
    byteCount = numberOfBytes;
}

inline uint8_t& ByteVec::byte(int n) {
    // WTH????
    uint8_t* p = (uint8_t*)(((char*)&byteCount) + sizeof(byteCount));
    return p[n];
}

inline int ByteVec::numberOfBytes(EMPTY) { return byteCount; }


class Word16Vec : public BinaryOb {
    STD_DECLS(Word16Vec);

   protected:
    int wordCount;

    /*
     * If there are any member variables added after wordCount, be sure
     * to modify the code for word(), since it uses the address of
     * wordCount to determine the end of the fixed part of the wordvec.
     */

    Word16Vec(int, pOb, pOb, int);
    Word16Vec(int);
    Word16Vec(pOb, pOb, int);
    Word16Vec(Word16Vec*, int);

   public:
    static Word16Vec* create(int);
    static Word16Vec* create(pOb, pOb, int);
    static Word16Vec* create(Word16Vec*, int);

    uint16_t& word(int);
    int numberOfWords(EMPTY);
    void reset(EMPTY);
    uint32_t sum(EMPTY);

    virtual Ob* indexedSize(EMPTY);
    virtual Ob* nth(int);
    virtual Ob* setNth(int, Ob*);
    virtual Ob* subObject(int, int);
};

inline Word16Vec::Word16Vec(int sz, pOb meta, pOb parent, int cnt)
    : BinaryOb(sz, meta, parent) {
    wordCount = cnt;
}

inline uint16_t& Word16Vec::word(int n) {
    uint16_t* p = (uint16_t*)(((char*)&wordCount) + sizeof(wordCount));
    return p[n];
}

inline int Word16Vec::numberOfWords(EMPTY) { return wordCount; }


class Word32Vec : public BinaryOb {
    STD_DECLS(Word32Vec);

   protected:
    Word32Vec(int, pOb, pOb);
    Word32Vec(int);
    Word32Vec(Word32Vec*, int);

   public:
    static Word32Vec* create(int);
    static Word32Vec* create(Word32Vec*, int);

    uint32_t& word(int);
    int numberOfWords(EMPTY);
    void reset(EMPTY);
    uint32_t sum(EMPTY);

    virtual Ob* indexedSize(EMPTY);
    virtual Ob* nth(int);
    virtual Ob* setNth(int, Ob*);
    virtual Ob* subObject(int, int);
};

inline Word32Vec::Word32Vec(int sz, pOb meta, pOb parent)
    : BinaryOb(sz, meta, parent) {}

inline uint32_t& Word32Vec::word(int n) {
    uint32_t* p = (uint32_t*)&slot(0);
    return p[n];
}

inline int Word32Vec::numberOfWords(EMPTY) {
    return (SIZE(this) - sizeof(Word32Vec)) / sizeof(uint32_t);
}

#endif
