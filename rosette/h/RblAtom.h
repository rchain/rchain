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

#if !defined(_RBL_Atom_h)
#define _RBL_Atom_h

#include "rosette.h"
#include "Ob.h"

// The RblAtom derived objects actually store the value of the object within
// the pointer. This allows them to be passed around as a pointer to an object,
// but still only consume the memory of one pointer. These are in essence pseudo
// pointers. They look like pointers to an object, but when accessed, some slight
// of hand comes into play to get access to the value of the object without actually
// dereferencing it as a pointer.
//
// During startup, Rosette instantiates one global instance of each type of RblAtom
// derived classes. See MODULE_INIT() in RblAtom.cc. When a pOb to 
// one of these classes is required, the BASE() function calls decodeAtom() which then
// extracts the value from the supplied pOb pseudo pointer, assigns the value into the
// cooresponding global instance "atom" member variable and returns the pointer to the global
// instance. Rosette code must be careful to only use this pointer temporarily, and 
// especially not call BASE() on any other pOb of the same type. Typical use is to call
// a member function to do such things as print out the value.


// To summarize, the slight of hand works like this:
//  1. At startup, allocate a single global instance of each of the classes derived from RblAtom.
//  2. When a pOb of one of the classes derived from RblAtom is accessed using
//     BASE(), the pointer is passed into decodeAtom() which then determines the
//     type of the object, and assigns the pointer into the "atom" member variable of the
//     corresponding global instance for its type.
//  3. The pointer to the global instance is then returned.
//  4. When an accessor method is called, the data is retrieved from the "atom" member variable.

class RblAtom : public Ob {
   protected:
    Ob* atom;

    friend Ob* decodeAtom(Ob*);

   public:
    RblAtom(int, Ob*, Ob*);

    virtual Ob* self();
    virtual Ob* cloneTo(Ob*, Ob*);
    virtual Ob* update(bool, Ctxt*);
    virtual pOb primitiveInitialize(pCtxt);
    virtual bool coversp(pOb);
};


class Symbol : public RblAtom {
    STD_DECLS(Symbol);

   public:
    Symbol();

    static Symbol* create();

    bool ConstantP();
    void printOn(FILE*);
    void printQuotedOn(FILE*);
    void displayOn(FILE*);
    const char* asCstring();
    char* asPathname();
    Pattern* makePattern();
    AttrNode* makeAttrNode(bool valueCtxt);
    Ob* unquote();
};


class RblBool : public RblAtom {
    STD_DECLS(RblBool);

   public:
    RblBool();

    static RblBool* create();
    const char* asCstring();

    virtual convertArgReturnPair convertActualArg(Ctxt*, Ob*);
    // virtual Ob*         convertActualRslt(Ctxt*, uint32_t);
};

class Char : public RblAtom {
    STD_DECLS(Char);

   public:
    Char();

    static Char* create();
    void printOn(FILE*);
    void displayOn(FILE*);
    const char* asCstring();
};


static const int FixnumFormatSize = 16;

class Fixnum : public RblAtom {
    STD_DECLS(Fixnum);

   public:
    Fixnum();

    static char format[FixnumFormatSize];
    friend BUILTIN_PRIM(fxFormat);

    static Fixnum* create();
    const char* asCstring();

    virtual convertArgReturnPair convertActualArg(Ctxt*, Ob*);
    virtual Ob* convertActualRslt(Ctxt*, uint32_t);
};


class Niv : public RblAtom {
    STD_DECLS(Niv);

   public:
    Niv();

    static Niv* create();
    const char* asCstring();
    Ob* invoke(Ctxt*);
};


class Sysval : public RblAtom {
    STD_DECLS(Sysval);

   public:
    Sysval();

    static Sysval* create();
    const char* asCstring();
};


class ExpandedLocation : public RblAtom {
    STD_DECLS(ExpandedLocation);

   public:
    ExpandedLocation();

    static ExpandedLocation* create();
    const char* asCstring();
};

#endif
