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

#if !defined(_RBL_Pattern_h)
#define _RBL_Pattern_h

#include "rosette.h"
#include "Ob.h"

class Pattern : public Ob {
   protected:
    Pattern(int, Ob*, Ob*);

   public:
    virtual int numberOfKeys(void);
    virtual void stuffKeys(Tuple*, int);
    virtual bool matchIntoArgvec(Tuple*, int, Ob*, int = -1);
    virtual bool fail(Ob*);

    virtual Ob* cloneTo(Ob*, Ob*);
};


class IdPattern : public Pattern {
    STD_DECLS(IdPattern);

   protected:
    IdPattern(Ob*);

   public:
    Ob* symbol;

    static IdPattern* create(Ob*);

    int numberOfKeys(void);
    void stuffKeys(Tuple*, int);
    bool matchIntoArgvec(Tuple*, int, Ob*, int = -1);
};


class ConstPattern : public Pattern {
    STD_DECLS(ConstPattern);

   protected:
    ConstPattern(Ob*);

   public:
    Ob* val;

    static ConstPattern* create(Ob*);

    int numberOfKeys(void);
    void stuffKeys(Tuple*, int);
    bool matchIntoArgvec(Tuple*, int, Ob*, int = -1);
};


class CompoundPattern : public Pattern {
   protected:
    CompoundPattern(int, Ob*, Ob*, TupleExpr*);

   public:
    TupleExpr* expr;

    virtual Tuple* match(Tuple*, int);
    virtual int keyExtent(int);
};


class IdVecPattern : public CompoundPattern {
    STD_DECLS(IdVecPattern);

   protected:
    IdVecPattern(TupleExpr*);

   public:
    static IdVecPattern* create(TupleExpr*);

    int numberOfKeys(void);
    void stuffKeys(Tuple*, int);
    int keyExtent(int);
    Tuple* match(Tuple*, int);
    bool matchIntoArgvec(Tuple*, int, Ob*, int = -1);
};


class IdAmperRestPattern : public CompoundPattern {
    STD_DECLS(IdAmperRestPattern);

   protected:
    IdAmperRestPattern(TupleExpr*);

   public:
    static IdAmperRestPattern* create(TupleExpr*);

    int numberOfKeys(void);
    void stuffKeys(Tuple*, int);
    int keyExtent(int);
    Tuple* match(Tuple*, int);
    bool matchIntoArgvec(Tuple*, int, Ob*, int = -1);
};


class ComplexPattern : public CompoundPattern {
    STD_DECLS(ComplexPattern);


   public:
    ComplexPattern(TupleExpr*, Tuple*, Tuple*);
    Tuple* patvec;
    Tuple* offsetvec;

    static ComplexPattern* create(TupleExpr*);

    int numberOfKeys(void);
    void stuffKeys(Tuple*, int);
    int keyExtent(int);
    Tuple* match(Tuple*, int);
    bool matchIntoArgvec(Tuple*, int, Ob*, int = -1);
};


class Template : public Ob {
    STD_DECLS(Template);

   protected:
    Template(Tuple*, Ob*, CompoundPattern*);

   public:
    Tuple* keytuple;
    CompoundPattern* pat;
    Ob* keymeta;

    static Template* create(TupleExpr*);
    static Template* create(Tuple*, Ob*, CompoundPattern*);

    Tuple* match(Tuple* argvec, int nargs) {
        return (pat->match(argvec, nargs));
    }

    Ob* fail(Ob*);
    Ob* cloneTo(Ob*, Ob*);
};


#endif
