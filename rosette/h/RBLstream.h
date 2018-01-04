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

#if !defined(_RBL_stream_h)
#define _RBL_stream_h

#include "rosette.h"
#include "BinaryOb.h"

class Reader;

class Istream : public Actor {
    STD_DECLS(Istream);

   protected:
    Istream(Ob*, pExt, Reader*);

   public:
    Ob* client;
    Reader* reader;

    static Istream* create(Reader*);
    virtual Ob* cloneTo(Ob*, Ob*);
};


class Ostream : public BinaryOb {
    STD_DECLS(Ostream);

   protected:
    Ostream(FILE*);

   public:
    FILE* stream;

    virtual ~Ostream();

    static Ostream* create(FILE*);
    virtual Ob* cloneTo(Ob*, Ob*);
};


extern StdOprn* oprnRead;
extern StdOprn* oprnResumeIO;

#endif
