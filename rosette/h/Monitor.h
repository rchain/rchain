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

#if !defined(_RBL_Monitor_h)
#define _RBL_Monitor_h

#include "rosette.h"
#include "Ob.h"

class Monitor : public Ob {
    STD_DECLS(Monitor);

   protected:
    Monitor(Ob*, Timer*, Word32Vec*, Word32Vec*);

   public:
    Ob* id;
    Timer* timer;
    Word32Vec* opcodeCounts;
    Word32Vec* obCounts;
    Ob* tracing;

    static Monitor* create(Ob*);

    void reset();
    void start();
    void stop();
    void printStats(FILE*);
};

#endif
