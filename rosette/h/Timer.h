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

#if !defined(_RBL_Timer_h)
#define _RBL_Timer_h

#include "rosette.h"
#include "BinaryOb.h"

#include <sys/time.h>
#include <sys/resource.h>


enum TimerMode { tmUser, tmGC, tmSys, nModes };


class Timer : public BinaryOb {
    STD_DECLS(Timer);

   protected:
    Timer();

   private:
    int running;
    TimerMode mode;
    timeval tv[nModes];
    struct rusage checkpoint;

    void updateTimer();
    void inc(timeval&, long, long);
    float fastTime(TimerMode);

   public:
    static Timer* create();

    void reset();
    void start();
    void stop();
    TimerMode setMode(TimerMode);
    float time(TimerMode);
    void printStats(FILE*);
};


#endif
