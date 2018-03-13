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

#include "Timer.h"

#define IN_TIMER

#include "BuiltinClass.h"

/* Timer.h includes
   <sys/time.h>
   <sys/resource.h>
*/


extern "C" {
#ifdef HAVE_GETRUSAGE
int getrusage(int, struct rusage*);
#else
int getrusage(int x, struct rusage* ru) { return 0; }
#endif
}

BUILTIN_CLASS(Timer) {}


Timer::Timer() : BinaryOb(sizeof(Timer), CLASS_META(Timer), CLASS_SBO(Timer)) {
    reset();
    Timer::updateCnt();
}


Timer* Timer::create() {
    void* loc = PALLOC(sizeof(Timer));
    return new (loc) Timer();
}


void Timer::updateTimer() {
    struct rusage current;

    getrusage(RUSAGE_SELF, &current);

    inc(tv[mode], current.ru_utime.tv_sec - checkpoint.ru_utime.tv_sec,
        current.ru_utime.tv_usec - checkpoint.ru_utime.tv_usec);

    inc(tv[tmSys], current.ru_stime.tv_sec - checkpoint.ru_stime.tv_sec,
        current.ru_stime.tv_usec - checkpoint.ru_stime.tv_usec);

    checkpoint = current;
}


void Timer::inc(timeval& tv, long secs, long usecs) {
    tv.tv_sec += secs;
    tv.tv_usec += usecs;
    if (tv.tv_usec >= 1000000) {
        tv.tv_usec -= 1000000;
        tv.tv_sec += 1;
    } else if (tv.tv_usec < 0) {
        tv.tv_usec += 1000000;
        tv.tv_sec -= 1;
    }
}


float Timer::fastTime(TimerMode m) {
    return (float)tv[m].tv_sec + ((float)tv[m].tv_usec / 1000000.0);
}


void Timer::reset() {
    running = false;
    mode = tmUser;
    for (int i = 0; i < nModes; i++) {
        tv[i].tv_sec = tv[i].tv_usec = 0;
    }
}


void Timer::stop() {
    if (running) {
        updateTimer();
        running = false;
    }
}


void Timer::start() {
    if (!running) {
        running = true;
        getrusage(RUSAGE_SELF, &checkpoint);
    }
}


TimerMode Timer::setMode(TimerMode new_mode) {
    if (running) {
        updateTimer();
    }

    TimerMode old_mode = mode;
    mode = new_mode;
    return old_mode;
}


float Timer::time(TimerMode m) {
    if (running) {
        updateTimer();
    }

    return fastTime(m);
}


void Timer::printStats(FILE* f) {
    if (running) {
        updateTimer();
    }

    fprintf(f, "time: %.2f secs (user), %.2f secs (gc), %.2f secs (system)",
            fastTime(tmUser), fastTime(tmGC), fastTime(tmSys));
}
