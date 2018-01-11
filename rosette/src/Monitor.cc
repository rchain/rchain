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

#include "Monitor.h"

#include "Number.h"
#include "Opcode.h"
#include "Prim.h"
#include "RBLstream.h"
#include "Timer.h"
#include "Tuple.h"
#include "Vm.h"

#include "BuiltinClass.h"


BUILTIN_CLASS(Monitor) {
    OB_FIELD("id", Monitor, id);
    OB_FIELD("timer", Monitor, timer);
    OB_FIELD("opcode-counts", Monitor, opcodeCounts);
    OB_FIELD("ob-counts", Monitor, obCounts);
    OB_FIELD("tracing", Monitor, tracing);
}


Monitor::Monitor(Ob* id, Timer* timer, Word32Vec* opcodeCounts,
                 Word32Vec* obCounts)
    : Ob(sizeof(Monitor), CLASS_META(Monitor), CLASS_SBO(Monitor)),
      id(id),
      timer(timer),
      opcodeCounts(opcodeCounts),
      obCounts(obCounts),
      tracing(RBLFALSE) {
    reset();
    Monitor::updateCnt();
};


Monitor* Monitor::create(Ob* id) {
    PROTECT(id);
    Timer* timer = Timer::create();
    PROTECT(timer);
    Word32Vec* opcodeCounts = Word32Vec::create(MaxOpcodes);
    PROTECT(opcodeCounts);
    Word32Vec* obCounts = Word32Vec::create(Base::nClasses);
    PROTECT(obCounts);
    void* loc = PALLOC(sizeof(Monitor));
    return new (loc) Monitor(id, timer, opcodeCounts, obCounts);
}


void Monitor::reset() {
    timer->reset();
    opcodeCounts->reset();
    obCounts->reset();
}


void Monitor::start() { timer->start(); }


void Monitor::stop() { timer->stop(); }


static void prettyPrint(uint32_t n, char* name, FILE* f) {
    if (n != 0) {
        fprintf(f, "%8ul %s%s\n", n, name, plural((int)n));
    }
}


void Monitor::printStats(FILE* f) {
    uint32_t total = 0;

    fprintf(f, "%s:\n", BASE(id)->asCstring());

    fprintf(f, "objects allocated:\n");
    int n = obCounts->numberOfWords();
    int i = 0;
    for (i = 0; i < n; i++) {
        uint32_t count = (int)obCounts->word(i);
        prettyPrint(count, Base::classNames[i], f);
        total += count;
    }

    fprintf(f, "%8ul total\n", total);
    fprintf(f, "bytecodes:\n");
    total = 0;
    for (i = 0; i < 256; i++) {
        uint32_t n = opcodeCounts->word(i);
        if (n > 0) {
            total += n;
            char* str = opcodeStrings[i];
            if (str) {
                fprintf(f, "%8ul %s\n", n, str);
            } else {
                fprintf(f, "%8ul ?%2x\n", n, i);
            }
        }
    }
    fprintf(f, "%8ul total\n", total);

    timer->printStats(f);
    putc('\n', f);
}


DEF("monitor-new", monitorNew, 0, 1) {
    Ob* id = SYMBOL("anonymous monitor");

    if (NARGS == 1) {
        id = ARG(0);
    }

    return Monitor::create(id);
}


DEF("monitor-start", monitorStart, 1, 1) {
    CHECK(0, Monitor, mon);
    mon->start();
    return mon;
}


DEF("monitor-stop", monitorStop, 1, 1) {
    CHECK(0, Monitor, mon);
    mon->stop();
    return mon;
}


DEF("monitor-convert", monitorConvert, 1, 1) {
    CHECK(0, Monitor, mon);
    PROTECT(mon);

    Timer* timer = mon->timer;
    PROTECT(timer);
    Tuple* times = Tuple::create(3, NIV);
    PROTECT(times);
    Ob* tmp = Float::create(timer->time(tmUser));
    ASSIGN(times, elem(0), tmp);
    tmp = Float::create(timer->time(tmGC));
    ASSIGN(times, elem(1), tmp);
    tmp = Float::create(timer->time(tmSys));
    ASSIGN(times, elem(2), tmp);

    Tuple* opCounts = Tuple::create(2, NIV);
    PROTECT(opCounts);
    tmp = mon->opcodeCounts->clone();
    ASSIGN(opCounts, elem(0), FIXNUM(((Word32Vec*)tmp)->sum()));
    ASSIGN(opCounts, elem(1), tmp);

    Tuple* obCounts = Tuple::create(2, NIV);
    PROTECT(obCounts);
    tmp = mon->obCounts->clone();
    ASSIGN(obCounts, elem(0), FIXNUM(((Word32Vec*)tmp)->sum()));
    ASSIGN(obCounts, elem(1), tmp);

    Tuple* result = Tuple::create(3, NIV);
    result->elem(0) = times;
    result->elem(1) = opCounts;
    result->elem(2) = obCounts;

    return result;
}


DEF("monitor-reset", monitorReset, 1, 1) {
    CHECK(0, Monitor, mon);
    mon->reset();
    return mon;
}


DEF("monitor-dump", monitorDump, 1, 2) {
    CHECK(0, Monitor, mon);
    FILE* file = stdout;

    if (NARGS == 2) {
        CHECK(1, Ostream, s);
        if (s->stream) {
            file = s->stream;
        }
    }

    mon->printStats(file);

    return NIV;
}
