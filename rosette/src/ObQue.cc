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

#include "ObQue.h"

static const int DefaultObQueSize = 32;

ObQue::ObQue() {
    array = new Ob*[DefaultObQueSize];
    size = DefaultObQueSize;
    init();
}


ObQue::ObQue(int sz) {
    array = new Ob*[sz];
    size = sz;
    init();
}

ObQue::~ObQue() { delete array; }

void ObQue::push(Ob* o) {
    if (head == tail && last_added) {
        resize();
    } else if (head == limit) {
        if (tail == array) {
            resize();
        } else {
            head = array;
        }
    }

    last_added = 1;
    (*head++) = o;
}

void ObQue::reset() {
    head = array;
    tail = array;
    last_added = 0;
}

void ObQue::resize() { resize(2 * size); }

void ObQue::resize(int newSize) {
    Ob** newArray = new Ob*[newSize];
    int offset;

    if (tail < head) {
        offset = (int)(head - tail);
        memcpy((char*)newArray, (char*)tail, offset * sizeof(Ob*));
    } else {
        offset = (int)(limit - tail);
        memcpy((char*)newArray, (char*)tail, offset * sizeof(Ob*));
        memcpy((char*)(newArray + offset), (char*)array,
               (head - array) * sizeof(Ob*));
        offset = (int)(empty() ? 0 : size - (tail - head));
    }

    delete array;
    array = newArray;
    tail = array;
    head = array + offset;
    size = newSize;
    limit = array + size;
}

int ObQue::traversePtrs(PSOb__PSOb f) {
    if (empty()) {
        return 0;
    } else {
        int sum = 0;
        pOb* p = (tail < head) ? tail : array;

        for (; p < head; p++) {
            sum += useIfPtr(p, f);
        }

        if (head <= tail) {
            for (p = tail; p < limit; p++) {
                sum += useIfPtr(p, f);
            }
        }

        return sum;
    }
}

int ObQue::traversePtrs(SI__PSOb f) {
    if (empty()) {
        return 0;
    } else {
        int sum = 0;
        pOb* p = (tail < head) ? tail : array;

        for (; p < head; p++) {
            sum += useIfPtr(*p, f);
        }

        if (head <= tail) {
            for (p = tail; p < limit; p++) {
                sum += useIfPtr(*p, f);
            }
        }

        return sum;
    }
}

void ObQue::traversePtrs(V__PSOb f) {
    if (empty()) {
        return;
    } else {
        pOb* p = (tail < head) ? tail : array;

        for (; p < head; p++) {
            useIfPtr(*p, f);
        }

        if (head <= tail) {
            for (p = tail; p < limit; p++) {
                useIfPtr(*p, f);
            }
        }
    }
}
