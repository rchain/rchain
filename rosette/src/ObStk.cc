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

#include "ObStk.h"

void ObStk::reset() { next = array; }


int ObStk::traversePtrs(PSOb__PSOb f) {
    int sum = 0;
    pOb* const n = (pOb*)next;
    for (pOb* p = (pOb*)array; p < n; p++) {
        sum += useIfPtr(p, f);
    }
    return sum;
}


int ObStk::traversePtrs(SI__PSOb f) {
    int sum = 0;
    pOb* const n = (pOb*)next;
    for (pOb* p = (pOb*)array; p < n; p++) {
        sum += useIfPtr(*p, f);
    }
    return sum;
}


void ObStk::traversePtrs(V__PSOb f) {
    pOb* const n = (pOb*)next;
    for (pOb* p = (pOb*)array; p < n; p++) {
        useIfPtr(*p, f);
    }
}


void ObStk::scavenge() { traversePtrs(MF_ADDR(Ob::relocate)); }


void ObStk::mark() { traversePtrs(MF_ADDR(Ob::mark)); }


void ObStk::check() { traversePtrs(MF_ADDR(Ob::checkOb)); }
