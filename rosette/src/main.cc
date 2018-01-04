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

/*
 * $Header: /mcc/project/carnot/root/master/pub-ess/src/main.cc,v 1.1.1.1
1993/02/12 01:25:49 tomlic Exp $
 *
 * $Log: main.cc,v $
// Revision 1.1.1.1  1993/02/12  01:25:49  tomlic
// pub release of rosette
//
 @EC */

#include "rosette.h"
#include "Prim.h"
#include "Reader.h"
#include "Vm.h"
#include <ctype.h>
#include <stdio.h>

extern int BigBang(int, char**, char**);
extern void BigCrunch();
extern int asyncHelper(int, int);

// reference to the configuration_force routine that causes loading
// of whatever libraries are desired for a given configuration

extern "C" void configuration_force_load();

static int _ForceLoadFlag_ = 0;

int main(int argc, char** argv, char** envp) {
    if (!BigBang(argc, argv, envp)) {
        vm->reset();
    }

    vm->execute();

    if (_ForceLoadFlag_) {
        configuration_force_load();
    }

    while (!feof(stdin)) {
        const char* prompt = "rosette> ";
        /*
         * Turn off any async handling that might have been left on by a
         * crash-and-burn at the higher levels.
         */
        heap->gc();
        StdinReader->resetState();

        printf("%s", prompt);
        Ob* x = StdinReader->readExpr();

        if (x == RBLEOF) {
            break;
        }

        clearerr(stdin);
        vm->evaluate(x);
    }

    putchar('\n');
    BigCrunch();
    exit(0);
}
