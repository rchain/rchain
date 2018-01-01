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

#define IN_CONSOLE

#include "rosette.h"

#ifndef NO_SYSENT_H
#include <sysent.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <errno.h>
#include <osfcn.h>

#include <sys/socket.h>
#include <unistd.h>

#ifdef MIPS_SGI_SYSV
#include <sys/param.h>
#include <sys/types.h>
#endif


#ifdef MIPS_SGI_SYSV
#include <unistd.h>

#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

int setpgrp();
int setsid();

#endif

#define OK 0
#define NOTOK (-1)

static int image;
static void finish() { exit(0); }
static void passthru(int sig) { (void)kill(image, sig); }
static void terminate() {
    (void)kill(image, SIGTERM);
    exit(0);
}


main(int, char** argv) {
    char buffer[BUFSIZ];
    register char* bp = buffer;
    int fds[2];

    // Basic signal handling

    signal(SIGCHLD, (SIG_PF)finish);     // exit on death of child
    signal(SIGPIPE, (SIG_PF)terminate);  // kill image on losing pipe
    signal(SIGTERM, (SIG_PF)terminate);  // kill image on terminate
    signal(SIGINT, (SIG_PF)passthru);    // pass thru to child

    // Create pipe used to feed our syncrhonous stdin to rosette image
    // which handles its end of the pipe as an asynchronous stdin.

    if (socketpair(AF_UNIX, SOCK_STREAM, 0, fds) == NOTOK) {
        perror("Unable to create pipe");
        exit(1);
    }

    switch (image = fork()) {
    case NOTOK:  // Only if no memory or max processes exceeded

        perror("Unable to fork rosette image");
        exit(1);

    case OK:  // Ready to exec the image

        // Bind image's stdin to the pipe

        if (dup2(fds[0], 0) == NOTOK) {
            perror("Unable to dup image stdin to pipe");
            _exit(1);
        }

        // NB(orig).
        setsid();

        sprintf(bp, "%s.image", *argv);
        *argv = bp;

        // Note: execve resets caught signals to their defaults
        execvp(bp, argv);

        perror("Unable to exec rosette image");
        _exit(1);

    default:  // Stuff input into pipe until EOF
        // Only the image will do the writing to stdout/stderr
        fclose(stdout);
        fclose(stderr);

        for (;;) {
            if (fgets(bp, sizeof(buffer), stdin)) {
                write(fds[1], bp, strlen(bp));
            } else {
                terminate();
            }
        }
    }
}
