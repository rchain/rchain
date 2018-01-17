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

#include "rosette.h"

#if defined(MIPS_SGI_SYSV) || defined(LINUX)
#include <unistd.h>
#endif

#include "Dynload.h"
#include "Prim.h"
#include "Tuple.h"

extern int RestoringImage;


DEF("image-dump", imageDump, 1, 1) {
    char* path = BASE(ARG(0))->asPathname();
    if (!path) {
        return PRIM_MISMATCH(0, "String or Symbol");
    }

    RestoringImage = 1;
    return RBLFALSE;
}


DEF("image-restore", imageRestore, 1, MaxArgs) {
    char* path = BASE(ARG(0))->asPathname();
    if (!path) {
        return PRIM_MISMATCH(0, "String or Symbol");
    }

    int argc = NARGS;
    char** argv = new char*[argc + 1];

    argv[0] = path;
    for (int i = 1; i < argc; i++) {
        const char* s1 = BASE(ARG(i))->asCstring();
        char* s2 = new char[strlen(s1) + 1];
        argv[i] = strcpy(s2, s1);
    }

    argv[argc] = 0;

    extern char** environ;
    execve(path, argv, environ);

    /*
     * If we get into this code, it's because the execve failed for some
     * reason, and we need to clean up and report the error.  Normally,
     * we needn't worry about cleanup because our image is about to be
     * overlaid anyway.
     */

    delete argv;
    return PRIM_ERROR(sys_errmsg());
}
