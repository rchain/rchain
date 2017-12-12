/* Mode: -*- C++ -*- */
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
 * $Header$
 *
 * $Log$
 @EC */

#ifdef __GNUG__
#pragma implementation
#endif

#include "CommandLine.h"

#include <string.h>
#include <sys/param.h>

#include "misc.h"

unsigned SurvivorSpaceSize = 128 * 1024;
unsigned InfantSpaceSize = 512 * 1024;
unsigned OldSpaceChunkSize = 32 * 1024;

#define DEFAULT "/usr/local/lib/rosette/lib"

int TenuringAge = 10;
int ParanoidAboutGC = 0;
char* DefaultBootDirectory = DEFAULT;
char BootDirectory[MAXPATHLEN] = DEFAULT;
char BootFile[MAXPATHLEN] = "";

/*
 * RestoringImage is set to 0 in the initial boot-rosette image, but it
 * will be set to one in any image dumped after that.  This allows global
 * constructors, such as the one for the virtual machine, to check its
 * value to determine whether they should actually do their work.
 */

int RestoringImage = 0;


void usage(const char* name) {
    suicide(
        "Usage: %s [-t num] [-p [num]] [-is num] [-ss num] [-os num] [-boot "
        "filename]",
        name);
}


void DeleteArgs(int i, int argc, char** argv, int n = 1) {
    for (; i + n < argc; i++)
        argv[i] = argv[i + n];
    argv[i] = 0;
}


int ParseCommandLine(int argc, char** argv) {
    /*
     * ParseCommandLine removes from argv those arguments that are
     * fielded by the Rosette interpreter and returns the adjusted value
     * of argc.  The remaining arguments are expected to passed on to the
     * user's program through the Rosette global variable "argv".  See
     * BigBang.cc for details.
     */

    const char* cmd_name = argv[0];

    /*
     * We always manually reset ParanoidAboutGC in this routine so that
     * its value starts out at 0 even when we are actually restoring a
     * dumped image.
     */

    ParanoidAboutGC = 0;

    DeleteArgs(0, argc, argv); /* Delete argv[0] (the command name) */
    argc--;

    int i = 0;

    while (i < argc) {
        if (strcmp(argv[i], "-t") == 0) {
            if (i + 1 < argc && sscanf(argv[i + 1], "%d", &TenuringAge))
                ;
            else
                usage(cmd_name);
            DeleteArgs(i, argc, argv, 2);
            argc -= 2;
            continue;
        }

        if (strcmp(argv[i], "-p") == 0) {
            int temp = 0;
            if (i + 1 < argc && sscanf(argv[i + 1], "%d", &temp)) {
                ParanoidAboutGC = temp;
                DeleteArgs(i, argc, argv, 2);
                argc -= 2;
            }
            else {
                ParanoidAboutGC = 1;
                DeleteArgs(i, argc, argv);
                argc--;
            }
            continue;
        }

        if (strcmp(argv[i], "-is") == 0) {
            int temp;
            if (i + 1 < argc && sscanf(argv[i + 1], "%d", &temp))
                InfantSpaceSize = temp * 1024;
            else
                usage(cmd_name);
            DeleteArgs(i, argc, argv, 2);
            argc -= 2;
            continue;
        }

        if (strcmp(argv[i], "-ss") == 0) {
            int temp;
            if (i + 1 < argc && sscanf(argv[i + 1], "%d", &temp))
                SurvivorSpaceSize = temp * 1024;
            else
                usage(cmd_name);
            DeleteArgs(i, argc, argv, 2);
            argc -= 2;
            continue;
        }

        if (strcmp(argv[i], "-os") == 0) {
            int temp;
            if (i + 1 < argc && sscanf(argv[i + 1], "%d", &temp))
                OldSpaceChunkSize = temp * 1024;
            else
                usage(cmd_name);
            DeleteArgs(i, argc, argv, 2);
            argc -= 2;
            continue;
        }

        if (strcmp(argv[i], "-boot") == 0) {
            if (i + 1 < argc)
                strcpy(BootFile, argv[i + 1]);
            else
                usage(cmd_name);
            DeleteArgs(i, argc, argv, 2);
            argc -= 2;
            continue;
        }

        /* Otherwise the argument is passed on to the Rosette program. */

        i++;
    }

    return argc;
}
