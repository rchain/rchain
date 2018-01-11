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

#include "CommandLine.h"

#include <string>
#include <string.h>
#include <sys/param.h>
#include <stdlib.h>
#include <getopt.h>

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
char RunFile[MAXPATHLEN] = "";

/*
 * RestoringImage is set to 0 in the initial boot-rosette image, but it
 * will be set to one in any image dumped after that.  This allows global
 * constructors, such as the one for the virtual machine, to check its
 * value to determine whether they should actually do their work.
 */

int RestoringImage = 0;


void usage(const char* name, bool fatal = false, const char* msg = NULL) {
    fprintf(stderr, "Usage: %s [OPTION]... [FILE]...\n"
            "Run FILEs in the rholang VM.\n"
            "\n"
            " -h, --help             Prints this message and exits.\n"
            " -v, --verbose          Verbose mode\n"
            " -q, --quiet            Disable verbose mode\n"
            " -t, --tenure=NUM_GCS   Number of GCs before tenuring an object\n"
            " -p, --paranoid-gc      Enable paranoid GC\n"
            " -i, --infant-size=KB   RAM to allocate for infant objects\n"
            " -s, --survivor-size=KB RAM to allocate for the survivors\n"
            " -o, --old-size=KB      RAM to allocate for the old generation\n"
            " -d, --boot-dir=DIR     BOOT directory\n"
            " -b, --boot=FILE        BOOT file\n"
            "\n", name);

    if (fatal) { 
        if (msg) {
            suicide("%s", msg);
        } else {
            suicide("Unknown error");
        }
    }
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

    int verbose_flag = 0;
    int c = 0;

    const struct option long_options[] = {
        /* Flags */
        {"verbose", no_argument, &verbose_flag, 1},
        {"quiet", no_argument, &verbose_flag, 0},

        /* Non-flags */
        {"help", no_argument, 0, 'h'},
        {"tenure", required_argument, 0, 't'},
        {"paranoid-gc", required_argument, 0, 'p'},
        {"infant-size", required_argument, 0, 'i'},
        {"survivor-size", required_argument, 0, 's'},
        {"old-size", required_argument, 0, 'o'},
        {"boot-dir", required_argument, 0, 'd'},
        {"boot", required_argument, 0, 'b'},
        {0, 0, 0, 0},
    };

    /*
     * We always manually reset ParanoidAboutGC in this routine so that
     * its value starts out at 0 even when we are actually restoring a
     * dumped image.
     */
    ParanoidAboutGC = 0;

    while (1) {
        int option_index = 0;
        c = getopt_long(argc, argv, "qt:p:i:s:o:b:",
                long_options, &option_index);

        if (-1 == c) {
            break;
        }

        int age = -1;
        size_t chars = 0;

        switch (c) {
            case 0:
                if (long_options[option_index].flag != 0) {
                    break;
                }
                fprintf(stderr, "option %s", long_options[option_index].name);
                if (optarg) {
                    fprintf (stderr, " with arg %s", optarg);
                }
                fprintf (stderr, "\n");
                break;

            case 'h':
                usage(argv[0]);
                exit(0);

            case 't':
                age = std::stoi(optarg);
                TenuringAge = age;
                break;

            case 'p':
                ParanoidAboutGC = std::stoi(optarg);
                break;

            case 'i':
                InfantSpaceSize = std::stoul(optarg, &chars, 10) * 1024;
                break;

            case 's':
                SurvivorSpaceSize = std::stoul(optarg, &chars, 10) * 1024;
                break;

            case 'o':
                OldSpaceChunkSize = std::stoul(optarg, &chars, 10) * 1024;
                break;

            case 'd':
                strncpy(BootDirectory, optarg, MAXPATHLEN);
                break;

            case 'b':
                strncpy(BootFile, optarg, MAXPATHLEN);
                break;

            default:
                // TODO(leaf): We should handle this better, since getopt_long
                // will return '?' or ':' to tell us something about what went
                // wrong.
                usage(argv[0], true, "Invalid argument.");
        }

        //DeleteArgs(0, argc, argv); /* Delete argv[0] (the command name) */

        if (optind < argc) {
            strncpy(RunFile, argv[optind], MAXPATHLEN);
            optind += 1;
        }

        // TODO(leaf): What to do with remaining args?
    }

    return optind;
}
