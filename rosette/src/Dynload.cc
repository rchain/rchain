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

#if defined(USE_DYNLOAD)
/*
 * The code for DynamicLoader::loadhelp is adapted from the code
 * developed by Dorab Patel for dynamic loading in the T3 system.  See
 * the following (unedited) comment for more details.
 */

/*
 *			D Y N L O A D . C
 *
 * This file contains functions to help dynamically load foreign UNIX files
 * into a running T process.
 *
 * written by Dorab Patel <dorab@neptune.cs.ucla.edu>
 * December 24, 1986
 * Copyright Dorab Patel (C) 1986
 * Permission is given to distribute this software free to anyone
 * using it for a non-commercial purpose. Comments/bug reports/fixes
 * are encouraged.
 *
 * $Revision: 1.1.1.1 $
 *
 * $Log: Dynload.cc,v $
// Revision 1.1.1.1  1993/02/12  01:25:32  tomlic
// pub release of rosette
//
// Revision 1.4  1993/01/19  02:59:28  carnot
// added release copyright
//
// Revision 1.3  1993/01/15  22:50:33  carnot
// sgi fixes refixed
//
// Revision 1.2  1993/01/15  19:19:21  lgreg
// Final cut.
//
// Revision 1.1.1.2  1992/10/23  22:20:12  tomlic
// updated kernel
//
// Revision 1.2  1992/03/04  01:10:27  wfs
// *** empty log message ***
//
// Revision 1.1  1992/01/06  21:05:41  tomlic
// Initial revision
//
// Revision 2.0.1.1  1991/09/23  14:05:15  carnot
// NCR initial version. LGM.
//
// Revision 2.0  1991/09/21  00:16:33  carnot
// Initial SUN configuration.
//
// Revision 1.3  1991/09/19  19:25:12  wfs
// Put prototype for setbuffer in an ifndef c_plusplus
//
// Revision 1.2  1991/09/19  19:02:45  wfs
// CC include files contain prototypes for system
// functions such as `chmod', but they may differ from the ones
// we had by adding a const etc.   We ifndef c_plusplus ours.
//
// Revision 1.1  1991/09/13  16:33:48  carnot
// Initial revision
//
 * Revision 1.1  86/12/24  18:20:44  dorab
 * Initial Revision
 *
 */
#include "rosette.h"
#include "Ob.h"
#ifdef DYNLOAD
#include DYNLOAD
#else
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <errno.h>
/*
 * Not everyone agrees on exactly what sbrk returns.  Maybe someday there
 * will be a standard...
 */

typedef void* sbrk_t;

#include <a.out.h>
int getegid(void);
int geteuid(void);
sbrk_t sbrk(int);

int getpagesize();
int unlink(const char*);

#include "misc.h"
#include "Dynload.h"

/*
 * You may want to verify that this is in fact a good page size for
 * your machine.
 */

#ifndef PAGESIZE
static const int PAGESIZE = 8192;
#endif


#ifndef LOADER
#define LOADER "/bin/ld"
#endif

#if defined(__GNUG__)
typedef char* caddr_t;
#endif


DynamicLoader::DynamicLoader(const char* initial_relocFile) {
    /*
     * If LdTemplate changes, particularly if any more interpreted fields
     * (e.g., %s) are added to it, the computation of ldCmdSpace in
     * loadhelp will probably also need to be changed.
     */

    loaderFile = LOADER;
    LdTemplate = "%s -N -A %%s -T %%lx %s %s -o %%s %s -lc -m32";

    modified = 0;
    uid = geteuid();
    gid = getegid();

    if (findCmd(initial_relocFile, relocFile, getenv("PATH"))) {
        fprintf(stderr, "loader cannot find initial relocation file: %s\n",
                initial_relocFile);
    } else {
        FILE* a_out = fopen(relocFile, "r");
        if (a_out == 0 || fread((char*)&myHdr, sizeof(myHdr), 1, a_out) != 1) {
            suicide("%s: %s\n", relocFile, sys_errmsg());
        }
    }
}


DynamicLoader::~DynamicLoader() {
    if (modified)
        unlink(relocFile);
}


int DynamicLoader::isExecutable(const char* pathname, short uid, short gid) {
    struct stat sbuf;

    return (stat(pathname, &sbuf) == 0 && !(sbuf.st_mode & S_IFDIR) &&
            (sbuf.st_uid == uid && (sbuf.st_mode & S_IEXEC) ||
             sbuf.st_gid == gid && (sbuf.st_mode & 010) ||
             (sbuf.st_mode & 001)));
}


/*
 * findCmd is used to locate the a.out file that generated this process.
 * It expects to be passed argv[0], and then searches the directories in
 * the user's PATH variable until it gets a hit, and then assumes that
 * the hit is the correct file.  The loader needs to have access to this
 * file's symbol table in order to be able to properly link in new object
 * files, and to be able to properly dump an image.  (findCmd is
 * essentially an internal version of /usr/ucb/which, but it is much
 * faster.)
 */

int DynamicLoader::findCmd(const char* name, char* pathBuf, const char* paths) {
    if (!paths) {
        paths = getenv("PATH");
    }

    /*
     * If name is a path already (relative or absolute), we simply use it
     * without consulting the PATH variable.  This is consistent with
     * /usr/ucb/which.
     */

    if (strchr(name, '/')) {
        if (isExecutable(name, uid, gid)) {
            strcpy(pathBuf, name);
            return 0;
        } else {
            pathBuf[0] = 0;
            return 1;
        }
    }

    char* buf = new char[strlen(paths) + 1];
    strcpy(buf, paths);

    for (const char* p = strtok(buf, ":"); p; p = strtok(NULL, ":")) {
        strcpy(pathBuf, p);
        strcat(pathBuf, "/");
        strcat(pathBuf, name);

        if (isExecutable(pathBuf, uid, gid)) {
            return 0;
        }
    }

    delete buf;
    pathBuf[0] = 0;
    return 1;
}


DynamicLoader::operator void*() {
    return (strcmp(relocFile, "") == 0 ? 0 : this);
}


/*
 * load takes an object file (objFile), and loads it into the current
 * process, using relocation information found in the namelist of
 * relocFile. In the process, it will create tmpFile, which has all the
 * current relocation info and which can be used for a subsequent load.
 * In addition, libStr is used to search any required libraries.
 * otherStr can be used for other ld arguments.  Returns 0 if all ok,
 * >0 otherwise.  It closes all files it has opened and unlinks the temp
 * file if there has been an error.
 *
 * TODO:
 * Do the text/data sizes need to be rounded to word boundaries ?
 */


int DynamicLoader::load(const char* objFile, char* msgBuf, const char* libStr,
                        const char* otherStr) {
    /*
     * Calculate the length of the ld command and allocate memory for it.
     * This code needs to be changed if LdCmdTemplate is changed.
     */

    int ldCmdSpace =
        strlen(loaderFile) + strlen(LdTemplate) +
        strlen(objFile) + /* strlen (relocFile) + strlen (tmpName) */
        +strlen(libStr) + strlen(otherStr) +
        16; /* 10 for loadPoint + 1 for null + fudge of 5 */

    char* ldCmd = new char[ldCmdSpace];
    sprintf(ldCmd, LdTemplate, loaderFile, otherStr, objFile, libStr);
    int result = loadhelp(ldCmd, msgBuf);
    delete ldCmd;
    return result;
}


int DynamicLoader::loadhelp(const char* ldTemplate, char* msgBuf) {
    sbrk_t endOfMem, loadPoint, loadCheck;
    long bytesToRead, bytesToExtend;
    char buffer[PAGESIZE];

    /*
     * tempnam mallocs space for the string that it returns; don't forget
     * to free it before leaving.
     */

    /*
     * Calculate the length of the ld command and allocate memory for it.
     * This code needs to be changed if LdCmdTemplate is changed.
     */

    char* tmpName = tempnam(".", "rbl");
    int ldCmdSpace = strlen(ldTemplate) + strlen(relocFile) + strlen(tmpName) +
                     16; /* 10 for loadPoint + 1 for null + fudge of 5 */

    char* ldCmd = new char[ldCmdSpace];
    FILE* tmpFile = 0;
    struct exec hdr;

    int rtnCode = 0;


    if ((endOfMem = sbrk(0)) == (sbrk_t)-1) {
        sprintf(msgBuf, "load: sbrk (0) failed");
        rtnCode = 1;
        goto quit;
    }

#ifdef LOAD_ON_PAGE_BOUNDARIES

    /*
     * there is a strong assumption that pageSize is a power of 2
     */

    /*
     * round up to the next higher pageSize
     */
    assert(PAGESIZE = getpagesize());
    if ((loadPoint = sbrk((int)(PAGESIZE - (endOfMem & (PAGESIZE - 1L))))) ==
        (sbrk_t)-1) {
        sprintf(msgBuf, "load: could not bump upto pagesize");
        rtnCode = 2;
        goto quit;
    }
    loadPoint = sbrk(0);

    /* sanity check */
    if (loadPoint == (sbrk_t)-1 || ((loadPoint & (PAGESIZE - 1L)) != 0L)) {
        sprintf(msgBuf, "load: not page aligned");
        rtnCode = 3;
        goto quit;
    }
#else
    loadPoint = endOfMem;
#endif

    sprintf(ldCmd, ldTemplate, relocFile, loadPoint, tmpName);

    /*
     * run the ld comand to do relocation
     */
    errno = 0;
    if (system(ldCmd) != 0) {
        strcpy(msgBuf, sys_errmsg());
        rtnCode = 4;
        goto quit;
    }

    if ((tmpFile = fopen(tmpName, "r")) == 0) {
        strcpy(msgBuf, sys_errmsg());
        rtnCode = 5;
        goto quit;
    }

    /*
     * Use buf to buffer tmpFile so that no mallocs will occur.
     */
    setbuffer(tmpFile, buffer, sizeof(buffer));

    if (fread((char*)&hdr, sizeof(hdr), 1, tmpFile) != 1) {
        strcpy(msgBuf, sys_errmsg());
        rtnCode = 6;
        goto quit;
    }

    if (N_BADMAG(hdr)) {
        sprintf(msgBuf, "bad magic number %o in %s", hdr.a_magic, tmpName);
        rtnCode = 7;
        goto quit;
    }

    bytesToRead = hdr.a_text + hdr.a_data;
    bytesToExtend = bytesToRead + hdr.a_bss;

    /*
     * Get the required memory
     */
    loadCheck = sbrk((int)bytesToExtend);
    if (loadCheck == (sbrk_t)-1) {
        sprintf(msgBuf, "not enough memory");
        rtnCode = 8;
        goto quit;
    }

    /*
     * Some last sanity checks
     */
    if (loadCheck != loadPoint) {
        sprintf(msgBuf, "load point is 0x%lx but sbrk(0) is 0x%lx",
                (uint32_t)loadPoint, (uint32_t)loadCheck);
        rtnCode = 9;
        goto quit;
    }

#ifdef LOAD_ON_PAGE_BOUNDARIES
    if (loadCheck & (PAGESIZE - 1L)) {
        sprintf(msgBuf, "allocated memory at 0x%lx - not page aligned",
                (uint32_t)loadCheck);
        rtnCode = 10;
        goto quit;
    }
#endif

    /*
     * Go to beginning of text
     */
    if (fseek(tmpFile, (long)N_TXTOFF(hdr), 0)) {
        strcpy(msgBuf, sys_errmsg());
        rtnCode = 11;
        goto quit;
    }

    /*
     * Read the text and data segments in
     */
    if (fread((char*)loadPoint, 1, (int)bytesToRead, tmpFile) != bytesToRead) {
        strcpy(msgBuf, sys_errmsg());
        rtnCode = 12;
        goto quit;
    }

quit:

    delete ldCmd;
    fclose(tmpFile);
    if (rtnCode == 0) {
        if (modified)
            unlink(relocFile);
        strcpy(relocFile, tmpName);
        modified = 1;
    }
    free(tmpName);

    return rtnCode;
}


/*
 * dump takes the name of an output file and a message buffer, and dumps
 * an a.out file containing the current image to the named file.  Failure
 * is indicated by a non-zero return code; a message describing the
 * failure is left in the buffer.
 *
 * A major deficiency is the handling of C++ .o files with global
 * constructors and/or destructors.  These are currently *ignored* since
 * I don't have a good way of finding them, so static objects with
 * constructors will *not* be properly initialized if they are loaded
 * through this interface, nor will they be torn down at program exit.
 * This problem may be corrected in the future.
 */


int DynamicLoader::dump(char* outFile, char* msgBuf) {
    int rtnCode = 0;
    FILE* reloc = 0;
    FILE* out = 0;
    struct exec relocHdr, outHdr;
    uint32_t textStart, dataStart, endOfMem;
    int textSpan, dataSpan, nBytes;
    int usermask;
    struct stat sbuf;

    /*
     * It would be nice to align these on a page boundary, but I don't
     * know how to do it.
     */

    char outBuffer[PAGESIZE];
    char relocBuffer[PAGESIZE];
    char buffer[PAGESIZE];

    if ((reloc = fopen(relocFile, "r")) == 0) {
        strcpy(msgBuf, sys_errmsg());
        rtnCode = 1;
        goto quit;
    }

    /*
     * Make sure that the buffer for reloc io doesn't lie in the region
     * being written out to the a.out file.
     */
    setbuffer(reloc, relocBuffer, sizeof(relocBuffer));

    if (fread((char*)&relocHdr, sizeof(relocHdr), 1, reloc) != 1) {
        strcpy(msgBuf, sys_errmsg());
        rtnCode = 2;
    }

    if ((out = fopen(outFile, "w")) == 0) {
        strcpy(msgBuf, sys_errmsg());
        rtnCode = 3;
        goto quit;
    }

    /*
     * Also make sure that the buffer for the output file doesn't lie in
     * the region being written out.
     */
    setbuffer(out, outBuffer, sizeof(outBuffer));

    endOfMem = (unsigned long)sbrk(0);

    outHdr = myHdr;
    outHdr.a_data = endOfMem - N_DATADDR(myHdr);
    outHdr.a_bss = 0;
    outHdr.a_syms = relocHdr.a_syms;
    outHdr.a_trsize = relocHdr.a_trsize;
    outHdr.a_drsize = relocHdr.a_drsize;

    if (fwrite((char*)&outHdr, sizeof(outHdr), 1, out) != 1) {
        strcpy(msgBuf, sys_errmsg());
        rtnCode = 4;
        goto quit;
    }

    /*
     * Be careful to correct for the header: it is included in N_TXTADDR and
     * a_text, but it has already been written out.
     */

    textStart = N_TXTADDR(outHdr) + sizeof(outHdr);
    textSpan = (int)(outHdr.a_text - sizeof(outHdr));

    if (fwrite((char*)textStart, sizeof(char), textSpan, out) != textSpan) {
        strcpy(msgBuf, sys_errmsg());
        rtnCode = 5;
        goto quit;
    }

    dataStart = N_DATADDR(outHdr);
    dataSpan = (int)outHdr.a_data;

    if (fwrite((char*)dataStart, sizeof(char), dataSpan, out) != dataSpan) {
        strcpy(msgBuf, sys_errmsg());
        rtnCode = 6;
        goto quit;
    }

    if (fseek(reloc, N_TXTOFF(relocHdr) + relocHdr.a_text + relocHdr.a_data,
              L_SET)) {
        strcpy(msgBuf, sys_errmsg());
        rtnCode = 7;
        goto quit;
    }

    while ((nBytes = fread(buffer, 1, sizeof(buffer), reloc)) ==
           sizeof(buffer)) {
        fwrite(buffer, 1, sizeof(buffer), out);
    }

    if (nBytes > 0) {
        fwrite(buffer, 1, nBytes, out);
    }

    if (!feof(reloc)) {
        sprintf(msgBuf,
                "not at end of reloc file after dumping string table\n");
        rtnCode = 8;
        goto quit;
    }

    if (ferror(out)) {
        sprintf(msgBuf, "problem writing symbol table");
        rtnCode = 9;
        goto quit;
    }

    /*
     * Find out the process's umask (and restore it)
     */
    usermask = umask(777);
    umask(usermask);

    if (stat(outFile, &sbuf) == -1) {
        strcpy(msgBuf, sys_errmsg());
        rtnCode = 10;
        goto quit;
    }

    if (chmod(outFile, (sbuf.st_mode | (0111 & ~usermask))) == -1) {
        strcpy(msgBuf, sys_errmsg());
        rtnCode = 11;
        goto quit;
    }

quit:
    if (reloc) {
        fclose(reloc);
    }

    if (out) {
        fclose(out);
        if (rtnCode != 0) {
            unlink(outFile);
        }
    }

    return rtnCode;
}


/*
 * Return the core address of the function functionName by consulting the
 * namelist in the file relocFile.  The calling procedure had better make
 * sure that relocFile exists.
 */


void* DynamicLoader::resolve(const char* functionName, char* msgBuf) {
    void* result = 0;
    FILE* f = 0;
    FILE* sf = 0;
    char* namebuf = 0;

#define QUIT(v)             \
    {                       \
        if (f)              \
            fclose(f);      \
        if (sf)             \
            fclose(sf);     \
        if (namebuf)        \
            delete namebuf; \
        return v;           \
    }

    if ((f = fopen(relocFile, "r")) == NULL) {
        if (msgBuf) {
            strcpy(msgBuf, sys_errmsg());
        }

        QUIT(0);
    }

    struct exec hdr;
    if (fread((char*)&hdr, sizeof(hdr), 1, f) != 1) {
        if (msgBuf) {
            strcpy(msgBuf, sys_errmsg());
        }

        QUIT(0);
    }
    if (N_BADMAG(hdr)) {
        if (msgBuf) {
            sprintf(msgBuf, "'%s' is not an object file", relocFile);
        }

        QUIT(0);
    }

    off_t symoff = N_SYMOFF(hdr);
    off_t stroff = N_STROFF(hdr);

    if (fseek(f, symoff, 0)) {
        if (msgBuf) {
            strcpy(msgBuf, sys_errmsg());
        }
        QUIT(0);
    }

    if ((sf = fopen(relocFile, "r")) == NULL) {
        if (msgBuf) {
            strcpy(msgBuf, sys_errmsg());
        }

        QUIT(0);
    }
    if (fseek(sf, stroff, 0)) {
        if (msgBuf) {
            strcpy(msgBuf, sys_errmsg());
        }

        QUIT(0);
    }

    const int NCHARS = strlen(functionName) + 1;
    namebuf = new char[NCHARS];
    const int NSYMS = BUFSIZ / sizeof(struct nlist);
    struct nlist sym[NSYMS];
    int n = 0;
    int remaining = (int)(hdr.a_syms / sizeof(struct nlist));

    while (remaining > 0) {
        n = remaining < NSYMS ? remaining : NSYMS;
        remaining -= n;
        if (fread((char*)&sym[0], sizeof(struct nlist), n, f) != n) {
            if (msgBuf) {
                strcpy(msgBuf, sys_errmsg());
            }

            QUIT(0);
        }
        for (int i = 0; i < n; i++) {
            /*
             * This mask ensures that we find only extern'ed text
             * entries.  It should be changed if it becomes necessary to
             * look for static entries, data entries, etc.
             */
            const unsigned char mask = N_TEXT + N_EXT;
            if (sym[i].n_un.n_strx == 0 || sym[i].n_type & N_STAB ||
                (sym[i].n_type & mask) != mask) {
                continue;
            }

            if (fseek(sf, stroff + sym[i].n_un.n_strx, 0)) {
                if (msgBuf) {
                    strcpy(msgBuf, sys_errmsg());
                }

                QUIT(0);
            }

            if (fread(namebuf, sizeof(char), NCHARS, sf) == 0) {
                if (msgBuf) {
                    strcpy(msgBuf, sys_errmsg());
                }

                QUIT(0);
            }

            if (strcmp(functionName, namebuf) == 0) {
                QUIT((void*)sym[i].n_value);
            }
        }
    }

    if (msgBuf) {
        sprintf(msgBuf, "'%s' not found", functionName);
    }

    QUIT(0);

#undef QUIT
}

#endif
#endif
