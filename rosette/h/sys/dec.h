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
 * $Header: /mcc/project/carnot/root/master/pub-ess/h/sys/dec.h,v 1.1.1.1
 1993/02/12 01:25:03 tomlic Exp $
 *
 * $Log: dec.h,v $
 * Revision 1.1.1.1  1993/02/12  01:25:03  tomlic
 * pub release of rosette
 *
 @EC */

#ifndef CONFIG_INCLUDED
#define CONFIG_INCLUDED

#define ARCH "dec"
#define MACHINE "dec-mips"
#define OS "ultrix"

typedef void (*SIG_PF)(int);

/* flag to use with fcntl to give non blocking io
   the ~ of it should give blocking io
 */
#define FCNTL_NONBLOCK (FNDELAY | FASYNC)

/* You must supply something to cause the current process
   to receive the io signal when input is available on fd.
   return is < 0 on failure.
*/
#define SET_SIGNAL_IO_DESIRED(result) \
    result = (desiredState ? fcntl(fd, F_SETOWN, -getpid()) : 0)

/* continue with no error (but print message) when async fails,
   eg if on a file descriptor
*/
#define ALLOW_ASYNC_ERRORS

#ifdef __cplusplus
#ifndef __GNUG__
extern "C" {
int setpgrp();
int getpid();
int execve(const char *, const char **, char **);
int getpagesize();
int strcasecmp();
int getdtablesize();
void *sbrk();
int setsid();
}
#endif
#endif

#define UNEXEC "unexmips.c"

#define DYNLOAD "Dynl_hpux.c"


#define HAVE_GETWD
#define HAVE_GETRUSAGE
#define HAVE_GETDTABLESIZE

char *core_end;

#define DATA_START 0x10000000
#define TEXT_START 0x00400000

#endif

/* the dec nlist is broken */
#define NEED_NLIST


#define RBL_WOULDBLOCK ((errno == EWOULDBLOCK) || (errno == EAGAIN))


/* sigio signal not sent on io on socket from remote machine */
/* #define NEED_ALARM */

#ifdef __cplusplus
extern "C" {
int unlink();
};
#endif

#define LD_COMMAND(buf, main, start, input, ldarg, extra, output)           \
    sprintf(buf, "ld -s -A %s -N -T %x %s %s %s -o %s", main, start, input, \
            ldarg, extra, output)


#define FASL_HEADERS             \
    struct filehdr faslheader;   \
    struct aouthdr aouthdr;      \
    struct scnhdr sectionheader; \
    HDRR symhdr

#define READ_HEADERS                 \
    fseek(fp, FILHSZ, SEEK_CUR);     \
    fread(&aouthdr, AOUTHSZ, 1, fp); \
    fread(&sectionheader, sizeof sectionheader, 1, fp)
#define COMPUTE_LOAD_SIZE \
    bytesToExtend = aouthdr.tsize + aouthdr.dsize + aouthdr.bsize


#define COPY_IN_INSTRS                    \
    fseek(fp, sectionheader.s_scnptr, 0); \
    fread((char *)sectionheader.s_vaddr, bytesToExtend, 1, fp);


#define PAGESIZE (1 << 12)

#define SYS_EXEC <aouthdr.h>
#define DYN_MEMBERS
