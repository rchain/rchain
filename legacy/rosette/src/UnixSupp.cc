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
#include <sys/types.h>
#include <sys/stat.h>

/*  */
extern "C" {
void force_unix_load() {
/*   Sed script for producing the list of foreign calls */
/*
bullwinkle% cat lib/ROSH/*.rbl | sed -e "/(defForeign (\([a-z_]*\)/p" -n | sed
-e "s:(defForeign (\([a-z_0-9]*\).*:DECL(\1):g"
*/


#define DECL(a)         \
    {                   \
        extern int a(); \
        (void)a();      \
    }
    /* UNIX SYSTEM CALLS */
    DECL(alarm)
    DECL(gethostent)
    DECL(unlink)
    DECL(localtime)
    DECL(sethostent)
    DECL(gmtime)
    DECL(asctime)
    DECL(ctime)
    DECL(endhostent)
    DECL(getservbyport)
    DECL(getservent)
    DECL(endservent)
    DECL(setservent)

#ifndef MIPS_SGI_SYSV
    DECL(lstat)
    DECL(fstat)
#endif

    DECL(time)
    DECL(chdir)
    DECL(endgrent)
    DECL(endnetent)
    DECL(endprotoent)
    DECL(endpwent)
    DECL(endrpcent)
    DECL(getgid)
    DECL(getgrent)
    DECL(getgrgid)
    DECL(getgrnam)
    DECL(gethostbyaddr)
    DECL(gethostbyname)
    DECL(gethostname)
    DECL(gethostid)
    DECL(gethostname)
    DECL(getlogin)
    DECL(getnetbyaddr)
    DECL(getnetbyname)
    DECL(getnetent)
    DECL(getpeername)
    DECL(getpgrp)
    DECL(getppid)
#ifndef HPUX
    DECL(getpriority)
#endif
    DECL(getprotobyname)
    DECL(getprotobynumber)
    DECL(getprotoent)
    DECL(getpwent)
    DECL(getpwnam)
    DECL(getpwuid)
    DECL(getrlimit)
    DECL(getrpcbyname)
    DECL(getrpcbynumber)
    DECL(getrpcent)
    DECL(getservbyname)
    DECL(gettimeofday)
    DECL(getpagesize)
    DECL(getuid)
    DECL(kill)
    DECL(link)

#ifndef MIPS_SGI_SYSV
    DECL(mkdir)
#endif

    DECL(readlink)
#ifndef MIPS_SGI_SYSV
    DECL(rename)
#endif
    DECL(rmdir)
    DECL(setgrent)
    DECL(setnetent)
    DECL(setpgrp)
#ifndef HPUX
    DECL(setpriority)
#endif
    DECL(setprotoent)
    DECL(setpwent)
    DECL(setrlimit)
    DECL(setrpcent)
    DECL(statfs)
    DECL(symlink)
#ifndef MIPS_SGI_SYSV
    DECL(system)
#endif
#if defined(sparc) || defined(mc68020)
    DECL(ualarm)
#endif

#ifndef MIPS_SGI_SYSV
    DECL(umask)
    DECL(chmod)
#endif


    /* from running the script apr 14 1992 */
}

typedef long rosette_time_t;
struct rosette_stat {
    rosette_time_t rst_atime;
    rosette_time_t rst_ctime;
    rosette_time_t rst_mtime;
};


int rosette_stat(char *path, struct rosette_stat *buf)

{
    struct stat buf1;
    int result;
    result = stat(path, &buf1);
    if (result == 0) {
#ifdef ROSETTE_STAT_STORE
        ROSETTE_STAT_STORE;
#else
        buf->rst_atime = (&buf1)->st_atime;
        buf->rst_mtime = (&buf1)->st_mtime;
        buf->rst_ctime = (&buf1)->st_ctime;
#endif
    }
    return result;
}
};
