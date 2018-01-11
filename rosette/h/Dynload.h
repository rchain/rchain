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

#if !defined(_RBL_Dynload_h)
#define _RBL_Dynload_h

#ifdef A_OUT_HDR
#include A_OUT_HDR
#endif

class DynamicLoader {
    const char* loaderFile;
    const char* LdTemplate;

#ifdef A_OUT_HDR
    exec myHdr;
#endif

    char relocFile[BUFSIZ];

    int modified;
    short uid;
    short gid;

    int isExecutable(const char* name, short uid, short gid);
    int findCmd(const char* name, char* pathBuf, const char* paths = NULL);
    void rmTmp(FILE*, const char*);

   public:
    DynamicLoader(const char* initial_relocFile);
    ~DynamicLoader();

    int loadhelp(const char* ldString, char* msgBuf);
    int load(const char* objFile, char* msgBuf, const char* libString = "",
             const char* otherString = "");
    int dump(char* outFile, char* msgBuf);
    void* resolve(const char* functionName, char* msgBuf = NULL);

    operator void*();
};

#endif
