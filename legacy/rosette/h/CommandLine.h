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

#if !defined(_RBL_CommandLine_h)
#define _RBL_CommandLine_h 1

#include "rosette.h"

extern unsigned SurvivorSpaceSize;
extern unsigned InfantSpaceSize;
extern unsigned OldSpaceChunkSize;

extern int TenuringAge;
extern int ParanoidAboutGC;
extern char BootDirectory[];
extern char BootFile[];
extern char RunFile[];
extern int RestoringImage;
extern bool ForceEnableRepl;
extern int VerboseFlag;
extern int DeferLookupFlag;
extern char ExportFile[];
extern char ImportFile[];

extern int ParseCommandLine(int, char**);

#endif
