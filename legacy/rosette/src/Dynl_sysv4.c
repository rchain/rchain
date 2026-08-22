/* Mode: -*- C -*- */
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
 * $Header: /mcc/project/carnot/root/master/pub-ess/src/Dynl_sysv4.c,v 1.1.1.1 1993/02/12 01:25:32 tomlic Exp $
 *
 * $Log: Dynl_sysv4.c,v $
 * Revision 1.1.1.1  1993/02/12  01:25:32  tomlic
 * pub release of rosette
 *
 @EC */

#ifndef __RCS_ID__
#define __RCS_ID__
static const char *rcsid = "$Header: /mcc/project/carnot/root/master/pub-ess/src/Dynl_sysv4.c,v 1.1.1.1 1993/02/12 01:25:32 tomlic Exp $";
#endif

#include "Dynload.h"

#include <nlist.h>

extern "C" {
  int unexec(char*,const char*,int,int,int);
  char *sbrk(int);
  void *get_symbol_address(const char *,const char *);
  int system(char *);
    
  
}

static int failed;
static char *failed_msg;

void fatal(char* fmt,int a1,int a2)
{ failed = 1;
  sprintf(failed_msg,fmt,a1,a2);
  return ;
}

int force_image_to_contain()
{ system("");
  return 0;
}

static
const char *imageName;
int
DynamicLoader::dump (char* outFile,
		     char* msgBuf)
{
  core_end = sbrk(0);
  failed = 0;
  failed_msg= msgBuf;
  unexec(outFile,imageName,0,0,0);
  return failed;
}
  
#define DUMMY return dummy_call();
int 
dummy_call()
{ printf("dummy call\n");
  return 1;
}
  
int 
DynamicLoader::load (const char* objFile,
		     char* msgBuf,
		     const char* libStr,
		     const char* otherStr)
{
  DUMMY;
}


DynamicLoader::DynamicLoader (const char* initial_relocFile)
{
  imageName = initial_relocFile;
}

int
DynamicLoader::loadhelp (const char* ldTemplate,
			 char* msgBuf)
{ return 0;
}



void*
DynamicLoader::resolve (const char* functionName, char* msgBuf)
{
  void *addr = get_symbol_address(imageName,functionName);
  if (addr) return addr;
  else 
  if (msgBuf)
    sprintf(msgBuf, "'%s' not found", functionName);
  return 0;
}


DynamicLoader::~DynamicLoader ()
{
  dummy_call();
}




