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
 * $Header: /mcc/project/carnot/root/master/pub-ess/src/Dynl_hpux.c,v 1.1.1.1 1993/02/12 01:25:28 tomlic Exp $
 *
 * $Log: Dynl_hpux.c,v $
 * Revision 1.1.1.1  1993/02/12  01:25:28  tomlic
 * pub release of rosette
 *
 @EC */

#ifndef __RCS_ID__
#define __RCS_ID__
static const char *rcsid = "$Header: /mcc/project/carnot/root/master/pub-ess/src/Dynl_hpux.c,v 1.1.1.1 1993/02/12 01:25:28 tomlic Exp $";
#endif

#include "Dynload.h"

#include <nlist.h>

extern "C" {
  int unexec(char*,const char*,int,int,int);
/*  char *sbrk(int); */
  void *get_symbol_address(const char *,const char *);

#ifdef HPUX_NOT_EVER_DEFINED
int set_fd_async(int fd,int on)
{  int ls = fd;                  /* SOCK_STREAM listen socket initialized */
   int flag;
   int pid;

   /* set process group for receiving SIGIO on t
   flag = -getpid();		/* process group negative == deliver to process */
   if (ioctl (ls, SIOCSPGRP, &flag) == -1) {
     perror ("can't set pgrp");
     return -1;
   }
   /* make non blocking */
   flag = (on = (on ? 1 : 0)); /* for ioctl, to turn on async */
   ioctl(ls, FIOSNBIO, &flag);
   /* make async */
   flag = on;
   if (ioctl (ls, FIOASYNC, &flag) == -1) {
     perror ("can't set async on socket");
     return -1;
   }

   return 0;
 }
#endif    
  
}

#ifdef MIPS_SGI_SYSV
char* core_end;
#endif

static int failed;
static char *failed_msg;

void fatal(char* fmt,int a1,int a2)
{ failed = 1;
  sprintf(failed_msg,fmt,a1,a2);
  return ;
}

static
const char *imageName;
int
DynamicLoader::dump (char* outFile,
		     char* msgBuf)
{
  core_end = (char *)sbrk(0);
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
{ }

#include <sys/param.h>
extern "C" {
#ifdef __GNUG__
unsigned
#endif
int 
getpagesize()
{ return 4096;
}

#ifndef HAVE_GETDTABLESIZE
getdtablesize()
{ 
  return 24;
}
#endif


#ifndef HAVE_GETWD

char *
getwd(char *buffer)
{
        return(getcwd(buffer, 1024));
}
#endif

void *
valloc(unsigned int size)
{ void *a;
  int i;
#ifndef MIPS_SGI_SYSV
  extern void *malloc(int );
#endif
  int p = getpagesize();
  a = malloc(size+p-1);
  i = (int) a;
  i = p* ((i+p-1)/p);

#ifdef MIPS_SGI_SYSV
  return (void *) i;
#else  
  return (char *) i;
#endif  
}

}
