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
 * $Header: /mcc/project/carnot/root/master/pub-ess/src/unexec.c,v 1.1.1.1 1993/02/12 01:25:37 tomlic Exp $
 *
 * $Log: unexec.c,v $
 * Revision 1.1.1.1  1993/02/12  01:25:37  tomlic
 * pub release of rosette
 *
 @EC */

#ifndef __RCS_ID__
#define __RCS_ID__
static const char *rcsid = "$Header: /mcc/project/carnot/root/master/pub-ess/src/unexec.c,v 1.1.1.1 1993/02/12 01:25:37 tomlic Exp $";
#endif

#include "rosette.h"

#define IN_UNEXEC
#ifndef MIPS_SGI_SYSV
char *core_end;
#endif

#include UNEXEC


int 
malloc_debug(n)
{ return 1;
}
/*
force_load_isode_iface()
{ dummy("force_load_isode_iface");
}
*/
force_load_librysap()
{ dummy("force_load_librysap");
}

dummy(x)
     char *x;
{printf("dummy %s\n",x);
}

malloc_verify()
{ return 1;
}

#include <nlist.h>

int
myfcntl(a,b,c)
     int a,b,c;
{ int ff = fcntl(a,b,c);
  printf("(%d)\n",ff);
  return ff;
}
int 
try()
 { 
 { int i = 5;
   FILE *fp = stdin;
   int ch;
   while (--i >=0)
     { ch = getc(fp);
       printf("(%c:%d)\n",ch,ch);
     }
return 0;
 }}


#ifdef NEED_NLIST
#include "our_nlist.c"
#endif


void *
get_symbol_address(file,name)
     char *file,*name;
{ struct nlist bil[4],*b;
  b = &bil[0];
  /* remove the _ or . prefix */
  bil[0].n_name = name+1;
  bil[1].n_name = 0;
  nlist(file,bil);
#ifdef MIPS_SGI_SYSV
  return (void *)bil[0].n_value;
#else
  return bil[0].n_value;
#endif  
}

