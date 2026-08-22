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
 * $Header: /mcc/project/carnot/root/master/pub-ess/src/our_nlist.c,v 1.1.1.1 1993/02/12 01:25:32 tomlic Exp $
 *
 * $Log: our_nlist.c,v $
 * Revision 1.1.1.1  1993/02/12  01:25:32  tomlic
 * pub release of rosette
 *
 @EC */

#ifndef __RCS_ID__
#define __RCS_ID__
static const char *rcsid = "$Header: /mcc/project/carnot/root/master/pub-ess/src/our_nlist.c,v 1.1.1.1 1993/02/12 01:25:32 tomlic Exp $";
#endif

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef MIPS_SGI_SYSV
#include <nlist.h>
#endif  


  
#ifdef __STDC__  
#define assert(x) do { if (x < 0) {fprintf(stderr,"assertion failed %s %d", \
					   __FILE__, __LINE__);}}while(0) 
#else
#define assert(x)
#endif

#ifdef STANDALONE_TEST
#include <nlist.h>
main(argc,argv)
  char *argv[];
{ struct nlist bil[4],*b;
  bil[0].n_name=argv[2];
  bil[1].n_name=0;
  nlist(argv[1],&bil);
  printf("%x %s",bil[0].n_value,bil[0].n_name);
  
}

#endif

int nlist_initialized;



struct node { char *name;
	      int value;}
;

struct { struct node *nodes;
	  int length;
	 char * string_table;
	 int string_table_length;
       }
  nlist_table;

node_compare(node1,node2)
char *node1, *node2;
{ return(strcmp( ((struct node *)node1)->name,
	         ((struct node *)node2)->name));}


nlist(file,lis)
char *file;
struct nlist *lis;
{ struct node sym,*answ;
  int i;
  if (nlist_initialized==0)
    { initialize_nlist(file);}
  while(lis->n_name)
    { int m;
      sym.name = lis->n_name;
      m = nlist_table.length;
      answ = bsearch((char *)&sym,nlist_table.nodes,m,
		     sizeof(sym),node_compare);
      if (answ)
	lis->n_value = answ->value;
      else lis->n_value = 0;
      lis++;
    }

  return 0;
}

#ifndef NM_STRING
/* produce table that looks like

012af3fd T malloc
412af3fd x our_buffer
...

*/
   
   
#define NM_STRING "nm -p %s | grep ' [TD] ' > %s "

#endif

initialize_nlist(file)
     char *file;
{ char buf[200];
  char tmpfile[200],*p;
  struct node *next;
  FILE *fp;
  int leng;
  int m,ch;
  nlist_table.length = 0;

  if (nlist_table.string_table)
    free(nlist_table.string_table);
  if (nlist_table.nodes)
    free(nlist_table.nodes);
  sprintf(tmpfile,"/tmp/%dros",getpid());
  sprintf(buf,NM_STRING,file,tmpfile);
  if (system(buf))
    { perror("can't nlist");    }
  if (0 >= (fp = fopen(tmpfile,"r")))
    { perror("can't nlist"); }
  { struct stat filestatus; 
    fstat(fileno(fp), &filestatus); 
    leng = filestatus.st_size;
  }
  
  p = nlist_table.string_table=malloc(leng);
  next = nlist_table.nodes = (struct node *)malloc(leng);



  while(fscanf(fp,"%x %c %s",&next->value,&ch,p)==3)
    { int l = strlen(p) +1;
      next->name=p;

      p += l;
      nlist_table.string_table_length += l;
      assert(nlist_table.string_table_length < leng);
      nlist_table.length ++;
      next ++;
      assert ((next - nlist_table.nodes)*sizeof(struct node) < leng);
    }
  m = nlist_table.length;
  
  qsort((char*)(nlist_table.nodes),
	(int)(nlist_table.length),sizeof(struct node),node_compare);
  fclose(fp);
  unlink(tmpfile);
  nlist_initialized =1;
}
