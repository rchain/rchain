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
 * $Header: /mcc/project/carnot/root/master/pub-ess/src/unexec1.c,v 1.1.1.1 1993/02/12 01:25:51 tomlic Exp $
 *
 * $Log: unexec1.c,v $
 * Revision 1.1.1.1  1993/02/12  01:25:51  tomlic
 * pub release of rosette
 *
 @EC */

#ifndef __RCS_ID__
#define __RCS_ID__
static const char *rcsid = "$Header: /mcc/project/carnot/root/master/pub-ess/src/unexec1.c,v 1.1.1.1 1993/02/12 01:25:51 tomlic Exp $";
#endif

#include <stdio.h>
#include "config.h"  

#ifdef HAVE_FCNTL
#include <fcntl.h>
#else
#include <sys/file.h>
#endif

#ifdef HAVE_AOUT
#undef BSD
#undef ATT
#define BSD
#include HAVE_AOUT
#endif


#ifdef ATT
#include <filehdr.h>
#include <aouthdr.h>
#include <scnhdr.h>
#endif



filecpy(to, from, n)
FILE *to, *from;
int n;
{ char buf[BUFSIZ];
  int m ;
  while(n > 0)
    {  m = fread(buf,1,(n >BUFSIZ ? BUFSIZ : n),from);
       if (m==0) error("error in file_copy");
       n -= m;
       if (m != fwrite(buf,1,m,to))
	 error("error in file_copy");
     }
}


/* actually write out the file we are saving */
tes()
{	fprintf(stderr,"ftell %d \n",ftell(stdin));
 }

unexec(save_file,original_file)
char *original_file, *save_file;
{	MEM_SAVE_LOCALS;
	char *data_begin, *data_end;
	int original_data,n;
	FILE *original, *save;
	extern char *sbrk();

	/* close the file descriptors */

	_cleanup();

	original = freopen(original_file,"rb",stdin);
	if (original == 0 ) {
	  fprintf(stderr, "Can't reopen the original file.\n");
	  exit(1);
	}

	unlink(save_file);

	save = freopen(save_file,"wb",stdout);
	if (save == 0) {
	  fprintf(stderr, "Can't reopen the save file.\n");
	  exit(1);
	}

	setbuf(original, stdin_buf);
	setbuf(save, stdout_buf);

	READ_HEADER;
	FILECPY_HEADER;
	
	/* copy up to beginning of data */
	{ int m;
	  char *d = data_begin;
	  n = header.a_data;
	  while(n > 0)
	    { m = (n > BUFSIZ ? BUFSIZ : n);
	      if (m != fwrite(d,1,m,save))
		error("error in save");
	      d += m;
	      n -= m;
	    }}

	fseek(original, original_data, 1);

	COPY_TO_SAVE;
	fclose(original);
	fclose(save);
	chmod(save_file,0755);
}

error1(s)
 char *s;
{ abort();
}
