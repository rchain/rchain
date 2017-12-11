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
 * $Header: /mcc/project/carnot/root/master/pub-ess/h/sys/bsd.h,v 1.1.1.1
 1993/02/12 01:25:03 tomlic Exp $
 *
 * $Log: bsd.h,v $
 * Revision 1.1.1.1  1993/02/12  01:25:03  tomlic
 * pub release of rosette
 *
 @EC */

#define BSD

#define HAVE_AOUT <a.out.h>

#define MEM_SAVE_LOCALS \
    struct exec header; \
    int stsize

#define READ_HEADER                              \
    fread(&header, sizeof(header), 1, original); \
    data_begin = DATA_BEGIN;                     \
    data_end = core_end;                         \
    original_data = header.a_data;               \
    header.a_data = data_end - data_begin;       \
    header.a_bss = 0;                            \
    fwrite(&header, sizeof(header), 1, save);

#define FILECPY_HEADER filecpy(save, original, header.a_text - sizeof(header));

#define COPY_TO_SAVE                                            \
    filecpy(save, original,                                     \
            header.a_syms + header.a_trsize + header.a_drsize); \
    fread(&stsize, sizeof(stsize), 1, original);                \
    fwrite(&stsize, sizeof(stsize), 1, save);                   \
    filecpy(save, original, stsize - sizeof(stsize))

#define NUMBER_OPEN_FILES getdtablesize()

#define LD_COMMAND(command, main, start, input, ldarg, output)           \
    sprintf(command, "ld -d -N -x -A %s -T %x %s %s -o %s", main, start, \
            input, ldarg, output)
