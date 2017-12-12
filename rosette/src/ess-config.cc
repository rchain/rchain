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
 * $Header: /mcc/project/carnot/root/master/pub-ess/src/ess-config.cc,v 1.1.1.1
1993/02/12 01:25:49 tomlic Exp $
 *
 * $Log: ess-config.cc,v $
// Revision 1.1.1.1  1993/02/12  01:25:49  tomlic
// pub release of rosette
//
 @EC */

#ifndef __RCS_ID__
#define __RCS_ID__
static const char *rcsid =
    "$Header: /mcc/project/carnot/root/master/pub-ess/src/ess-config.cc,v "
    "1.1.1.1 1993/02/12 01:25:49 tomlic Exp $";
#endif

extern "C" {
void force_unix_load();
#ifdef ISODE_INTERFACE
void force_load_libisode();
void force_load_isode_iface();
#endif
};

extern "C" {
void configuration_force_load() {
    force_unix_load();
#ifdef ISODE_INTERFACE
    force_load_libisode();
    force_load_isode_iface();
#endif
}
}
