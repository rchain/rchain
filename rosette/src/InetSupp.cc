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
 * $Header$
 *
 * $Log$
 @EC */

#ifdef __GNUG__
#pragma implementation
#endif

/* Inet-support.cc: provides primitives for manipulating instances of
 * struct in_addr*
 */

/* INCLUDES */

#include "rosette.h"

#include "Ob.h"
#include "Addr.h"
#include <sys/errno.h>
#include <fcntl.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#ifndef NO_SYSENT_H
#include <sysent.h>
#endif

#ifdef SYSV4
#define __STDC__ 1
extern "C" {
#include <arpa/inet.h>
#include <netdb.h>
};
#define __MALLOC_H

#undef __STDC__
#else
#ifdef __GNUG__
extern "C" {
#include <arpa/inet.h>
#include <netdb.h>
};
#else
#include <arpa/inet.h>
#include <netdb.h>
#endif
#endif

#include <stdio.h>
#include <varargs.h>
#include <memory.h>
#include <osfcn.h>

#include "BinaryOb.h"
#include "Ctxt.h"
#include "Operation.h"
#include "Prim.h"
#include "Reader.h"
#include "RBLstream.h"
#include "RBLstring.h"
#include "Tuple.h"
#include "Vm.h"

#ifdef MAP_BACK_ADDRESS
extern uint32_t nontrivial_pre_fixnum_to_addr(int);
extern int nontrivial_addr_to_pre_fixnum(Ob*);
#endif
/*  */
DEF("prim_inet_addr", prim_inet_addr, 1, 1) {
    CHECK(0, RBLstring, str);

    return FIXNUM(inet_addr((char*)&str->byte(0)));
}

DEF("prim_inet_network", prim_inet_network, 1, 1) {
    CHECK(0, RBLstring, str);

    return FIXNUM(inet_network((char*)&str->byte(0)));
}

static local_page_size = getpagesize();

DEF("prim_inet_makeaddr", prim_inet_makeaddr, 3, 3) {
    CHECK_ADDR(0, in_addr_addr);
    CHECK_FIXNUM(1, net);
    CHECK_FIXNUM(2, lna);
    struct in_addr* ap = (struct in_addr*)in_addr_addr;

    if (in_addr_addr >= local_page_size) {
        *ap = inet_makeaddr(net, lna);
        return ARG(0);
    }
    else
        PRIM_ERROR("invalid address");
}

DEF("prim_inet_lnaof", prim_inet_lnaof, 1, 1) {
    CHECK_ADDR(0, in_addr_addr);
    struct in_addr* ap = (struct in_addr*)in_addr_addr;

    if (in_addr_addr >= local_page_size)
        return FIXNUM(inet_lnaof(*ap));
    else
        PRIM_ERROR("invalid address");
}

DEF("prim_inet_netof", prim_inet_netof, 1, 1) {
    CHECK_ADDR(0, in_addr_addr);
    struct in_addr* ap = (struct in_addr*)in_addr_addr;

    if (in_addr_addr >= local_page_size)
        return FIXNUM(inet_netof(*ap));
    else
        PRIM_ERROR("invalid address");
}

DEF("prim_inet_ntoa", prim_inet_ntoa, 1, 1) {
    CHECK_ADDR(0, in_addr_addr);
    struct in_addr* ap = (struct in_addr*)in_addr_addr;

    if (in_addr_addr >= local_page_size)
        return RBLstring::create(inet_ntoa(*ap));
    else
        PRIM_ERROR("invalid address");
}
