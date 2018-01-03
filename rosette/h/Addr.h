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

#if !defined(_RBL_Addr_h)
#define _RBL_Addr_h

#ifndef ADDR_TO_PRE_FIXNUM
#ifdef MAP_BACK_ADDRESS
#define ADDR_TO_PRE_FIXNUM(x) nontrivial_addr_to_pre_fixnum(x)
#define PRE_FIXNUM_TO_ADDR(x) nontrivial_pre_fixnum_to_addr(x)
#else
#define ADDR_TO_PRE_FIXNUM(x) ((uint32_t)(int)(x))
#define PRE_FIXNUM_TO_ADDR(x) ((int)(Ob *)(x))
#endif
#endif

#define CHECK_ADDR(n, var)                    \
    if (!IS_FIXNUM(ARG(n)))                   \
        return PRIM_MISMATCH((n), "Address"); \
    uint32_t var = PRE_FIXNUM_TO_ADDR(FIXVAL(ARG(n)));

#define ADDR_TO_FIXNUM(x) FIXNUM(ADDR_TO_PRE_FIXNUM((Ob *)(void *)(x)))

#endif /* _RBL_Addr_h */
