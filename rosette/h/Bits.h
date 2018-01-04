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

#if !defined(_RBL_Bits_h)
#define _RBL_Bits_h

/* Make a mask for bits i < j, masking j-i bits */
#define MASK_RANGE(i, j) ((~(~0 << (j - i))) << i)

#define GET_FIELD(x, i, wid) (((x)&MASK_RANGE((i), (i + wid))) >> (i))
#define SET_FIELD(x, i, wid, val) \
    ((x) &= (~MASK_RANGE((i), (i + wid))), ((x) |= ((val) << (i))))

#define GET_LF(x, i, wid) GET_FIELD(x.locfields, i, wid)
#define SET_LF(x, i, wid, val) SET_FIELD(x.locfields, i, wid, val)

#define GET_FLAG(x, flag) ((x & (1 << flag)) ? 1 : 0)
#define SET_FLAG(x, flag) (x |= (1 << flag))
#define REMOVE_FLAG(x, flag) (x &= (~(1 << flag)))

#endif /* _RBL_Bits_h */
