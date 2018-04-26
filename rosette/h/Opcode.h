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

#if !defined(_RBL_Opcode_h)
#define _RBL_Opcode_h

#include "rosette.h"
#include "BinaryOb.h"

/*

halt			0000 0000 xxxx xxxx
push			     0001 xxxx xxxx
pop			     0010 xxxx xxxx
nargs			     0011 nnnn nnnn		nargs <- n
alloc			     0100 nnnn nnnn		argvec <- new Tuple (n)
push/alloc		     0101 nnnn nnnn		push; alloc n
extend			     0110 vvvv vvvv		extend with litvec[v]

outstanding		0000 10pp pppp pppp n:8 x:8	pc <- p;
outstanding <- n
fork			     11pp pppp pppp


xmit/tag		0001 00un mmmm vvvv		unwind if u;
                                                        invoke trgt with m args
and tag = litvec[v]
                                                        nxt if n;
xmit/arg		     01un mmmm aaaa		same, but tag = arg[a]
xmit/reg		     10un mmmm rrrr		same, but tag = reg[r]
xmit			     11un mmmm mmmm		same, but use
current tag

xmit/tag/xtnd		0010 00un mmmm mmmm v:8 x:8	tag = litvec[v]
xmit/arg/xtnd		     01un mmmm mmmm a:8 x:8	tag = arg[a]
xmit/reg/xtnd		     10un mmmm mmmm r:8 x:8	tag = reg[r]
send			     11un mmmm mmmm		tag = limbo


applyprim/tag		0011 00un mmmm mmmm k:8 v:8	*litvec[v] <-
apply prim[k] to m args
applyprim/arg		     01un mmmm mmmm k:8 a:8	arg[a] <- ""
applyprim/reg		     10un mmmm mmmm k:8 r:8	reg[r] <- ""
applycmd		     11un mmmm mmmm k:8 x:8	apply prim[k] to m
args; no result


rtn/tag			0100 00xn vvvv vvvv		return using
litvec[v] as tag
rtn/arg			     01xn aaaa aaaa		use arg[a] as tag
rtn/reg			     10xn rrrr rrrr		use reg[r] as tag
rtn			     11xn xxxx xxxx		use current tag

upcall rtn		0101 00xn vvvv vvvv		use litvec[v] as tag
upcall resume		     0100 xxxx xxxx
nxt			     0101 xxxx xxxx		invoke next strand


jump			0110 00nn nnnn nnnn		pc <- n
jump on #f		     01nn nnnn nnnn		if (rslt == #f) pc <- n
jump/cut		     10nn nnnn nnnn m:8 x:8	pc <- n; cut m levels
off env


lookup to arg		0111 aaaa dvvv vvvv		arg[a] <-
lookup(litvec[v]) with optional (d)efer
lookup to reg		1000 rrrr dvvv vvvv		reg[r] <-
lookup(litvec[v]) with optional (d)efer


xfer lex to arg		1001 illl oooo aaaa		arg[a] <- lex[i,l,o]
xfer lex to reg		1010 illl oooo rrrr		reg[r] <- lex[i,l,o]

xfer global to arg	1011 0000 aaaa aaaa g:16	arg[a] <- global[g]
xfer global to reg	     0001 xxxx rrrr g:16	reg[r] <- global[g]
xfer arg to arg		     0010 dddd ssss		arg[d] <- arg[s]
xfer rslt to arg	     0100 aaaa aaaa		arg[a] <- rslt
xfer arg to rslt	     0101 aaaa aaaa		rslt <- arg[a]
xfer rslt to reg	     0110 xxxx rrrr		reg[r] <- rslt
xfer reg to rslt	     0111 xxxx rrrr		rslt <- reg[r]
xfer rslt to dest	     1000 vvvv vvvv		*litvec[v] <- rslt
xfer src to rslt	     1001 vvvv vvvv		rslt <- *litvec[v]

xfer ind lit to arg	1011 1010 aaaa vvvv		arg[a] <- litvec[v]
xfer ind lit to reg	     1011 rrrr vvvv		reg[r] <- litvec[v]
xfer ind lit to rslt	     1100 vvvv vvvv		rslt <- litvec[v]

xfer imm lit to arg	1100 0vvv aaaa aaaa		arg[a] <- fixnum(v)
                             1000 aaaa aaaa		arg[a] <- #t
                             1001 aaaa aaaa		arg[a] <- #f
                             1010 aaaa aaaa		arg[a] <- nil
                             1011 aaaa aaaa		arg[a] <- #niv

xfer imm lit to reg	1101 0vvv xxxx rrrr		reg[r] <- fixnum(v)
                             1000 xxxx rrrr		reg[r] <- #t
                             1001 xxxx rrrr		reg[r] <- #f
                             1010 xxxx rrrr		reg[r] <- nil
                             1011 xxxx rrrr		reg[r] <- #niv

*/


/*
 * These values can be used as masks to strip out (or mask in)
 * interesting bits in the opcode field of an instruction.  Make sure
 * that you are playing with a meaningful opcode before using them; only
 * formats 4 and 5 conform to this usage.
 */

enum UnwindCode { UnwindOff = 0x0, UnwindOn = 0x2 };
enum NextCode { NextOff = 0x0, NextOn = 0x1 };
enum IndirectCode { IndirectOff = 0x0, IndirectOn = 0x8 };


enum Opcode {
    opHalt = 0x00,      /* Format 0 */
    opPush = 0x01,      /* Format 0 */
    opPop = 0x02,       /* Format 0 */
    opNargs = 0x03,     /* Format 0 */
    opAlloc = 0x04,     /* Format 0 */
    opPushAlloc = 0x05, /* Format 0 */
    opExtend = 0x06,    /* Format 0 */

    opOutstanding = 0x08, /* Format 6 + 16-bit extension */
    opFork = 0x0c,        /* Format 6 */

    opXmitTag = 0x10,     /* Format 4 */
    opXmitArg = 0x14,     /* Format 4 */
    opXmitReg = 0x18,     /* Format 4 */
    opXmit = 0x1c,        /* Format 5 */
    opXmitTagXtnd = 0x20, /* Format 5 + 16-bit extension */
    opXmitArgXtnd = 0x24, /* Format 5 + 16-bit extension */
    opXmitRegXtnd = 0x28, /* Format 5 + 16-bit extension */

    opSend = 0x2c, /* Format 5 */

    opApplyPrimTag = 0x30,
    opApplyPrimArg = 0x34,
    opApplyPrimReg = 0x38,
    opApplyCmd = 0x3c,

    opRtnTag = 0x40,
    opRtnArg = 0x44,
    opRtnReg = 0x48,
    opRtn = 0x4c,

    opUpcallRtn = 0x50,
    opUpcallResume = 0x54,
    opNxt = 0x55,

    opJmp = 0x60,
    opJmpFalse = 0x64,
    opJmpCut = 0x68,

    opLookupToArg = 0x70,
    opLookupToReg = 0x80,

    opXferLexToArg = 0x90,
    opXferLexToReg = 0xa0,

    opXferGlobalToArg = 0xb0,
    opXferGlobalToReg = 0xb1,
    opXferArgToArg = 0xb2,

    opXferRsltToArg = 0xb4,
    opXferArgToRslt = 0xb5,
    opXferRsltToReg = 0xb6,
    opXferRegToRslt = 0xb7,
    opXferRsltToDest = 0xb8,
    opXferSrcToRslt = 0xb9,

    opIndLitToArg = 0xba,
    opIndLitToReg = 0xbb,
    opIndLitToRslt = 0xbc,

    opImmediateLitToArg = 0xc0,

    opImmediateLitToReg = 0xd0,


    MaxOpcodes = 256
};

#define GET_FW(x, y, z) get_field<uint16_t>(x.word, y, z)
#define SET_FW(x, y, z, val) set_field<uint16_t>(&(x.word), y, z, val)


#define OP_f0_op0(x) GET_FW(x, 0, 8)
#define OP_f0_opcode(x) GET_FW(x, 8, 8)

#define OP_f1_op1(x) GET_FW(x, 0, 4)
#define OP_f1_op0(x) GET_FW(x, 4, 4)
#define OP_f1_short(x) GET_FW(x, 8, 8)

#define OP_f2_op1(x) GET_FW(x, 0, 8)
#define OP_f2_op0(x) GET_FW(x, 8, 4)
#define OP_f2_short(x) GET_FW(x, 12, 4)

#define OP_f3_op2(x) GET_FW(x, 0, 4)
#define OP_f3_op1(x) GET_FW(x, 4, 4)
#define OP_f3_op0(x) GET_FW(x, 8, 4)
#define OP_f3_short(x) GET_FW(x, 12, 4)

#define OP_f4_op0(x) GET_FW(x, 0, 4)
#define OP_f4_nargs(x) GET_FW(x, 4, 4)
#define OP_f4_next(x) GET_FW(x, 8, 1)
#define OP_f4_unwind(x) GET_FW(x, 9, 1)
#define OP_f4_nil(x) GET_FW(x, 10, 6)

#define OP_f5_op0(x) GET_FW(x, 0, 8)
#define OP_f5_next(x) GET_FW(x, 8, 1)
#define OP_f5_unwind(x) GET_FW(x, 9, 1)
#define OP_f5_short(x) GET_FW(x, 10, 6)

#define OP_f6_pc(x) GET_FW(x, 0, 10)
#define OP_f6_short(x) GET_FW(x, 10, 6)

#define OP_f7_op0(x) GET_FW(x, 0, 4)
#define OP_f7_offset(x) GET_FW(x, 4, 4)
#define OP_f7_level(x) GET_FW(x, 8, 3)
#define OP_f7_indirect(x) GET_FW(x, 11, 1)
#define OP_f7_short(x) GET_FW(x, 12, 4)

#define OP_e0_op1(x) GET_FW(x, 0, 8)
#define OP_e0_op0(x) GET_FW(x, 8, 8)

#define WORD_OP_e0_op1(x) get_field<uint16_t>(x, 0, 8)
#define WORD_OP_e0_op0(x) get_field<uint16_t>(x, 8, 8)


#define OP_e1_op0(x) GET_FW(x, 0, 16)

/* and now the matching store ops */

#define SET_OP_f0_op0(x, val) SET_FW(x, 0, 8, val)
#define SET_OP_f0_opcode(x, val) SET_FW(x, 8, 8, val)

#define SET_OP_f1_op1(x, val) SET_FW(x, 0, 4, val)
#define SET_OP_f1_op0(x, val) SET_FW(x, 4, 4, val)
#define SET_OP_f1_short(x, val) SET_FW(x, 8, 8, val)

#define SET_OP_f2_op1(x, val) SET_FW(x, 0, 8, val)
#define SET_OP_f2_op0(x, val) SET_FW(x, 8, 4, val)
#define SET_OP_f2_short(x, val) SET_FW(x, 12, 4, val)

#define SET_OP_f3_op2(x, val) SET_FW(x, 0, 4, val)
#define SET_OP_f3_op1(x, val) SET_FW(x, 4, 4, val)
#define SET_OP_f3_op0(x, val) SET_FW(x, 8, 4, val)
#define SET_OP_f3_short(x, val) SET_FW(x, 12, 4, val)

#define SET_OP_f4_op0(x, val) SET_FW(x, 0, 4, val)
#define SET_OP_f4_nargs(x, val) SET_FW(x, 4, 4, val)
#define SET_OP_f4_next(x, val) SET_FW(x, 8, 1, val)
#define SET_OP_f4_unwind(x, val) SET_FW(x, 9, 1, val)
#define SET_OP_f4_nil(x, val) SET_FW(x, 10, 6, val)

#define SET_OP_f5_op0(x, val) SET_FW(x, 0, 8, val)
#define SET_OP_f5_next(x, val) SET_FW(x, 8, 1, val)
#define SET_OP_f5_unwind(x, val) SET_FW(x, 9, 1, val)
#define SET_OP_f5_short(x, val) SET_FW(x, 10, 6, val)

#define SET_OP_f6_pc(x, val) SET_FW(x, 0, 10, val)
#define SET_OP_f6_short(x, val) SET_FW(x, 10, 6, val)

#define SET_OP_f7_op0(x, val) SET_FW(x, 0, 4, val)
#define SET_OP_f7_offset(x, val) SET_FW(x, 4, 4, val)
#define SET_OP_f7_level(x, val) SET_FW(x, 8, 3, val)
#define SET_OP_f7_indirect(x, val) SET_FW(x, 11, 1, val)
#define SET_OP_f7_short(x, val) SET_FW(x, 12, 4, val)

#define SET_OP_e0_op1(x, val) SET_FW(x, 0, 8, val)
#define SET_OP_e0_op0(x, val) SET_FW(x, 8, 8, val)

#define SET_OP_e1_op0(x, val) SET_FW(x, 0, 16, val)

class Instr {
   public:
    Instr() : word(0) {}
    operator int() { return (int)word; }
    uint16_t word;
};

extern char* opcodeStrings[];

#endif
