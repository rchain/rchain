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

#if !defined(_RBL_Location_h)
#define _RBL_Location_h

#include "Ob.h"

class Ctxt;

typedef unsigned char ArgNum;

#define GenericTypeSize 3
#define GenericTagSize 6
#define GenericExcessSize WordSize - (GenericTypeSize + GenericTagSize)

#define CtxtRegIndexSize 4
#define CtxtRegExcessSize GenericExcessSize - CtxtRegIndexSize

#define ArgRegIndexSize 8
#define MaxArgs (1 << ArgRegIndexSize) - 1

#define ArgRegExcessSize GenericExcessSize - ArgRegIndexSize

#define LexLevelSize 5
#define LexOffsetSize 14
#define LexExcessSize GenericExcessSize - (1 + LexLevelSize + LexOffsetSize)

#define AddrLevelSize 5
#define AddrOffsetSize 14
#define AddrExcessSize GenericExcessSize - (1 + AddrLevelSize + AddrOffsetSize)

#define GlobalOffsetSize 16
#define GlobalExcessSize GenericExcessSize - GlobalOffsetSize

#define BitFieldLevelSize 4
#define BitFieldOffsetSize 12
#define BitFieldSpanSize 5

#define BitField00OffsetSize 17
#define BitField00SpanSize 5

enum LocationType {
    LT_CtxtRegister = 0x0,
    LT_ArgRegister = 0x1,
    LT_LexVariable = 0x2,
    LT_GlobalVariable = 0x3,
    LT_BitField = 0x4,
    LT_Limbo = 0x5,
    LT_BitField00 = 0x6,
    LT_AddrVariable = 0x7
};

enum CtxtRegName {
    CRN_Rslt = 0x0,
    CRN_Trgt = 0x1,
    CRN_Argvec = 0x2,
    CRN_Env = 0x3,
    CRN_Code = 0x4,
    CRN_Ctxt = 0x5,
    CRN_Self = 0x6,
    CRN_SelfEnv = 0x7,
    CRN_Rcvr = 0x8,
    CRN_Monitor = 0x9,

    NumberOfCtxtRegs = 0xa
};

#define GENERIC_SIZE (GenericTagSize + GenericTypeSize)

#define GET_GENERIC_TAG(x) GET_LF(x, 0, GenericTagSize)
#define GET_GENERIC_TYPE(x) GET_LF(x, GenericTagSize, GenericTypeSize)

#define GET_CTXTREG_INDEX(x) \
    GET_LF(x, GENERIC_SIZE + CtxtRegExcessSize, CtxtRegIndexSize)

#define GET_ARGREG_INDEX(x) \
    GET_LF(x, GENERIC_SIZE + ArgRegExcessSize, ArgRegIndexSize)

#define GET_LEXVAR_OFFSET(x) GET_LF(x, GENERIC_SIZE, LexOffsetSize)
#define GET_LEXVAR_LEVEL(x) \
    GET_LF(x, GENERIC_SIZE + LexOffsetSize, LexLevelSize)
#define GET_LEXVAR_IND(x) \
    GET_LF(x, GENERIC_SIZE + LexOffsetSize + LexLevelSize, 1)

#define GET_ADDRVAR_OFFSET(x) GET_LF(x, GENERIC_SIZE, AddrOffsetSize)
#define GET_ADDRVAR_LEVEL(x) \
    GET_LF(x, GENERIC_SIZE + AddrOffsetSize, AddrLevelSize)
#define GET_ADDRVAR_IND(x) \
    GET_LF(x, GENERIC_SIZE + AddrOffsetSize + AddrLevelSize, 1)

#define GET_GLOBALVAR_OFFSET(x) \
    GET_LF(x, GENERIC_SIZE + GlobalExcessSize, GlobalOffsetSize)

#define GET_BITFIELD_SPAN(x) GET_LF(x, GENERIC_SIZE, BitFieldSpanSize)
#define GET_BITFIELD_OFFSET(x) \
    GET_LF(x, GENERIC_SIZE + BitFieldSpanSize, BitFieldOffsetSize)
#define GET_BITFIELD_LEVEL(x)                                       \
    GET_LF(x, GENERIC_SIZE + BitFieldSpanSize + BitFieldOffsetSize, \
           BitFieldLevelSize)
#define GET_BITFIELD_SIGN(x)                                         \
    GET_LF(x, GENERIC_SIZE + BitFieldSpanSize + BitFieldOffsetSize + \
                  BitFieldLevelSize,                                 \
           1)
#define GET_BITFIELD_IND(x)                                          \
    GET_LF(x, GENERIC_SIZE + BitFieldSpanSize + BitFieldOffsetSize + \
                  BitFieldLevelSize + 1,                             \
           1)

#define GET_BITFIELD00_SPAN(x) GET_LF(x, GENERIC_SIZE, BitField00SpanSize)
#define GET_BITFIELD00_OFFSET(x) \
    GET_LF(x, GENERIC_SIZE + BitField00SpanSize, BitField00OffsetSize)
#define GET_BITFIELD00_SIGN(x) \
    GET_LF(x, GENERIC_SIZE + BitField00SpanSize + BitField00OffsetSize, 1)

union Location {
    Ob* atom;
    unsigned int locfields;
};

#define SET_CTXTREG_TYPE SET_GENERIC_TYPE
#define SET_ARGREG_TYPE SET_GENERIC_TYPE
#define SET_LEXVAR_TYPE SET_GENERIC_TYPE
#define SET_ADDRVAR_TYPE SET_GENERIC_TYPE
#define SET_GLOBALVAR_TYPE SET_GENERIC_TYPE
#define SET_BITFIELD_TYPE SET_GENERIC_TYPE
#define SET_BITFIELD00_TYPE SET_GENERIC_TYPE
#define SET_BITFIELD00_TAG SET_GENERIC_TYPE

#define SET_GENERIC_TAG(x, val) SET_LF(x, 0, GenericTagSize, val)
#define SET_GENERIC_TYPE(x, val) SET_LF(x, GenericTagSize, GenericTypeSize, val)
#define SET_CTXTREG_INDEX(x, val) \
    SET_LF(x, GENERIC_SIZE + CtxtRegExcessSize, CtxtRegIndexSize, val)
#define SET_ARGREG_INDEX(x, val) \
    SET_LF(x, GENERIC_SIZE + ArgRegExcessSize, ArgRegIndexSize, val)
#define SET_LEXVAR_OFFSET(x, val) SET_LF(x, GENERIC_SIZE, LexOffsetSize, val)
#define SET_LEXVAR_LEVEL(x, val) \
    SET_LF(x, GENERIC_SIZE + LexOffsetSize, LexLevelSize, val)
#define SET_LEXVAR_IND(x, val) \
    SET_LF(x, GENERIC_SIZE + LexOffsetSize + LexLevelSize, 1, val)
#define SET_ADDRVAR_OFFSET(x, val) SET_LF(x, GENERIC_SIZE, AddrOffsetSize, val)
#define SET_ADDRVAR_LEVEL(x, val) \
    SET_LF(x, GENERIC_SIZE + AddrOffsetSize, AddrLevelSize, val)
#define SET_ADDRVAR_IND(x, val) \
    SET_LF(x, GENERIC_SIZE + AddrOffsetSize + AddrLevelSize, 1, val)
#define SET_GLOBALVAR_OFFSET(x, val) \
    SET_LF(x, GENERIC_SIZE + GlobalExcessSize, GlobalOffsetSize, val)
#define SET_BITFIELD_SPAN(x, val) SET_LF(x, GENERIC_SIZE, BitFieldSpanSize, val)
#define SET_BITFIELD_OFFSET(x, val) \
    SET_LF(x, GENERIC_SIZE + BitFieldSpanSize, BitFieldOffsetSize, val)
#define SET_BITFIELD_LEVEL(x, val)                                  \
    SET_LF(x, GENERIC_SIZE + BitFieldSpanSize + BitFieldOffsetSize, \
           BitFieldLevelSize, val)
#define SET_BITFIELD_SIGN(x, val)                                    \
    SET_LF(x, GENERIC_SIZE + BitFieldSpanSize + BitFieldOffsetSize + \
                  BitFieldLevelSize,                                 \
           1, val)
#define SET_BITFIELD_IND(x, val)                                     \
    SET_LF(x, GENERIC_SIZE + BitFieldSpanSize + BitFieldOffsetSize + \
                  BitFieldLevelSize + 1,                             \
           1, val)
#define SET_BITFIELD00_SPAN(x, val) \
    SET_LF(x, GENERIC_SIZE, BitField00SpanSize, val)
#define SET_BITFIELD00_OFFSET(x, val) \
    SET_LF(x, GENERIC_SIZE + BitField00SpanSize, BitField00OffsetSize, val)
#define SET_BITFIELD00_SIGN(x, val) \
    SET_LF(x, GENERIC_SIZE + BitField00SpanSize + BitField00OffsetSize, 1, val)

int operator==(Location loc1, Location loc2);
int operator!=(Location loc1, Location loc2);


extern bool store(Location, Ctxt*, Ob*);
extern Ob* fetch(Location, Ctxt*);
extern void printRep(Location, char*);
extern Ob* valWrt(Location, Ob*);
extern Ob* setValWrt(Location, Ob*, Ob*);
extern void adjustLevel(Location, int);


extern Location CtxtReg(CtxtRegName);
extern Location ArgReg(int);
extern Location LexVar(int, int, int = 0);
extern Location AddrVar(int, int, int = 0);
extern Location GlobalVar(int);
extern Location BitField(int, int, int, int = 0, int = 0);
extern Location BitField00(int, int, int = 0);

extern Location LocLimbo;
extern Location LocRslt;
extern Location LocTrgt;

#endif
