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

#if !defined(_RBL_BuiltinClass_h)
#define _RBL_BuiltinClass_h

#include "Ob.h"


typedef Ob* pOb;
typedef Actor* pSBO;
typedef StdMeta* pMeta;

class BuiltinClass;
typedef void (*FIELD_FN)(BuiltinClass*);

static const int INDIRECT = 1;


class BuiltinClass {
    static int nClasses;
    static BuiltinClass* root;
    static uint32_t* counts;
    static char** names;


    int const index;
    char* const name;
    pMeta* const clientMeta;
    pSBO* const clientSBO;
    BuiltinClass* const link;
    FIELD_FN const fieldfn;

    void alloc();
    void init();
    void enter();

   public:
    BuiltinClass(char*, pMeta*, pSBO*, FIELD_FN);

    static void allocBuiltinClasses();
    static void initBuiltinClasses();
    static void enterBuiltinClasses();

    void obfield(const char*, int, int);
    void addrfield(const char*, int, int);
    void bitfield(const char*, int, int, int);
};


#define STARTUP_NAME(classname) name2(_bcf_bc_, classname)

#define BUILTIN_CLASS(classname)                                           \
    void classname::updateCnt() {}                                         \
    char* classname::typestring() { return _STRING(classname); }           \
    pMeta CLASS_META(classname) = (pMeta)INVALID;                          \
    pSBO CLASS_SBO(classname) = (pSBO)INVALID;                             \
    static void STARTUP_NAME(classname)(BuiltinClass*);                    \
    static BuiltinClass name2(_bc_, classname)(                            \
        _STRING(classname), &CLASS_META(classname), &CLASS_SBO(classname), \
        &STARTUP_NAME(classname));                                         \
    static void STARTUP_NAME(classname)(BuiltinClass * __BUILTIN_CLASS__)

#define OB_FIELD(ext_fieldname, classname, fieldname)                         \
    __BUILTIN_CLASS__->obfield(ext_fieldname, SLOT_NUM(classname, fieldname), \
                               !INDIRECT)

#define OB_FIELD_INDIRECT(ext_fieldname, offset) \
    __BUILTIN_CLASS__->obfield(ext_fieldname, offset, INDIRECT)

#define ADDR_FIELD(ext_fieldname, classname, fieldname) \
    __BUILTIN_CLASS__->addrfield(ext_fieldname,         \
                                 SLOT_NUM(classname, fieldname), !INDIRECT)

#define ADDR_FIELD_INDIRECT(ext_fieldname, offset) \
    __BUILTIN_CLASS__->addrfield(ext_fieldname, offset, INDIRECT)

#define BIT_FIELD(ext_fieldname, classname, fieldname, sz)                   \
    __BUILTIN_CLASS__->bitfield(ext_fieldname,                               \
                                offsetof(classname, fieldname) * BITS(char), \
                                sz, !INDIRECT)


#endif
