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

#if !defined(_RBL_Operation_h)
#define _RBL_Operation_h

#include "rosette.h"
#include "Ob.h"

class StdOprn : public Actor {
    STD_DECLS(StdOprn);

   protected:
    StdOprn(StdExtension*);

   public:
    static StdOprn* create(Ob*, Ob* = RBLFALSE);
    virtual bool isSynchronousTrgt();
    virtual Ob* dispatch(Ctxt*);
};


static const int STDOPRN_ID_SLOT = 0;
static const int STDOPRN_SYNC_SLOT = 1;

static const int BUILTIN_STDOPRN_SLOTS = 2;


class BuiltinOprn {
   private:
    static BuiltinOprn* root;

    char* name;
    bool sync;
    StdOprn** clientOprn;
    Prim** topBinding;
    BuiltinOprn* link;

    void init();

   public:
    BuiltinOprn(char*, char*, StdOprn**, Prim**);

    static void initBuiltinOprns();
};


#define OPRN_NAME(name) name2(_oi_, name)

#define DEF_OPRN(type, ext_name, int_name, top_binding)                 \
    StdOprn* int_name = (StdOprn*)INVALID;                              \
    extern Prim* top_binding;                                           \
    BuiltinOprn OPRN_NAME(int_name)(ext_name, _STRING(type), &int_name, \
                                    &top_binding)


extern StdOprn* oprnRuntimeError;
extern StdOprn* oprnMissingMethod;
extern StdOprn* oprnFormalsMismatch;
extern StdOprn* oprnMissingBinding;
extern StdOprn* oprnVmError;

#endif
