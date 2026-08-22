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

#if !defined(_RBL_ModuleInit_h)
#define _RBL_ModuleInit_h

#include "rosette.h"

class Module {
   protected:
    static Module* root;

    const char* name;
    Module* link;
    void (*initFn)();

   public:
    Module(const char*, void (*)());

    static void initModules();
};

/*
 * Module objects are used to capture initialization code for a module.
 * The code is captured by invoking the MODULE_INIT macro thusly:
 *
 * 	MODULE_INIT(somename)
 * 	{
 * 	    ...
 * 	    somecode();
 * 	    ...
 * 	}
 *
 * The code in the body is guaranteed to be invoked AFTER all of the
 * heap, meta, sbo, prim, and operation initialization is performed, so
 * the code can refer to just about anything it likes.  However, there is
 * no guarantee about the order in which different modules are
 * initialized, so the code in one MODULE_INIT instance should not depend
 * on things that are initialized by another MODULE_INIT.
 */

#define MODULE_INIT_FN(name) name3(_mi_, name, _initFn)

#define MODULE_INIT(name)                                                     \
    static void MODULE_INIT_FN(name)();                                       \
    static Module name3(_mi_, name, _Module_Instance)(_STRING(name),          \
                                                      &MODULE_INIT_FN(name)); \
    static void MODULE_INIT_FN(name)()


#endif
