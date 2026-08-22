# Mode: -*- MAKEFILE -*-
# @BC
#		                Copyright (c) 1993
#	    by Microelectronics and Computer Technology Corporation (MCC)
#				All Rights Reserved
#
#	Permission to use, copy, modify, and distribute this software and its
#	documentation for any purpose and without fee is hereby granted,
#	provided that this notice be retained unaltered, and that the name of
#	MCC and its shareholders and participants shall not be used in
#	advertising or publicity pertaining to distribution of the software
#	without specific written prior permission.
#
#	THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
#	IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
#	WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#

#
# $Header: /mcc/project/carnot/root/master/pub-ess/include/mk/supp.kern.src.mk,v 1.1.1.1 1993/02/12 01:25:55 tomlic Exp $
#
# $Log: supp.kern.src.mk,v $
# Revision 1.1.1.1  1993/02/12  01:25:55  tomlic
# pub release of rosette
#
# @EC


# supp.rose.src.mk

boot_images	= rosette rosh ess

#ifneq ($(ARCH), hppa)
other_images	= console
#endif

console_objs	= Console.o

lib_nm		= # rbl

lib_objs	= Actor.o RblAtom.o BigBang.o BinaryOb.o Code.o CommandLine.o \
		Compile.o Cstruct.o ForeignFun.o Ctxt.o Dynload.o Dump-world.o \
		Expr.o Heap.o Interrupt.o Labels.o Location.o Mbox.o \
		Meta.o Method.o ModuleInit.o Monitor.o MI.o Number.o \
		Ob.o Operation.o Parser.o Pattern.o Prim.o Proc.o \
		Queue.o RBLstring.o RBLstream.o Reader.o Table.o Tuple.o \
		Vm.o ObQue.o ObStk.o PtrCollect.o ResizeAry.o RblStack.o \
		StreamUtils.o StringStore.o Timer.o misc.o Timestamp.o \
		regexp.o main.o $(_ESS.Extras_)
#		ff-test.o

supps.rosette	= BaseSupp.o
supps.rosh	= $(supps.rosette) InetSupp.o UnixSupp.o

ifdef IsodeInterface
supps.ess	= $(supps.rosh) SocketSupp.o IsodeSupp.o
else
supps.ess	= $(supps.rosh) SocketSupp.o
endif

supps.local	= $(supps.ess)

common_objs	=

supps.external	=

KERNEL		= yes

include SRC.mk

BaseSupp.o:
	$(C++) $(CPPFLAGS) -DBUILDPATH=\"$(parent)\" $(C++FLAGS) -c BaseSupp.cc

include Make.deps
