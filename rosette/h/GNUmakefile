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
# $Header: /mcc/project/carnot/root/master/pub-ess/h/GNUmakefile,v 1.1.1.1 1993/02/12 01:24:53 tomlic Exp $
#
# $Log: GNUmakefile,v $
# Revision 1.1.1.1  1993/02/12  01:24:53  tomlic
# pub release of rosette
#
# @EC


################################################################
# local defs
################################################################

includes	= BinaryOb.h		Method.h		RblAtom.h \
		Bits.h			ModuleInit.h		RblStack.h \
		BuiltinClass.h		Monitor.h		Reader.h \
		Code.h			Number.h		ResizeAry.h \
		CommandLine.h		Ob.h			ResizeablePtrArray.h \
		Compile.h		ObQue.h			StreamUtils.h \
		Cstruct.h		ObStk.h			Table.h \
		Ctxt.h			Opcode.h		Timer.h \
		Dynload.h		Operation.h		Timestamp.h \
		Expr.h			Parser.h		Tuple.h \
		ForeignFun.h		Pattern.h		Unix.h \
		GNUmakefile		Prim.h			Vm.h \
		Heap.h			Proc.h			assert.h \
		Interrupt.h		PtrCollect.h		misc.h \
		Labels.h		PtrCollection.h		osfcn.h \
		Location.h		PtrQue.h		regexp.h \
		MI.h			Queue.h			Mbox.h \
		RBLstream.h		Meta.h			RBLstring.h \
		sys

################################################################
# local rules follow
################################################################

Parts		= rosette rosh ess

include H.mk

install: $(Rose.dev.inc)
	(cd $(Rose.dev.inc); rm -rf $(includes))
	(cp -r $(includes) $(Rose.dev.inc))

$(Rose.dev.inc): $(dir $(Rose.dev.inc))
	mkdir $(Rose.dev.inc)

$(dir $(Rose.dev.inc)):
	mkdir $(dir $(Rose.dev.inc))
