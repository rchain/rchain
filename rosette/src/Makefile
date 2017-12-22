# Mode: -*- MAKEFILE -*-
# @BC
#		                Copyright (c) 1996
#	                      by Rosette WebWorks Inc.
#				All Rights Reserved
#
#	Permission to use, copy, modify, and distribute this software and its
#	documentation for any purpose and without fee is hereby granted,
#	provided that this notice be retained unaltered, and that the name of
#	RWI shall not be used in advertising or publicity pertaining to
#	distribution of the software without specific written prior permission.
#
#	THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
#	IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
#	WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#

#
# $Header$
#
# $Log$
#
# @EC

ARCH		= linux

# Using -Wall with some warnings overridden for now:
#  -Wno-write-strings				Until we can deal with the use of char* n = "FooBar"; type of constructs
#  -Wno-invalid-offsetof			Until a better understanding of nested macro usage is had
#  -Wno-delete-incomplete			Until a better understand of the weird use of pointers and memory management in the code is had
#  -Wno-deprecated-declarations		Until we can refactor to remove the deprecated behaviors
#  -Wno-unused-variables			Until a better understanding of nested macro usage is had
#  -Wno-sign-compare				Until a better understanding of nested macro usage is had
#  -Wno-init-self                      Until I understand why he's doing this and why removing it breaks things
#
#  The plan is to eventually compile with -Wall and -Werror
#
CPPFLAGS	= -std=c++11 -ggdb -I ../h -I ../h/sys -DARCH_INC=\"$(ARCH).h\" -m32 -Wall -Wno-write-strings -Wno-invalid-offsetof -Wno-delete-incomplete -Wno-deprecated-declarations -Wno-unused-variable -Wno-sign-compare -Wno-init-self

#LDCC		= gcc -g $(W_STATIC) -DGCC27X -lm
#LDCC		= g++ -g $(W_STATIC) -DGCC27X -lm -m32
LDCC		= g++ -ggdb $(W_STATIC) -DGCC27X -lm -m32

OBJS		= Actor.o RblAtom.o BigBang.o BinaryOb.o Code.o CommandLine.o \
		Compile.o Cstruct.o ForeignFun.o Ctxt.o Dynload.o Dump-world.o \
		Expr.o Heap.o Interrupt.o Labels.o Location.o Mbox.o \
		Meta.o Method.o ModuleInit.o Monitor.o MI.o Number.o \
		Ob.o Operation.o Parser.o Pattern.o Prim.o Proc.o \
		Queue.o RBLstring.o RBLstream.o Reader.o Table.o Tuple.o \
		Vm.o ObQue.o ObStk.o PtrCollect.o ResizeAry.o RblStack.o \
		StreamUtils.o StringStore.o Timer.o misc.o Timestamp.o \
		Opcode.o main.o $(_ESS.Extras_)


PARENT		= $(dir $(cwd))

all:		$(OBJS) BaseSupp.o rosette-config.o
	$(LDCC) $(OBJS) BaseSupp.o rosette-config.o -o ../bin/boot-ess

BaseSupp.o:
	$(CXX) $(CPPFLAGS) -DBUILDPATH=\"$(PARENT)\" $(C++FLAGS) -c BaseSupp.cc

#include Make.deps
