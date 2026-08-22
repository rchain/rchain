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
# $Header: /mcc/project/carnot/root/master/pub-ess/include/mk/default.mk,v 1.1.1.1 1993/02/12 01:25:58 tomlic Exp $
#
# $Log: default.mk,v $
# Revision 1.1.1.1  1993/02/12  01:25:58  tomlic
# pub release of rosette
#
# @EC


# default.mk

cwd	       := $(shell pwd)
parent	       := $(dir $(cwd))

TARGETS		= all install clean clobber

.EXPORT_ALL_VARIABLES:

.PHONY:		$(Dirs) $(TARGETS) \
		$(addprefix default-,$(TARGETS)) \
		$(addprefix local-,$(TARGETS))

$(TARGETS): % : default-% $(addsuffix /%,$(Dirs)) local-%

%/all:;		$(MAKE) -C $(@D) RWD=$(RWD)$(@D) $(@F)

%/install:;	$(MAKE) -C $(@D) RWD=$(RWD)$(@D) $(@F)

%/clean:;	$(MAKE) -C $(@D) RWD=$(RWD)$(@D) $(@F)

%/clobber:;	$(MAKE) -C $(@D) RWD=$(RWD)$(@D) $(@F)

# DO NOT PLACE ANY TARGETS ABOVE THIS LINE

default-clean:;		rm -f *.a *.o a.out core

default-clobber: 	clean
			rm -f *~* "#*#"

default-%:

local-%:
