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
# $Header: /mcc/project/carnot/root/master/pub-ess/include/mk/COMPONENT.mk,v 1.1.1.1 1993/02/12 01:25:54 tomlic Exp $
#
# $Log: COMPONENT.mk,v $
# Revision 1.1.1.1  1993/02/12  01:25:54  tomlic
# pub release of rosette
#
# @EC


# COMPONENT.mk

Trgts.Parts	:= $(foreach p, $(Parts), $(addsuffix /$(p),$(Dirs)))

include default.defs

include default.mk

.PHONY:		$(Trgts.Parts)

dummy $(Parts): % :	$(addsuffix /%,$(Dirs))

dummy $(Trgts.Parts):
	$(MAKE) -C $(@D) RWD=$(RWD)$(@D) $(@F)
