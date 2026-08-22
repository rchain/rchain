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
# $Header: /mcc/project/carnot/root/master/pub-ess/include/mk/RBL.mk,v 1.1.1.1 1993/02/12 01:25:55 tomlic Exp $
#
# $Log: RBL.mk,v $
# Revision 1.1.1.1  1993/02/12  01:25:55  tomlic
# pub release of rosette
#
# @EC


# steering makefile for rbl source directories

include default.defs

Dirs		= MODULES $(Configs)

include default.mk

$(Configs):
	$(MAKE) -C $@ RWD=$(RWD)$@/
