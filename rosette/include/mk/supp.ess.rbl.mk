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
# $Header: /mcc/project/carnot/root/master/pub-ess/include/mk/supp.ess.rbl.mk,v 1.1.1.1 1993/02/12 01:25:57 tomlic Exp $
#
# $Log: supp.ess.rbl.mk,v $
# Revision 1.1.1.1  1993/02/12  01:25:57  tomlic
# pub release of rosette
#
# @EC


# supp.ess.rbl.mk

KERNEL		= yes

Boot		= ../rosette/boot.rbl

Dirs		=

Trgt		= ess

include ess.rbl.deps

image		= ess

prompt		= Ess

provides	= (Provide \"./\")

include switch.defs

include SWITCH.mk
