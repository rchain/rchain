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
# $Header: /mcc/project/carnot/root/master/pub-ess/include/mk/RblModule.mk,v 1.1.1.1 1993/02/12 01:25:53 tomlic Exp $
#
# $Log: RblModule.mk,v $
# Revision 1.1.1.1  1993/02/12  01:25:53  tomlic
# pub release of rosette
#
# @EC


# RblModule.mk

Dirs		=

include switch.defs

include default.defs

include default.mk

install: $(Trgt.dir) $(Arch.dev.bin)
	 (if [ ! -d $(Trgt.dir) ]; then \
	   mkdir $(Trgt.dir); \
	 fi)
	 (cd $(Trgt.dir); rm -f *.rbl)
	 cp *.rbl $(Trgt.dir)/

# for some reason make is not running the rule below, so it's added in above

$(Arch.dev.bin) $(Trgt.dir):
	(if [ ! -d $(@) ]; then \
	   mkdir $(@); \
	 fi)
