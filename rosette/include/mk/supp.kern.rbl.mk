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
# $Header: /mcc/project/carnot/root/master/pub-ess/include/mk/supp.kern.rbl.mk,v 1.1.1.1 1993/02/12 01:26:00 tomlic Exp $
#
# $Log: supp.kern.rbl.mk,v $
# Revision 1.1.1.1  1993/02/12  01:26:00  tomlic
# pub release of rosette
#
# @EC


# supp.ess.rbl.mk

include switch.defs


Dirs		=

Trgt		= ESS

rbls	= sockets tree-spaces rda-aide \
		  manifest-isode isoaddrs isoqos isodb tsap ssap psap \
		  acsap asn.1 acse-listener osi-service-agent
#		  rosap ron rosy

image		= ess

prompt		= ESS

boot_line	= (Provide \"./\") (Require \"$(image)\")

KERNEL		= yes

include ESS.mk
