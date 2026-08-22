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
# $Header: /mcc/project/carnot/root/master/pub-ess/include/mk/SWITCH.mk,v 1.1.1.1 1993/02/12 01:25:55 tomlic Exp $
#
# $Log: SWITCH.mk,v $
# Revision 1.1.1.1  1993/02/12  01:25:55  tomlic
# pub release of rosette
#
# @EC


include default.mk

ifdef rbls
real_rbls      := $(foreach r,$(rbls),$(addsuffix .rbl,$(r)))
endif

ifdef pre_image.rbls
real_pre_image.rbls := $(foreach r,$(pre_image.rbls),$(addsuffix .rbl,$(r)))
endif

ifdef mods
real_mods      := $(foreach m,$(mods),\
		     $(addprefix ../MODULES/,$(addsuffix .module,$(m))))
real_rbls      := $(foreach m,$(mods),\
		     $(foreach r,$($(m).mod), \
			$(addprefix ../$(m)/,$(addsuffix .rbl,$(r)))))
endif

tmp.rbl		= $(RWD)tmp$(ARCH)-$(image_name).rbl

Mods.dir	= $(Rose.dir)/lib/rbl/MODULES

Boot		= $(BASIC.dir)/boot.rbl

ifndef KERNEL
provides	= (Provide \"$(Mods.dir)/\") \
		  (Provide \"../MODULES/\") \
		  (Provide \"./\")
boot_image	= $(bindir)/boot-$(image)-ess
pre_image_deps  = $(boot_image) $(real_pre_image.rbls)
pre_image	= $(bindir)/$(image)-ess.pre-image
ifndef mods
image_mod	= ../MODULES/$(image).module
endif
image_name	= $(image)-ess.image
real_image 	= $(bindir)/$(image)-ess.image
pre_boot_line	= "(seq (prompt: \"PRE:$(prompt)> \") \
			$(provides) \
			(Require \"ess\") \
			(dump-world \"$(pre_image)\"))"
else #KERNEL
pre_image	= $(bindir)/boot-$(image)
image_name	= $(image).image
real_image	= $(bindir)/$(image_name)
booting		= -boot $(Boot) -docs
endif

ifdef provides
prov_req	= $(provides) (Require \"$(image)\")
endif

ifndef boot_line
boot_line	= "(seq (prompt: \"$(prompt)> \") \
			$(prov_req) \
			(dump-world \"$(real_image)\"))" 
endif

image_deps	= $(image_mod) $(real_rbls) $(real_mods) $(pre_image)

all:	$(real_image)

$(real_image): $(image_deps)
	echo $(boot_line) >$(tmp.rbl)
	$(pre_image) $(booting) <$(tmp.rbl)
	- (cd $(bindir); \
	   if [ -f console ]; then \
	      ln console $(image); \
	    fi)
	rm $(tmp.rbl)

ifndef KERNEL
$(pre_image):	$(pre_image_deps)
	(cd $(rosh.dir); \
	 echo  $(pre_boot_line) >$(tmp.rbl); \
	 $(boot_image) -boot $(Boot) -docs <$(tmp.rbl); \
	 rm $(tmp.rbl))
endif

install: $(Trgt.dir) $(Arch.dev.bin)
ifdef Trgt
	- (if [ -f *.module ]; then \
	     cp *.module $(Rbl.mods); \
	   fi)
	(cd $(Trgt.dir); rm -f *.rbl)
	cp *.rbl $(Trgt.dir)/
endif
	(cd $(Arch.dev.bin); rm -f $(image_name))
	- (cd $(bindir); \
	   cp $(image_name) $(Arch.dev.bin); \
	   if [ -f console ]; then \
	      cp console $(Arch.dev.bin); \
	   fi)
	- (cd $(Arch.dev.bin); \
	   if [ -f console ]; then \
	      ln console $(image); \
	   fi)

$(Arch.dev.bin) $(Trgt.dir):
	(if [ ! -d $(@) ]; then \
	   mkdir $(@); \
	 fi)
