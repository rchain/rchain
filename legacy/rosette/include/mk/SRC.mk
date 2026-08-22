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
# $Header: /mcc/project/carnot/root/master/pub-ess/include/mk/SRC.mk,v 1.1.1.1 1993/02/12 01:25:56 tomlic Exp $
#
# $Log: SRC.mk,v $
# Revision 1.1.1.1  1993/02/12  01:25:56  tomlic
# pub release of rosette
#
# @EC


# SRC.mk

CPPFLAGS	=

Dirs		=

include $(ARCH).defs

include $(ARCH)-src.defs

include default.mk

CPP_DIRS       := $(addprefix -I,$(incl.dirs))

CPPFLAGS       := $(CPPFLAGS) -I. -I../h -I../h/sys $(CPP_DIRS)

CPPFLAGS       := $(CPPFLAGS) -DARCH_INC=\"$(ARCH).h\"

ifdef IsodeInterface
CPPFLAGS       := $(CPPFLAGS) -DISODE_INTERFACE
endif

LDCC	       := $(C++) -g $(W_STATIC)

ifdef lib_nm
library		= lib$(lib_nm).a
endif

LD_DIRS        := -L. $(addprefix -L,$(Base.libdirs)

LD_DIRS	       := $(LD_DIRS) \
		  $(foreach id,$(extern.lib.ids),$(addprefix -L,$($(id).libdir)))

LD_LIBS	       := $(addprefix -l,$(Base.libnms) $(_LIBS.Arch_))

LD_LIBS        := $(LD_LIBS) $(_LIBS.Others_)

LD_LIBS	       := $(foreach id,$(extern.lib.ids),\
			$(foreach nm,$($(id).libnms),$(addprefix -l,$(nm)))) \
		  $(LD_LIBS)

LD_LIBS.others := $(LD_LIBS)

LD_LIBS        := $(ESS_OBJS) $(LD_LIBS)

ifndef KERNEL
ess.obj.deps   := $(ESS_OBJS)
endif

ifdef lib_nm
LD_LIBS	       := -l$(lib_nm) $(LD_LIBS)
LD_LIBS.others := -l$(lib_nm) $(LD_LIBS.others)
endif

real_boots     := $(foreach b, $(boot_images), $(bindir)/boot-$(b))

real_others    := $(foreach o, $(other_images), $(bindir)/$(o))

real_c_others  := $(foreach o, $(other_c_images), $(bindir)/$(o))

all_objs       := $(common_objs) $(supps.local) \
		  $(foreach i, $(boot_images), $(i)-config.o) \
		  $(foreach i, $(other_images), $($(i)_objs)) \
		  $(foreach i, $(other_c_images), $($(i)_objs))

all:		$(library) $(real_boots) $(real_others) $(real_c_others)

ifdef boot_images
$(boot_images): % : $(bindir)/boot-%

$(real_boots): $(library) $(lib_objs) $(all_objs) $(ess.obj.deps)
	if $(LDCC) $(patsubst boot-%,%,$(@F))-config.o \
		$(supps.$(patsubst boot-%,%,$(@F))) \
		$(supps.external) -o $@ $(LD_DIRS) $(LD_LIBS); then \
           echo $@ Built; \
	else \
	   rm -f $@; \
	fi
endif

ifdef other_images
$(other_images): % : $(bindir)/%

$(real_others): $(library) $(all_objs)
	$(LDCC) $($(@F)_objs) -o $@ $(LD_DIRS) $(LD_LIBS.others)
endif

ifdef other_c_images
$(other_c_images): % : $(bindir)/%

$(real_c_others): $(library) $(all_objs)
	$(CC) $($(@F)_objs) -o $@ $(LD_DIRS) $(LD_LIBS.others)
endif

ifndef custom_lib
ifdef lib_nm
$(library): $(foreach o,$(lib_objs),$(library)($(o)))
	ranlib $(library)
endif
endif

ifdef lib_objs
inst-trgts	= inst-lib
endif

ifdef lib_nm
inst-trgts	= inst-lib
endif

ifdef supps.local
inst-trgts     := $(inst-trgts) inst-supps
endif

ifdef boot_images
inst-trgts     := $(inst-trgts) inst-boots
endif

ifdef other_images
inst-trgts     := $(inst-trgts) inst-others
endif

inst-dirs	= $(Arch.dev.dir) $(Arch.dev.bin) $(Arch.dev.lib) \
		  $(Rose.dev.lib) $(Rose.dev.objs) $(Rose.dev.supps)

install: $(inst-dirs) $(inst-trgts)

inst-lib: $(Rose.dev.lib) $(Rose.dev.objs)
	ifdef KERNEL
	  (cd $(Rose.dev.objs); rm -f $(library) $(lib_objs))
	  cp $(library) $(lib_objs) $(Rose.dev.objs)/
	  ifdef library
	    ifdef RANLIB
	      (cd $(Rose.dev.objs); $(RANLIB) $(library))
	    endif
	  endif
	else
	  (cd $(Rose.dev.lib); rm -f $(library))
	  cp $(library) $(Rose.dev.lib)/
	  ifdef library
	    ifdef RANLIB
	      (cd $(Rose.dev.lib); $(RANLIB) $(library))
	    endif
	  endif
	endif

inst-supps: $(Rose.dev.supps)
	(cd $(Rose.dev.supps); rm -f $(supps.local))
	cp $(supps.local) $(Rose.dev.supps)

$(inst-dirs):
	mkdir $@

inst-boots:

inst-others:
	(cd $(Arch.dev.bin); rm -f $(other_images))
	(cd $(bindir); cp $(other_images) $(Arch.dev.bin))

depends:
	get-depends >Make.deps
