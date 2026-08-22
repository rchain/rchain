###############################################################################
#   Instructions to Make, for compilation of ISODE password lookup demo
###############################################################################

###############################################################################
#
# $Header: /mcc/project/carnot/root/master/pub-ess/isode/rbllookup/Makefile,v 1.1.1.1 1993/02/12 01:26:23 tomlic Exp $
#
#
# $Log: Makefile,v $
# Revision 1.1.1.1  1993/02/12  01:26:23  tomlic
# pub release of rosette
#
# Revision 1.3  1993/01/19  21:02:37  carnot
# Touch up for release 2.0
#
# Revision 7.0  89/11/23  22:56:32  mrose
# Release 6.0
# 
###############################################################################

###############################################################################
#
#				 NOTICE
#
#    Acquisition, use, and distribution of this module and related
#    materials are subject to the restrictions of a license agreement.
#    Consult the Preface in the User's Manual for the full terms of
#    this agreement.
#
###############################################################################


###############################################################################
# Options
###############################################################################

LIBDIR	=	$(ARCHPFX)/lib/rblisode/

XFLAGS=		$(CFLAGS) -DPEPYPATH

###############################################################################
# Generation Rules for program modules
###############################################################################

.c.o:;		$(CC) $(XFLAGS) -c $*.c


###############################################################################
# Programs and Libraries
###############################################################################

LIBES	=	$(TOPDIR)libisode.a
LLIBS   =	$(TOPDIR)llib-lisode


###############################################################################
# Files
###############################################################################

HFILES	=	ryinitiator.h ryresponder.h
CFILES	=	lookupd.c lookup.c ryinitiator.c ryresponder.c
RYFILES	=	lookup.ry


##################################################################
# Here it is...
##################################################################

all:		lookupd lookup PasswordLookup-RIops.o
inst-all:	inst-rbllookup # inst-lookupd inst-lookup manuals
install:	inst-all clean
lint:		l-lookupd l-lookup


##################################################################
# lookupd
##################################################################

inst-lookupd:	$(SBINDIR)ros.lookup

$(SBINDIR)ros.lookup:	lookupd
		-cp $@ zros.lookup
		-rm -f $@
		cp lookupd $@
		-@ls -gls $@
		-@echo ""

lookupd:	lookupd.o PasswordLookup-Rops.o ryresponder.o liblookup.a
		$(LDCC) $(LDFLAGS) -o $@ lookupd.o \
			PasswordLookup-Rops.o ryresponder.o liblookup.a \
			$(LIBES) $(LSOCKET)

l-lookupd:	PasswordLookup-ops.c LOOKUP-C true
		$(LINT) $(LFLAGS) -DPERFORMER lookupd.c \
			PasswordLookup-ops.c ryresponder.c \
			$(LOOKUP-C) $(LLIBS)

lookupd.o:	ryresponder.h PasswordLookup-ops.h \
			PasswordLookup-types.h

PasswordLookup-Rops.o:	PasswordLookup-ops.c PasswordLookup-ops.h
		$(CC) $(XFLAGS) -DPERFORMER -c PasswordLookup-ops.c
		mv PasswordLookup-ops.o $@

ryresponder.o:	ryresponder.h


##################################################################
# lookup
##################################################################

inst-lookup:	$(BINDIR)lookup

$(BINDIR)lookup:	lookup
		-cp $@ zlookup
		-rm -f $@
		cp lookup $@
		-@ls -gls $@
		-@echo ""

lookup:		lookup.o PasswordLookup-Iops.o ryinitiator.o liblookup.a
		$(LDCC) $(LDFLAGS) -o $@ lookup.o PasswordLookup-Iops.o \
			ryinitiator.o liblookup.a $(LIBES) $(LSOCKET)

l-lookup:	PasswordLookup-ops.c LOOKUP-C true
		$(LINT) $(LFLAGS) -DINVOKER lookup.c PasswordLookup-ops.c \
			ryinitiator.c $(LOOKUP-C) PasswordLookup-stubs.c \
			$(LLIBS)

lookup.o:	ryinitiator.h PasswordLookup-ops.h PasswordLookup-types.h

PasswordLookup-Iops.o:	PasswordLookup-ops.c PasswordLookup-ops.h
		$(CC) $(XFLAGS) -DINVOKER -c PasswordLookup-ops.c
		mv PasswordLookup-ops.o $@

PasswordLookup-RIops.o:	PasswordLookup-ops.c PasswordLookup-ops.h
		$(CC) $(XFLAGS) -DINVOKER -DPERFORMER -c PasswordLookup-ops.c
		mv PasswordLookup-ops.o $@

ryinitiator.o:	ryinitiator.h


################################################################
# liblookup
################################################################

liblookup.a:	LOOKUP-O
		-rm -f $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $(ARFLAGS) $@ $(LOOKUP-O) 
		-@ls -l $@
		-@echo "RBLLOOKUP library built normally"

LOOKUP-O =	LOOKUP-[0-9]*.o
LOOKUP-C =	LOOKUP-[0-9]*.c

LOOKUP-O:	LOOKUP-C
		@$(MAKE) `/bin/ls $(LOOKUP-C) | sed 's/\.c$$/.o/'`
		-@touch $@

LOOKUP-C:	PasswordLookup-types.py $(TOPDIR)pepy/xpepy
		-@rm -f $(LOOKUP-C) $(LOOKUP-O)
		$(TOPDIR)pepy/xpepy -a ryr_advise -m -A -b LOOKUP $(PYFLAGS) \
			PasswordLookup-types.py
		-@touch $@

PasswordLookup-types.py:	PasswordLookup-asn.py $(TOPDIR)pepy/xposy
		$(TOPDIR)pepy/xposy -f -h -m -o $@ $(POFLAGS) \
			PasswordLookup-asn.py
PasswordLookup-types.h:	PasswordLookup-types.py

PasswordLookup-asn.py:	lookup.ry $(TOPDIR)rosy/xrosy
		$(TOPDIR)rosy/xrosy $(RYFLAGS) -m -o $@ lookup.ry
PasswordLookup-ops.c:	lookup.ry
PasswordLookup-ops.h:	lookup.ry
PasswordLookup-stubs.c:	lookup.ry

inst-rbllookup:	liblookup.a
		cp PasswordLookup-RIops.o $(LIBDIR)
		rm -f $(LIBDIR)librbllookup.a
		cp liblookup.a $(LIBDIR)librbllookup.a
		$(UTILDIR)make-lib.sh $(SYSTEM) $(LIBDIR)librbllookup.a -ranlib
		ls -gls $(LIBDIR)librbllookup.a


################################################################
# manual pages
################################################################

MANUALS	=	lookupd.8c lookup.1c

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


################################################################
# clean
################################################################

clean:;		rm -f *.o *.a PasswordLookup* LOOKUP* lookupd lookup z* _* core

grind:;		iprint READ-ME Makefile
		tgrind -lc $(HFILES) $(CFILES)
		tgrind -lpepy -d $(TOPDIR)pepy/grindefs $(RYFILES)
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve

true:;
