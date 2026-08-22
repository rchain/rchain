###############################################################################
#   Instructions to Make, for compilation of ISODE processes for
#	SunLink OSI and X.25 7.0 on SunOS 4
###############################################################################

###############################################################################
#
# $Header: /mcc/project/carnot/root/master/pub-ess/isode/config/sun3-osi.make,v 1.1.1.1 1993/02/12 01:26:09 tomlic Exp $
#
#
# $Log: sun3-osi.make,v $
# Revision 1.1.1.1  1993/02/12  01:26:09  tomlic
# pub release of rosette
#
# Revision 1.3  1993/01/21  00:32:34  carnot
# modified etc path
#
# Revision 1.2  1993/01/19  21:01:49  carnot
# Touch up for release 2.0
#
# Revision 9.0  1992/06/16  12:08:13  isode
# Release 8.0
#
#
##############################################################################

##############################################################################
#
#				 NOTICE
#
#    Acquisition, use, and distribution of this module and related
#    materials are subject to the restrictions of a license agreement.
#    Consult the Preface in the User's Manual for the full terms of
#    this agreement.
#
###############################################################################

#    See sunos4.make for shared library options

###############################################################################
# Options
###############################################################################

ARCHPFX	=	$(PREFIX)/$(ARCH)

OPTIONS	=	-I. -I$(TOPDIR)h -I/usr/sunlink/osi/include $(PEPYPATH) $(KRBOPT)

HDIR	=	$(TOPDIR)h/
UTILDIR	=	$(TOPDIR)util/
BINDIR	=	$(ARCHPFX)/bin/
SBINDIR	=	$(ARCHPFX)/sbin/
ETCDIR	=	$(ARCHPFX)/etc/
LOGDIR	=	$(ARCHPFX)/tmp/
INCDIRM	=	$(ARCHPFX)/include/isode
INCDIR	=	$(INCDIRM)/
PEPYDIRM=	$(INCDIR)pepy
PEPYDIR	=	$(PEPYDIRM)/
PEPSYDIRM=	$(INCDIR)pepsy
PEPSYDIR=	$(PEPSYDIRM)/
LIBDIR	=	$(ARCHPFX)/lib/isode/
LINTDIR	=	$(ARCHPFX)/lib/isode/lint/

LIBISODE=	$(TOPDIR)libisode.a
LIBDSAP	=	$(TOPDIR)libdsap.a

SYSTEM	=	-bsd42
MANDIR	=	$(PREFIX)/man/
MANOPTS	=	-bsd42


###############################################################################
# Programs and Libraries
###############################################################################

MAKE	=	./make DESTDIR=$(DESTDIR) $(MFLAGS) -k
SHELL	=	/bin/sh

CC      =	cc
# -O loses...
CFLAGS  =	      $(OPTIONS)
LIBCFLAGS=	      $(CFLAGS)
LINT    =	lint
LFLAGS  =	-bhuz $(OPTIONS)
LD	=	ld
LDCC	=	$(CC)
LDFLAGS =	-s -L/usr/sunlink/osi/lib
ARFLAGS	=

LN	=	ln

LSOCKET	=	-losi $(KRBLIB)


###############################################################################
# Generation Rules for library modules
###############################################################################

# -x may be harmful on earlier releases of SunOS, your mileage may vary...

.c.o:;		$(CC) $(LIBCFLAGS) -c $*.c
		-ld -x -r $@
		mv a.out $@
