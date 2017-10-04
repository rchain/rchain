###############################################################################
#   Instructions to Make, for compilation of ISODE processes for generic SVR3
###############################################################################

###############################################################################
#
# $Header: /mcc/project/carnot/root/master/pub-ess/isode/config/sgi.make,v 1.1.1.1 1993/02/12 01:26:08 tomlic Exp $
#
#
# $Log: sgi.make,v $
# Revision 1.1.1.1  1993/02/12  01:26:08  tomlic
# pub release of rosette
#
# Revision 1.3  1993/01/21  00:32:32  carnot
# modified etc path
#
# Revision 1.2  1993/01/19  21:01:46  carnot
# Touch up for release 2.0
#
# Revision 1.1  1992/01/02  23:18:34  lavender
# Initial revision
#
# Revision 8.0  91/07/17  12:20:46  isode
# Release 7.0
# 
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

ARCHPFX	=	$(PREFIX)/$(ARCH)

OPTIONS	=	-I. -I$(TOPDIR)h $(PEPYPATH) $(KRBOPT)

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

SYSTEM	=	-sys5
MANDIR	=	$(PREFIX)/man/
MANOPTS	=	-sys5


###############################################################################
# Programs and Libraries
###############################################################################

MAKE	=	./make DESTDIR=$(DESTDIR) $(MFLAGS) -k
SHELL	=	/bin/sh

CC      =	cc
CFLAGS  =	-Wf,-XNg1500 -Olimit 3000 -cckr -O1 $(OPTIONS)
LIBCFLAGS=	      $(CFLAGS)
LINT    =	lint
LFLAGS  =	-bhuz $(OPTIONS)
LD	=	ld
LDCC	=	$(CC)
LDFLAGS =	
ARFLAGS	=

LN	=	cp

LSOCKET	=	$(KRBLIB)


###############################################################################
# Generation Rules for library modules
###############################################################################

.c.o:;		$(CC) $(LIBCFLAGS) -c $*.c
