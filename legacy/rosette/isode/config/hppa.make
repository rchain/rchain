###############################################################################
#   Instructions to Make, for compilation of ISODE processes for HP-UX
###############################################################################

###############################################################################
#
# $Header: /mcc/project/carnot/root/master/pub-ess/isode/config/hppa.make,v 1.1.1.1 1993/02/12 01:26:05 tomlic Exp $
#
#
# $Log: hppa.make,v $
# Revision 1.1.1.1  1993/02/12  01:26:05  tomlic
# pub release of rosette
#
# Revision 1.4  1993/01/21  00:32:28  carnot
# modified etc path
#
# Revision 1.3  1993/01/19  22:28:49  carnot
# defer man page installation
#
# Revision 1.2  1993/01/19  21:01:32  carnot
# Touch up for release 2.0
#
# Revision 8.0  91/07/17  12:20:27  isode
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
MANOPTS	=	-none


###############################################################################
# Programs and Libraries
###############################################################################

MAKE	=	./make DESTDIR=$(DESTDIR) $(MFLAGS) -k
SHELL	=	/bin/sh

CC      =	cc
#
# You may need to add +Ns2000 +Nd3000 to CFLAGS to
# make the compiler use larger tables on the Series 300.
# The Pre-Processor symbol table needs expanding for some programs.
# add the following to CLFAGS to expand the tables to 256000 bytes...
#      -W p,-H256000
#
CFLAGS  =	-O    $(OPTIONS)
LIBCFLAGS=	      $(CFLAGS)
LINT    =	lint
LFLAGS  =	-bhuz $(OPTIONS)
LD	=	ld
LDCC	=	$(CC)
LDFLAGS =	-ns $(OSILIBDIR)
ARFLAGS	=

LN	=	ln

LSOCKET	=	


###############################################################################
# Generation Rules for library modules
###############################################################################

.c.o:;		$(CC) $(LIBCFLAGS) -c $*.c
