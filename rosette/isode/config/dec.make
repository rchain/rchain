###############################################################################
#   Instructions to Make, for compilation of ISODE processes for Ultrix
#   versions greater than 3.1.
###############################################################################

###############################################################################
#
# $Header: /mcc/project/carnot/root/master/pub-ess/isode/config/dec.make,v 1.1.1.1 1993/02/12 01:26:04 tomlic Exp $
#
#
# $Log: dec.make,v $
# Revision 1.1.1.1  1993/02/12  01:26:04  tomlic
# pub release of rosette
#
# Revision 1.3  1993/01/21  00:32:26  carnot
# modified etc path
#
# Revision 1.2  1993/01/19  21:01:27  carnot
# Touch up for release 2.0
#
# Revision 9.0  1992/06/16  12:08:13  isode
# Release 8.0
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
# Ultrix 4.2 and X Windows DUAs
###############################################################################

# To build the X-Windows DUA you will need to add
#
#	-I/usr/include/mit
#
# to OPTIONS above, before ANY other header file path.
#
# You will also need to include
#
#	-L/usr/local/lib
#
# to LDFLAGS below.
#
# You also need to ensure /usr/lib/libXext-mit.a is linked into
# /usr/local/lib/libXext.a to get the MIT version of the library.
# Alternatively, modify Makefile link commands to use -lXext-mit 
# instead of -lXext.


###############################################################################
# Options
###############################################################################

ARCHPFX=	$(PREFIX)/$(ARCH)

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

SYSTEM	=	-bsd42
MANDIR	=	$(PREFIX)/man/
MANOPTS	=	-bsd42


###############################################################################
# Programs and Libraries
###############################################################################
#
# If using the -O option in CFLAGS you might also need to add 
#     -Olimit 2000 
# to ensure everything gets optimized
#

MAKE	=	./make DESTDIR=$(DESTDIR) $(MFLAGS) -k
SHELL	=	/bin/sh

CC      =	cc
CFLAGS  =	-O1 -Olimit 2000 $(OPTIONS)
LIBCFLAGS=	$(CFLAGS)
LINT    =	lint
LFLAGS  =	-bhuz $(OPTIONS)
LD	=	ld
LDCC	=	$(CC)
LDFLAGS =	-s
ARFLAGS	=

LN	=	ln

# Add -lx25 -ldnet to LSOCKET below for the DEMSA X.25 router
LSOCKET	=	$(KRBLIB)


###############################################################################
# Generation Rules for library modules
###############################################################################

.c.o:;		$(CC) $(LIBCFLAGS) -c $*.c
		-ld -x -r $@
		mv a.out $@
