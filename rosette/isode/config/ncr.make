###############################################################################
#   Instructions to Make, for compilation of ISODE processes for generic SVR4
###############################################################################

###############################################################################
#
# $Header: /mcc/project/carnot/root/master/pub-ess/isode/config/ncr.make,v 1.1.1.1 1993/02/12 01:26:07 tomlic Exp $
#
#
# $Log: ncr.make,v $
# Revision 1.1.1.1  1993/02/12  01:26:07  tomlic
# pub release of rosette
#
# Revision 1.3  1993/01/21  00:32:30  carnot
# modified etc path
#
# Revision 1.2  1993/01/19  21:01:39  carnot
# Touch up for release 2.0
#
# Revision 1.1  1992/01/02  23:06:44  lavender
# Initial revision
#
# Revision 8.0  91/07/17  12:20:47  isode
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
#
# Donated to ISODE by X-Tel Services Ltd
#
# Based on ICL DRS/NX 6000 Version 4.0  Level 4  Increment 4
#
###############################################################################

###############################################################################
# Options
###############################################################################

ARCHPFX	=	$(PREFIX)/$(ARCH)

OPTIONS	=	-I. -I$(TOPDIR)h $(OSIINCDIR) $(PEPYPATH) $(KRBOPT)

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

SYSTEM	=	-sys5r4
MANDIR	=	$(PREFIX)/man/
MANOPTS	=	-none


###############################################################################
# Shared libraries
###############################################################################

#    You can build the ISODE and DSAP libraries dynamically.
#    This option is not the default as its portability is questionable.
#    We advise you build statically first, then when that works, you
#    may wish to try the shared library options.
#
#    If you have changed LIBDIR above, shared libraries may not work.

#    First, comment out the definitions of LIBISODE and LIBDSAP above

#    Second, uncomment these three lines:

#SHAREDLIB=      shared
#LIBISODE=	-L$(TOPDIR) -lisode -lm 
#LIBDSAP=	-L$(TOPDIR) -ldsap

#    Third, add
#		-K PIC
#    to LIBCFLAGS below

#    You will need to install the libraries before the binaries will run.

###############################################################################
# Programs and Libraries
###############################################################################

MAKE	=	./make DESTDIR=$(DESTDIR) $(MFLAGS) -k
SHELL	=	/bin/sh

# For generic Sys5 R4 use /usr/bin/cc and /usr/bin/ld explicitly, 
# otherwise you may pick up /usr/ucb/cc.  If you don't have SVR4_UCB
# defined that would be a complete flop!
#
# If SVR4_UCB is defined you'll need to use /usr/ucb/cc and /usr/ucb/ld
#

CC      =	/usr/bin/cc
LD	=	/usr/bin/ld
CFLAGS  =       $(OPTIONS) $(LOPTIONS) 
LIBCFLAGS=      $(CFLAGS)
LINT    =	/usr/bin/lint
LFLAGS  =	-s $(OPTIONS)
LDCC	=	$(CC)
LDFLAGS = 	
ARFLAGS	=

# use cp instead of ln
LN	=	cp

# You won't need -lsocket or -nsl if you have SVR4_UCB defined.

LSOCKET=	-lsocket -lnsl $(KRBLIB)

###############################################################################
# Generation Rules for library modules
###############################################################################

.c.o:;		$(CC) $(LIBCFLAGS) -c $*.c
