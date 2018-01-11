###############################################################################
#   Instructions to Make, for compilation of ROSETTE TSAP modules
###############################################################################
###############################################################################
#
# $Header: /mcc/project/carnot/root/master/pub-ess/isode/rbltsap/Makefile,v 1.1.1.1 1993/02/12 01:26:27 tomlic Exp $
#
#
# $Log: Makefile,v $
# Revision 1.1.1.1  1993/02/12  01:26:27  tomlic
# pub release of rosette
#
# Revision 1.3  1993/01/19  21:02:57  carnot
# Touch up for release 2.0
#
# Revision 8.0  91/07/17  13:04:37  isode
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

LIBDIR	=	$(ARCHPFX)/lib/rblisode/

LIBES	=	librbltsap.a $(TOPDIR)libcompat.a
HFILES	=	$(HDIR)tsap.h $(HDIR)isoaddrs.h \
		$(HDIR)manifest.h $(HDIR)general.h $(HDIR)config.h

##################################################################
# Here it is...
##################################################################

all:		rbltsap
inst-all:	inst-rbltsap
install:	inst-all clean

################################################################
# librbltsap
################################################################

.c.o:;		$(CC) $(CFLAGS) -DRBL -c $*.c

CFILES	=	tsaprovider.c tsaplisten.c
OFILES	=	tsaprovider.o tsaplisten.o

rbltsap:	$(OFILES)

inst-rbltsap: 	$(TOPDIR)libisode.a
		rm -f $(LIBDIR)librblisode.a
		cp $(TOPDIR)libisode.a $(LIBDIR)librblisode.a
		ar r $(LIBDIR)librblisode.a $(OFILES)
		@$(UTILDIR)make-lib.sh $(SYSTEM) $(LIBDIR)librblisode.a -ranlib
		ls -gls $(LIBDIR)librblisode.a

tsaprovider.o:	$(HDIR)tpkt.h $(HFILES) $(HDIR)x25.h $(HDIR)isoservent.h \
		$(HDIR)tailor.h $(HDIR)logger.h $(HDIR)mpkt.h
tsaplisten.o:	$(HDIR)tpkt.h $(HFILES)  $(HDIR)mpkt.h $(HDIR)internet.h \
		$(HDIR)x25.h $(HDIR)tp4.h

################################################################
# clean
################################################################

clean:;		rm -f *.o *.a z* _* core





