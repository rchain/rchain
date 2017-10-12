##########################################################################
#   Instructions to Make, for compilation of Carnot modules
##########################################################################

###############################################################################
#
# $Header: /mcc/project/carnot/root/master/pub-ess/isode/config/Makefile,v 1.1.1.1 1993/02/12 01:26:11 tomlic Exp $
#
#
# $Log: Makefile,v $
# Revision 1.1.1.1  1993/02/12  01:26:11  tomlic
# pub release of rosette
#
# Revision 1.4  1993/01/21  00:54:22  carnot
# ETCDIR removed, not needed
#
# Revision 1.3  1993/01/19  22:48:43  carnot
# isotailor bug fix
#
# Revision 1.2  1993/01/19  21:01:18  carnot
# Touch up for release 2.0
#
# Revision 9.0  1992/06/16  12:05:01  isode
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

LIBDIR	=	$(ARCHPFX)/lib/rblisode/

##################################################################
# Here it is...
##################################################################

all:;

cfg-tcp:	$(ARCH).h $(ARCH).make
		-@echo ""
		-@echo "Configuring ISODE for $(ARCH) with TCP/IP only"
		-@echo ""
		rm -f $(TOPDIR)h/config.h $(TOPDIR)config/CONFIG.make
		cp $(ARCH).h $(TOPDIR)h/config.h
		cp $(ARCH).make $(TOPDIR)config/CONFIG.make		
		cp *.local $(TOPDIR)support/
		@echo "s%PREFIX%$$CARNOTDIR%g" > isotailor.sed
		cat isotailor.tcp | sed -f isotailor.sed > isotailor
		-@echo ""

cfg-osi:	$(ARCH)-osi.h $(ARCH)-osi.make
		-@echo ""
		-@echo "Configuring ISODE for $(ARCH) with TCP/IP and OSI"
		-@echo ""
		rm -f $(TOPDIR)h/config.h $(TOPDIR)config/CONFIG.make
		cp $(ARCH)-osi.h $(TOPDIR)h/config.h
		cp $(ARCH)-osi.make $(TOPDIR)config/CONFIG.make		
		cp *.local $(TOPDIR)support/
		@echo "s%PREFIX%$$CARNOTDIR%g" > isotailor.sed
		cat isotailor.osi | sed -f isotailor.sed > isotailor
		-@echo ""
				
inst-all:	tailor rc

tailor:		$(ETCDIR)isotailor

$(ETCDIR)isotailor:	isotailor
		-cp $@ $(ETCDIR)isotailor~
		-cp isotailor $@
		-chmod a+r $@
		-@ls -gls $@
		-@echo ""

rc:		$(ETCDIR)isode.rc $(ETCDIR)quipu.rc

$(ETCDIR)isode.rc:	isode.rc
		-cp $@ $(ETCDIR)isode.rc~
		-cp isode.rc $@
		-chmod 755 $@
		-@ls -gls $@
		-@echo ""

$(ETCDIR)quipu.rc:	quipu.rc
		-cp $@ $(ETCDIR)quipu.rc~
		-cp quipu.rc $@
		-chmod 755 $@
		-@ls -gls $@
		-@echo ""

install:	inst-all clean

clean:;		rm -f z* *~ *.sed isotailor

