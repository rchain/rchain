##########################################################################
#   Instructions to Make, for compilation of Carnot modules
##########################################################################

###############################################################################
#
# $Header: /mcc/project/carnot/root/master/pub-ess/isode/Makefile,v 1.1.1.1 1993/02/12 01:26:03 tomlic Exp $
#
#
# $Log: Makefile,v $
# Revision 1.1.1.1  1993/02/12  01:26:03  tomlic
# pub release of rosette
#
# Revision 1.3  1993/01/19  21:01:04  carnot
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

DIRS	=	config rbltsap rblimisc rbllookup

all:;		@for i in $(DIRS); \
		    do (echo "cd $$i; $(MAKE) all"; \
			      cd $$i; $(MAKE) all); \
		    done

inst-all:	make-dirs
		@for i in $(DIRS); \
		    do (echo "cd $$i; $(MAKE) inst-all"; \
			      cd $$i; $(MAKE) inst-all); \
		    done

make-dirs:;	@for i in $(LIBDIR); \
		    do (echo $$i X | \
			sed -e 's%\(.*\)/ X%if test ! -d \1; then mkdir \1; chmod 755 \1; else true; fi%' | \
			sh -v); \
		    done

install:	inst-all clean

clean:;		rm -f *.a z*
		@for i in $(DIRS); \
		    do (echo "cd $$i; $(MAKE) clean"; \
			      cd $$i; $(MAKE) clean); \
		    done
