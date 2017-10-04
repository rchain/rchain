###############################################################################
#   Instructions to Make, for compilation of ROS-based miscellaneous services
###############################################################################

###############################################################################
#
# $Header: /mcc/project/carnot/root/master/pub-ess/isode/rblimisc/Makefile,v 1.1.1.1 1993/02/12 01:26:12 tomlic Exp $
#
#
# $Log: Makefile,v $
# Revision 1.1.1.1  1993/02/12  01:26:12  tomlic
# pub release of rosette
#
# Revision 1.3  1993/01/19  21:02:13  carnot
# Touch up for release 2.0
#
# Revision 7.0  89/11/23  21:57:34  mrose
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

LIBDIR	=	$(ARCHPFX)/lib/rblisode/

.SUFFIXES:	.ry .py .c .o

.c.o:;		$(CC) $(CFLAGS) -DPEPYPATH -c $*.c

.py.c:;		$(TOPDIR)pepy/xpepy -m $(PYFLAGS) $<


LIBES   =	libimisc.a $(TOPDIR)libisode$(LPP).a
LLIBS   =	$(TOPDIR)llib-lisode$(LPP)
CFILES	=	imiscd.c imisc.c IMISC-ops.c ryinitiator.c ryresponder.c
RYFILES	=	imisc.ry
HFILES	=	$(HDIR)rosy.h $(HDIR)rosap.h $(HDIR)acsap.h $(HDIR)psap2.h \
		$(HDIR)psap.h $(HDIR)ssap.h $(HDIR)isoaddrs.h \
		$(HDIR)manifest.h $(HDIR)general.h $(HDIR)config.h

ROS	=	ros.


##################################################################
# Here it is...
##################################################################

all:		imiscd imisc
inst-all:	inst-rblimisc
install:	inst-all clean

lint:		l-imiscd l-imisc

all-lpp:;	$(MAKE) LPP=-lpp all
inst-lpp:;	$(MAKE) LPP=-lpp ROS=lpp. inst-all
install-lpp:;	$(MAKE) LPP=-lpp ROS=lpp. install
lint-lpp:;	$(MAKE) LPP=-lpp lint

##################################################################
# imiscd
##################################################################

inst-imiscd:	$(SBINDIR)$(ROS)imisc

$(SBINDIR)$(ROS)imisc:	ximiscd$(LPP)
		-cp $@ z$(ROS)imisc
		-rm -f $@
		cp ximiscd$(LPP) $@
		-@ls -gls $@
		-@echo ""

imiscd:		ximiscd$(LPP)
		-@echo ""

ximiscd$(LPP):	imiscd.o IMISC-RIops.o ryresponder.o $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ imiscd.o IMISC-RIops.o ryresponder.o \
			$(LIBES) $(LSOCKET)

l-imiscd:	IMISC-ops.c IMISC-C true
		$(LINT) $(LFLAGS) -DPERFORMER -DPEPYPATH imiscd.c IMISC-ops.c \
			ryresponder.c $(IMISC-C) $(LLIBS)

imiscd.o:	ryresponder.h IMISC-ops.h IMISC-types.h $(HFILES) \
			$(HDIR)logger.h

IMISC-RIops.o:	IMISC-ops.c IMISC-ops.h $(HFILES)
		$(CC) $(CFLAGS) -DINVOKER -DPERFORMER -DPEPYPATH -c IMISC-ops.c
		mv IMISC-ops.o $@

ryresponder.o:	ryresponder.h $(HFILES) $(HDIR)tailor.h


##################################################################
# imisc
##################################################################

inst-imisc:	$(BINDIR)imisc$(LPP)

$(BINDIR)imisc$(LPP):	ximisc$(LPP)
		-cp $@ zximisc$(LPP)
		-rm -f $@
		cp ximisc$(LPP) $@
		-@ls -gls $@
		-@echo ""

imisc:		ximisc$(LPP)
		-@echo ""

ximisc$(LPP):	imisc.o IMISC-RIops.o ryinitiator.o $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ imisc.o IMISC-RIops.o ryinitiator.o \
			$(LIBES) $(LSOCKET)

l-imisc:	IMISC-ops.c IMISC-C true
		$(LINT) $(LFLAGS) -DINVOKER imisc.c IMISC-ops.c \
			ryinitiator.c $(IMISC-C) IMISC-stubs.c $(LLIBS)

imisc.o:	ryinitiator.h IMISC-ops.h IMISC-types.h $(HFILES)

ryinitiator.o:	ryinitiator.h $(HFILES)


################################################################
# libimisc
################################################################

libimisc.a:	IMISC-O
		-rm -f $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $(ARFLAGS) $@ $(IMISC-O) 
		-@ls -l $@
		-@echo "RBLIMISC library built normally"

IMISC-O	=	IMISC-[0-9]*.o
IMISC-C	=	IMISC-[0-9]*.c

IMISC-O:	IMISC-C
		@$(MAKE) `/bin/ls $(IMISC-C) | sed 's/\.c$$/.o/'`
		-@touch $@

IMISC-C:	IMISC-types.py $(TOPDIR)pepy/xpepy
		-@rm -f $(IMISC-C) $(IMISC-O)
		$(TOPDIR)pepy/xpepy -a PY_advise -m -A -b IMISC $(PYFLAGS) \
			IMISC-types.py
		-@touch $@

IMISC-types.py:	IMISC-asn.py $(TOPDIR)pepy/xposy
		$(TOPDIR)pepy/xposy -f -h -m -o $@ $(POFLAGS) IMISC-asn.py
IMISC-types.h:	IMISC-types.py

IMISC-asn.py:	imisc.ry $(TOPDIR)rosy/xrosy
		$(TOPDIR)rosy/xrosy -m $(RYFLAGS) -o $@ imisc.ry
IMISC-ops.c:	imisc.ry
IMISC-ops.h:	imisc.ry
IMISC-stubs.c:	imisc.ry

inst-rblimisc:	libimisc.a
		cp IMISC-RIops.o $(LIBDIR)
		rm -f $(LIBDIR)librblimisc.a
		cp libimisc.a $(LIBDIR)librblimisc.a
		$(UTILDIR)make-lib.sh $(SYSTEM) $(LIBDIR)librblimisc.a -ranlib
		ls -gls $(LIBDIR)librblimisc.a

################################################################
# manual pages
################################################################

MANUALS	=	imiscd.8c imisc.1c

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


################################################################
# clean
################################################################

clean:;		rm -f *.o *.a IMISC* x* z* _* core

grind:;		iprint Makefile
		tgrind -lc ryinitiator.h ryresponder.h $(CFILES)
		tgrind -lpepy -d $(TOPDIR)pepy/grindefs $(RYFILES)
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve

true:;
