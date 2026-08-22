/* sunlink7.h - site configuration file for SunLink OSI and X.25 7.0 on
	SunOS 4 */

/* 
 * $Header: /mcc/project/carnot/root/master/pub-ess/isode/config/sun3-osi.h,v 1.1.1.1 1993/02/12 01:26:09 tomlic Exp $
 *
 *
 * $Log: sun3-osi.h,v $
 * Revision 1.1.1.1  1993/02/12  01:26:09  tomlic
 * pub release of rosette
 *
 * Revision 1.2  1993/01/19  21:01:47  carnot
 * Touch up for release 2.0
 *
 * Revision 9.0  1992/06/16  12:08:13  isode
 * Release 8.0
 *
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#ifndef	_CONFIG_
#define	_CONFIG_

#define	BSD42			/* Berkeley UNIX */
#define	SUNOS4			/*   with Sun's enhancements */
#define	WRITEV			/*   real Berkeley UNIX */
#define	BSD43			/*   4.3BSD or later */

#define	VSPRINTF		/* has vprintf(3s) routines */

#define	TCP			/* has TCP/IP (of course) */
#define	SOCKETS			/*   provided by sockets */

#define TP4			/* has TP4 */
#define SUN_TP4			/*   provided by SunLink OSI */
#define	SUNLINK_5_2		/*     define if using SunLink OSI release 5.2
				       or greater */
#define	SUNLINK_6_0		/*     define if using SunLink OSI release 6.0
				       or greater */
#define	SUNLINK_7_0		/*     define if using SunLink OSI release 7.0
				       or greater */

#define	GETDENTS		/* has getdirent(2) call */
#define	NFS			/* network filesystem -- has getdirentries() */

#endif
