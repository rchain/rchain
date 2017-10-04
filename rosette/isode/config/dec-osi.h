/* ultrix.h - site configuration file for Ultrix version greater than 3.1 */

/* 
 * $Header: /mcc/project/carnot/root/master/pub-ess/isode/config/dec-osi.h,v 1.1.1.1 1993/02/12 01:26:05 tomlic Exp $
 *
 *
 * $Log: dec-osi.h,v $
 * Revision 1.1.1.1  1993/02/12  01:26:05  tomlic
 * pub release of rosette
 *
 * Revision 1.2  1993/01/19  21:01:21  carnot
 * Touch up for release 2.0
 *
 * Revision 8.0  91/07/17  12:20:48  isode
 * Release 7.0
 * 
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
#define	WRITEV			/*   real Berkeley UNIX */
#define BSD43			/* 4.3BSD networking or later */

#define	VSPRINTF		/* has vprintf(3s) routines */

#define	TCP			/* has TCP/IP (of course) */
#define	SOCKETS			/*   provided by sockets */
#define TP4                     /* generic tp4 stack option */
#define BSD_TP4                 /* BSD tp4 base */
#define DNU_TP4                 /* >= dnu 5.0 */

/***** #define BIND			/* has h_addr_list in netdb.h */

#define	GETDENTS		/* has getdirent(2) call */

#endif
