/* sgi.h - site configuration file for generic SGI/IRIX */

/* 
 * $Header: /mcc/project/carnot/root/master/pub-ess/isode/config/sgi.h,v 1.1.1.1 1993/02/12 01:26:08 tomlic Exp $
 *
 *
 * $Log: sgi.h,v $
 * Revision 1.1.1.1  1993/02/12  01:26:08  tomlic
 * pub release of rosette
 *
 * Revision 1.2  1993/01/19  21:01:44  carnot
 * Touch up for release 2.0
 *
 * Revision 8.0  91/07/17  12:20:46  isode
 * Release 8.0
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

#define	SYS5			/* AT&T UNIX */
#define IRIX			/* SGI IRIX */
#define SVR4			/* SYS5 Release 4 */

#define	VSPRINTF		/* libc includes vsprintf() and vfprintf ()*/

#define	TCP			/* has TCP/IP */
#define	SOCKETS			/*   provided by sockets */

#define NFS			/* network file system */

#endif
