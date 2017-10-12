/* sys54.h - site configuration file for generic SVR4 */

/* 
 * $Header: /mcc/project/carnot/root/master/pub-ess/isode/config/ncr.h,v 1.1.1.1 1993/02/12 01:26:07 tomlic Exp $
 *
 *
 * $Log: ncr.h,v $
 * Revision 1.1.1.1  1993/02/12  01:26:07  tomlic
 * pub release of rosette
 *
 * Revision 1.2  1993/01/19  21:01:38  carnot
 * Touch up for release 2.0
 *
 * Revision 1.1  1992/01/02  23:06:44  lavender
 * Initial revision
 *
 * Revision 8.0  91/07/17  12:20:47  isode
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

#define	SYS5			/* AT&T UNIX */
#define SVR4			/*   Systems V Release 4 */

/* If you have the UCB extensions, define SVR4-UCB */
#undef	SVR4_UCB	/* NOT ON NCR 3400 ! */	


#define	VSPRINTF		/* has vprintf(3s) routines */

#define GETDENTS

#define	TCP			/* has TCP/IP */
#define	SOCKETS			/*   provided by sockets */

#endif
