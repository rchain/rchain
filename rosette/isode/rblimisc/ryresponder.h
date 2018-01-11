/* ryresponder.h - include file for the generic idempotent responder */

/* 
 * $Header: /mcc/project/carnot/root/master/pub-ess/isode/rblimisc/ryresponder.h,v 1.1.1.1 1993/02/12 01:26:15 tomlic Exp $
 *
 *
 * $Log: ryresponder.h,v $
 * Revision 1.1.1.1  1993/02/12  01:26:15  tomlic
 * pub release of rosette
 *
 * Revision 1.2  1993/01/19  21:02:28  carnot
 * Touch up for release 2.0
 *
 * Revision 7.0  89/11/23  21:57:46  mrose
 * Release 6.0
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


#include "rosy.h"
#include "logger.h"


static struct dispatch {
    char   *ds_name;
    int	    ds_operation;

    IFP	    ds_vector;
};


void	adios (), advise ();
void	acs_advise ();
void	ros_adios (), ros_advise ();
void	ryr_advise ();

int	ryresponder ();


extern int  debug;
