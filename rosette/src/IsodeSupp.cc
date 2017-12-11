/* Mode: -*- C++ -*- */
/* @BC
 *		                Copyright (c) 1993
 *	    by Microelectronics and Computer Technology Corporation (MCC)
 *				All Rights Reserved
 *
 *	Permission to use, copy, modify, and distribute this software and its
 *	documentation for any purpose and without fee is hereby granted,
 *	provided that this notice be retained unaltered, and that the name of
 *	MCC and its shareholders and participants shall not be used in
 *	advertising or publicity pertaining to distribution of the software
 *	without specific written prior permission.
 *
 *	THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 *	IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 *	WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

/*
 * $Header$
 *
 * $Log$
 @EC */

#ifdef __GNUG__
#pragma implementation
#endif

/* Isode-support.cc: provides routines that are exported to Rosette
 * to interface to the Isode library
 */

/* INCLUDES */

#include "rosette.h"

#include <math.h>
#include <sys/errno.h>
#include <fcntl.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <stdio.h>
#include <varargs.h>
#include <memory.h>
#include <osfcn.h>
#ifdef MIPS_SGI_SYSV
#include <bstring.h>
#endif

#include "BinaryOb.h"
#include "Ctxt.h"
#include "Operation.h"
#include "Reader.h"
#include "RBLstream.h"
#include "RBLstring.h"
#include "Tuple.h"
#include "Vm.h"

/*the following defines try to compensate for old-style C procedure
 * prototypes w/o going into the isode header files and modifying them
 */

#define str2oid() dummy_str2oid()
#define oid_cmp() dummy_oid_cmp()
#define pe_free() dummy_pe_free()
#define AcErrString() dummy_AcErrString()
#define _str2aei() dummy_str2aei()
#define aei2addr() dummy_aei2addr()
#define ode2oid() dummy_ode2oid()
#define oid_cpy() dummy_oid_cpy()
#define addr2ref() dummy_addr2ref()
#define RoInvokeRequest() dummy_RoInvokeRequest()
#define rosaplose() dummy_rosaplose()
#define oid2ode_aux() dummy_oid2ode_aux()
#define AcAsynAssocRequest() dummy_AcAsynAssocRequest()
#define AcAssocResponse() dummy_AcAssocResponse()
#define AcRelRequest() dummy_AcRelRequest()
#define AcRelResponse() dummy_AcRelResponse()
#define AcUAbortRequest() dummy_AcUAbortRequest()
#define oid_free() dummy_oid_free()
#define RoSetService() dummy_RoSetService()
#define RoSetIndications() dummy_RoSetIndications()
#define PSetIndications() dummy_PSetIndications()
#define findacblk() dummy_findacblk()
#define freeacblk() dummy_freeacblk()

/*
 * this is a not really general solution to trying to get things to work
 * with Isode 7.0.  we get multiple defns of various standard system
 * routines like getenv, and malloc when the isode supplied general.h
 * is included so the simplest approach at the moment seems to be to declare
 * that it has already been included
 */
#define _GENERAL_

extern "C" {
#include "acpkt.h"
#include "acsap.h"
#include "psap2.h"
#include "rosap.h"
#include "rosy.h"
}

#undef str2oid
#undef oid_cmp
#undef pe_free
#undef AcErrString
#undef _str2aei
#undef aei2addr
#undef ode2oid
#undef oid_cpy
#undef addr2ref
#undef RoInvokeRequest
#undef rosaplose
#undef oid2ode_aux
#undef AcAsynAssocRequest
#undef AcAssocResponse
#undef AcRelRequest
#undef AcRelResponse
#undef AcUAbortRequest
#undef oid_free
#undef RoSetService
#undef RoSetIndications
#undef PSetIndications
#undef findacblk
#undef freeacblk

typedef void (*R_VFP)(...);
typedef int (*HFP)(...);

extern "C" {
OID str2oid(char*);
int oid_cmp(OID, OID);
void pe_free(PE);
char* AcErrString(int);
AEI _str2aei(char*, char*, char*, int, char*, char*);
PSAPaddr* aei2addr(AEI);
OID ode2oid(char*);
OID oid_cpy(OID);
SSAPref* addr2ref(char*);
int RoInvokeRequest(int, int, int, PE, int, IP, int, struct RoSAPindication*);
int rosaplose(struct RoSAPindication*, int, char*, char*, int, int, char*);
char* oid2ode_aux(OID, int);
int RyAssocRequest(OID, AEI, struct PSAPaddr*, struct PSAPctxlist*,
                   struct SSAPref*, struct RoSAPindication*, IFP, int, Ob*,
                   R_VFP);
int AcAsynAssocRequest(OID, AEI, AEI, struct PSAPaddr*, struct PSAPaddr*,
                       struct PSAPctxlist*, OID, int, int, long, int,
                       struct SSAPref*, PE*, int, struct QOStype*,
                       struct AcSAPconnect*, struct AcSAPindication*, int);
int AcAssocResponse(int, int, int, OID, AEI, struct PSAPaddr*,
                    struct PSAPctxlist*, int, int, int, long, int,
                    struct SSAPref*, PE*, int, struct AcSAPindication*);
int AcRelRequest(int, int, PE*, int, int, struct AcSAPrelease*,
                 struct AcSAPindication*);
int AcRelResponse(int, int, int, PE*, int, struct AcSAPindication*);
int AcUAbortRequest(int, PE*, int, struct AcSAPindication*);
int oid_free(OID);
int RoSetService(int, HFP, struct RoSAPindication*);
int RoSetIndications(int, R_VFP, struct RoSAPindication*);
int PSetIndications(int, HFP, HFP, HFP, HFP, HFP, HFP, HFP,
                    struct PSAPindication*);
struct assocblk* findacblk(int);
void freeacblk(struct assocblk*);
}

/* from general.h */
#define NVEC 100

/* DATA */

extern "C" {
/* ISODE ROUTINES */
char* isodeversion;
int AcInit();
void endisoservent();
struct RyOperation* findopbyop();
void getisoservent();
void getisoserventbyname();
void getisoserventbyselector();
char* _paddr2str();
void RNetListen();
int RoErrorRequest();
int RoResultRequest();
int RoSelectMask();
void setisoservent();
char* sprintref();
}

int RpsDATAser(int sd, struct PSAPdata* px);
int RpsTOKENser(int sd, struct PSAPtoken* pt);
int RpsSYNCser(int sd, PSAPsync* pn);
int RpsACTIVITYser(int sd, PSAPactivity* pv);
int RpsREPORTser(int sd, PSAPreport* pp);
int RpsFINISHser(int sd, struct PSAPfinish* pf);
int RpsABORTser(int sd, PSAPabort* pa);

extern "C" {
void force_load_libisode() {
    /* ISODE ROSY ROUTINES */
    (void)AcInit();
    (void)endisoservent();
    (void)findopbyop();
    (void)getisoservent();
    (void)getisoserventbyname();
    (void)getisoserventbyselector();
    (void)_paddr2str();
    (void)RNetListen();
    (void)RoErrorRequest();
    (void)RoResultRequest();
    (void)RoSelectMask();
    (void)setisoservent();
    (void)sprintref();
}
} /* end extern "C" */

/*  */
extern StdOprn* oprnResumeIO;
extern Prim* obRuntimeError;

pOb ROI2ByteVec(struct RoSAPindication* indication, pTuple protos) {
    int type = indication->roi_type;

    if (type >= 0 && type < protos->numberOfElements()) {
        ByteVec* proto = (ByteVec*)protos->nth(type);
        int sz = proto->numberOfBytes();
        ByteVec* bv = ByteVec::create(proto, sz);
        memcpy((char*)&bv->byte(0), (char*)&indication->roi_un, sz);
        return bv;
    }
    else
        return NIV;
}

/* handler for ROSetIndications */
extern "C" {
void IndicationToRosette(int fd, struct RoSAPindication* indication) {
    pTuple tpl = (pTuple)vm->ioPool[fd];
    pOb ob = tpl->nth(0);
    PROTECT(ob);
    pTuple protos = (pTuple)tpl->nth(1);
    pTuple oprns = (pTuple)tpl->nth(2);
    pOb oprn = oprns->nth(indication->roi_type);
    PROTECT(oprn);

    pOb ind = ROI2ByteVec(indication, protos);
    PROTECT(ind);

    pTuple av = Tuple::create(3, NIV);

    ASSIGN(av, elem(0), ob);
    ASSIGN(av, elem(1), FIXNUM(fd));
    ASSIGN(av, elem(2), ind);

    pCtxt c = Ctxt::create(oprn, av);

    BASE(ob)->receive(c);
}
} /* end extern "C" */

/* handler for PSAP indications */
int RPsHandler(int fd, int type, char* ind) {
    pTuple tpl = (pTuple)vm->ioPool[fd];
    pOb ob = tpl->nth(0);
    pTuple protos = (pTuple)tpl->nth(1);
    ByteVec* proto = (ByteVec*)protos->nth(type);
    int sz = proto->numberOfBytes();
    pTuple oprns = (pTuple)tpl->nth(2);
    pOb oprn = oprns->nth(type);
    PROTECT(ob);
    PROTECT(oprn);

    ByteVec* bv = ByteVec::create(proto, sz);
    memcpy((char*)&bv->byte(0), ind, sz);
    PROTECT(bv);

    pTuple av = Tuple::create(3, NIV);

    ASSIGN(av, elem(0), ob);
    ASSIGN(av, elem(1), FIXNUM(fd));
    ASSIGN(av, elem(2), bv);

    pCtxt c = Ctxt::create(oprn, av);

    BASE(ob)->receive(c);

    return 0;
}

/*    GET PCTXTID from AcSAPStart */

int get_pcid(char* asn_str, struct AcSAPstart* acs) {
    int i;
    OID asn = str2oid(asn_str);

    struct PSAPstart* ps = &acs->acs_start;
    struct PSAPctxlist* pcl = &ps->ps_ctxlist;
    struct PSAPcontext* pc;

    for (i = 0, pc = pcl->pc_ctx; i < pcl->pc_nctx; i++, pc++)
        if (oid_cmp(pc->pc_asn, asn) == 0)
            return pc->pc_id;

    return NOTOK;
}

/*    ENCODE AND INVOKE */

int Ro_Encode_and_Invoke(int fd, int op, int invokeID, HFP efn, caddr_t in,
                         struct RoSAPindication* roi) {
    PE pe = NULLPE;
    int result;

    PY_pepy[0] = 0;
    if (!efn || (result = (*efn)(&pe, 1, NULL, NULLCP, in)) == OK)
        result = RoInvokeRequest(fd, op, ROS_ASYNC, pe, invokeID, NULLIP,
                                 ROS_NOPRIO, roi);
    else
        result =
            rosaplose(roi, ROS_CONGEST, NULLCP,
                      "error encoding argument for invocation %d, op %d, [%s]",
                      invokeID, op, PY_pepy);

    if (pe)
        pe_free(pe);

    return result;
}

/*  */
/* the following routines are imported by routines in tsaplisten and
 * tsaprovider.  They must each be wrapped in extern "C" so that they
 * may be referred to w/o dealing w/ mangled names.
 */

extern "C" {
void AcceptError2Rosette(pTuple tpl, int fd, char* err) {
    pOb acter = tpl->nth(0);
    pTuple oprns = (pTuple)tpl->nth(1);
    pOb oprn = oprns->nth(0);
    PROTECT(acter);
    PROTECT(oprn);
    pTuple av = Tuple::create(3, NIV);
    PROTECT(av);

    ASSIGN(av, elem(0), acter);
    ASSIGN(av, elem(1), FIXNUM(fd));
    RBLstring* s = RBLstring::create(strlen(err), err);
    ASSIGN(av, elem(2), s);

    pCtxt c = Ctxt::create(oprn, av);

    BASE(acter)->receive(c);
}

void Disconnect2Rosette(pTuple tpl, int fd, char* td, int td_sz) {
    pOb acter = tpl->nth(0);
    pTuple oprns = (pTuple)tpl->nth(1);
    pOb oprn = oprns->nth(1);
    PROTECT(acter);
    PROTECT(oprn);
    pTuple av = Tuple::create(3, NIV);
    PROTECT(av);

    ASSIGN(av, elem(0), acter);
    ASSIGN(av, elem(1), FIXNUM(fd));
    ByteVec* bv = ByteVec::create(td_sz);
    memcpy((char*)&bv->byte(0), td, td_sz);
    ASSIGN(av, elem(2), bv);

    pCtxt c = Ctxt::create(oprn, av);

    BASE(acter)->receive(c);
}

void IsodeAccept2Rosette(pTuple tpl, int fd, char* acs, int acs_sz) {
    pOb acter = tpl->nth(0);
    pTuple oprns = (pTuple)tpl->nth(1);
    pOb oprn = oprns->nth(2);
    PROTECT(acter);
    PROTECT(oprn);
    pTuple av = Tuple::create(4, NIV);
    PROTECT(av);

    ASSIGN(av, elem(0), acter);
    ASSIGN(av, elem(1), FIXNUM(fd));
    ASSIGN(av, elem(2), FIXNUM(acs_sz));
    ByteVec* bv = ByteVec::create(acs_sz);
    memcpy((char*)&bv->byte(0), acs, acs_sz);
    ASSIGN(av, elem(3), bv);

    pCtxt c = Ctxt::create(oprn, av);

    BASE(acter)->receive(c);
}

void AddIsodeIoHandler(int fd, IO_HANDLER* handler) {
    vm->addIoHandler(fd, handler, INVALID, 1);
}

void SetIoPool(int fd, pOb ob) { vm->ioPool[fd] = ob; }

void DeleteIsodeIoHandler(int fd) { vm->deleteIoHandler(fd); }
} /* end extern "C" */
/*    ROSETTE INITIATOR */

/* ARGSUSED */

void rosette_advise(struct AcSAPabort* aca, char* event) {
    char buffer[BUFSIZ];

    if (aca->aca_cc > 0)
        (void)sprintf(buffer, "[%s] %*.*s", AcErrString(aca->aca_reason),
                      aca->aca_cc, aca->aca_cc, aca->aca_data);
    else
        (void)sprintf(buffer, "[%s]", AcErrString(aca->aca_reason));

    printf("%s: %s (source %d)", event, buffer, aca->aca_source);
}

#ifdef DONT_TRY_TO_COMPILE
extern "C" {
int Assoc_Initiate(char* myname, char* hostname, char* myservice,
                   char* mycontext, char* mypci, Ob* ob, int port) {
    /*
    int	iloop,
          sd;
    char	buffer[BUFSIZ],
          *vec[NVEC + 1];
    register struct dispatch   *ds;
    */
    struct SSAPref sfs;
    register struct SSAPref* sf;
    register struct PSAPaddr* pa;
    struct AcSAPconnect accs;
    register struct AcSAPconnect* acc = &accs;
    struct AcSAPindication acis;
    register struct AcSAPindication* aci = &acis;
    register struct AcSAPabort* aca = &aci->aci_abort;
    AEI aei;
    OID ctx, pci;
    struct PSAPctxlist pcs;
    register struct PSAPctxlist* pc = &pcs;
    struct RoSAPindication rois;
    register struct RoSAPindication* roi = &rois;
    register struct RoSAPpreject* rop = &roi->roi_preject;

    if ((aei = str2aei(hostname, myservice)) == NULLAEI)
        printf("%s-%s: unknown application-entity", hostname, myservice);
    if ((pa = aei2addr(aei)) == NULLPA) {
        printf("address translation failed");
        return NOTOK;
    }

    if (port)
        (pa->pa_addr).sa_addr.ta_addrs[0].na_un.un_na_tcp.na_tcp_port = port;

    if ((ctx = ode2oid(mycontext)) == NULLOID)
        printf("%s: unknown object descriptor", mycontext);
    if ((ctx = oid_cpy(ctx)) == NULLOID)
        printf("out of memory");
    if ((pci = ode2oid(mypci)) == NULLOID)
        printf("%s: unknown object descriptor", mypci);
    if ((pci = oid_cpy(pci)) == NULLOID)
        printf("out of memory");
    pc->pc_nctx = 1;
    pc->pc_ctx[0].pc_id = 1;
    pc->pc_ctx[0].pc_asn = pci;
    pc->pc_ctx[0].pc_atn = NULLOID;

    if ((sf = addr2ref(PLocalHostName())) == NULL) {
        sf = &sfs;
        (void)bzero((char*)sf, sizeof *sf);
    }

    printf("%s", myname);
    if (sf->sr_ulen > 2)
        printf(" running on host %s", sf->sr_udata + 2);
    if (sf->sr_clen > 2)
        printf(" at %s", sf->sr_cdata + 2);
    printf(" [%s, ", oid2ode(ctx));
    printf("%s]\n", oid2ode(pci));
    printf("using %s\n", isodeversion);

    printf("%s... ", hostname);
    (void)fflush(stdout);

    return RyAssocRequest(ctx, aei, pa, pc, sf, roi, RoPService, NULL, ob,
                          (R_VFP)IndicationToRosette);
}
}
#endif

int RyAssocRequest(OID ctx, AEI aei, struct PSAPaddr* pa,
                   struct PSAPctxlist* pc, struct SSAPref* sf,
                   struct RoSAPindication* roi, IFP svc, int asy, Ob* ob,
                   R_VFP hndlr) {
    struct AcSAPconnect accs;
    struct AcSAPconnect* acc = &accs;
    struct AcSAPindication acis;
    struct AcSAPindication* aci = &acis;
    int sd;

    if (AcAsynAssocRequest(ctx, NULLAEI, aei, NULLPA, pa, pc, NULLOID, 0,
                           ROS_MYREQUIRE, SERIAL_NONE, 0, sf, NULLPEP, 0,
                           NULLQOS, acc, aci, asy) == NOTOK) {
        rosette_advise(&aci->aci_abort, "A-ASSOCIATE.REQUEST");

        return NOTOK;
    }
    else {
        if (acc->acc_result != ACS_ACCEPT) {
            printf("association rejected: [%s]\n",
                   AcErrString(acc->acc_result));

            return NOTOK;
        }
        else {
            sd = acc->acc_sd;
            ACCFREE(acc);

            if (RoSetService(sd, (HFP)svc, roi) == NOTOK)
                printf("set RO/PS fails\n");

            if (RoSetIndications(sd, hndlr, roi) == NOTOK)
                printf("set RO/Handler fails\n");

            SetIoPool(sd, ob);

            return sd;
        }
    }
}


extern "C" {
int PsAssocRequest(OID ctx, AEI aei, struct PSAPaddr* pa,
                   struct PSAPctxlist* pc, struct SSAPref* sf,
                   struct PSAPindication* pi, int asy, Ob* ob) {
    struct AcSAPconnect accs;
    struct AcSAPconnect* acc = &accs;
    struct AcSAPindication acis;
    struct AcSAPindication* aci = &acis;
    int sd;

    if (AcAsynAssocRequest(ctx, NULLAEI, aei, NULLPA, pa, pc, NULLOID, 0,
                           ROS_MYREQUIRE, SERIAL_NONE, 0, sf, NULLPEP, 0,
                           NULLQOS, acc, aci, asy) == NOTOK) {
        rosette_advise(&aci->aci_abort, "A-ASSOCIATE.REQUEST");

        return NOTOK;
    }
    else {
        if (acc->acc_result != ACS_ACCEPT) {
            printf("association rejected: [%s]\n",
                   AcErrString(acc->acc_result));

            return NOTOK;
        }
        else {
            sd = acc->acc_sd;
            ACCFREE(acc);

            if (PSetIndications(sd, (HFP)RpsDATAser, (HFP)RpsTOKENser,
                                (HFP)RpsSYNCser, (HFP)RpsACTIVITYser,
                                (HFP)RpsREPORTser, (HFP)RpsFINISHser,
                                (HFP)RpsABORTser, pi) == NOTOK)
                return NOTOK;

            SetIoPool(sd, ob);

            return sd;
        }
    }
}
}
/*  */

int RpsDATAser(int sd, struct PSAPdata* px) {
    return RPsHandler(sd, PI_DATA, (char*)px);
}

void RpsDATAfree(struct PSAPdata* px) { PXFREE(px); }

int RpsTOKENser(int sd, PSAPtoken* pt) {
    return RPsHandler(sd, PI_TOKEN, (char*)pt);
}

int RpsSYNCser(int sd, PSAPsync* pn) {
    return RPsHandler(sd, PI_SYNC, (char*)pn);
}

int RpsACTIVITYser(int sd, PSAPactivity* pv) {
    return RPsHandler(sd, PI_ACTIVITY, (char*)pv);
}

int RpsREPORTser(int sd, PSAPreport* pp) {
    return RPsHandler(sd, PI_REPORT, (char*)pp);
}

int RpsFINISHser(int sd, struct PSAPfinish* pf) {
    return RPsHandler(sd, PI_FINISH, (char*)pf);
}

int RpsABORTser(int sd, PSAPabort* pa) {
    struct assocblk* acb;

    if ((acb = findacblk(sd)) != NULL) {
        acb->acb_fd = NOTOK;
        freeacblk(acb);
    }

    return RPsHandler(sd, PI_ABORT, (char*)pa);
}

/* RpsFree:
 *	Added by G. Lavender to fix memory leaks.
 * 	RpsFree is called from Rosette code after
 * 	the PE's imbedded in the indication structures
 * 	are decoded. (Elegance goes out the window when
 * 	under pressure at the SQLAcess testbed)
 */

extern "C" {
void RpsFree(void* pi, int type) {
    struct PSAPdata* px;
    struct PSAPtoken* pt;
    struct PSAPsync* pn;
    struct PSAPactivity* pv;
    struct PSAPreport* pr;
    struct PSAPfinish* pf;
    struct PSAPabort* pa;

    if (!pi)
        return;

    switch (type) {
    case PI_DATA:
        px = (struct PSAPdata*)pi;
        PXFREE(px);
        break;
    case PI_TOKEN:
        pt = (struct PSAPtoken*)pi;
        PTFREE(pt);
        break;
    case PI_SYNC:
        pn = (struct PSAPsync*)pi;
        PNFREE(pn);
        break;
    case PI_ACTIVITY:
        pv = (struct PSAPactivity*)pi;
        PVFREE(pv);
        break;
    case PI_REPORT:
        pr = (struct PSAPreport*)pi;
        PPFREE(pr);
        break;
    case PI_FINISH:
        pf = (struct PSAPfinish*)pi;
        PFFREE(pf);
        break;
    case PI_ABORT:
        pa = (struct PSAPabort*)pi;
        PAFREE(pa)
        break;
    default:
        break;
    }
}
}

/*  */

/* ARGSUSED */

#define SR_DUPLEX 0x0002
#define ROI_NOTOK -2

extern "C" {
int AcAccept(struct AcSAPstart* acs, struct SSAPref* sf, HFP svc, Ob* ob,
             R_VFP hndlr, struct AcSAPindication* aci,
             struct RoSAPindication* roi) {
    int r, sd = acs->acs_sd;
    struct PSAPstart* ps = &(acs->acs_start);

    r = AcAssocResponse(sd, ACS_ACCEPT, ACS_USER_NULL, NULLOID, NULLAEI, NULLPA,
                        NULLPC, PC_ACCEPT, ps->ps_prequirements, SR_DUPLEX,
                        SERIAL_NONE, ps->ps_settings, sf, NULLPEP, 0, aci);
    if (r == NOTOK)
        return NOTOK;

    ACSFREE(acs);

    if (RoSetService(sd, svc, roi) == NOTOK)
        return ROI_NOTOK;

    if (RoSetIndications(sd, hndlr, roi) == NOTOK)
        return ROI_NOTOK;

    SetIoPool(sd, ob);

    return sd;
}
}

extern "C" {
int PsAcAccept(struct AcSAPstart* acs, struct SSAPref* sf, Ob* ob,
               struct AcSAPindication* aci, struct PSAPindication* pi) {
    int r, sd = acs->acs_sd;
    struct PSAPstart* ps = &(acs->acs_start);

    r = AcAssocResponse(sd, ACS_ACCEPT, ACS_USER_NULL, NULLOID, NULLAEI, NULLPA,
                        NULLPC, PC_ACCEPT, ps->ps_prequirements, SR_DUPLEX,
                        SERIAL_NONE, ps->ps_settings, sf, NULLPEP, 0, aci);
    if (r == NOTOK)
        return NOTOK;

    ACSFREE(acs);

    if (PSetIndications(sd, (HFP)RpsDATAser, (HFP)RpsTOKENser, (HFP)RpsSYNCser,
                        (HFP)RpsACTIVITYser, (HFP)RpsREPORTser,
                        (HFP)RpsFINISHser, (HFP)RpsABORTser, pi) == NOTOK)
        return NOTOK;

    SetIoPool(sd, ob);

    return sd;
}
}

/*  */

/* ARGSUSED */

extern "C" {
int AssocRelRequest(int sd, struct AcSAPrelease* acr,
                    struct AcSAPindication* aci) {
    int result;

    if ((result = AcRelRequest(sd, ACF_NORMAL, NULLPEP, 0, NOTOK, acr, aci)) ==
        OK)
        vm->deleteIoHandler(sd);

    return result;
}
}

extern "C" {
int AssocUAbortRequest(int sd, struct AcSAPindication* aci) {
    int result;

    if ((result = AcUAbortRequest(sd, NULLPEP, 0, aci)) == OK)
        vm->deleteIoHandler(sd);

    return result;
}
}

extern "C" {
int AssocRelResponse(int sd, int status, int reason,
                     struct AcSAPindication* aci) {
    int result;

    if ((result = AcRelResponse(sd, status, reason, NULLPEP, 0, aci)) == OK)
        vm->deleteIoHandler(sd);

    return result;
}
}
/*  */
extern "C" {
void force_load_isode_iface() {
    AcFINISHser();
    addr2ref((char*)0);
    ode2oid((char*)0);
    tm2ut();
    _str2aei((char*)0, (char*)0, (char*)0, 0, (char*)0, (char*)0);
    Ro_Encode_and_Invoke(0, 0, 0, (HFP)0, (caddr_t)0,
                         (struct RoSAPindication*)0);
}
} /* end extern "C" */
