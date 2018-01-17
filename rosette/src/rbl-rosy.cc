/* Mode: -*- C++ -*- */
// vim: set ai ts=4 sw=4 expandtab
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

/* rbl-rosy.c - Rosette rosy interface */

/* INCLUDES */

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

extern "C" {
#include "acpkt.h"
#include "acsap.h"
#include "psap2.h"
#include "rosy.h"
}

#include <sys/errno.h>
#include "BinaryOb.h"
#include "Ctxt.h"
#include "Operation.h"
#include "Reader.h"
#include "RBLstream.h"
#include "RBLstring.h"
#include "Tuple.h"
#include "Vm.h"

#include <memory.h>
#include <osfcn.h>


/* DATA */

extern "C" {
/* ISODE ROSY ROUTINES */
char* isodeversion;
AcInit();
AcRelResponse();
endisoservent();
findopbyop();
getisoservent();
getisoserventbyname();
getisoserventbyselector();
_paddr2str();
RNetListen();
RoErrorRequest();
RoResultRequest();
RoSelectMask();
setisoservent();
sprintref();
str2oid();
}

/* the following are defined via extern "C" to avoid name-mangling so they
 * may be exported to Rosette
 */
extern "C" {
void* fdopenOstream(int fd, char* mode);
void IndicationToRosette(int fd, struct ROI* indication);
int get_pcid(char* asn_str, struct AcSAPstart* acs);
}


int RpsDATAser(int sd, struct PSAPdata* px);
int RpsTOKENser(int sd, PSAPtoke* pt);
int RpsSYNCser(int sd, PSAPsync* pn);
int RpsACTIVITYser(int sd, PSAPactivity* pv);
int RpsREPORTser(int sd, PSAPreport* pp);
int RpsFINISHser(int sd, struct PSAPfinish* pf);
int RpsABORTser(int sd, PSAPabort* pa);

void force_load_libisode() {
    /* ISODE ROSY ROUTINES */
    AcInit();
    AcRelResponse();
    endisoservent();
    findopbyop();
    getisoservent();
    getisoserventbyname();
    getisoserventbyselector();
    _paddr2str();
    RNetListen();
    RoErrorRequest();
    RoResultRequest();
    RoSelectMask();
    setisoservent();
    sprintref();
    str2oid();
}

/*  */
/* wrappers for virtual machine routines that are exported via foreign
 * functions to Rosette
 */

## #START insert of Isode - support.cc####extern StdOprn* oprnResumeIO;
extern Prim* obRuntimeError;

void AddIsodeIoHandler(int fd, IO_HANDLER* handler) {
    vm->addIoHandler(fd, handler, INVALID, 1);
}

void SetIoPool(int fd, pOb ob) { vm->ioPool[fd] = ob; }

struct ROI {
    int roi_type;
    int roi_un;
};

pOb ROI2ByteVec(struct ROI* indication, pTuple protos) {
    int type = indication->roi_type;

    if (type >= 0 && type < protos->numberOfElements()) {
        ByteVec* proto = (ByteVec*)protos->nth(type);
        int sz = proto->numberOfBytes();
        ByteVec* bv = ByteVec::create(proto, sz);
        memcpy((char*)&bv->byte(0), (char*)&indication->roi_un, sz);
        return bv;
    } else {
        return NIV;
    }
}

/* handler for ROSetIndications */
void IndicationToRosette(int fd, struct ROI* indication) {
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

/* handlers for asynchronous accept of ISODE connections */

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

void* fdopenOstream(int fd, char* mode) {
    FILE* f = fdopen(fd, mode);
    if (f) {
        return (void*)Ostream::create(f);
    } else {
        return FIXNUM(-errno);
    }
}


/* SIGIO handlers for basic tcp interactions */

extern "C" {
int accept(int, void*, int*);
}

void ConnectEventToRosette(int type, int fd, Ob* ob) {
    int cinfo[2]; /* [fd status] */
    int r, more;

    PROTECT(ob);

    more = 1;
    for (; more;) {
        pTuple av = Tuple::create(5, NIV);

        ASSIGN(av, elem(0), ob);
        ASSIGN(av, elem(1), FIXNUM(fd));
        ASSIGN(av, elem(2), FIXNUM(type));

        pCtxt c = Ctxt::create(oprnResumeIO, av);

        r = read(fd, (char*)cinfo, sizeof(int) * 2);
        if ((r == -1) && (errno == EWOULDBLOCK)) {
            break;
        }

        if (r == 0) { /* connected fd and status both == #niv */
            close(fd);
            vm->deleteIoHandler(fd);
            more = 0;
        } else if (r < 0) {
            ASSIGN(av, elem(4), FIXNUM(-errno)); /* connected fd == #niv */
            more = 0;
        } else {
            ASSIGN(av, elem(3), FIXNUM(cinfo[0])); /* connected fd */
            ASSIGN(av, elem(4), FIXNUM(cinfo[1])); /* connect status */
        }

        BASE(ob)->receive(c);
    }
}

void AcceptEventToRosette(int type, int fd, Ob* ob) {
    PROTECT(ob);
    int more;

    more = 1;
    for (; more;) {
        int new_fd = accept(fd, (void*)0, (int*)0);

        if ((new_fd == -1) && (errno == EWOULDBLOCK)) {
            break; /* no more connections to accept at this time */
        }

        pTuple av = Tuple::create(4, NIV);

        ASSIGN(av, elem(0), ob);
        ASSIGN(av, elem(1), FIXNUM(fd));
        ASSIGN(av, elem(2), FIXNUM(type));

        if (new_fd == -1) {
            ASSIGN(av, elem(3), FIXNUM(-errno));
            more = 0;
        } else {
            ASSIGN(av, elem(3), FIXNUM(new_fd));
        }

        pCtxt c = Ctxt::create(oprnResumeIO, av);

        BASE(ob)->receive(c);
    }
}

#define TCP_BUF_SIZE 2048
static char tcp_in_buf[TCP_BUF_SIZE];

void TcpEventToRosette(int type, int fd, Ob* ob) {
    int r, more;

    PROTECT(ob);

    more = 1;
    for (; more;) {
        pTuple av = Tuple::create(4, NIV);

        ASSIGN(av, elem(0), ob);
        ASSIGN(av, elem(1), FIXNUM(fd));
        ASSIGN(av, elem(2), FIXNUM(type));

        pCtxt c = Ctxt::create(oprnResumeIO, av);

        r = read(fd, tcp_in_buf, TCP_BUF_SIZE);
        if ((r == -1) && (errno == EWOULDBLOCK)) { /* no more for now */
            break;                                 /* don't signal ob */
        }

        if (r == 0) { /* eof signalled by #niv in elem(3) */
            close(fd);
            vm->deleteIoHandler(fd);
            more = 0;
        } else if (r < 0) { /* err signalled via neg fixnum in elem(3) */
            ASSIGN(av, elem(3), FIXNUM(-errno));
            more = 0;
        } else {
            PROTECT(c);
            PROTECT(av);
            ByteVec* bv = ByteVec::create(r);
            memcpy((char*)&bv->byte(0), tcp_in_buf, r);
            ASSIGN(av, elem(3), bv);
        }

        BASE(ob)->receive(c);
    }
}

void StringEventToRosette(int type, int fd, Ob* ob) {
    int r, more;

    PROTECT(ob);

    more = 1;
    for (; more;) {
        pTuple av = Tuple::create(4, NIV);

        ASSIGN(av, elem(0), ob);
        ASSIGN(av, elem(1), FIXNUM(fd));
        ASSIGN(av, elem(2), FIXNUM(type));

        pCtxt c = Ctxt::create(oprnResumeIO, av);

        r = read(fd, tcp_in_buf, TCP_BUF_SIZE);
        if ((r == -1) && (errno == EWOULDBLOCK)) { /* no more for now */
            break;                                 /* don't signal ob */
        }

        if (r == 0) { /* eof signalled by #niv in elem(3) */
            close(fd);
            vm->deleteIoHandler(fd);
            more = 0;
        } else if (r < 0) { /* err signalled via neg fixnum in elem(3) */
            ASSIGN(av, elem(3), FIXNUM(-errno));
            more = 0;
        } else {
            PROTECT(c);
            PROTECT(av);
            RBLstring* str = RBLstring::create(r, tcp_in_buf);
            ASSIGN(av, elem(3), str);
        }

        BASE(ob)->receive(c);
    }
}

void deleteIoHandler(int fd) { vm->deleteIoHandler(fd); }

## #END insert of Isode -
    support.cc## #

    /* PRIMS should go to Ob.cc !!! */

    static local_page_size = getpagesize();

DEF("M-get", addressGetField, 3, 3) {
    CHECK_FIXNUM(0, base);
    CHECK_FIXNUM(1, span);
    CHECK(2, RblBool, sign);

    int offset = base % 4;
    int addr = base - offset;

    if (base < local_page_size) {
        PRIM_ERROR("invalid address");
    } else {
        pOb rslt = BASE((pOb)addr)->getField(0, 0, offset, span, BOOLVAL(sign));

        return (rslt == INVALID ? PRIM_ERROR("invalid bit range") : rslt);
    }
}

DEF("M-set", addressSetField, 3, 3) {
    CHECK_FIXNUM(0, base);
    CHECK_FIXNUM(1, span);
    CHECK_FIXNUM(2, val);

    int offset = base % 4;
    int addr = base - offset;

    if (base < local_page_size) {
        PRIM_ERROR("invalid address");
    } else {
        pOb rslt = BASE((pOb)addr)->setField(0, 0, offset, span, (uint32_t)val);

        return (rslt == INVALID ? PRIM_ERROR("invalid bit range") : rslt);
    }
}

DEF("char*->string", char_star_to_string, 1, 1) {
    CHECK_FIXNUM(0, addr);

    if (base >= local_page_size) {
        return RBLstring::create((char*)addr);
    } else {
        PRIM_ERROR("invalid address");
    }
}

DEF("ob@", ob_address, 1, 1) { return FIXNUM(&(BASE(ARG(0)))); }

DEF("saddr", struct_address, 1, 1) {
    return FIXNUM(&(BASE(FIXVAL(ARG(0)))->slot(0)));
}

DEF("malloc", unix_malloc, 1, 1) {
    CHECK_FIXNUM(0, n_bytes);

    return FIXNUM(malloc(n_bytes));
}

DEF("u_free", unix_free, 1, 1) {
    CHECK_FIXNUM(0, addr);

    if (addr >= local_page_size) {
        return FIXNUM(free(addr));
    } else {
        PRIM_ERROR("invalid address");
    }
}

DEF("memcpy", unix_memcpy, 3, 3) {
    CHECK_FIXNUM(0, dest_addr);
    CHECK_FIXNUM(1, src_addr);
    CHECK_FIXNUM(2, n_bytes);

    if ((dest_addr >= local_page_size) && (src_addr >= local_page_size)) {
        return FIXNUM(memcpy(dest_addr, src_addr, n_bytes));
    } else {
        PRIM_ERROR("invalid address");
    }
}

DEF("_c2bv", c_struct_to_byte_vec, 3, 3) {
    CHECK_FIXNUM(1, src_addr);
    CHECK_FIXNUM(2, n_bytes);

    if (src_addr >= local_page_size) {
        memcpy(&(BASE(ARG(0))->slot(0)), src_addr, n_bytes);
        return ARG(0);
    } else {
        PRIM_ERROR("invalid address");
    }
}

DEF("c2bv", c_struct_to_byte_vec, 2, 2) {
    CHECK(0, ByteVec, dest_bv);
    CHECK_FIXNUM(1, src_addr);

    if (src_addr >= local_page_size) {
        memcpy(&(dest_bv->byte(0)), src_addr, dest_bv->numberOfBytes());
        return dest_bv;
    } else {
        PRIM_ERROR("invalid address");
    }
}

DEF("c2str", cpy_char_star_to_string, 2, 2) {
    CHECK(0, RBLstring, dest_str);
    CHECK_FIXNUM(1, src_addr);

    if (src_addr >= local_page_size) {
        memcpy(&(dest_str->byte(0), src_addr, dest_str->numberOfBytes()));
        return dest_str;
    } else {
        PRIM_ERROR("invalid address");
    }
}

DEF("string->fx", string_to_fx, 1, 1) {
    CHECK(0, RBLstring, str);
    return FIXNUM(strtol(&(str->byte(0)), (char**)0, 0));
}

DEF("fx->string", fx_to_string, 1, 1) {
    CHECK_FIXNUM(0, val);
    char buf[16];
    sprintf(buf, "%d", val);
    return RBLstring::create(buf);
}

DEF("strlen", c_strlen, 1, 1) {
    CHECK_FIXNUM(0, addr);
    if (addr >= local_page_size) {
        return FIXNUM(strlen((char*)addr));
    } else {
        PRIM_ERROR("invalid address");
    }
}

DEF("prim-string->", cpy_string_char_star, 2, 2) {
    CHECK_FIXNUM(0, dest_addr);
    CHECK(1, RBLstring, src_str);

    if (dest_addr >= local_page_size) {
        strcpy(dest_addr, &(src_str->byte(0)));
        return src_str;
    } else {
        PRIM_ERROR("invalid address");
    }
}

DEF("set-io-pool", set_io_pool, 1, 1) {
    CHECK_FIXNUM(0, fd);
    SetIoPool(fd, ARG(1));
    return ARG(1);
}

DEF("delete-io-handler", delete_io_handler, 1, 1) {
    CHECK_FIXNUM(0, fd);
    deleteIoHandler(fd);
    return ARG(0);
}

/* END PRIMS for Ob.cc (or where ever) */

/*    Internet address manipulation */

DEF("prim_inet_addr", prim_inet_addr, 1, 1) {
    CHECK(0, RBLstring, str);
    return FIXNUM(inet_addr(&str->byte(0)));
}

DEF("prim_inet_network", prim_inet_network, 1, 1) {
    CHECK(0, RBLstring, str);
    return FIXNUM(inet_network(&str->byte(0)));
}

DEF("prim_inet_makeaddr", prim_inet_makeaddr, 3, 3) {
    CHECK_FIXNUM(0, in_addr_addr);
    CHECK_FIXNUM(1, net);
    CHECK_FIXNUM(2, lna);
    struct in_addr* ap = (struct in_addr*)in_addr_addr;

    if (in_addr_addr >= local_page_size) {
        *ap = inet_makeaddr(net, lna);
        return ARG(0);
    } else {
        PRIM_ERROR("invalid address");
    }
}

DEF("prim_inet_lnaof", prim_inet_lnaof, 1, 1) {
    CHECK_FIXNUM(0, in_addr_addr);
    struct in_addr* ap = (struct in_addr*)in_addr_addr;

    if (in_addr_addr >= local_page_size) {
        return FIXNUM(inet_lnaof(*ap));
    } else {
        PRIM_ERROR("invalid address");
    }
}

DEF("prim_inet_netof", prim_inet_netof, 1, 1) {
    CHECK_FIXNUM(0, in_addr_addr);
    struct in_addr* ap = (struct in_addr*)in_addr_addr;

    if (in_addr_addr >= local_page_size) {
        return FIXNUM(inet_netof(*ap));
    } else {
        PRIM_ERROR("invalid address");
    }
}

DEF("prim_inet_ntoa", prim_inet_ntoa, 1, 1) {
    CHECK_FIXNUM(0, in_addr_addr);
    struct in_addr* ap = (struct in_addr*)in_addr_addr;

    if (in_addr_addr >= local_page_size) {
        return RBLstring::create(inet_ntoa(*ap));
    } else {
        PRIM_ERROR("invalid address");
    }
}

/*    SOCKET ROUTINES */

int setSocketAsync(int fd) {
    int x;
    if (fcntl(fd, F_SETFL, FNDELAY | FASYNC) < 0) {
        return -errno;
    }

    if (fcntl(fd, F_SETOWN, getpid()) < 0) {
        return -errno;
    }

    return fd;
}

void makeAsync(int fd, Ob* acter) {
    setSocketAsync(fd);
    AddIsodeIoHandler(fd, TcpEventToRosette);
    SetIoPool(fd, acter);
}

int makeTcpReader(int fd, Ob* acter) {
    setSocketAsync(fd);
    AddIsodeIoHandler(fd, StringEventToRosette);
    SetIoPool(fd, acter);
    return fd;
}

int connected_pipe[2];
#define connected_in connected_pipe[0]
#define connected_out connected_pipe[1]

int initConnects(Ob* acter) {
    if (pipe(connected_pipe) < 0) {
        return -errno;
    }

    setSocketAsync(connected_in);
    AddIsodeIoHandler(connected_in, ConnectEventToRosette);
    SetIoPool(connected_in, acter);

    return connected_in;
}

int tcpConnectAux(char* addr, int len, int port) {
    int connect_info[2];
#define FD connect_info[0]
#define RESULT connect_info[1]

    struct sockaddr_in sin;

    sin.sin_family = AF_INET;
    bcopy(addr, (char*)&sin.sin_addr, len);
    sin.sin_port = htons(port);

    FD = socket(PF_INET, SOCK_STREAM, 0);
    if (FD < 0) {
        return -errno;
    }

    if ((RESULT = fork()) == 0) {
        sigblock(-1); /* avoid gratuitous interrupts on stdin */
        if (connect(FD, (struct sockaddr*)&sin, sizeof(sin)) < 0) {
            RESULT = -errno;
        }

        write(connected_out, connect_info, sizeof(int) * 2);
        _exit(0);
    }

    if (RESULT < 0) {
        return -errno;
    }

    return FD;
}

int tcpListen(int port, Ob* acter) {
    int fd, result;
    struct sockaddr_in sin;

    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = htonl(INADDR_ANY);
    sin.sin_port = htons(port);

    fd = socket(PF_INET, SOCK_STREAM, 0);
    if (fd < 0) {
        return -errno;
    }

    result = 1;
    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &result, sizeof result);
    setSocketAsync(fd);
    AddIsodeIoHandler(fd, AcceptEventToRosette);
    SetIoPool(fd, acter);

    result = bind(fd, (struct sockaddr*)&sin, sizeof sin);
    if (result < 0) {
        return -errno;
    }

    result = listen(fd, 5);
    return fd;
}

int tcpAccept(int fd, Ob* acter) {
    int new_fd;

    new_fd = accept(fd, (struct sockaddr*)0, (int*)0);
    if (fd < 0) {
        return -errno;
    }

    makeAsync(new_fd, acter);
    return new_fd;
}

int getSocketPort(int fd) {
    int len;
    struct sockaddr_in sin;

    if (getsockname(fd, (struct sockaddr*)&sin, &len) < 0) {
        return -errno;
    }

    return ntohs(sin.sin_port);
}

char* getPeerAddr(int fd) {
    int len;
    struct sockaddr_in sin;

    if (getpeername(fd, (struct sockaddr*)&sin, &len) < 0) {
        return "";
    } else {
        return inet_ntoa(sin.sin_addr);
    }
}

char* getPeerName(int fd) {
    int len;
    struct sockaddr_in sin;
    struct hostent* hep;

    if (getpeername(fd, (struct sockaddr*)&sin, &len) < 0) {
        return "";
    }

    hep = gethostbyaddr(&(sin.sin_addr), sizeof(sin.sin_addr), AF_INET);

    if (hep) {
        return hep->h_name;
    } else {
        return inet_ntoa(sin.sin_addr);
    }
}

int tcpConnectByname(char* nm, int port) {
    struct hostent *hp, *gethostbyname();

    hp = gethostbyname(nm);
    if (hp == 0) {
        return -errno;
    }

    return tcpConnectAux((char*)hp->h_addr, hp->h_length, port);
}

int tcpConnectByaddr(char* addr, int port) {
    unsigned long ia;

    ia = inet_addr(addr);
    if (ia < 0) {
        return -errno;
    }

    return tcpConnectAux((char*)&ia, sizeof(ia), port);
}

int uWrite(int fd, char* buf, int len) {
    int result;

    if ((result = write(fd, buf, len)) < 0) {
        result = -errno;
    }

    return result;
}

int uRead(int fd, char* buf, int len) {
    int result;

    if ((result = read(fd, buf, len)) < 0) {
        result = -errno;
    }

    return result;
}

int uClose(int fd) {
    int result;

    if ((fd < 0) || (fd >= NOFILE)) {
        return -(EBADF);
    }

    if ((result = close(fd)) < 0) {
        result = -errno;
    }

    deleteIoHandler(fd);

    return result;
}

/*    GET PCTXTID from AcSAPStart */

int get_pcid(char* asn_str, struct AcSAPstart* acs) {
    int i;
    OID asn = str2oid(asn_str);

    struct PSAPstart* ps = &acs->acs_start;
    struct PSAPctxlist* pcl = &ps->ps_ctxlist;
    struct PSAPcontext* pc;

    for (i = 0, pc = pcl->pc_ctx; i < pcl->pc_nctx; i++, pc++) {
        if (oid_cmp(pc->pc_asn, asn) == 0) {
            return pc->pc_id;
        }
    }

    return NOTOK;
}


/*    ENCODE AND INVOKE */

int Ro_Encode_and_Invoke(int fd, int op, int invokeID, IFP efn, caddr_t in,
                         struct RoSAPindication* roi) {
    PE pe = NULLPE;
    int result;

    PY_pepy[0] = 0;
    if (!efn || (result = (*efn)(&pe, 1, NULL, NULLCP, in)) == OK) {
        result = RoInvokeRequest(fd, op, ROS_ASYNC, pe, invokeID, NULLIP,
                                 ROS_NOPRIO, roi);
    } else {
        result =
            rosaplose(roi, ROS_CONGEST, NULLCP,
                      "error encoding argument for invocation %d, op %d, [%s]",
                      invokeID, op, PY_pepy);
    }

    if (pe) {
        pe_free(pe);
    }

    return result;
}

/*    ROSETTE INITIATOR */

/* ARGSUSED */

void rosette_advise(struct AcSAPabort* aca, char* event) {
    char buffer[BUFSIZ];

    if (aca->aca_cc > 0) {
        (void)sprintf(buffer, "[%s] %*.*s", AcErrString(aca->aca_reason),
                      aca->aca_cc, aca->aca_cc, aca->aca_data);
    } else {
        (void)sprintf(buffer, "[%s]", AcErrString(aca->aca_reason));
    }

    printf("%s: %s (source %d)", event, buffer, aca->aca_source);
}

int Assoc_Initiate(char* myname, char* hostname, char* myservice,
                   char* mycontext, char* mypci, Ob* ob, int port) {
    int iloop, sd;
    char buffer[BUFSIZ], *vec[NVEC + 1];
    register struct dispatch* ds;
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

    if ((aei = str2aei(hostname, myservice)) == NULLAEI) {
        printf("%s-%s: unknown application-entity", hostname, myservice);
    }

    if ((pa = aei2addr(aei)) == NULLPA) {
        printf("address translation failed");
        return NOTOK;
    }

    if (port) {
        (pa->pa_addr).sa_addr.ta_addrs[0].na_un.un_na_tcp.na_tcp_port = port;
    }

    if ((ctx = ode2oid(mycontext)) == NULLOID) {
        printf("%s: unknown object descriptor", mycontext);
    }

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
        bzero((char*)sf, sizeof *sf);
    }

    printf("%s", myname);
    if (sf->sr_ulen > 2) {
        printf(" running on host %s", sf->sr_udata + 2);
    }

    if (sf->sr_clen > 2) {
        printf(" at %s", sf->sr_cdata + 2);
    }

    printf(" [%s, ", oid2ode(ctx));
    printf("%s]\n", oid2ode(pci));
    printf("using %s\n", isodeversion);
    printf("%s... ", hostname);
    fflush(stdout);

    return RyAssocRequest(ctx, aei, pa, pc, sf, roi, RoPService, NULL, ob,
                          IndicationToRosette);
}

int RyAssocRequest(OID ctx, AEI aei, struct PSAPaddr* pa,
                   struct PSAPctxlist* pc, struct SSAPref* sf,
                   struct RoSAPindication* roi, IFP svc, int asy, Ob* ob,
                   IFP hndlr) {
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
    } else {
        if (acc->acc_result != ACS_ACCEPT) {
            printf("association rejected: [%s]\n",
                   AcErrString(acc->acc_result));

            return NOTOK;
        } else {
            sd = acc->acc_sd;
            ACCFREE(acc);

            if (RoSetService(sd, svc, roi) == NOTOK) {
                printf("set RO/PS fails\n");
            }

            if (RoSetIndications(sd, hndlr, roi) == NOTOK) {
                printf("set RO/Handler fails\n");
            }

            SetIoPool(sd, ob);
            return sd;
        }
    }
}

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
    } else {
        if (acc->acc_result != ACS_ACCEPT) {
            printf("association rejected: [%s]\n",
                   AcErrString(acc->acc_result));
            return NOTOK;
        } else {
            sd = acc->acc_sd;
            ACCFREE(acc);

            if (PSetIndications(sd, RpsDATAser, RpsTOKENser, RpsSYNCser,
                                RpsACTIVITYser, RpsREPORTser, RpsFINISHser,
                                RpsABORTser, pi) == NOTOK)
                return NOTOK;

            SetIoPool(sd, ob);
            return sd;
        }
    }
}
/*  */

int RpsDATAser(int sd, struct PSAPdata* px) {
    return RPsHandler(sd, PI_DATA, (char*)px);
}

void RpsDATAfree(struct PSAPdata* px) { PXFREE(px); }

int RpsTOKENser(int sd, PSAPtoke* pt) {
    return RPsHandler(sd, PI_TOKEN, (char*)pt);
}

int RpsSYNCser(int sd, PSAPsync* pn) { RPsHandler(sd, PI_SYNC, (char*)pn); }

int RpsACTIVITYser(int sd, PSAPactivity* pv) {
    return RPsHandler(sd, PI_ACTIVITY, pv);
}

int RpsREPORTser(int sd, PSAPreport* pp) {
    return RPsHandler(sd, PI_REPORT, pp);
}

int RpsFINISHser(int sd, struct PSAPfinish* pf) {
    return RPsHandler(sd, PI_FINISH, pf);
}

int RpsABORTser(int sd, PSAPabort* pa) {
    struct assocblk* acb;

    if ((acb = findacblk(sd)) != NULL) {
        acb->acb_fd = NOTOK;
        freeacblk(acb);
    }

    return RPsHandler(sd, PI_ABORT, pa);
}

/* RpsFree:
 *	Added by G. Lavender to fix memory leaks.
 * 	RpsFree is called from Rosette code after
 * 	the PE's imbedded in the indication structures
 * 	are decoded. (Elegance goes out the window when
 * 	under pressure at the SQLAcess testbed)
 */

void RpsFree(void* pi, int type) {
    struct PSAPdata* px;
    struct PSAPtoken* pt;
    struct PSAPsync* pn;
    struct PSAPactivity* pv;
    struct PSAPreport* pr;
    struct PSAPfinish* pf;
    struct PSAPabort* pa;

    if (!pi) {
        return;
    }

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

/*  */

/* ARGSUSED */

#define SR_DUPLEX 0x0002
#define ROI_NOTOK -2

int AcAccept(struct AcSAPstart* acs, struct SSAPref* sf, int svc, Ob* ob,
             IFP hndlr, struct AcSAPindication* aci,
             struct RoSAPindication* roi) {
    int r, sd = acs->acs_sd;
    struct PSAPstart* ps = &(acs->acs_start);

    r = AcAssocResponse(sd, ACS_ACCEPT, ACS_USER_NULL, NULLOID, NULLAEI, NULLPA,
                        NULLPC, PC_ACCEPT, ps->ps_prequirements, SR_DUPLEX,
                        SERIAL_NONE, ps->ps_settings, sf, NULLPEP, 0, aci);
    if (r == NOTOK) {
        return NOTOK;
    }

    ACSFREE(acs);

    if (RoSetService(sd, svc, roi) == NOTOK) {
        return ROI_NOTOK;
    }

    if (RoSetIndications(sd, hndlr, roi) == NOTOK) {
        return ROI_NOTOK;
    }

    SetIoPool(sd, ob);
    return sd;
}

int PsAcAccept(struct AcSAPstart* acs, struct SSAPref* sf, Ob* ob,
               struct AcSAPindication* aci, struct PSAPindication* pi) {
    int r, sd = acs->acs_sd;
    struct PSAPstart* ps = &(acs->acs_start);

    r = AcAssocResponse(sd, ACS_ACCEPT, ACS_USER_NULL, NULLOID, NULLAEI, NULLPA,
                        NULLPC, PC_ACCEPT, ps->ps_prequirements, SR_DUPLEX,
                        SERIAL_NONE, ps->ps_settings, sf, NULLPEP, 0, aci);
    if (r == NOTOK) {
        return NOTOK;
    }

    ACSFREE(acs);

    if (PSetIndications(sd, RpsDATAser, RpsTOKENser, RpsSYNCser, RpsACTIVITYser,
                        RpsREPORTser, RpsFINISHser, RpsABORTser, pi) == NOTOK) {
        return NOTOK;
    }

    SetIoPool(sd, ob);
    return sd;
}

/*  */

/* ARGSUSED */

int AssocRelRequest(int sd, struct AcSAPrelease* acr,
                    struct AcSAPindication* aci) {
    int result;

    if ((result = AcRelRequest(sd, ACF_NORMAL, NULLPEP, 0, NOTOK, acr, aci)) ==
        OK) {
        deleteIoHandler(sd);
    }

    return result;
}

int AssocUAbortRequest(int sd, struct AcSAPindication* aci) {
    int result;

    if ((result = AcUAbortRequest(sd, NULLPEP, 0, aci)) == OK) {
        deleteIoHandler(sd);
    }

    return result;
}

int AssocRelResponse(int sd, int status, int reason,
                     struct AcSAPindication* aci) {
    int result;

    if ((result = AcRelResponse(sd, status, reason, NULLPEP, 0, aci)) == OK) {
        deleteIoHandler(sd);
    }

    return result;
}
/*  */

void force_load_isode_iface() {
    MemFetch();
    MemSet();
    char_star2string();
    Rinet_makeaddr();
    Rinet_lnaof();
    Rinet_netof();
    Rinet_ntoa();
    setSocketAsync();
    makeAsync();
    makeTcpReader();
    initConnects();
    tcpConnectAux();
    tcpListen();
    tcpAccept();
    getSocketPort();
    getPeerAddr();
    getPeerName();
    tcpConnectByname();
    tcpConnectByaddr();
    uWrite();
    uRead();
    uClose();
    get_pcid();
    Ro_Encode_and_Invoke();
    rosette_advise();
    Assoc_Initiate();
    RyAssocRequest();
    PsAssocRequest();
    RpsDATAser();
    RpsDATAfree();
    RpsTOKENser();
    RpsSYNCser();
    RpsACTIVITYser();
    RpsREPORTser();
    RpsFINISHser();
    RpsABORTser();
    RpsFree();
    AcAccept();
    PsAcAccept();
    AssocRelRequest();
    AssocUAbortRequest();
    AssocRelResponse();
}

/*  */

DEF("makeAsync", make_async, 2, 2) {
    CHECK_FIXNUM(0, fd);
    makeAsync(fd, ARG(1));
    return NIV;
}

DEF("uClose", unix_close, 1, 1) {
    CHECK_FIXNUM(0, fd);
    return FIXNUM(uClose(fd));
}

DEF("uRead", unix_read, 3, 3) {
    CHECK_FIXNUM(0, fd);
    CHECK(1, ByteVec, buf);
    CHECK_FIXNUM(2, len);
    return FIXNUM(uRead(fd, &buf->byte(0), len));
}

DEF("uWrite", unix_write, 3, 3) {
    CHECK_FIXNUM(0, fd);
    CHECK(1, ByteVec, buf);
    CHECK_FIXNUM(2, len);
    return FIXNUM(uWrite(fd, &buf->byte(0), len));
}

DEF("makeTcpReader", make_tcp_reader, 2, 2) {
    CHECK_FIXNUM(0, fd);
    return FIXNUM(makeTcpReader(fd, ARG(1)));
}

DEF("tcpListen", tcp_listen, 2, 2) {
    CHECK_FIXNUM(0, fd);
    return FIXNUM(tcpListen(fd, ARG(1)));
}

DEF("tcpAccept", tcp_accept, 2, 2) {
    CHECK_FIXNUM(0, fd);
    return FIXNUM(tcpAccept(fd, ARG(1)));
}

DEF("get-socket-port", get_socket_port, 1, 1) {
    CHECK_FIXNUM(0, fd);
    return getSocketPort(fd);
}

DEF("init-connects", init_connects, 1, 1) { return initConnects(ARG(0)); }

DEF("get-peer-addr", get_peer_addr, 1, 1) {
    CHECK_FIXNUM(0, fd);
    return getPeerAddr(fd);
}

DEF("get-peer-name", get_peer_name, 1, 1) {
    CHECK_FIXNUM(0, fd);
    return getPeerName(fd);
}

DEF("tcp-connect-by-name", tcp_connect_by_name, 2, 2) {
    CHECK(0, RBLstring, nm);
    CHECK_FIXNUM(1, port);
    return FIXNUM(tcpConnectByname(&nm->byte(0), port));
}

DEF("tcp-connect-by-addr", tcp_connect_by_addr, 2, 2) {
    CHECK(0, RBLstring, addr);
    CHECK_FIXNUM(1, port);
    return FIXNUM(tcpConnectByaddr(&addr->byte(0), port));
}
