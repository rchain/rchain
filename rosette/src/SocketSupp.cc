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

/* Socket-support.cc: provides primitives routines for interfacing
 * Rosette to Unix sockets - particularly for TCP support
 */

/* INCLUDES */
#include "rosette.h"

#ifdef MIPS_SGI_SYSV
#include <sys/errno.h>
#else
#include <errno.h>
#endif
#include <fcntl.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/socket.h>
#ifdef MIPS_SGI_SYSV
#include <unistd.h>
#include <libc.h>
#else
#ifndef NO_SYSENT_H
#include <sysent.h>
#else
#include <unistd.h>
#include <libc.h>
#endif
#endif

#ifdef SYSV4
#define __STDC__ 1
extern "C" {
#include <netinet/in.h>
#ifndef MIPS_SGI_SYSV
#define __MALLOC_H
#endif
#include <netdb.h>
};
#undef __STDC__
#else

#ifdef __GNUG__
extern "C" {
#include <netinet/in.h>
#include <arpa/inet.h>
};
#else
#include <netinet/in.h>
#include <arpa/inet.h>
#endif

#include <netdb.h>
#endif

#include <stdio.h>
#include <varargs.h>
#include <memory.h>
#include <osfcn.h>

#include "BinaryOb.h"
#include "Ctxt.h"
#include "Operation.h"
#include "Prim.h"
#include "Reader.h"
#include "RBLstream.h"
#include "RBLstring.h"
#include "Tuple.h"
#include "Vm.h"
/*  */
extern StdOprn* oprnResumeIO;
extern Prim* obRuntimeError;
extern int setSocketAsync(int);
extern void AddIsodeIoHandler(int, IO_HANDLER*);
extern void SetIoPool(int, pOb);

/* SIGIO handlers for basic tcp interactions */
/*
extern "C" {
  int accept (int, void*, int*);
}
*/
void ConnectEventToRosette(VM_EVENT type, int fd, void* ob) {
    int cinfo[2]; /* [fd status] */
    int r, more;

    PROTECT(ob);

    more = 1;
    for (; more;) {
        pTuple av = Tuple::create(5, NIV);

        ASSIGN(av, elem(0), (Ob*)ob);
        ASSIGN(av, elem(1), FIXNUM(fd));
        ASSIGN(av, elem(2), FIXNUM(type));

        pCtxt c = Ctxt::create(oprnResumeIO, av);

        r = read(fd, (char*)cinfo, sizeof(int) * 2);
        if ((r == -1) && (RBL_WOULDBLOCK))
            break;

        if (r == 0) { /* connected fd and status both == #niv */
            close(fd);
            vm->deleteIoHandler(fd);
            more = 0;
        }
        else if (r < 0) {
            ASSIGN(av, elem(4), FIXNUM(-errno)); /* connected fd == #niv */
            more = 0;
        }
        else {
            ASSIGN(av, elem(3), FIXNUM(cinfo[0])); /* connected fd */
            ASSIGN(av, elem(4), FIXNUM(cinfo[1])); /* connect status */
        }

        BASE((Ob*)ob)->receive(c);
    }
}

void AcceptEventToRosette(VM_EVENT type, int fd, void* ob) {
    PROTECT(ob);
    int more;

    more = 1;
    for (; more;) {
        int new_fd = accept(fd, (struct sockaddr*)0, (int*)0);

        if ((new_fd == -1) && (RBL_WOULDBLOCK))
            break; /* no more connections to accept at this time */

        pTuple av = Tuple::create(4, NIV);

        ASSIGN(av, elem(0), (Ob*)ob);
        ASSIGN(av, elem(1), FIXNUM(fd));
        ASSIGN(av, elem(2), FIXNUM(type));

        if (new_fd == -1) {
            ASSIGN(av, elem(3), FIXNUM(-errno));
            more = 0;
        }
        else
            ASSIGN(av, elem(3), FIXNUM(new_fd));

        pCtxt c = Ctxt::create(oprnResumeIO, av);

        BASE((Ob*)ob)->receive(c);
    }
}

#define TCP_BUF_SIZE 2048
static char tcp_in_buf[TCP_BUF_SIZE];

void TcpEventToRosette(VM_EVENT type, int fd, void* ob) {
    int r, more;

    PROTECT(ob);

    more = 1;
    for (; more;) {
        pTuple av = Tuple::create(4, NIV);

        ASSIGN(av, elem(0), (Ob*)ob);
        ASSIGN(av, elem(1), FIXNUM(fd));
        ASSIGN(av, elem(2), FIXNUM(type));

        pCtxt c = Ctxt::create(oprnResumeIO, av);

        r = read(fd, tcp_in_buf, TCP_BUF_SIZE);
        if ((r == -1) && (RBL_WOULDBLOCK)) /* no more for now */
            break;                         /* don't signal ob */

        if (r == 0) { /* eof signalled by #niv in elem(3) */
            close(fd);
            vm->deleteIoHandler(fd);
            more = 0;
        }
        else if (r < 0) { /* err signalled via neg fixnum in elem(3) */
            ASSIGN(av, elem(3), FIXNUM(-errno));
            more = 0;
        }
        else {
            PROTECT(c);
            PROTECT(av);
            ByteVec* bv = ByteVec::create(r);
            memcpy((char*)&bv->byte(0), tcp_in_buf, r);
            ASSIGN(av, elem(3), bv);
        }

        BASE((Ob*)ob)->receive(c);
    }
}

void StringEventToRosette(VM_EVENT type, int fd, void* ob) {
    int r, more;

    PROTECT(ob);

    more = 1;
    for (; more;) {
        r = read(fd, tcp_in_buf, TCP_BUF_SIZE);
        if ((r == -1) && (RBL_WOULDBLOCK)) /* no more for now */
            break;                         /* don't signal ob */

        pTuple av = Tuple::create(4, NIV);

        ASSIGN(av, elem(0), (Ob*)ob);
        ASSIGN(av, elem(1), FIXNUM(fd));
        ASSIGN(av, elem(2), FIXNUM(type));

        pCtxt c = Ctxt::create(oprnResumeIO, av);

        if (r == 0) { /* eof signalled by #niv in elem(3) */
            close(fd);
            vm->deleteIoHandler(fd);
            more = 0;
        }
        else if (r < 0) { /* err signalled via neg fixnum in elem(3) */
            ASSIGN(av, elem(3), FIXNUM(-errno));
            more = 0;
        }
        else {
            PROTECT(c);
            PROTECT(av);
            RBLstring* str = RBLstring::create(r, tcp_in_buf);
            ASSIGN(av, elem(3), str);
        }

        BASE((Ob*)ob)->receive(c);
    }
}

/*    SOCKET ROUTINES */

int makeAsync(int fd, Ob* acter) {
    setSocketAsync(fd);
    AddIsodeIoHandler(fd, TcpEventToRosette);
    SetIoPool(fd, acter);
    TcpEventToRosette((VM_EVENT)0, fd, (void*)acter);
    return fd;
}

int makeTcpReader(int fd, Ob* acter) {
    setSocketAsync(fd);
    AddIsodeIoHandler(fd, StringEventToRosette);
    SetIoPool(fd, acter);
    StringEventToRosette((VM_EVENT)0, fd, (void*)acter);
    return fd;
}

int connected_pipe[2];
#define connected_in connected_pipe[0]
#define connected_out connected_pipe[1]

int initConnects(Ob* acter) {
    if (socketpair(AF_UNIX, SOCK_STREAM, 0, connected_pipe) < 0)
        return -errno;

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
    if (FD < 0)
        return -errno;

    if ((RESULT = fork()) == 0) {
        sigblock(-1); /* avoid gratuitous interrupts on stdin */
        if (connect(FD, (struct sockaddr*)&sin, sizeof(sin)) < 0)
            RESULT = -errno;
        write(connected_out, (const char*)connect_info, sizeof(int) * 2);
        _exit(0);
    }

    if (RESULT < 0)
        return -errno;

    return FD;
}

int tcpListen(int port, Ob* acter) {
    int fd, result;
    struct sockaddr_in sin;

    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = htonl(INADDR_ANY);
    sin.sin_port = htons(port);

    fd = socket(PF_INET, SOCK_STREAM, 0);
    if (fd < 0)
        return -errno;

    result = 1;
    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (const char*)&result,
               sizeof result);
    setSocketAsync(fd);
    AddIsodeIoHandler(fd, AcceptEventToRosette);
    SetIoPool(fd, acter);

    result = bind(fd, (struct sockaddr*)&sin, sizeof sin);
    if (result < 0)
        return -errno;

    result = listen(fd, 5);

    return fd;
}

int tcpAccept(int fd, Ob* acter) {
    int new_fd;

    new_fd = accept(fd, (struct sockaddr*)0, (int*)0);
    if (fd < 0)
        return -errno;

    return makeAsync(new_fd, acter);
}

int getSocketPort(int fd) {
    int len;
    struct sockaddr_in sin;

    if (getsockname(fd, (struct sockaddr*)&sin, &len) < 0)
        return -errno;

    return ntohs(sin.sin_port);
}

char* getPeerAddr(int fd) {
    int len;
    struct sockaddr_in sin;

    if (getpeername(fd, (struct sockaddr*)&sin, &len) < 0)
        return "";
    else
        return inet_ntoa(sin.sin_addr);
}

char* getPeerName(int fd) {
    int len;
    struct sockaddr_in sin;
    struct hostent* hep;

    if (getpeername(fd, (struct sockaddr*)&sin, &len) < 0)
        return "";

    hep = gethostbyaddr((char*)&(sin.sin_addr), sizeof(sin.sin_addr), AF_INET);

    if (hep)
        return hep->h_name;
    else
        return inet_ntoa(sin.sin_addr);
}

int tcpConnectByname(char* nm, int port) {
    struct hostent* hp; /* , *gethostbyname(); */

    hp = gethostbyname(nm);
    if (hp == 0)
        return -errno;

    return tcpConnectAux((char*)hp->h_addr, hp->h_length, port);
}

int tcpConnectByaddr(char* addr, int port) {
    in_addr_t ia;

    ia = inet_addr(addr);
    if (ia < 0)
        return -errno;

    return tcpConnectAux((char*)&ia, sizeof(ia), port);
}

/* Export interface to Rosette via primitives */

DEF("setAsyncByteVec", make_async, 2, 2) {
    CHECK_FIXNUM(0, fd);

    return FIXNUM(makeAsync(fd, ARG(1)));
}

DEF("setAsyncString", make_tcp_reader, 2, 2) {
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

    return FIXNUM(getSocketPort(fd));
}

DEF("init-connects", init_connects, 1, 1) {
    return FIXNUM(initConnects(ARG(0)));
}

DEF("get-peer-addr", get_peer_addr, 1, 1) {
    CHECK_FIXNUM(0, fd);

    return RBLstring::create(getPeerAddr(fd));
}

DEF("get-peer-name", get_peer_name, 1, 1) {
    CHECK_FIXNUM(0, fd);

    return RBLstring::create(getPeerName(fd));
}

DEF("tcp-connect-by-name", tcp_connect_by_name, 2, 2) {
    CHECK(0, RBLstring, nm);
    CHECK_FIXNUM(1, port);

    return FIXNUM(tcpConnectByname((char*)&nm->byte(0), port));
}

DEF("tcp-connect-by-addr", tcp_connect_by_addr, 2, 2) {
    CHECK(0, RBLstring, addr);
    CHECK_FIXNUM(1, port);

    return FIXNUM(tcpConnectByaddr((char*)&addr->byte(0), port));
}
