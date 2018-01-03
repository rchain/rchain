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

/* Basic-support.cc: provides a variety of useful prims related to
 * system programming in Rosette
 */

#include "rosette.h"

#include <algorithm>
#include <cerrno>
#include <regex>
#include <string>

#include <arpa/inet.h>
#include <fcntl.h>
#include <memory.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdarg.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>
#include <unistd.h>

#include "RblAtom.h"
#include "BinaryOb.h"
#include "Ctxt.h"
#include "MI.h"
#include "Ob.h"
#include "Operation.h"
#include "Prim.h"
#include "Reader.h"
#include "RBLstream.h"
#include "RBLstring.h"
#include "StreamUtils.h"
#include "Table.h"
#include "Tuple.h"
#include "Vm.h"
#include "Addr.h"

#ifdef SYSV4
#include <stropts.h>
#endif

#ifdef MAP_BACK_ADDRESS
extern uint32_t nontrivial_pre_fixnum_to_addr(int);
extern int nontrivial_addr_to_pre_fixnum(Ob*);
#endif

// NB(leaf): Gets the page size at program startup.
extern const uint32_t local_page_size = (uint32_t)sysconf(_SC_PAGESIZE);

extern StdOprn* oprnResumeIO;
extern Prim* obRuntimeError;

extern Ob* obSBO;
extern Ob* emptyMbox;

DEF_OPRN(Sync, "kind", oprnKind, obRuntimeError);

Ob* newSBO(Ob* proto_sbo, Ob* id, Ob* prnt, Ob* ctxt) {
    PROTECT(proto_sbo);
    PROTECT(id);
    PROTECT(prnt);
    PROTECT(ctxt);
    MboxOb* new_meta = (MboxOb*)(proto_sbo->meta()->clone());
    ASSIGN(new_meta, mbox, emptyMbox);
    PROTECT(new_meta);
    MboxOb* new_sbo = (MboxOb*)(proto_sbo->clone());

    ASSIGN(new_sbo, meta(), new_meta);
    ASSIGN(new_sbo, mbox, emptyMbox);
    ASSIGN(new_sbo, parent(), prnt);

    new_meta->add(new_sbo, oprnKind, id, (Ctxt*)ctxt);

    return new_sbo;
}

int slashify_char(char c, char buf[], int slash_blank) {
    const char escapeChar = '\\';

    if (c == escapeChar) {
        buf[0] = c;
        buf[1] = c;
        buf[2] = 0;
    } else if (slash_blank && (c == ' ')) {
        buf[0] = escapeChar;
        buf[1] = ' ';
    } else if (isprint(c)) {
        buf[0] = c;
        buf[1] = 0;
    } else {
        buf[0] = escapeChar;
        switch (c) {
        case '\n':
            buf[1] = 'n';
            break;
        case '\b':
            buf[1] = 'b';
            break;
        case '\f':
            buf[1] = 'f';
            break;
        case '\t':
            buf[1] = 't';
            break;
        case '\r':
            buf[1] = 'r';
            break;
        default:
            if (isprint(c)) {
                buf[1] = c;
            } else {
                sprintf(&buf[1], "x%02x", (uint8_t)c);
            }
            break;
        }

        switch (c) {
        case '\n':
        case '\b':
        case '\f':
        case '\t':
        case '\r':
            buf[2] = 0;
            break;
        default:
            if (isprint(c)) {
                buf[2] = 0;
            } else {
                buf[4] = 0;
            }
            break;
        }
    }

    return strlen((char*)buf);
}

DEF("ch->printString", chToPrintString, 1, 1) {
    CHECK(0, Char, cOb);
    char c = CHARVAL(cOb);
    char buf[8];

    buf[0] = '#';
    buf[1] = '\\';

    slashify_char(c, &buf[2], 0);

    return RBLstring::create((char*)buf);
}

DEF("str->printString", strToPrintString, 1, 1) {
    CHECK(0, RBLstring, str);
    PROTECT(str);
    char buf[8];
    int sz = (str->numberOfBytes()) - 1;

    int n = 0;
    int i = 0;

    for (; i < sz; i++) {
        n += slashify_char(str->byte(i), buf, 0);
    }

    RBLstring* result = RBLstring::create(n + 1, (char)0);

    char* x = (char*)&result->byte(0);

    for (i = 0; i < sz; i++) {
        x += slashify_char(str->byte(i), x, 0);
    }

    return result;
}

DEF("symb->printString", symbToPrintString, 1, 1) {
    CHECK_SYM(0, symb);
    char* str = SYMPTR(symb);

    char buf[8];
    int sz = strlen(str); /* (str->numberOfBytes()) - 1; */

    int n = 0;
    int i = 0;

    for (; i < sz; i++) {
        n += slashify_char(*(str + i), buf, 1);
    }

    RBLstring* result = RBLstring::create(n + 1, (char)0);

    char* x = (char*)&result->byte(0);

    for (i = 0; i < sz; i++) {
        x += slashify_char(*(str + i), x, 1);
    }

    return result;
}

DEF("prim-new-SBO", obNewSBO, 3, 3) {
    PROTECT(__CTXT__);
    return newSBO(ARG(0), ARG(1), ARG(2), __CTXT__);
}

Ob* genActor(Ob* proto, Ob* sbo) {
    PROTECT(sbo);
    PROTECT(proto);

    MboxOb* new_actor = (MboxOb*)(proto->clone());
    PROTECT(new_actor);

    MboxOb* new_meta = (MboxOb*)(new_actor->meta()->clone());
    ASSIGN(new_meta, mbox, emptyMbox);

    ASSIGN(new_actor, mbox, emptyMbox);
    ASSIGN(new_actor, parent(), sbo);
    ASSIGN(new_actor, meta(), new_meta);

    return new_actor;
}

DEF("prim-gen-actor", obGenActor, 3, 3) {
    PROTECT(__CTXT__);
    Ob* new_sbo = newSBO(obSBO, ARG(1), ARG(2), __CTXT__);
    PROTECT(new_sbo);
    Ob* new_actor = genActor(ARG(0), new_sbo);
    PROTECT(new_actor);

    Tuple* rslt = Tuple::create(2, INVALID);
    ASSIGN(rslt, elem(0), new_sbo);
    ASSIGN(rslt, elem(1), new_actor);

    return rslt;
}

uint32_t mem_get_field(uint32_t* addr, int offset, int span, int sign) {
    uint32_t ans = 0;

    switch (span) {
    case 8:
        if (sign) {
            ans = *(int8_t*)((int8_t*)addr + (offset / BITS(int8_t)));
        } else {
            ans = *(uint8_t*)((int8_t*)addr + (offset / BITS(int8_t)));
        }
        break;
    case 16:
        if (sign) {
            ans = *(int16_t*)((int16_t*)addr + (offset / BITS(int16_t)));
        } else {
            ans = *(uint16_t*)((int16_t*)addr + (offset / BITS(int16_t)));
        }
        break;
    case 32:
        if (sign) {
            ans = *(int32_t*)((int32_t*)addr + (offset / BITS(int32_t)));
        } else {
            ans = *(uint32_t*)((int32_t*)addr + (offset / BITS(int32_t)));
        }
        break;
    default: {
        // Fields that are not multiples of 8 bits are not expected. Previously,
        // this contained some complex and questionable big-endian dependent
        // code.
        ans = 0;
        break;
    }
    }

    return ans;
}

uint32_t* mem_set_field(uint32_t* addr, int offset, int span, uint32_t bits) {
    switch (span) {
    case 8:
        *(uint8_t*)((int8_t*)addr + (offset / BITS(int8_t))) = (uint8_t)bits;
        break;
    case 16:
        *(uint16_t*)((int16_t*)addr + (offset / BITS(int16_t))) =
            (uint16_t)bits;
        break;
    case 32:
        *(uint32_t*)((int32_t*)addr + (offset / BITS(int32_t))) =
            (uint32_t)bits;
        break;
    default: {
        // Fields that are not multiples of 8 bits are not expected. Previously,
        // this contained some complex and questionable big-endian dependent
        // code.
        break;
    }
    }

    return addr;
}

void AddIsodeIoHandler(int fd, IO_HANDLER* handler) {
    vm->addIoHandler(fd, handler, INVALID, 1);
}

void SetIoPool(int fd, pOb ob) { vm->ioPool[fd] = ob; }

int setSocketAsync(int fd) {
    int desiredState = 1;
    int flags;
    int result;

    SET_SIGNAL_IO_DESIRED(result);
    if (result == -1) {
        return -errno;
    }

#ifndef HPUX
    if ((flags = fcntl(fd, F_GETFL, 0)) == -1) {
        return -errno;
    }

    if (fcntl(fd, F_SETFL, flags | FCNTL_NONBLOCK) == -1) {
        return -errno;
    }

#else
    flags = desiredState;
    if (ioctl(fd, FIOSNBIO, &flags) == -1) {
        return -errno;
    }

    flags = desiredState;
    if (ioctl(fd, FIOASYNC, &flags) == -1) {
        return -errno;
    }

#endif

    return fd;
}

void deleteIoHandler(int fd) { vm->deleteIoHandler(fd); }

Ob* fdopenOstream(int fd, char* mode) {
    FILE* f = fdopen(fd, mode);
    if (f) {
        return Ostream::create(f);
    } else {
        return FIXNUM(-errno);
    }
}

Ob* fdopenIstream(int fd) {
    FILE* f = fdopen(fd, "r");
    if (f) {
        Reader* rdr = Reader::create(f);
        return Istream::create(rdr);
    } else {
        return FIXNUM(-errno);
    }
}

DEF("identity1", obIdentity1, 0, MaxArgs) {
    if (NARGS == 0) {
        return NIV;
    } else {
        return ARG(0);
    }
}

pOb Ob::primitiveInitialize(pCtxt ctxt) {
    int n = ctxt->nargs - 1;

    n = std::min(n, numberOfSlots());

    for (int i = 0; i < n; i++) {
        ASSIGN(self(), slot(i), ctxt->arg(i + 1));
    }

    return self();
}

pOb RblAtom::primitiveInitialize(pCtxt ctxt) {
    int n = ctxt->nargs - 1;

    if (n >= 1) {
        return ctxt->arg(1);
    } else {
        return self();
    }
}

pOb Actor::primitiveInitialize(pCtxt ctxt) {
    PROTECT_THIS(Actor);
    PROTECT(ctxt);
    int n = ctxt->nargs - 1;

    n = std::min(n, extension->numberOfSlots());

    for (int i = 0; i < n; i++) {
        ASSIGN(extension, slot(i), ctxt->arg(i + 1));
    }

    mbox->nextMsg(SELF, NIL);

    return SELF;
}

DEF("prim-init", obInitialize, 1, MaxArgs) {
    return BASE(ARG(0))->primitiveInitialize(__CTXT__);
}

DEF("M-get", addressGetField, 3, 3) {
    CHECK_ADDR(0, base);
    CHECK_FIXNUM(1, span);
    CHECK(2, RblBool, sign);

    int offset = (int)base % 4;
    uint32_t* addr = (uint32_t*)(base - offset);

    if (base < local_page_size) {
        return PRIM_ERROR("invalid address");
    } else if ((span >= 1) && (span <= 4)) {
        uint32_t rslt =
            mem_get_field(addr, offset * 8, span * 8, BOOLVAL(sign));

        return FIXNUM((int)rslt);
    }

    return PRIM_ERROR("invalid span");
}


DEF("M-set", addressSetField, 3, 3) {
    CHECK_ADDR(0, base);
    CHECK_FIXNUM(1, span);
    CHECK_FIXNUM(2, val);

    int offset = (int)base % 4;
    uint32_t* addr = (uint32_t*)(base - offset);

    if (base < local_page_size) {
        return PRIM_ERROR("invalid address");
    } else if ((span >= 1) && (span <= 4)) {
        mem_set_field(addr, offset * 8, span * 8, (uint32_t)val);
        return ADDR_TO_FIXNUM((int)addr);
    }

    return PRIM_ERROR("invalid span");
}

DEF("char*->string", char_star_to_string, 1, 1) {
    CHECK_ADDR(0, addr);

    if (addr >= local_page_size) {
        return RBLstring::create((char*)addr);
    }

    return PRIM_ERROR("invalid address");
}

DEF("ob@", ob_address, 1, 1) { return ADDR_TO_FIXNUM((int)BASE(ARG(0))); }

DEF("slot0", ob_slot_0, 1, 1) {
    return ADDR_TO_FIXNUM((int)&BASE(ARG(0))->slot(0));
}

DEF("saddr", struct_address, 1, 1) {
    return ADDR_TO_FIXNUM((int)&((ByteVec*)(BASE(ARG(0))))->byte(0));
}

DEF("basic-bytevec-new", basicByteVecNew, 2, 2) {
    CHECK_FIXNUM(1, n);
    return ByteVec::create(n);
}

DEF("tenured-bytevec-new", tenuredByteVecNew, 2, 2) {
    CHECK_FIXNUM(1, n);
    return heap->tenure(ByteVec::create(n));
}


DEF("malloc", unix_malloc, 1, 1) {
    CHECK_FIXNUM(0, n_bytes);

    return ADDR_TO_FIXNUM(malloc(n_bytes));
}

DEF("u_bzero", unix_bzero, 2, 2) {
    CHECK_ADDR(0, addr);
    CHECK_FIXNUM(1, sz);
    if ((addr >= local_page_size) || (addr == 0)) {
        bzero((char*)addr, sz);
        return FIXNUM(0);
    }

    return PRIM_ERROR("invalid address");
}

DEF("u_free", unix_free, 1, 1) {
    CHECK_ADDR(0, addr);

    if ((addr >= local_page_size) || (addr == 0)) {
        free((char*)addr);
        return FIXNUM(0);
    }

    return PRIM_ERROR("invalid address");
}

DEF("memcpy", unix_memcpy, 3, 3) {
    CHECK_ADDR(0, dest_addr);
    CHECK_ADDR(1, src_addr);
    CHECK_FIXNUM(2, n_bytes);

    if ((dest_addr >= local_page_size) && (src_addr >= local_page_size)) {
        return ADDR_TO_FIXNUM(
            memcpy((char*)dest_addr, (char*)src_addr, n_bytes));
    }

    return PRIM_ERROR("invalid address");
}

DEF("fcntl", unix_fcntl, 3, 3) {
    CHECK_FIXNUM(0, fd);
    CHECK_FIXNUM(1, mode);
    CHECK_FIXNUM(2, flags);

    int rslt = fcntl(fd, mode, flags);

    if (rslt < 0) {
        return FIXNUM(-errno);
    }

    return FIXNUM(rslt);
}

DEF("sh", unix_system, 1, 1) {
    CHECK(0, RBLstring, str);
    PROTECT(str);

    char* cmd = (char*)&str->byte(0);

    int rslt = system(cmd);

    if (rslt < 0) {
        return FIXNUM(-errno);
    }

    return FIXNUM(rslt);
}

DEF("sbrk", unix_sbrk, 1, 1) {
    CHECK_FIXNUM(0, addr);

    int rslt = (int)sbrk(addr);

    if (rslt < 0) {
        return FIXNUM(-errno);
    }

    return FIXNUM(rslt);
}

DEF("exit", unix_exit, 0, 1) {
    int r = 0;

    if (NARGS == 1) {
        r = FIXVAL(ARG(0));
    }

    exit(r);
    return INVALID;
}

DEF("unlink", unix_unlink, 1, 1) {
    CHECK(0, RBLstring, str);
    PROTECT(str);

    char* fnm = (char*)&str->byte(0);

    int rslt = unlink(fnm);

    if (rslt < 0) {
        return FIXNUM(-errno);
    }
    return FIXNUM(rslt);
}

DEF("_c2bv", _c_struct_to_byte_vec, 3, 3) {
    CHECK_ADDR(1, src_addr);
    CHECK_FIXNUM(2, n_bytes);

    if (src_addr >= local_page_size) {
        memcpy((char*)&BASE(ARG(0))->slot(0), (char*)src_addr, n_bytes);
        return ARG(0);
    }

    return PRIM_ERROR("invalid address");
}

DEF("c2bv", c_struct_to_byte_vec, 2, 2) {
    CHECK(0, ByteVec, dest_bv);
    CHECK_ADDR(1, src_addr);

    if (src_addr >= local_page_size) {
        memcpy(&dest_bv->byte(0), (void*)src_addr, dest_bv->numberOfBytes());
        return dest_bv;
    }

    return PRIM_ERROR("invalid address");
}

DEF("c2str", cpy_char_star_to_string, 2, 2) {
    CHECK(0, RBLstring, dest_str);
    CHECK_ADDR(1, src_addr);

    if (src_addr >= local_page_size) {
        int len =
            std::min((int)strlen((char*)src_addr), dest_str->numberOfBytes());
        memcpy(&dest_str->byte(0), (void*)src_addr, len);
        return dest_str;
    }

    return PRIM_ERROR("invalid address");
}

DEF("string->fx", string_to_fx, 1, 1) {
    CHECK(0, RBLstring, str);

    return FIXNUM(strtol((char*)&(str->byte(0)), (char**)0, 0));
}

DEF("fx->string", fx_to_string, 1, 1) {
    CHECK_FIXNUM(0, val);
    char buf[16];

    sprintf(buf, "%d", val);

    return RBLstring::create(buf);
}

DEF("strlen", c_strlen, 1, 1) {
    CHECK_ADDR(0, addr);

    if (addr >= local_page_size) {
        return FIXNUM(strlen((char*)addr));
    }

    return PRIM_ERROR("invalid address");
}

DEF("prim-string->", cpy_string_char_star, 2, 2) {
    CHECK_ADDR(0, dest_addr);
    CHECK(1, RBLstring, src_str);

    if (dest_addr >= local_page_size) {
        strcpy((char*)dest_addr, (char*)&src_str->byte(0));
        return src_str;
    }

    return PRIM_ERROR("invalid address");
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

DEF("uOpen", unix_open, 3, 3) {
    CHECK(0, RBLstring, path);
    CHECK_FIXNUM(1, flags);
    CHECK_FIXNUM(2, mode);
    return FIXNUM(open((char*)&path->byte(0), flags, mode));
}

DEF("uClose", unix_close, 1, 1) {
    CHECK_FIXNUM(0, fd);
    return FIXNUM(close(fd));
}

DEF("uRead", unix_read, 3, 3) {
    CHECK_FIXNUM(0, fd);
    CHECK_FIXNUM(2, len);

    if (IS_A(ARG(1), ByteVec)) {
        return FIXNUM(read(fd, (char*)&((ByteVec*)ARG(1))->byte(0), len));
    } else if (IS_A(ARG(1), RBLstring)) {
        return FIXNUM(read(fd, (char*)&((RBLstring*)ARG(1))->byte(0), len));
    }

    return PRIM_ERROR("unix_read on invalid type");
}

DEF("uWrite", unix_write, 3, 3) {
    CHECK_FIXNUM(0, fd);
    CHECK_FIXNUM(2, len);

    if (IS_A(ARG(1), ByteVec)) {
        return FIXNUM(write(fd, (char*)&((ByteVec*)ARG(1))->byte(0), len));
    } else if (IS_A(ARG(1), RBLstring)) {
        return FIXNUM(write(fd, (char*)&((RBLstring*)ARG(1))->byte(0), len));
    }

    return PRIM_ERROR("unix_write on invalid type");
}

DEF("fd-open-ostream", fd_open_ostream, 2, 2) {
    CHECK_FIXNUM(0, fd);
    CHECK(1, RBLstring, mode);
    return fdopenOstream(fd, (char*)&mode->byte(0));
}

DEF("fd-open-istream", fd_open_istream, 1, 1) {
    CHECK_FIXNUM(0, fd);
    return fdopenIstream(fd);
}

DEF("regexpCompare", regexpCompare, 2, 2) {
    CHECK(0, RBLstring, re);
    CHECK(1, RBLstring, str);

    auto str_s = (char*)&str->byte(0);

    try {
        std::regex r(re->asCstring());  // Compile the regexp

        if (std::regex_match(str_s, r)) {  // See if it matches
            return RBLstring::create(str_s);
        } else {
            return RBLFALSE;
        }
    } catch (const std::regex_error& e) {
        warning("Regex expression error: %s code=%d", e.what(), e.code());
        return RBLFALSE;
    }
}

DEF("socketpair", sysSocketpair, 0, 0) {
    int sv[2];

    if (socketpair(AF_UNIX, SOCK_STREAM, 0, sv) == -1) {
        return FIXNUM(-errno);
    }

    PROTECT(__CTXT__);

    Tuple* ans = Tuple::create(2, NIV);

    ans->elem(0) = FIXNUM(sv[0]);
    ans->elem(1) = FIXNUM(sv[1]);

    return ans;
}

void setup_io(int in, int out, int err) {
    int nfds = getdtablesize();

    for (int fd = 0; fd < nfds; fd++) {
        if ((fd != in) && (fd != out) && (fd != err)) {
            (void)close(fd);
        }
    }

    int tmp_in = dup(in);
    int tmp_out = dup(out);
    int tmp_err = dup(err);

    if (tmp_in != 0) {
        (void)dup2(tmp_in, 0);
        (void)close(tmp_in);
    }

    if (tmp_out != 1) {
        (void)dup2(tmp_out, 1);
        (void)close(tmp_out);
    }

    if (tmp_err != 2) {
        (void)dup2(tmp_err, 2);
        (void)close(tmp_err);
    }
}

DEF("prim-execvp", sysExecv, 5, 6) {
    CHECK(0, RBLstring, path);
    CHECK_ADDR(1, argv);
    CHECK_FIXNUM(2, in);
    CHECK_FIXNUM(3, out);
    CHECK_FIXNUM(4, err);
    int splitp;
    if (NARGS == 5) {
        splitp = true;
    } else {
        CHECK(5, RblBool, x);
        splitp = BOOLVAL(x);
    }

    int pid;

    if ((pid = vfork()) == 0) {
        char buf[32];

        (void)setpgrp();

        if (splitp) {
            setup_io(in, out, err);
        }

        execvp((const char*)&path->byte(0), (char**)argv);

        sprintf(buf, "(exec-failed %d %d)\n", getpid(), errno);
        write(err, buf, strlen(buf));

        _exit(127);
    }

    if (pid == -1) {
        return FIXNUM(-errno);
    }

    return FIXNUM(pid);
}

DEF("ostream-display-join", ostreamDisplayJoin, 2, MaxArgs) {
    CHECK(0, Ostream, strm);

    if (NARGS == 2) {
        return NIV;
    }

    if (strm->stream) {
        int n = NARGS;
        errno = 0;
        for (int i = 2; i < n; i++) {
            BASE(ARG(1))->displayOn(strm->stream);
            BASE(ARG(i))->displayOn(strm->stream);
        }

        if (errno != 0) {
            return FIXNUM(-errno);
        } else {
            return NIV;
        }
    }

    return PRIM_ERROR("cannot display on closed ostream");
}

DEF("prim-configuration-parameters", configParams, 0, 0) {
    struct C_C {
        char f1;
        char f2;
    } c_c;
    struct C_S {
        char f1;
        short f2;
    } c_s;
    struct C_L {
        char f1;
        long f2;
    } c_l;

    PROTECT(__CTXT__);

    Tuple* parms = Tuple::create(16, NIV);

    ASSIGN(parms, elem(0), FIXNUM(sizeof(c_c.f2)));
    ASSIGN(parms, elem(1), FIXNUM(sizeof(c_s.f2)));
    ASSIGN(parms, elem(2), FIXNUM(sizeof(c_l.f2)));

    ASSIGN(parms, elem(3), FIXNUM(((char*)&c_c.f2 - &c_c.f1)));
    ASSIGN(parms, elem(4), FIXNUM(((char*)&c_s.f2 - &c_s.f1)));
    ASSIGN(parms, elem(5), FIXNUM(((char*)&c_l.f2 - &c_l.f1)));

    ASSIGN(parms, elem(6), FIXNUM(getpagesize()));
    ASSIGN(parms, elem(7), FIXNUM(getdtablesize()));

    ASSIGN(parms, elem(8), SYMBOL(ARCH));
    ASSIGN(parms, elem(9), SYMBOL(MACHINE));
    ASSIGN(parms, elem(11), SYMBOL(OS));

#if defined(BUILDPATH)
    ASSIGN(parms, elem(10), SYMBOL(BUILDPATH));
#endif

#if defined(__GNUG__)
    ASSIGN(parms, elem(12), SYMBOL("GNU"));
#else
    ASSIGN(parms, elem(12), SYMBOL("Cfront2.1"));
#endif

    return parms;
}

DEF("tuple-unzip", tupleUnZip, 1, 2) {
    CHECK(0, Tuple, tpl);

    int n = 2;
    if (NARGS == 2) {
        if (IS_FIXNUM(ARG(1))) {
            n = FIXVAL(ARG(1));
        } else {
            return PRIM_ERROR("Fixnum expected for stride");
        }
    }

    if (n == 1) {
        return tpl->clone();
    }

    int sz = tpl->numberOfElements();
    if (n == 0) {
        n = sz;
    }

    int i = 0;
    int j = 0;

    PROTECT(tpl);
    PROTECT(__CTXT__);
    Tuple* ans = Tuple::create(n, NIV);
    PROTECT(ans);
    int sdn = sz / n;
    int smn = sz % n;

    Tuple* tmp;

    for (; i < n; i++) {
        if (i < smn) {
            j = sdn + 1;
        } else {
            j = sdn;
        }

        tmp = Tuple::create(j, NIV);
        ASSIGN(ans, elem(i), tmp);
    }

    for (j = 0; j < n; j++) {
        tmp = (Tuple*)ans->elem(j);
        for (i = 0; i <= sdn; i++) {
            int x = i * n + j;
            if (x < sz) {
                ASSIGN(tmp, elem(i), tpl->elem(x));
            }
        }
    }

    return ans;
}

DEF("tuple-zip", tupleZip, 1, MaxArgs) {
    int sz = 0;
    int i = 0;
    int j = 0;
    int n = NARGS;
    int x = 0;
    int mx = 0;
    for (; i < n; i++) {
        if (IS_A(ARG(i), Tuple)) {
            j = ((Tuple*)ARG(i))->numberOfElements();
            sz += j;
            if (mx < j) {
                mx = j;
            }
        } else {
            return PRIM_ERROR("non Tuple");
        }
    }
    PROTECT(__CTXT__);
    Tuple* ans = Tuple::create(sz, NIV);


    for (i = 0; i < mx; i++) {
        for (j = 0; j < n; j++) {
            if (i < ((Tuple*)ARG(j))->numberOfElements()) {
                ASSIGN(ans, elem(x), ((Tuple*)ARG(j))->elem(i));
                x++;
            }
        }
    }

    return ans;
}

DEF("tuple-exclude", tupleExclude, 2, 2) {
    CHECK(0, Tuple, tpl);
    PROTECT(tpl);
    int n = tpl->numberOfElements();
    Tuple* ans = Tuple::create(n, NIV);
    int i = 0;
    int j = 0;

    for (; i < n; i++) {
        if (tpl->elem(i) != ARG(1)) {
            ASSIGN(ans, elem(j), tpl->elem(i));
            j++;
        }
    }

    return ans->subObject(0, j);
}

DEF("tuple-include", tupleInclude, 2, 2) {
    CHECK(0, Tuple, tpl);
    PROTECT(tpl);
    int n = tpl->numberOfElements();
    Tuple* ans = Tuple::create(n, NIV);
    int i = 0;
    int j = 0;

    for (; i < n; i++) {
        if (tpl->elem(i) == ARG(1)) {
            ASSIGN(ans, elem(j), tpl->elem(i));
            j++;
        }
    }

    return ans->subObject(0, j);
}

DEF("tuple-reverse", tupleReverse, 1, 1) {
    CHECK(0, Tuple, tpl);
    int n = tpl->numberOfElements();
    PROTECT(tpl);
    PROTECT(__CTXT__);

    Tuple* ans = Tuple::create(n, NIV);

    int i = 0;
    int j = n - 1;
    for (; i < n; i++, j--) {
        ASSIGN(ans, elem(j), tpl->elem(i));
    }

    return ans;
}

DEF("prim->tuple", prim2Tuple, 1, 1) {
    pOb base = BASE(ARG(0));
    int n = FIXVAL(base->indexedSize());

    PROTECT(base);
    PROTECT(__CTXT__);

    Tuple* ans = Tuple::create(n, NIV);

    for (int i = 0; i < n; i++) {
        ASSIGN(ans, elem(i), base->nth(i));
    }

    return ans;
}


DEF("prim-externalAppCBRegistry", primextAppCBRegistry, 0, 0) {
    return vm->extAppCBRegistry;
}

DEF("prim-externalAppCBRegister", primextAppCBRegister, 1, 1) {
    CHECK(0, Tuple, tpl);
    PROTECT(tpl);
    pOb key = FIXNUM(vm->extAppCBKeyGen++);
    vm->extAppCBRegistry->addKey(key, tpl);
    return key;
}

DEF("prim-externalAppCBLookup", primextAppCBLookup, 1, 1) {
    return vm->extAppCBRegistry->getKey(ARG(0));
}

DEF("prim-externalAppCBUnregister", primextAppCBUnregister, 1, 1) {
    return vm->extAppCBRegistry->addKey(ARG(0), ABSENT);
}
