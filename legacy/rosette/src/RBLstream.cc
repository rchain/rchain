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

#include "RBLstream.h"
#include <sys/errno.h>
#include <stdio.h>
#include "BinaryOb.h"
#include "Ctxt.h"
#include "Operation.h"
#include "Prim.h"
#include "Reader.h"
#include "RBLstring.h"
#include "Tuple.h"

#include "BuiltinClass.h"
#include "ModuleInit.h"

#include <memory.h>
#include <errno.h>

#include <ctime>

BUILTIN_CLASS(Istream) {
    OB_FIELD("client", Istream, client);
    OB_FIELD("reader", Istream, reader);
}

#if defined(_OREGON_)
#define FLAGS flags
#define _IOERR _IO_ERROR
#else
#define FLAGS _flag
#endif


Istream::Istream(Ob* mbox, pExt ext, Reader* reader)
    : Actor(sizeof(Istream), CLASS_META(Istream), CLASS_SBO(Istream), mbox,
            ext),
      client(NIV),
      reader(reader) {
    Istream::updateCnt();
}


Istream* Istream::create(Reader* r) {
    PROTECT(r);
    pExt ext = StdExtension::create(0);
    void* loc = PALLOC1(sizeof(Istream), ext);
    return new (loc) Istream(emptyMbox, ext, r);
}


Ob* Istream::cloneTo(Ob*, Ob*) {
    warning("can't clone istreams");
    return this;
}


BUILTIN_CLASS(Ostream) {}


Ostream::Ostream(FILE* ostrm)
    : BinaryOb(sizeof(Ostream), CLASS_META(Ostream), CLASS_SBO(Ostream)),
      stream(ostrm) {
    heap->registerForeignOb(this);
    Ostream::updateCnt();
}


Ostream::~Ostream() {
    if (stream) {
        fclose(stream);
        stream = 0;
    }
}


Ostream* Ostream::create(FILE* ostrm) {
    void* loc = PALLOC(sizeof(Ostream));
    return new (loc) Ostream(ostrm);
}


Ob* Ostream::cloneTo(Ob*, Ob*) {
    warning("cannot clone ostreams");
    return this;
}


Reader* StdinReader = (Reader*)INVALID;


MODULE_INIT(RBLstream) {
    StdinReader = (Reader*)heap->tenure(Reader::create(stdin));
    Define("stdin", Istream::create(StdinReader));
    Define("stdout", Ostream::create(stdout));
    Define("stderr", Ostream::create(stderr));
}


#include <sys/types.h>
#include <sys/stat.h>


DEF("istream-new", makeIstream, 1, 2) {
    char* path = BASE(ARG(0))->asPathname();
    const char* mode = "r";

    if (!path) {
        return PRIM_MISMATCH(0, "String or Symbol");
    }

    if (NARGS == 2) {
        CHECK(1, RBLstring, mode_string);
        mode = mode_string->asCstring();
    }

    FILE* f = fopen(path, mode);

    if (f) {
        Reader* r = Reader::create(f);
        return Istream::create(r);
    } else {
        return PRIM_ERROR(sys_errmsg());
    }
}


DEF("istream-read", istreamRead, 1, 1) {
    CHECK(0, Istream, stream);
    return stream->reader->readExpr();
}


DEF("istream-resume-io", istreamResumeIO, 1, 1) {
    CHECK(0, Istream, stream);
    return stream->reader->resume();
}


DEF("istream-readch", istreamReadch, 1, 1) {
    CHECK(0, Istream, stream);
    return stream->reader->readCh();
}


static char readline_buf[1024];

DEF("istream-readline", istreamReadLine, 1, 1) {
    CHECK(0, Istream, stream);

    pOb cOb = stream->reader->readCh();
    if (cOb == RBLEOF) {
        return RBLEOF;
    }

    char c;

    for (int i = 0; i < 1023; i++) {
        if (cOb == RBLEOF) {
            readline_buf[i] = 0;
            return RBLstring::create(readline_buf);
        }

        c = CHARVAL(cOb);
        if (c == '\n') {
            readline_buf[i] = 0;
            return RBLstring::create(readline_buf);
        } else {
            readline_buf[i] = c;
            cOb = stream->reader->readCh();
        }
    }

    readline_buf[1023] = 0;
    return RBLstring::create(readline_buf);
}


DEF("istream-clear", istreamClear, 1, 2) {
    CHECK(0, Istream, stream);
    stream->reader->resetState();
    return NIV;
}


DEF("istream-rdstate", istreamRdState, 1, 1) {
    CHECK_NOVAR(0, Istream);
    return PRIM_ERROR("de-implemented");
}


DEF("istream-close", istreamClose, 1, 1) {
    CHECK(0, Istream, stream);

    if (stream->reader != NIV) {
        if (stream->reader->file) {
            fclose(stream->reader->file);
            stream->reader->file = 0;
            stream->reader = (Reader*)NIV;
        }
    }

    return NIV;
}


DEF("ostream-new", makeOstream, 1, 2) {
    char* path = BASE(ARG(0))->asPathname();
    const char* mode = "a";

    if (!path) {
        return PRIM_MISMATCH(0, "String or Symbol");
    }

    if (NARGS == 2) {
        CHECK(1, RBLstring, mode_string);
        mode = mode_string->asCstring();
    }

    FILE* f = fopen(path, mode);

    if (f) {
        return Ostream::create(f);
    } else {
        return PRIM_ERROR(sys_errmsg());
    }
}


DEF("ostream-display", ostreamDisplay, 2, MaxArgs) {
    CHECK(0, Ostream, strm);

    if (strm->stream) {
        int n = NARGS;
        errno = 0;
        for (int i = 1; i < n; i++) {
            BASE(ARG(i))->displayOn(strm->stream);
        }

        if (errno != 0) {
            return FIXNUM(-errno);
        } else {
            return NIV;
        }
    } else {
        return PRIM_ERROR("cannot display on closed ostream");
    }
}


DEF("ostream-print", ostreamPrint, 2, MaxArgs) {
    CHECK(0, Ostream, strm);

    if (strm->stream) {
        int n = NARGS;
        errno = 0;
        for (int i = 1; i < n; i++) {
            if (i > 1) {
                putc(' ', strm->stream);
            }

            BASE(ARG(i))->printQuotedOn(strm->stream);
        }

        if (errno != 0) {
            return FIXNUM(-errno);
        } else {
            return NIV;
        }
    } else {
        return PRIM_ERROR("cannot print on closed ostream");
    }
}


DEF("ostream-close", ostreamClose, 1, 1) {
    CHECK(0, Ostream, strm);
    if (strm->stream) {
        fclose(strm->stream);
        strm->stream = 0;
    }

    return NIV;
}


DEF("stream-status", streamStat, 1, 1) {
    char* path = BASE(ARG(0))->asPathname();

    if (!path) {
        return PRIM_MISMATCH(0, "String or Symbol");
    }

    static struct stat statbuf;
    int status = stat(path, &statbuf);
    if (status) {
        return NIV;
    } else {
        ByteVec* result = ByteVec::create(sizeof(struct stat));
        memcpy(&result->byte(0), &statbuf, sizeof(struct stat));
        return result;
    }
}


DEF("prim-display", obDisplay, 1, MaxArgs) {
    int nargs = NARGS;

    for (int i = 0; i < nargs; i++) {
        BASE(ARG(i))->displayOn(stdout);
    }

    return NIV;
}


DEF("prim-print", obPrint, 1, MaxArgs) {
    int n = NARGS;
    for (int i = 0; i < n; i++) {
        if (i > 0) {
            putchar(' ');
        }

        BASE(ARG(i))->printQuotedOn(stdout);
    }

    return NIV;
}


DEF("ostream-log-time", obLogTime, 1, 1) {
    CHECK(0, Ostream, strm);
    if (strm->stream) {
        char buf[128];

        time_t rawtime;
        struct tm* timeinfo;
        time(&rawtime);
        timeinfo = localtime(&rawtime);
        strftime(buf, sizeof(buf), "%Y-%m-%d %I:%M:%S\n", timeinfo);

        int length = fprintf(strm->stream, "%s", buf);
        if (length <= 0) {
            return PRIM_ERROR("Error writing to ostream");
        } else {
            return NIV;
        }
    } else {
        return PRIM_ERROR("cannot print on closed ostream");
    }
}

DEF("log-time-string", obLogTimeString, 0, 1) {
    char buf[128];
    time_t rawtime;
    struct tm timeinfo;

    rawtime = time(NULL);
    localtime_r(&rawtime, &timeinfo);
    strftime(buf, sizeof buf, "%F %T", &timeinfo);

    return RBLstring::create(buf);
}

DEF("prim-flush", obFlush, 0, 1) {
    if (NARGS == 0) {
        fflush(stdout);
    } else {
        CHECK(0, Ostream, strm);
        if (strm->stream) {
            if (fflush(strm->stream) == EOF) {
                return FIXNUM(-errno);
            }
        } else {
            return PRIM_ERROR("cannot flush closed ostream");
        }
    }

    return NIV;
}

DEF("getFd", obGetFd, 1, 1) {
    if (IS_A(ARG(0), Istream)) {
        return FIXNUM(fileno(((Istream*)ARG(0))->reader->file));
    } else if (IS_A(ARG(0), Ostream)) {
        return FIXNUM(fileno(((Ostream*)ARG(0))->stream));
    } else {
        return FIXNUM(-1);
    }
}

DEF_OPRN(Sync, "print", oprnPrint, obPrint);
DEF_OPRN(Sync, "display", oprnDisplay, obDisplay);
DEF_OPRN(Std, "read-expr", oprnReadExpr, istreamRead);
DEF_OPRN(Std, "resume-io", oprnResumeIO, istreamResumeIO);
