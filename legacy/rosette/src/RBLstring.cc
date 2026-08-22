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

#include "RBLstring.h"
#include "CommandLine.h"
#include "Ctxt.h"
#include "Prim.h"
#include "Tuple.h"
#include "BuiltinClass.h"


#include <assert.h>
#include <ctype.h>
#include <memory.h>
#ifndef __GNUG__
#include <strings.h>
#endif


BUILTIN_CLASS(RBLstring) {}


RBLstring::RBLstring(int n, char c)
    : ByteVec(sizeof(RBLstring) + align(n), CLASS_META(RBLstring),
              CLASS_SBO(RBLstring), n) {
    if (c == 0) {
        memset(&byte(0), c, n);
    } else {
        memset(&byte(0), c, n - 1);
        byte(n - 1) = 0;
    }
    RBLstring::updateCnt();
}


RBLstring::RBLstring(int n, char* s)
    : ByteVec(sizeof(RBLstring) + align(n), CLASS_META(RBLstring),
              CLASS_SBO(RBLstring), n) {
    strcpy((char*)&byte(0), s);
    RBLstring::updateCnt();
}


RBLstring* RBLstring::create(int n, char c) {
    return gc_new_space<RBLstring>(n, n, c);
}


RBLstring* RBLstring::create(char* s) {
    int n = strlen(s) + 1;
    return gc_new_space<RBLstring>(n, n, s);
}

RBLstring* RBLstring::create(int n, char* s) {
    auto str = gc_new_space<RBLstring>(align(n + 1), n + 1, (char)0);
    memcpy((char*)&str->byte(0), s, n);
    return str;
}

void RBLstring::printOn(FILE* f) {
    char* str = (char*)&byte(0);
    char c;
    fputc('\"', f);

    do {
        c = *str++;
        if ('\0' == c) {  // End of the string
            break;
        }

        if (c == '\\') {
            fputc('\\', f);
            fputc('\\', f);
        } else if (c == '\"') {
            fputc('\\', f);
            fputc('\"', f);
        } else if (isprint(c)) {
            fputc(c, f);
        } else {
            fputc('\\', f);

            switch (c) {
            case '\n':
                fputc('n', f);
                break;
            case '\f':
                fputc('f', f);
                break;
            case '\t':
                fputc('t', f);
                break;
            case '\r':
                fputc('r', f);
                break;
            default:
                fprintf(f, "x%.2x", (int)c);
                break;
            }
        }
    } while (true);

    fputc('\"', f);
}


void RBLstring::displayOn(FILE* f) { fputs((char*)&byte(0), f); }


char* RBLstring::asPathname() { return (char*)&byte(0); }
Ob* RBLstring::indexedSize() { return FIXNUM(numberOfBytes() - 1); }
Ob* RBLstring::nth(int n) { return RBLCHAR(byte(n)); }


Ob* RBLstring::setNth(int n, Ob* v) {
    byte(n) = CHARVAL(v);
    return this;
}


const char* RBLstring::asCstring() {
    /*
     * This is dangerous, since a scavenge can move the string, rendering
     * meaningless the pointer that we have returned.
     */
    return (char*)&byte(0);
}


Ob* RBLstring::subObject(int start, int n) {
    PROTECT_THIS(RBLstring);
    RBLstring* result = RBLstring::create(n + 1);
    memcpy(&result->byte(0), &SELF->byte(start), n * sizeof(uint8_t));
    result->byte(n) = 0;
    return result;
}


/* case sensitive compares */

DEF("string=", stringEq, 2, 2) {
    CHECK(0, RBLstring, str1);
    if (!IS_A(ARG(1), RBLstring)) {
        return RBLFALSE;
    }

    RBLstring* str2 = (RBLstring*)ARG(1);
    return RBLBOOL(strcmp((char*)&str1->byte(0), (char*)&str2->byte(0)) == 0);
}

DEF("string!=", stringNEq, 2, 2) {
    CHECK(0, RBLstring, str1);
    if (!IS_A(ARG(1), RBLstring)) {
        return RBLFALSE;
    }

    RBLstring* str2 = (RBLstring*)ARG(1);
    return RBLBOOL(strcmp((char*)&str1->byte(0), (char*)&str2->byte(0)) != 0);
}

DEF("string<", stringLess, 2, 2) {
    CHECK(0, RBLstring, str1);
    if (!IS_A(ARG(1), RBLstring)) {
        return RBLFALSE;
    }

    RBLstring* str2 = (RBLstring*)ARG(1);
    return RBLBOOL(strcmp((char*)&str1->byte(0), (char*)&str2->byte(0)) < 0);
}

DEF("string<=", stringLEQ, 2, 2) {
    CHECK(0, RBLstring, str1);
    if (!IS_A(ARG(1), RBLstring)) {
        return RBLFALSE;
    }

    RBLstring* str2 = (RBLstring*)ARG(1);
    return RBLBOOL(strcmp((char*)&str1->byte(0), (char*)&str2->byte(0)) <= 0);
}

DEF("string>", stringGtr, 2, 2) {
    CHECK(0, RBLstring, str1);
    if (!IS_A(ARG(1), RBLstring)) {
        return RBLFALSE;
    }

    RBLstring* str2 = (RBLstring*)ARG(1);
    return RBLBOOL(strcmp((char*)&str1->byte(0), (char*)&str2->byte(0)) > 0);
}

DEF("string>=", stringGEQ, 2, 2) {
    CHECK(0, RBLstring, str1);
    if (!IS_A(ARG(1), RBLstring)) {
        return RBLFALSE;
    }

    RBLstring* str2 = (RBLstring*)ARG(1);
    return RBLBOOL(strcmp((char*)&str1->byte(0), (char*)&str2->byte(0)) >= 0);
}

/* case in-sensitive compares */

#ifndef STRCASECMP
#define STRCASECMP strcasecmp
#endif


DEF("string-ci=", string_ciEq, 2, 2) {
    CHECK(0, RBLstring, str1);
    if (!IS_A(ARG(1), RBLstring)) {
        return RBLFALSE;
    }

    RBLstring* str2 = (RBLstring*)ARG(1);
    return RBLBOOL(STRCASECMP((char*)&str1->byte(0), (char*)&str2->byte(0)) ==
                   0);
}

DEF("string-ci!=", string_ciNEq, 2, 2) {
    CHECK(0, RBLstring, str1);
    if (!IS_A(ARG(1), RBLstring)) {
        return RBLFALSE;
    }

    RBLstring* str2 = (RBLstring*)ARG(1);
    return RBLBOOL(STRCASECMP((char*)&str1->byte(0), (char*)&str2->byte(0)) !=
                   0);
}

DEF("string-ci<", string_ciLess, 2, 2) {
    CHECK(0, RBLstring, str1);
    if (!IS_A(ARG(1), RBLstring)) {
        return RBLFALSE;
    }

    RBLstring* str2 = (RBLstring*)ARG(1);
    return RBLBOOL(STRCASECMP((char*)&str1->byte(0), (char*)&str2->byte(0)) <
                   0);
}

DEF("string-ci<=", string_ciLEQ, 2, 2) {
    CHECK(0, RBLstring, str1);
    if (!IS_A(ARG(1), RBLstring)) {
        return RBLFALSE;
    }

    RBLstring* str2 = (RBLstring*)ARG(1);
    return RBLBOOL(STRCASECMP((char*)&str1->byte(0), (char*)&str2->byte(0)) <=
                   0);
}

DEF("string-ci>", string_ciGtr, 2, 2) {
    CHECK(0, RBLstring, str1);
    if (!IS_A(ARG(1), RBLstring)) {
        return RBLFALSE;
    }

    RBLstring* str2 = (RBLstring*)ARG(1);
    return RBLBOOL(STRCASECMP((char*)&str1->byte(0), (char*)&str2->byte(0)) >
                   0);
}

DEF("string-ci>=", string_ciGEQ, 2, 2) {
    CHECK(0, RBLstring, str1);
    if (!IS_A(ARG(1), RBLstring)) {
        return RBLFALSE;
    }

    RBLstring* str2 = (RBLstring*)ARG(1);
    return RBLBOOL(STRCASECMP((char*)&str1->byte(0), (char*)&str2->byte(0)) >=
                   0);
}


DEF("string-concat", stringConcat, 0, MaxArgs) {
    const int N = NARGS;
    int n = 1;
    int i = 0;
    for (; i < N; i++) {
        CHECK(i, RBLstring, string);
        n += strlen((char*)&string->byte(0));
    }

    PROTECT(__CTXT__);
    RBLstring* result = RBLstring::create(n);
    char* p = (char*)&result->byte(0);
    *p = 0;
    for (i = 0; i < N; i++) {
        RBLstring* str = (RBLstring*)ARG(i);
        strcat(p, (char*)&str->byte(0));
    }

    return result;
}

DEF("string-join", stringJoin, 3, 3) {
    CHECK_FIXNUM(0, code);
    const char* sep = BASE(ARG(1))->asCstring();
    int seplen = strlen(sep);
    CHECK(2, Tuple, strs);

    const int N = strs->numberOfElements();
    int n = 1;
    if ((code & 1) && (N > 0)) {
        n += seplen;
    }

    int i = 0;
    for (; i < N; i++) {
        if (!(IS_A(strs->elem(i), RBLstring))) {
            return PRIM_ERROR("non string");
        }

        n += strlen((char*)&((RBLstring*)strs->elem(i))->byte(0));
        if (i < N - 1) {
            n += seplen;
        }
    }

    if ((code & 2) && (N > 0)) {
        n += seplen;
    }

    PROTECT(__CTXT__);
    RBLstring* result = RBLstring::create(n);
    char* p = (char*)&result->byte(0);
    *p = 0;

    if ((code & 1) && (N > 0)) {
        strcat(p, sep);
    }

    for (i = 0; i < N; i++) {
        RBLstring* str = (RBLstring*)strs->elem(i);
        strcat(p, (char*)&str->byte(0));
        if (i < N - 1) {
            strcat(p, sep);
        }
    }

    if ((code & 2) && (N > 0)) {
        strcat(p, sep);
    }

    return result;
}

DEF("string-size", stringSize, 1, 1) {
    CHECK(0, RBLstring, string);
    return FIXNUM(string->numberOfBytes());
}


DEF("string-set-nth", stringSetNth, 3, 3) {
    CHECK(0, RBLstring, string);
    CHECK_FIXNUM(1, n);
    if (!IS(OTchar, ARG(2))) {
        return PRIM_MISMATCH(2, "Char");
    }

    if (n < 0 || n >= string->numberOfBytes()) {
        return PRIM_ERROR("index error");
    }

    string->byte(n) = CHARVAL(ARG(2));
    return string;
}


DEF("string-length", stringLength, 1, 1) {
    CHECK(0, RBLstring, string);
    return FIXNUM(strlen((char*)&string->byte(0)));
}


DEF("string-new", stringNew, 1, 2) {
    CHECK_FIXNUM(0, n);
    char c = ' ';
    if (NARGS > 1) {
        if (!IS(OTchar, ARG(1))) {
            return PRIM_MISMATCH(1, "Char");
        }

        c = CHARVAL(ARG(1));
    }
    return RBLstring::create(n + 1, c);
}

int stringMemQAux(RBLstring* string, char c) {
    int sz = strlen((char*)&string->byte(0));

    for (int i = 0; i < sz; i++) {
        if (string->byte(i) == c) {
            return 1;
        }
    }
    return 0;
}

DEF("string-mem?", stringMemQ, 2, 2) {
    CHECK(0, RBLstring, string);
    if (!IS(OTchar, ARG(1))) {
        return PRIM_MISMATCH(1, "Char");
    }

    char c = CHARVAL(ARG(1));
    return (stringMemQAux(string, c) ? RBLTRUE : RBLFALSE);
}

DEF("string-get-token", stringGetToken, 3, 3) {
    CHECK(0, RBLstring, string);
    CHECK_FIXNUM(1, w);
    CHECK(2, RBLstring, separators);

    int x = 0;
    int y = 0;
    int sz = strlen((char*)&string->byte(0));

    for (; w >= 0; w--) {
        for (x = y; x < sz; x++) {
            if (!stringMemQAux(separators, string->byte(x))) {
                break;
            }
        }

        for (y = x; y < sz; y++) {
            if (stringMemQAux(separators, string->byte(y))) {
                break;
            }
        }
    }

    return string->subObject(x, y - x);
}

DEF("string-split", stringSplit, 2, 3) {
    CHECK(0, RBLstring, string);
    char* str = (char*)&string->byte(0);
    int sz = strlen(str);

    if (sz == 0) {
        return NIL;
    }

    CHECK(1, RBLstring, separators);
    int nsep = strlen((char*)&separators->byte(0));

    int lim = FIXVAL(MAX_FIXNUM);
    if (NARGS == 3) {
        if (IS_FIXNUM(ARG(2))) {
            lim = FIXVAL(ARG(2));
        } else {
            return PRIM_ERROR("Fixnum expected for limit");
        }
    }

    PROTECT(string);
    PROTECT(separators);
    PROTECT(__CTXT__);

    int i = 0;
    int x = 0;
    int y = 0;

    if (nsep == 0) {
        Tuple* ans = Tuple::create(sz, NIV);
        for (; x < sz; x++) {
            ASSIGN(ans, elem(x), RBLCHAR(string->byte(x)));
        }

        return ans;
    } else {
        for (x = 0; ((i < lim) && (x < sz));) {
            for (; x < sz; x++) {
                if (!stringMemQAux(separators, string->byte(x))) {
                    break;
                }
            }

            for (y = x; y < sz; y++) {
                if (stringMemQAux(separators, string->byte(y))) {
                    break;
                }
            }

            if (y > x) {
                i++;
                x = y;
            }
        }

        if (i == 0) {
            return NIL;
        }

        Tuple* ans = Tuple::create(i + 1, NIV);
        PROTECT(ans);
        i = 0;
        for (x = 0; ((i < lim) && (x < sz));) {
            for (; x < sz; x++) {
                if (!stringMemQAux(separators, string->byte(x))) {
                    break;
                }
            }

            for (y = x; y < sz; y++) {
                if (stringMemQAux(separators, string->byte(y))) {
                    break;
                }
            }

            if (y > x) {
                ASSIGN(ans, elem(i), string->subObject(x, y - x));
                i++;
                x = y;
            }
        }

        if (y + 1 < sz) {
            y++;
            ASSIGN(ans, elem(i), string->subObject(y, sz - y));
        }

        return ans;
    }
}

convertArgReturnPair RBLstring::convertActualArg(Ctxt* ctxt, Ob* obj) {
    if (typep(obj) == RBLTRUE) {
        cnvArgRetPair.val = (uint32_t)(&((RBLstring*)obj)->byte(0));
        cnvArgRetPair.failp = 0;
    } else {
        cnvArgRetPair.val = (uint32_t)-1;
        cnvArgRetPair.failp = 1;
    }
    return cnvArgRetPair;
}

Ob* RBLstring::convertActualRslt(Ctxt*, uint32_t obj) {
    return (RBLstring::create((char*)obj));
}
