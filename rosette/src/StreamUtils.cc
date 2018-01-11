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
#include "StreamUtils.h"

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>


int isoctal(char c) { return (isdigit(c) && (c < '8')); }


int readError(FILE* f, const char* fmt, ...) {
    va_list args;

    fprintf(stderr, "error: ");
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    putc('\n', stderr);
#if !defined(LINUX)
    f->_flag &= _IOERR;
#endif
    return -1;
}


int eofError(FILE* f) { return readError(f, "unexpected eof"); }


char getEscapedChar(FILE* f) {
    int c;

    if ((c = getc(f)) == EOF)
        return c;

    switch (c) {
    case 'n':
        c = '\n';
        break;
    case 'f':
        c = '\f';
        break;
    case 'r':
        c = '\r';
        break;
    case 't':
        c = '\t';
        break;
    case 'x': {
        char buf[4];
        char* p = buf;
        if ((c = getc(f), *p++ = c, c) == EOF ||
            (c = getc(f), *p++ = c, c) == EOF)
            return EOF;
        *p++ = '\0';
        c = (int)strtol(buf, 0, 16);
        break;
    }
    case '\n':
        c = '\0';
        break;
    }

    return c;
}


char getSafeChar(FILE* f, char escapeChar) {
    char c;

    if ((c = getc(f)) == EOF)
        return EOF;

    return (c == escapeChar) ? getEscapedChar(f) : c;
}


int putEscapedChar(char c, FILE* f) {
    switch (c) {
    case '\b':
        fputc('b', f);
        break;
    case '\f':
        fputc('f', f);
        break;
    case '\n':
        fputc('n', f);
        break;
    case '\r':
        fputc('r', f);
        break;
    case '\t':
        fputc('t', f);
        break;
    default:
        if (isprint(c))
            fputc(c, f);
        else
            fprintf(f, "x%.2x", (int)c);
        break;
    }

    return 0;
}


int putSafeChar(char c, FILE* f, char escapeChar) {
    if (c == escapeChar) {
        putc(c, f);
        putc(c, f);
    } else if (isprint(c))
        putc(c, f);
    else {
        putc(escapeChar, f);
        putEscapedChar(c, f);
    }

    return 0;
}


int escapedCharWidth(char c) {
    switch (c) {
    case '\n':
    case '\f':
    case '\t':
    case '\r':
        return 1;
    default:
        return (isprint(c) ? 1 : 3);
    }
}


int safeStrlen(char* str, char escapeChar) {
    int n = 0;
    for (char c = *str; c; c = *++str)
        n += (isprint(c) && c != escapeChar) || 1 + escapedCharWidth(c);
    return n;
}


char* safeStrcpy(char* dest, char* src, char escapeChar) {
    for (char c = *src; c; c = *++src) {
        if (c == escapeChar) {
            *dest++ = c;
            *dest++ = c;
        } else if (isprint(c))
            *dest++ = c;
        else {
            *dest++ = escapeChar;
            switch (c) {
            case '\n':
                *dest++ = 'n';
                break;
            case '\f':
                *dest++ = 'f';
                break;
            case '\t':
                *dest++ = 't';
                break;
            case '\r':
                *dest++ = 'r';
                break;
            default:
                sprintf(dest, "x%.2x", (int)c);
                dest += 3;
                break;
            }
        }
    }
    *dest = 0;
    return dest;
}
