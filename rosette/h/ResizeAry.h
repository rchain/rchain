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
 *
 @EC */

#if !defined(_ResizeablePtrArray_h)
#define _ResizeablePtrArray_h 1

#ifdef __GNUG__
#pragma interface
#endif

#include "rosette.h"

class ResizeablePtrArray {
   protected:
    void** array;
    int size;

   public:
    ResizeablePtrArray();
    ResizeablePtrArray(int);
    ~ResizeablePtrArray();

    int capacity();

    void resize(int);
    void resize();

    void*& operator[](int);
};

inline ResizeablePtrArray::ResizeablePtrArray() {
    array = 0;
    size = 0;
}

inline ResizeablePtrArray::ResizeablePtrArray(int sz) {
    array = new void*[sz];
    size = sz;
}

inline int ResizeablePtrArray::capacity() { return size; }

inline void ResizeablePtrArray::resize() { resize(2 * size); }

inline void*& ResizeablePtrArray::operator[](int n) { return array[n]; }

#endif
