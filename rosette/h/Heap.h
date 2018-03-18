/* Mode: -*- C++ -*- */
// vim: set ai ts=4 sw=4 expandtab
/* @BC
 *		                Copyright (c) 1993
 *	    by Microelectronics and Computer Technology Corporation (MCC)
 *                                      and
 *		                Copyright (c) 1996
 *	                      by Rosette WebWorks Inc.
 *				All Rights Reserved
 *
 *	Permission to use, copy, modify, and distribute this software and its
 *	documentation for any purpose and without fee is hereby granted,
 *	provided that this notice be retained unaltered, and that the name of
 *	RWI or MCC and its shareholders and participants shall not be used in
 *	advertising or publicity pertaining to distribution of the software
 *	without specific written prior permission.
 *
 *	THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 *	IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 *	WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if !defined(_RBL_Heap_h)
#define _RBL_Heap_h

#include "Ob.h"

class RootSet {
   public:
    virtual void preScavenge();
    virtual void scavenge();
    virtual void postScavenge();

    virtual void preGC();
    virtual void mark();
    virtual void postGC();

    virtual void check();
};

typedef void (RootSet::*RootSet_Fn)();


class Ob;
class NewSpace;
class OldSpace;
class ForeignObTbl;
class GCAgenda;
class ObStk;
class PtrCollection;


class Heap {
   public:
    NewSpace* const newSpace;
    OldSpace* const oldSpace;
    ForeignObTbl* const foreignObs;
    GCAgenda* const gcAgenda;
    ObStk* const tenuredObs;
    PtrCollection* const rootSets;

    /*
     * It is important that the declarations of newSpaceBase and
     * newSpaceLimit (which are cached versions of newSpace->base and
     * newSpace->limit) come AFTER the declaration of newSpace, since
     * their initialization must occur after the initialization of
     * newSpace in the constructor.
     */

    void* const newSpaceBase;
    void* const newSpaceLimit;

    int scavengeCount;
    int gcCount;
    int totalScavenges;
    int totalGCs;

    Ob* copyAndForward(Ob*);
    void traverseRootSets(RootSet_Fn);

    Heap(unsigned, unsigned, unsigned);
    ~Heap();

    void addRootSet(RootSet*);
    void deleteRootSet(RootSet*);

    int size();
    void* alloc(unsigned);
    void* scavengeAndAlloc(unsigned);
    void remember(Ob*);
    void scavenge();
    void gc();
    Ob* tenure(Ob*);
    void tenureEverything();

    bool is_new(Ob* p) {
        return ((void*)p >= newSpaceBase && (void*)p < newSpaceLimit);
    }

    bool validPtrAfterScavenge(Ob*);
    void registerForeignOb(Ob*);
    void registerGCAgenda(Ob*);

    void resetCounts();
    void printCounts(FILE*);
};

extern Heap* heap;


/*
 * ********************** IMPORTANT!!!!!! *****************************
 *
 * The implicit pointer promotion involved in these protection routines
 * (and, indeed, in all of the scavenging and gc routines) will probably
 * only work under C++ objects built through single inheritance.  They
 * all make an assumption that a pointer's value is not changed during
 * promotion, e.g., that a pMeta value can be promoted to a pOb value and
 * still point to the same piece of physical memory.  That's reasonable
 * enough under single inheritance, but is probably false under multiple
 * inheritance.
 *
 * One can still run into problems, even under single inheritance.  Some
 * compilers (such as Oregon C++) produce temporaries to hold the results
 * of such promotion, probably in anticipation of multiple inheritance.
 * This is not ordinarily a problem, but if the promoted pointer is being
 * used in an lvalue context (as it is in many scavenging contexts where
 * we need to update pointer values), introduction of a temporary means
 * disaster.  Oregon, at least, issues a warning in such a situation; if
 * you get such a warning from your compiler, INVESTIGATE CAREFULLY!
 */


/*
 * Use the PROTECT macro to protect any temporary pOb values that you
 * use.  Anytime a function is invoked that might allocate space in the
 * heap, there is a chance of a scavenge that may invalidate heap
 * pointers that you are holding.  PROTECT will make sure that that those
 * pointers are properly updated.  PROTECT_THIS is a special case that
 * remembers the "this" pointer, and also updates it.  You *MUST*
 * dereference through SELF after a scavenge, since "this" may not be
 * valid anymore.  For example,
 *
 * 	pOb
 * 	Actor::foobar (pMeta widget)
 * 	{
 * 	    PROTECT_THIS(Actor);
 * 	    PROTECT(widget);
 * 	    ... do something that may cause a scavenge...
 * 	    return SELF->baz(SELF, SELF->membervar, widget);
 * 	}
 *
 * Without the PROTECTs and dereferences through SELF, you may get
 * garbage.  Maybe not today, maybe not tomorrow, but soon, and for the
 * rest of your life.
 *
 * In fact, a valuable debugging technique when things just start
 * breaking indiscriminately, is to set up a breakpoint on
 * Heap::scavenge() and enable it just before the disaster occurs, and
 * then walk back up the call chain.  Invariably, you will find yourself
 * in a routine that makes use of some formal parameter or local variable
 * that has been initialized but not protected and is used again later in
 * the routine.  Conditional statements and expressions are particularly
 * rich sources for intermittent problems.
 */

class ProtectedItem {
   protected:
    static ProtectedItem* root;
    ProtectedItem* next;
    void* item;

    static void scavenge();
    static void mark();
    static void check();

    friend class Heap;
    friend void Init_Heap();

   public:
    ProtectedItem(void* v) : next(root), item(v) {
        root = this;
    }

    ~ProtectedItem() { root = next; }
};


#define PROTECT(v) ProtectedItem name2(_, v)(&v)
#define PROTECT_THIS(type) \
    type* __this__ = this; \
    PROTECT(__this__)
#define SELF __this__

template <typename Type, typename... Types>
struct ProtectedItems {
    ProtectedItem p_;
    ProtectedItems<Types...> rest_;
    ProtectedItems(Type arg, Types... args) : p_((void*)&arg), rest_(args...) {}
};

template <typename Type>
struct ProtectedItems<Type> {
    ProtectedItem p_;
    ProtectedItems(Type arg) : p_((void*)&arg) {}
};

/*
 * The PALLOC macros are used to conditionally protect local variables
 * while allocating a chunk of heap.  They take the addresses of the
 * variables to be protected, but don't actually protect them unless a
 * scavenge is actually going to occur.  These are only used within the
 * various create() routines.
 */

static const int alignmentmask = 3;

int align(int size);

template <typename T>
T* gc_alloc(size_t sz) {
    auto p = (T*)heap->alloc(align(sz));
    if (!p) {
        p = (T*)heap->scavengeAndAlloc(sz);
    }

    return p;
}


template <typename T, typename... ArgTypes>
T* gc_alloc(size_t sz, ArgTypes... args) {
    auto p = (T*)heap->alloc(align(sz));
    if (!p) {
        ProtectedItems<ArgTypes...>(args...);
        return (T*)heap->scavengeAndAlloc(sz);
    }

    return p;
}

template <typename T>
T* gc_new() {
    auto sz = align(sizeof(T));
    T* loc = (T*)heap->alloc(sz);

    if (!loc) {
        loc = (T*)heap->scavengeAndAlloc(sz);
    }

    return new (loc) T();
}

template <typename T, typename... ArgTypes>
T* gc_new(ArgTypes... args) {
    T* loc = (T*)heap->alloc(align(sizeof(T)));

    // TODO(leaf): Figure a way to protect args and scavange.
    if (!loc) {
        ProtectedItems<ArgTypes...>(args...);
        loc = (T*)heap->scavengeAndAlloc(align(sizeof(T)));
    }

    return new (loc) T(args...);
}

template <typename T>
T* gc_new_space(const size_t extra_space) {
    auto sz = align(sizeof(T) + extra_space);
    T* loc = (T*)heap->alloc(sz);

    if (!loc) {
        loc = (T*)heap->scavengeAndAlloc(sz);
    }

    return new (loc) T();
}

template <typename T, typename... ArgTypes>
T* gc_new_space(const size_t extra_space, ArgTypes... args) {
    T* loc = (T*)heap->alloc(align(sizeof(T) + extra_space));

    // TODO(leaf): Figure a way to protect args and scavange.
    if (!loc) {
        ProtectedItems<ArgTypes...>(args...);
        loc = (T*)heap->scavengeAndAlloc(align(sizeof(T)));
    }

    return new (loc) T(args...);
}

#define PALLOC(n) palloc(n)
#define PALLOC1(n, o0) palloc1((n), &(o0))
#define PALLOC2(n, o0, o1) palloc2((n), &(o0), &(o1))
#define PALLOC3(n, o0, o1, o2) palloc3((n), &(o0), &(o1), &(o2))
#define PALLOC4(n, o0, o1, o2, o3) palloc4((n), &(o0), &(o1), &(o2), &(o3))
#define PALLOC5(n, o0, o1, o2, o3, o4) \
    palloc5((n), &(o0), &(o1), &(o2), &(o3), &(o4))
#define PALLOC6(n, o0, o1, o2, o3, o4, o5) \
    palloc6((n), &(o0), &(o1), &(o2), &(o3), &(o4), &(o5))


extern void* palloc(unsigned);
extern void* palloc1(unsigned, void*);
extern void* palloc2(unsigned, void*, void*);
extern void* palloc3(unsigned, void*, void*, void*);
extern void* palloc4(unsigned, void*, void*, void*, void*);
extern void* palloc5(unsigned, void*, void*, void*, void*, void*);
extern void* palloc6(unsigned, void*, void*, void*, void*, void*, void*);


/*
 * Align pads a size request to the next longword boundary, which is
 * necessary for compatibility with the tagging scheme that uses the low
 * two bits of a word for tag info (00 for a pointer).
 */

int align(int size);


#define IS_OLD(p) (!heap->is_new(p))
#define IS_NEW(p) (heap->is_new(p))

#endif
