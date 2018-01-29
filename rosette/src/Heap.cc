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

#include "rosette.h"
#include "BinaryOb.h"
#include "CommandLine.h"
#include "Ctxt.h"
#include "Heap.h"
#include "ObStk.h"
#include "PtrCollect.h"
#include "misc.h"

#include <memory.h>
#ifdef MIPS_SGI_SYSV
#include <sys/types.h>
#include <sys/immu.h>
#endif


/*
 * valloc doesn't appear to be declared in everyone's <stdlib.h>.
 */

#if defined(MALLOC_DEBUGGING)
extern "C" {
int malloc_verify();
}
#endif

static const int MaxFixedSize = sizeof(Ctxt);

int align(int size) { return ((size + alignmentmask) & ~alignmentmask); }


/*
 * The heap management code for RBL.  This is a two-tiered management
 * strategy for allocation and reclamation of heap-resident objects.
 *
 * The first tier is a generation-scavenging system adapted from the one
 * presented by Ungar in "Generaton Scavenging: A Non-disruptive High
 * Performance Storage Reclamation Algorithm" in the Proceedings of the
 * 1984 ACM SIGSOFT/SIGPLAN Software Engineering Symposium on Practical
 * Software Development Environments (issued as the May 1984 issue of
 * SIGPLAN Notices).  This scheme relies on the property that most
 * objects die young, and that those that don't tend to live a long time.
 * This is exploited by partitioning the heap into four spaces: an infant
 * space, two survivor spaces, and an old space.  (We refer to the
 * combination of the infant space and the two survivor spaces as the new
 * space.)  New objects are allocated out of the infant space until it is
 * exhausted, at which point a scavenge is undertaken.
 *
 * The scavenge moves all reachable objects from infant space (and one of
 * the survivor spaces) into the other survivor space (or possibly the
 * old space).  After the scavenge, there are no reachable objects in the
 * infant space or the first survivor space, so they can be reused
 * without scanning.  Thus, the cost of the scavenge is proportional only
 * to the number of objects *IN NEW SPACE* that are reachable.  It does
 * *NOT* depend on the number of objects in old space or the amount of
 * garbage in new space.
 *
 * An object is reachable if it is pointed to from the RBL runtime stack
 * or from an old space object, or if it is pointed to by a reachable
 * object (transitive closure).  The set of old objects that point to new
 * objects (objects in either infant space or survivor space) is
 * maintained in the heap's rememberedSet.  Proper maintenance of this
 * set requires that all stores into heap-resident objects be monitored:
 * whenever a pointer to a new object is written into an old object, the
 * address of that old object is added to the rememberedSet.
 *
 * Objects that survive enough scavenges are promoted (tenured) to old
 * space, so that they needn't be scavenged again after tenuring.  We
 * currently use a constant tenuring threshold; the system can be tuned
 * by using more sophisticated techniques as described by Ungar and
 * Jackson in "Tenuring Policies for Generation-Based Storage
 * Reclamation" in the 1988 OOPSLA Proceedings (issued as the November
 * 1988 issue of SIGPLAN Notices).
 *
 * We have incorporated an adaptation of one of the improvements
 * suggested by Ungar and Jackson: we provide primitive support for
 * "foreign objects", objects that are allocated out of the C++ heap
 * rather than the RBL heap.  This allows us to allocate such things as
 * large bitmaps, etc., so that they don't occupy valuable RBL heap space
 * and still have them properly garbage-collected.  The exact mechanism
 * is described later.
 *
 * The second tier of the system is based on the Quick Fit system
 * described by Weinstock and Wulf in "An Efficient Algorithm for Heap
 * Storage Allocation" in the October, 1988 issue of SIGPLAN Notices.
 * Old space is divided into two parts: that which has been allocated in
 * the past (and possibly reclaimed), and that which has never been
 * allocated (called the tail).  An array of free lists is maintained for
 * a small number of fixed chunk sizes: each free list holds chunks of a
 * specific size (the free list array is indexed by chunk size).  The
 * basic allocation scheme is:
 *
 * 1. If the request is for one of the fixed chunk sizes, and the
 * relevant free list is not empty, remove the the chunk from the free
 * list and return its address.
 *
 * 2. Otherwise, if there is enough room on the tail to satisfy the
 * request, increment the tail pointer by the requested amount and return
 * its pre-incremented value.
 *
 * 3. Otherwise, perform a traditional mark/scan garbage collection to
 * recover lost storage, and try steps 1 and 2 again.
 *
 * 4. If there is still not enough storage available, obtain a new tail
 * from the system and allocate the chunk from it.
 *
 * When objects are freed, their storage is returned to the appropriate
 * free list: the size-specific free list if the storage is one of the
 * special sizes, or the miscellaneous free list otherwise.  No attempt
 * is currently made to coalesce contiguous regions of memory when they
 * are freed.
 *
 * The mark/scan garbage collection has a couple of additional twists to
 * incorporate foreign objects and the scavenger's remembered set.  The
 * marking phase is traditional, marking all nodes reachable from the RBL
 * runtime stack.
 */


static int nextMultipleOf(int sz, int multiple) {
    int r = sz % multiple;

    if (r == 0) {
        return sz;
    } else {
        int q = sz / multiple;
        return (q + 1) * multiple;
    }
}


void RootSet::preScavenge() {}
void RootSet::scavenge() {}
void RootSet::postScavenge() {}
void RootSet::preGC() {}
void RootSet::mark() {}
void RootSet::postGC() {}
void RootSet::check() {}


/*
 * A "Space" simply identifies a region of memory through a traditional
 * base/limit pair.  Allocation and deallocation operations are deferred
 * to its subclasses.
 */

class Space {
   protected:
    void* const base;   // The address of the beginning of the space.
    void* const limit;  // The address of the end of the space.
    void* next;         // The dividing line between allocated and free.

    friend class OldSpace;
    friend class Heap;
    friend class SpaceTrav;
    friend Ob* Ob::relocate();

   public:
    Space(void*, unsigned);

    void reset();
    void* alloc(unsigned);
    void free(Ob*);
    int size();
    bool contains(Ob*);
    void scan();
    void check();
};


class SpaceTrav {
    Space* sp;
    void* current;

   public:
    SpaceTrav(Space*);

    bool valid();
    Ob* get();
    void advance();

    operator void*();
};


SpaceTrav::SpaceTrav(Space* space) {
    sp = space;
    current = sp->base;
}
bool SpaceTrav::valid() { return current < sp->next; }
Ob* SpaceTrav::get() { return (Ob*)current; }
void SpaceTrav::advance() { current = (char*)current + SIZE((Ob*)current); }
SpaceTrav::operator void*() { return valid() ? this : NULL; }


Space::Space(void* b, unsigned sz) : base(b), limit((char*)b + sz) {
    if (b == 0) {
        perror("heap allocation");
        exit(1);
    }
    next = b;
}


void Space::reset() { next = base; }


int Space::size() { return ((char*)limit - (char*)base); }


void Space::free(Ob* p) {
    if (!FREED(p)) {
        SET_FLAG(HDR_FLAGS(p), f_freed);
        if (FOREIGN(p) && !FORWARDED(p)) {
            p->Ob::~Ob();
        }
    }
}


void* Space::alloc(unsigned sz) {
    register void* current = next;
    register void* temp = (char*)current + sz;

    if (temp <= limit) {
        next = temp;
        return current;
    }

    return 0;
}


bool Space::contains(Ob* p) { return (base <= (void*)p) && ((void*)p < limit); }


void Space::scan() {
    for (SpaceTrav st(this); st; st.advance()) {
        Ob* p = st.get();
        if (MARKED(p)) {
            REMOVE_FLAG(HDR_FLAGS(p), f_marked);
        } else {
            free(p);
        }
    }
}


void Space::check() {
    for (SpaceTrav st(this); st; st.advance()) {
        st.get()->check();
    }
}


/*
 * Every NewSpace has a RememberedSet that holds pointers to objects in
 * older spaces that contain references to objects in the NewSpace.  The
 * RememberedSet is updated whenever a pointer to a young (new) object is
 * stored into an older object.  It may be compacted during scavenging.
 */


class RememberedSet : public ObStk {
    void reallyRemember(Ob*);

   public:
    void scan();
    void remember(Ob*);
};


void RememberedSet::scan() {
    for (PtrCollectionTrav pct(this); pct; pct.advance()) {
        void*& p = pct.get();
        if (!MARKED((Ob*)p)) {
            REMOVE_FLAG(HDR_FLAGS(((Ob*)p)), f_remembered);
            p = NULL;
        }
    }

    compact();
}


void RememberedSet::reallyRemember(Ob* p) {
    add(p);
    SET_FLAG(HDR_FLAGS(p), f_remembered);
}


void RememberedSet::remember(Ob* p) {
    if (!REMEMBERED(p)) {
        reallyRemember(p);
    }
}


class NewSpace : public Space {
   protected:
    Space* const infants;
    Space* survivors;
    Space* pastSurvivors;
    RememberedSet* const rememberedSet;

    friend Ob* Ob::relocate();
    friend class Heap;

   public:
    NewSpace(unsigned, unsigned);
    ~NewSpace();

    void* alloc(unsigned);
    void scavenge();
    void scan();
    void check();
    void remember(Ob*);
};


NewSpace::NewSpace(unsigned isize, unsigned ssize)
    : Space((void*)valloc(isize + 2 * ssize), isize + 2 * ssize),
      infants(new Space((char*)base + 2 * ssize, isize)),
      survivors(new Space((char*)base + ssize, ssize)),
      pastSurvivors(new Space(base, ssize)),
      rememberedSet(new RememberedSet) {
    /*
     * The NewSpace is allocated as one big chunk (the call to valloc
     * above), and that chunk is divided into three regions: an infant
     * space and two survivor spaces.  The initializations above rely on
     * the fact that the base and limit member variables of the NewSpace
     * are inititialized by the implicit superclass (Space) constructor
     * call before the initializations of infants et al take place.
     */
}


NewSpace::~NewSpace() { ::free(base); }


void* NewSpace::alloc(unsigned sz) { return infants->alloc(sz); }


void NewSpace::scavenge() {
    PtrCollectionTrav rst(rememberedSet);
    SpaceTrav st(survivors);

    /*
     * Scavenging the rememberedSet can cause more Ob's to be moved (via
     * copyAndForward) to the survivor space.  Similarly, scavenging the
     * survivor space can cause more objects to be remembered.
     *
     * This loop can probably be sped up significantly by ditching the
     * Trav's.  Since this is the heart of the scavenger, it's probably
     * worthwhile to break the encapsulation here.
     */

    while (true) {
        while (rst) {
            void*& rp = rst.get();
            Ob* p = (Ob*)rp;

            if (p->traversePtrs(MF_ADDR(Ob::relocate)) == 0) {
                /*
                 * traversePtrs() returns the number of pointers within
                 * *p that still point into new space.  If there are
                 * none, there is no point in keeping this p in the
                 * remembered set.
                 */
                REMOVE_FLAG(HDR_FLAGS(p), f_remembered);
                rp = NULL;
            }
            rst.advance();
        }

        /*
         * At this point we know that everything in the remembered set
         * has been traversed.  If there is nothing new in the survivor
         * space, then there is nothing left to be scavenged and we get
         * out.
         */

        if (!st) {
            break;
        }

        do {
            Ob* p = (Ob*)st.get();
            if (!FREED(p)) {
                p->traversePtrs(MF_ADDR(Ob::relocate));
            }
            st.advance();
        } while (st);
    }

    rememberedSet->compact();

    /*
     * Swap the roles of the survivor spaces.
     */
    Space* tmp = survivors;
    survivors = pastSurvivors;
    pastSurvivors = tmp;

    infants->reset();
    survivors->reset();
}


void NewSpace::scan() {
    /*
     * It is imperative that the remembered set be scanned first, since
     * it decides whether to eliminate entries based on whether they are
     * marked or not.  It it is not scanned first, entries may be
     * incorrectly eliminated.
     */

    rememberedSet->scan();
    infants->scan();
    survivors->scan();
    pastSurvivors->scan();
}


void NewSpace::check() {
    rememberedSet->check();
    infants->check();
    survivors->check();
    pastSurvivors->check();
}


void NewSpace::remember(Ob* p) { rememberedSet->remember(p); }


class OldSpaceChunk : public Space {
   protected:
    OldSpaceChunk* nextChunk;
    OldSpace* parent;

    OldSpaceChunk(unsigned, OldSpaceChunk*, OldSpace*);
    ~OldSpaceChunk();

    void scan();
    void free(Ob*);
    void checkUnrememberedPtrs();

    friend class OldSpace;
};


class OldSpace {
   protected:
    OldSpaceChunk* currentChunk;
    Ob** fixedFreeLists;
    Ob* miscFreeList;
    unsigned chunkSize;
    int chunkCount;

    void addChunk(int);

    void link(Ob*);
    Ob* unlink(Ob*&);

    void* miscAlloc(unsigned);
    void resetFreeLists();
    void checkFreeLists(char*);

    friend class OldSpaceChunk;
    friend class Heap;

   public:
    OldSpace(unsigned);
    ~OldSpace();

    void* alloc(unsigned);
    void free(Ob*);
    bool contains(Ob*);
    int size();
    void scan();
    void check();
    void checkUnrememberedPtrs();
};


OldSpaceChunk::OldSpaceChunk(unsigned int sz, OldSpaceChunk* n, OldSpace* p)
    : Space((void*)valloc(sz), sz), nextChunk(n), parent(p) {}


OldSpaceChunk::~OldSpaceChunk() { ::free(base); }


void OldSpaceChunk::checkUnrememberedPtrs() {
    for (SpaceTrav st(this); st; st.advance()) {
        pOb p = st.get();
        if (!REMEMBERED(p) && !FREED(p) &&
            p->traversePtrs(MF_ADDR(Ob::gcSensitive)) != 0) {
            warning("%s contains an unremembered reference to new space!",
                    p->typestring());
            heap->remember(p);
        }
    }
}


void OldSpaceChunk::free(Ob* p) {
    Space::free(p);
    parent->link(p);
}


void OldSpace::addChunk(int sz) {
    int excess = (char*)currentChunk->limit - (char*)currentChunk->next;
    if (excess >= MinObSize) {
        /*
         * We pass NULL as the meta and parent to the new scrap ob.
         * Ordinarily, this would be a bad thing to do, but since this
         * scrap is never actually traversed (because it is always free),
         * it doesn't matter that an invalid pointer is stored in those
         * fields.
         */
        Ob* p = new (currentChunk->next) Ob(excess, NULL, NULL);
        currentChunk->next = currentChunk->limit;
        SET_FLAG(HDR_FLAGS(p), f_freed);
        currentChunk->free(p);
    }
    currentChunk = new OldSpaceChunk(sz, currentChunk, this);
    chunkCount++;
}


void OldSpaceChunk::scan() {
    for (SpaceTrav st(this); st; st.advance()) {
        Ob* p = st.get();
        if (MARKED(p)) {
            REMOVE_FLAG(HDR_FLAGS(p), f_marked);
        } else {
            free(p);
        }
    }
}


OldSpace::OldSpace(unsigned oldSpaceChunkSize) {
    /*
     * Make sure that chunkCount is initialized before currentChunk,
     * because the constructor for currentChunk will use the value of
     * chunkCount when creating the chunk's namestring.
     */

    chunkSize = oldSpaceChunkSize;
    chunkCount = 1;
    currentChunk = new OldSpaceChunk(chunkSize, NULL, this);
    fixedFreeLists = new Ob*[MaxFixedSize + 1];
    for (int i = 0; i <= MaxFixedSize; i++) {
        fixedFreeLists[i] = 0;
    }
    miscFreeList = 0;
}


OldSpace::~OldSpace() {
    OldSpaceChunk* chunk = currentChunk;
    while (currentChunk) {
        chunk = currentChunk->nextChunk;
        delete currentChunk;
        currentChunk = chunk;
    }
    delete fixedFreeLists;
}


bool OldSpace::contains(Ob* p) {
    for (OldSpaceChunk* chunk = currentChunk; chunk; chunk = chunk->nextChunk) {
        if (currentChunk->contains(p)) {
            return true;
        }
    }

    return false;
}


int OldSpace::size() {
    int s = 0;
    for (OldSpaceChunk* chunk = currentChunk; chunk; chunk = chunk->nextChunk) {
        s += chunk->size();
    }

    return s;
}


void OldSpace::link(Ob* p) {
    int sz = SIZE(p);
    Ob*& freelist = (sz <= MaxFixedSize) ? fixedFreeLists[sz] : miscFreeList;
    p->forwardingAddress() = freelist;
    SET_FLAG(HDR_FLAGS(p), f_freed);
    freelist = p;
}


Ob* OldSpace::unlink(Ob*& freelist) {
    Ob* p = freelist;
    freelist = p->forwardingAddress();
    REMOVE_FLAG(HDR_FLAGS(p), f_freed);
    return p;
}


void* OldSpace::alloc(unsigned sz) {
    void* p;

    if (sz <= MaxFixedSize && fixedFreeLists[sz]) {
        return (void*)unlink(fixedFreeLists[sz]);
    }

    p = currentChunk->alloc(sz);
    if (NULL != p) {
        return p;
    }

    p = miscAlloc(sz);
    if (NULL != p) {
        return p;
    }

    addChunk(nextMultipleOf(sz, OldSpaceChunkSize));
    return currentChunk->alloc(sz);
}


void* OldSpace::miscAlloc(unsigned sz) {
    for (Ob** next = &miscFreeList; *next;
         next = &(*next)->forwardingAddress()) {
        if (SIZE(*next) == sz) {
            return (void*)unlink(*next);
        }
    }

    return 0;
}


void OldSpace::free(Ob* p) { currentChunk->free(p); }


void OldSpace::resetFreeLists() {
    miscFreeList = 0;
    for (int i = 0; i <= MaxFixedSize; i++) {
        fixedFreeLists[i] = 0;
    }
}


void OldSpace::checkFreeLists(char* title) {
    Ob* p;

    for (int i = 0; i <= MaxFixedSize; i++) {
        for (p = fixedFreeLists[i]; p != NULL; p = p->forwardingAddress()) {
            if (SIZE(p) != i) {
                warning(
                    "%d-byte object (at 0x%x) on free list %s for %d-byte "
                    "objects",
                    SIZE(p), (int)p, title, i);
            }
        }
    }
}


void OldSpace::scan() {
    checkFreeLists("(before scan)");

    resetFreeLists();
    for (OldSpaceChunk* chunk = currentChunk; chunk; chunk = chunk->nextChunk) {
        chunk->scan();
    }

    checkFreeLists("(after scan)");
}


void OldSpace::check() {
    for (OldSpaceChunk* chunk = currentChunk; chunk; chunk = chunk->nextChunk) {
        chunk->check();
    }

    checkFreeLists("(after scavenge)");
}


void OldSpace::checkUnrememberedPtrs() {
    for (OldSpaceChunk* chunk = currentChunk; chunk; chunk = chunk->nextChunk) {
        chunk->checkUnrememberedPtrs();
    }
}


class ForeignObTbl : public ObStk {
   public:
    void scavenge();
    void scan();
};


void ForeignObTbl::scavenge() {
    /*
     * In a scavenge, any foreign object that has been forwarded (which
     * means it is now necessarily in either survivorSpace or oldSpace)
     * is assumed to survive.  Objects that have been tenured in oldSpace
     * are removed from the table; once they move to oldSpace they can
     * only be recovered by the scan phase of a full-fledged garbage
     * collection, so there is no point in keeping a record of them in
     * this table.  Anything that has not been forwarded is assumed to be
     * garbage and is deleted and its table slot reused.
     */

    for (PtrCollectionTrav pct(this); pct; pct.advance()) {
        Ob* p = (Ob*)pct.get();
        if (FORWARDED(p)) {
            p = p->forwardingAddress();
            pct.get() = IS_OLD(p) ? NULL : p;
        } else {
            p->Ob::~Ob();
            pct.get() = NULL;
        }
    }
    compact();
}


void ForeignObTbl::scan() {
    /*
     * Any foreign object that has not been marked is assumed to be
     * garbage, and the reference to it is deleted and its table slot
     * reused.  We do not deallocate the objects that we are forgetting,
     * since that will be accomplished during the rest of the scan (doing
     * it here would lead to a double deallocation).
     */

    for (PtrCollectionTrav pct(this); pct; pct.advance()) {
        void*& p = pct.get();
        Ob* h = (Ob*)p;
        if (!MARKED(h)) {
            p = NULL;
        }
    }

    compact();
}


class GCAgenda : public ObStk {
   public:
    void scavenge();
    void scan();
};


void GCAgenda::scavenge() {
    /*
     * scavengeFixup should return TRUE if an object is to remain on the
     * gcAgenda after the fixup.  If FALSE is returned, the object will
     * be removed from the gcAgenda and will have to cause itself to be
     * re-installed later if so required.
     */
    for (PtrCollectionTrav pct(this); pct; pct.advance()) {
        void*& p = pct.get();
        Ob* h = (Ob*)p;
        if (FORWARDED(h)) {
            h = h->forwardingAddress();
            p = h->scavengeFixup() ? h : NULL;
        } else if (!IS_OLD(h) || !h->scavengeFixup()) {
            p = NULL;
        }
    }
    compact();
}


void GCAgenda::scan() {
    /*
     * Unmarked objects are unconditionally deleted from the gcAgenda.
     * Marked objects will be removed from the gcAgenda if they respond
     * FALSE to gcFixup.
     */
    for (PtrCollectionTrav pcct(this); pcct; pcct.advance()) {
        void*& p = pcct.get();
        Ob* h = (Ob*)p;
        if (!MARKED(h) || !h->gcFixup()) {
            p = NULL;
        }
    }
    compact();
}


Heap* heap;


Heap::Heap(unsigned infantSpaceSize, unsigned survivorSpaceSize,
           unsigned oldSpaceChunkSize)
    : newSpace(new NewSpace(infantSpaceSize, survivorSpaceSize)),
      oldSpace(new OldSpace(oldSpaceChunkSize)),
      foreignObs(new ForeignObTbl),
      gcAgenda(new GCAgenda),
      tenuredObs(new ObStk),
      rootSets(new PtrCollection),
      newSpaceBase(newSpace->base),
      newSpaceLimit(newSpace->limit) {
    scavengeCount = 0;
    gcCount = 0;
    totalScavenges = 0;
    totalGCs = 0;
}


Heap::~Heap() {
    delete newSpace;
    delete oldSpace;
    delete foreignObs;
    delete gcAgenda;
    delete rootSets;
    delete tenuredObs;
}


void Heap::traverseRootSets(RootSet_Fn fn) {
    for (PtrCollectionTrav pct(rootSets); pct; pct.advance()) {
        RootSet* rs = (RootSet*)pct.get();
        (rs->*fn)();
    }
}


void Heap::addRootSet(RootSet* rs) { rootSets->add(rs); }


void Heap::deleteRootSet(RootSet* rs) {
    for (PtrCollectionTrav pct(rootSets); pct; pct.advance()) {
        if (rs == pct.get()) {
            pct.get() = NULL;
            rootSets->compact();
            return;
        }
    }

    suicide("tried to delete non-existent root set");
}


int Heap::size() { return newSpace->size() + oldSpace->size(); }


/*
 * magicLoc and catchMagic can prove to be invaluable during debugging.
 * If a location is getting clobbered by a stray pointer, you can use a
 * debugger to set magicLoc to determine when it is allocated and trap on
 * entry to catchMagic.  This is tremendously faster than setting
 * debugger breakpoints.
 */

#ifdef DEBUG
static void catchMagic() {}
#endif

void* Heap::alloc(unsigned sz) {
    void* loc = newSpace->alloc(sz);
#ifdef DEBUG
    if (loc == magicLoc) {
        catchMagic();
    }
#endif
    return loc;
}


void* Heap::scavengeAndAlloc(unsigned sz) {
    scavenge();
    void* loc = alloc(sz);
    if (!loc) {
        suicide("scavengeAndAlloc -- out of space");
    }

    return loc;
}


void Heap::remember(Ob* p) {
    /*
     * Heap::remember (as well as NewSpace::remember and
     * RememberedSet::remember) assumes that the argument is in fact a
     * valid pointer (i.e., not a fixnum or some other nonsense).  This
     * must be guaranteed by the caller.
     */
    newSpace->remember(p);
}


Ob* Heap::copyAndForward(Ob* oldLoc) {
    Ob* newLoc = 0;

#ifdef DEBUG
    assert(!FORWARDED(oldLoc));
#endif

    if (AGE(oldLoc) < TenuringAge) {
        AGE(oldLoc)++;
        newLoc = (Ob*)newSpace->survivors->alloc(SIZE(oldLoc));
    }

    if (newLoc == 0) {
        newLoc = (Ob*)oldSpace->alloc(SIZE(oldLoc));
        oldLoc->forwardTo(newLoc);
        remember(newLoc);
        /*
         * The call to remember() *must* be made *after* the call to
         * forwardTo() because remember() sets a header bit that
         * forwardTo() clobbers.
         */
    } else {
        oldLoc->forwardTo(newLoc);
    }

    return newLoc;
}


void Heap::scavenge() {
    if (ParanoidAboutGC) {
        oldSpace->checkUnrememberedPtrs();
    }

    int oldChunkCount = oldSpace->chunkCount;

    traverseRootSets(MF_ADDR(RootSet::preScavenge));

    /*
     * The order of scavenging here is important.  In particular, the
     * root sets need to be scavenged first, since they hold the roots of
     * the reachable objects, and the foreignObTbl needs to be scavenged
     * *LAST*, since it decides to deallocate foreign obs based on
     * whether or not they have been forwarded during scavenging.
     */

    traverseRootSets(MF_ADDR(RootSet::scavenge));
    ProtectedItem::scavenge();

    /*
     * There is no need to scavenge the tenured objects since they are
     * (by definition) all in old space.
     */

    newSpace->scavenge();
    gcAgenda->scavenge();
    foreignObs->scavenge();

    if (ParanoidAboutGC) {
        traverseRootSets(MF_ADDR(RootSet::check));
        ProtectedItem::check();
        tenuredObs->check();

        newSpace->check();
        oldSpace->check();
        foreignObs->check();
        gcAgenda->check();

#if defined(MALLOC_DEBUGGING)
        if (!malloc_verify()) {
            suicide("Heap::scavenge -- malloc_verify found a problem");
        }
#endif
    }

    scavengeCount++;

    traverseRootSets(MF_ADDR(RootSet::postScavenge));

    /*
     * If scavenging forced us to add new chunks to old space, we perform
     * a GC after the fact to look for unnecessarily tenured objects.
     */

    if (oldChunkCount != oldSpace->chunkCount) {
        gc();
    }
}


int nMarked;


void Heap::gc() {
    traverseRootSets(MF_ADDR(RootSet::preGC));

    nMarked = 0;

    traverseRootSets(MF_ADDR(RootSet::mark));
    ProtectedItem::mark();
    tenuredObs->mark();

    /*
     * The order in which we do these scans is *EXTREMELY* important:
     * since the foreignObTbl and gcAgenda eliminate those entries that
     * have not been marked, they *MUST* perform their scans before any
     * of the other scans that might reset those mark bits.  Similarly,
     * since newSpace checks its RememberedSet (whose entries all point
     * into oldSpace) and deletes those that are not marked, it must
     * perform its scan prior to the oldSpace scan.
     */

    foreignObs->scan();
    gcAgenda->scan();
    newSpace->scan();
    oldSpace->scan();

    gcCount++;

    traverseRootSets(MF_ADDR(RootSet::postGC));
}


Ob* Heap::tenure(Ob* o) {
    if (!IS_PTR(o)) {
        return o;
    }

    AGE(o) = TenuringAge;
    Ob* newLoc = copyAndForward(o);
    if (FOREIGN(o)) {
        foreignObs->scavenge();
    }

    tenuredObs->add(newLoc);

    return newLoc;
}


void Heap::tenureEverything() {
    gc();
    int tempTenuringAge = TenuringAge;
    TenuringAge = 0;
    scavenge();
    TenuringAge = tempTenuringAge;
}


bool Heap::validPtrAfterScavenge(Ob* p) {
    return newSpace->pastSurvivors->contains(p) || !newSpace->contains(p);
}


void Heap::registerForeignOb(Ob* p) {
    SET_FLAG(HDR_FLAGS(p), f_foreign);
    foreignObs->add(p);
}


void Heap::registerGCAgenda(Ob* p) { gcAgenda->add(p); }


void Heap::resetCounts() {
    totalScavenges += scavengeCount;
    scavengeCount = 0;
    totalGCs += gcCount;
    gcCount = 0;
}


void Heap::printCounts(FILE* f) {
    fprintf(f, "heap: %d/%d scavenges, %d/%d garbage collects\n", scavengeCount,
            scavengeCount + totalScavenges, gcCount, gcCount + totalGCs);
}


ProtectedItem* ProtectedItem::root = 0;


void ProtectedItem::scavenge() {
    for (ProtectedItem* pi = ProtectedItem::root; pi; pi = pi->next) {
        pOb* p = (pOb*)(pi->item);
        useIfPtr(p, MF_ADDR(Ob::relocate));
    }
}


void ProtectedItem::mark() {
    for (ProtectedItem* pi = ProtectedItem::root; pi; pi = pi->next) {
        pOb* p = (pOb*)(pi->item);
        useIfPtr(*p, MF_ADDR(Ob::mark));
    }
}


void ProtectedItem::check() {
    for (ProtectedItem* pi = ProtectedItem::root; pi; pi = pi->next) {
        pOb* p = (pOb*)(pi->item);
        useIfPtr(*p, MF_ADDR(Ob::checkOb));
    }
}


Ob* Ob::relocate() {
    /*
     * Please excuse the goto's in the following code, as well as the
     * explicit references to the various base and limit pointers, but
     * the compiler did an inadequate job of compiling this important
     * routine.
     *
     * Notice that this routine depends critically upon the relative
     * ordering of the old space, the infant space and the past survivor
     * space:
     *
     * 	past survivor addresses < infant addresses < old space addresses
     *
     * If that ordering changes, this code had better change as well.
     */

    if (FREED(this)) {
        warning("relocate called on freed %s", typestring());
        return (Ob*)INVALID;
    }

    if ((void*)this >= heap->newSpace->limit)
        goto nochange;
    if ((void*)this >= heap->newSpace->infants->base)
        goto relocate;
    if ((void*)this < heap->newSpace->pastSurvivors->base)
        goto nochange;
    if ((void*)this < heap->newSpace->pastSurvivors->limit)
        goto relocate;

nochange:
    return this;

relocate:
    if (FORWARDED(this)) {
        return forwardingAddress();
    } else {
        return heap->copyAndForward(this);
    }
}


void* palloc(unsigned sz) {
    void* loc = heap->alloc(sz);
    return loc ? loc : heap->scavengeAndAlloc(sz);
}


void* palloc1(unsigned sz, void* ob0) {
    void* loc = heap->alloc(sz);
    if (!loc) {
        ProtectedItem pob0(ob0);
        loc = heap->scavengeAndAlloc(sz);
    }
    return loc;
}


void* palloc2(unsigned sz, void* ob0, void* ob1) {
    void* loc = heap->alloc(sz);
    if (!loc) {
        ProtectedItem pob0(ob0);
        ProtectedItem pob1(ob1);
        loc = heap->scavengeAndAlloc(sz);
    }
    return loc;
}


void* palloc3(unsigned sz, void* ob0, void* ob1, void* ob2) {
    void* loc = heap->alloc(sz);
    if (!loc) {
        ProtectedItem pob0(ob0);
        ProtectedItem pob1(ob1);
        ProtectedItem pob2(ob2);
        loc = heap->scavengeAndAlloc(sz);
    }
    return loc;
}


void* palloc4(unsigned sz, void* ob0, void* ob1, void* ob2, void* ob3) {
    void* loc = heap->alloc(sz);
    if (!loc) {
        ProtectedItem pob0(ob0);
        ProtectedItem pob1(ob1);
        ProtectedItem pob2(ob2);
        ProtectedItem pob3(ob3);
        loc = heap->scavengeAndAlloc(sz);
    }
    return loc;
}


void* palloc5(unsigned sz, void* ob0, void* ob1, void* ob2, void* ob3,
              void* ob4) {
    void* loc = heap->alloc(sz);
    if (!loc) {
        ProtectedItem pob0(ob0);
        ProtectedItem pob1(ob1);
        ProtectedItem pob2(ob2);
        ProtectedItem pob3(ob3);
        ProtectedItem pob4(ob4);
        loc = heap->scavengeAndAlloc(sz);
    }
    return loc;
}


void* palloc6(unsigned sz, void* ob0, void* ob1, void* ob2, void* ob3,
              void* ob4, void* ob5) {
    void* loc = heap->alloc(sz);
    if (!loc) {
        ProtectedItem pob0(ob0);
        ProtectedItem pob1(ob1);
        ProtectedItem pob2(ob2);
        ProtectedItem pob3(ob3);
        ProtectedItem pob4(ob4);
        ProtectedItem pob5(ob5);
        loc = heap->scavengeAndAlloc(sz);
    }
    return loc;
}
