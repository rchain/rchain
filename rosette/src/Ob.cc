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

#include "Ob.h"
#include "RblAtom.h"
#include "Code.h"
#include "CommandLine.h"
#include "Compile.h"
#include "Ctxt.h"
#include "Interrupt.h"
#include "Location.h"
#include "Meta.h"
#include "MI.h"
#include "Monitor.h"
#include "Operation.h"
#include "Pattern.h"
#include "Prim.h"
#include "RBLstring.h"
#include "Table.h"
#include "Tuple.h"
#include "Vm.h"
#include "Addr.h"
#include "ModuleInit.h"
#include "config.h"

#include "Export.h"

#include <cstdlib>
#include <ctime>

#ifdef MAP_BACK_ADDRESS
extern uint32_t nontrivial_pre_fixnum_to_addr(int);
extern int nontrivial_addr_to_pre_fixnum(Ob*);
#endif

extern "C" {
char* getcwd(char*);
}

#include <assert.h>
#include <memory.h>

#if defined(_OREGON_)
#undef NULL
#endif
#include <sys/param.h>


pMeta META(pOb ob) { return (pMeta)ob->meta(); }
pSBO SBO(pOb ob) { return (pSBO)ob->parent(); }

pOb BASE(pOb v) { return TAG(v) == OTptr ? v : decodeAtom(v); }

int TAG(pOb x) {
    TagExtract te;
    te.ptr = x;
    return GET_LF(te, 0, TagSize);
}

int ESCTAG(pOb x) {
    TagExtract te;
    te.ptr = x;
    return GET_LF(te, 0, EscTagSize);
}

/**
 * NB(leaf): Originally, this was a macro defined as follows:
 *
 * #define SIGN_EXTEND(a, n) ((((int)a) << (WordSize - (n))) >> (WordSize -
 * (n)))
 *
 * Looking at the function definition, it's not clear what's going on here
 * other than a funky cast to int. Is it even possible for this function to
 * return a value other than (int)a?
 */
static const int SIGN_EXTEND(const int a, const int sz) {
    const auto x = a << sz;
    return x >> sz;
}

const int TAGVAL(pOb x) {
    TagExtract te;
    te.ptr = x;
    auto tmp = GET_LF(te, TagSize, WordSize - TagSize);
    return SIGN_EXTEND(tmp, TagSize);
}

const int ESCVAL(pOb x) {
    TagExtract te;
    te.ptr = x;
    auto tmp = GET_LF(te, EscTagSize, WordSize - EscTagSize);
    return SIGN_EXTEND(tmp, EscTagSize);
}


char* Base::typestring() { return "unknown type"; }


void Base::updateCnt() { suicide("suicide in Base::updateCnt()"); }

IdType nextId = 1;
Ob::Ob(InPlace_Constructor*, int sz) : header(sz) {}


Ob::~Ob() {}


void* Ob::operator new(size_t) {
    suicide("operator new not allowed for Rosette objects\n");
    return NULL;
}


void Ob::operator delete(void*) {}


void Ob::notImplemented(char* opname) {
    warningx("%s::%s not implemented on ", typestring(), opname);
    printOn(stderr);
    putc('\n', stderr);
}


void Ob::forwardTo(pOb p) {
#ifdef DEBUG
    assert(!FORWARDED(this));
    assert(!REMEMBERED(this));
#endif
    memcpy(p, this, SIZE(this));
    SET_FLAG(HDR_FLAGS(this), f_forwarded);
    forwardingAddress() = p;
}


bool Ob::reallyCheckStore(pOb val) {
    if (IS_PTR(val) && IS_NEW(val)) {
        heap->remember(this);
        return true;
    }

    return false;
}


/*
 * Ob::relocate is defined in Heap.cc because of its dependence on some
 * heap internals.
 */


/*
 * We make this pretty big to avoid spurious messages about GlobalEnv.
 */

static const int SuspicionThreshold = sizeof(Ob) + 8192 * sizeof(pOb);


bool Ob::suspicious() {
    return (SIZE(this) < MinObSize || (SIZE(this) > SuspicionThreshold));
}


void Ob::clobberVtbl(pOb proto) {
    /*
     * ********************** WARNING!!!! ***************************
     *
     * This code relies on objects being represented with a pointer
     * to their virtual function table stored in their first word.  Ob.h
     * is careful to try to arrange this, but when porting this code to a
     * new machine or compiler, you need to confirm that this is in fact
     * the case.
     */

    *(void**)this = *(void**)proto;
}


extern int nMarked;


void Ob::mark() {
    if (MARKED(this)) {
        return;
    }

    assert(!FORWARDED(this));
    nMarked++;
    SET_FLAG(HDR_FLAGS(this), f_marked);
    traversePtrs(MF_ADDR(Ob::mark));
}


void Ob::check() {
    extern int InBigBang;

    char* ts = typestring();

    if (!heap->validPtrAfterScavenge(this)) {
        suicide("%s not properly scavenged", ts);
    }

    /*
     * If we're initializing stuff in BigBang, metas and parents are
     * going to be invalid for a while.  Ignore them.
     */

    if (!InBigBang && (meta() == INVALID || parent() == INVALID)) {
        warning("%s has invalid meta or parent", ts);
    }

    if (suspicious()) {
        warning("%s has suspicious size of %d bytes", ts, SIZE(this));
    }

    if (MARKED(this)) {
        suicide("live %s still marked", ts);
    }

    if (!FREED(this)) {
        if (FORWARDED(this)) {
            suicide("live %s still forwarded", ts);
        }

        // This traversal just makes sure that all of the constituent
        // pOb's are valid.  It doesn't recurse.

        traversePtrs(MF_ADDR(Ob::checkOb));
    }
}


void Ob::checkOb() {
    // This routine is only invoked from Ob's that are still reachable,
    // so the object had better not still be forwarded or free.  Unlike
    // the marking and counting routines, this does not recurse.

    extern int InBigBang;

    char* ts = typestring();

    if (FREED(this)) {
        suicide("live %s has been freed", ts);
    } else if (MARKED(this)) {
        suicide("live %s still marked", ts);
    }

    if (!heap->validPtrAfterScavenge(this)) {
        suicide("%s not properly scavenged", ts);
    }

    if (!InBigBang && (meta() == INVALID || parent() == INVALID)) {
        warning("%s has invalid meta or parent", ts);
    }

    if (suspicious()) {
        warning("%s has suspicious size of %d bytes", ts, SIZE(this));
    }

    if (FORWARDED(this)) {
        suicide("live %s still forwarded", ts);
    }

    if (IS_OLD(this) && !REMEMBERED(this) &&
        traversePtrs(MF_ADDR(Ob::gcSensitive)) != 0) {
        warning("%s is old, contains ptrs to new space, and is not remembered",
                ts);
        heap->remember(this);
    }
}


int Ob::size() {
    if (VISITED(this)) {
        return 0;
    } else {
        SET_FLAG(HDR_FLAGS(this), f_visited);
        return SIZE(this) + traversePtrs(MF_ADDR(Ob::size));
    }
}


int Ob::obCount() {
    if (VISITED(this)) {
        return 0;
    } else {
        SET_FLAG(HDR_FLAGS(this), f_visited);
        return 1 + traversePtrs(MF_ADDR(Ob::obCount));
    }
}


void Ob::unvisit() {
    if (VISITED(this)) {
        REMOVE_FLAG(HDR_FLAGS(this), f_visited);
        traversePtrs(MF_ADDR(Ob::unvisit));
    }
}


int inlineUseIfPtr(void* v, PSOb__PSOb f) {
    pOb* pp = (pOb*)v;
    pOb p = *pp;
    if (IS_PTR(p)) {
        pOb q = (PTR(p)->*f)();
        if (p != q) {
            *pp = q;
            return 1;
        } else {
            return 0;
        }
    } else {
        return 0;
    }
}


int useIfPtr(void* v, PSOb__PSOb f) { return inlineUseIfPtr(v, f); }

int inlineUseIfPtr(pOb v, SI__PSOb f) {
    if (!IS_PTR(v)) return 0;
    return (v->*f)();
}

int useIfPtr(pOb v, SI__PSOb f) { return inlineUseIfPtr(v, f); }


void inlineUseIfPtr(pOb v, V__PSOb f) {
    if (IS_PTR(v)) {
        (PTR(v)->*f)();
    }
}


void useIfPtr(pOb v, V__PSOb f) { inlineUseIfPtr(v, f); }


int Ob::traversePtrs(PSOb__PSOb f) {
    int sum = 0;
    pOb* const end = endp();
    pOb* p = _slot;

    while (p < end) {
        sum += inlineUseIfPtr(p++, f);
    }

    return sum;
}


int Ob::traversePtrs(SI__PSOb f) {
    int sum = 0;
    pOb* end = endp();
    pOb* p = _slot;

    while (p < end) {
        sum += inlineUseIfPtr(*p++, f);
    }

    return sum;
}


void Ob::traversePtrs(V__PSOb f) {
    pOb* end = endp();
    pOb* p = _slot;

    while (p < end) {
        inlineUseIfPtr(*p++, f);
    }
}


UNIMPLEMENTED(bool, Ob, gcFixup, ());
UNIMPLEMENTED(bool, Ob, scavengeFixup, ());


pOb obcpy(pOb dest, pOb src, int sz) {
    memcpy(dest, src, sz);
    dest->header.all = 0;
    dest->header.fields.size = sz;
    dest->objectId = nextId++;  // New object gets a new ID
    if (FOREIGN(src)) {
        SET_FLAG(HDR_FLAGS(dest), f_foreign);
    } else {
        REMOVE_FLAG(HDR_FLAGS(dest), f_foreign);
    }
    return dest;
}


extern pOb emptyMbox;


pOb Ob::self() { return this; }
bool Ob::ConstantP() { return true; }
Prim* Ob::InlineablePrimP() { return (Prim*)INVALID; }
void Ob::printOn(FILE* f) { fputs(asCstring(), f); }
void Ob::printQuotedOn(FILE* f) { printOn(f); }
void Ob::displayOn(FILE* f) { printOn(f); }
pOb Ob::container() { return this; }
pOb Ob::mailbox() { return emptyMbox; }
pOb Ob::setMailbox(pOb) { return self(); }
char* Ob::asPathname() { return 0; }

const char* Ob::asCstring() {
    sprintf(Ob::stringbuf, "{%s}", typestring());
    return Ob::stringbuf;
}

UNIMPLEMENTED(int, Ob, addSlot, (pOb, pOb));
UNIMPLEMENTED(pOb, Ob, indexedSize, ());
UNIMPLEMENTED(pOb, Ob, nth, (int));
UNIMPLEMENTED(pOb, Ob, setNth, (int, pOb));
UNIMPLEMENTED(pOb, Ob, subObject, (int, int));


pOb Ob::rcons(pOb val) {
    int sz = SIZE(this);
    int newsz = sz + sizeof(pOb);
    pOb self = this;
    pOb ob = (pOb)PALLOC2(newsz, self, val);
    obcpy(ob, self, sz);
    SIZE(ob) = newsz;
    pOb* p = (pOb*)((char*)ob + sz);
    *p = val;

    return ob;
}


pOb Ob::clone() { return cloneTo(meta(), parent()); }


pOb Ob::dup() { return this; }


pOb Ob::cloneTo(pOb new_meta, pOb new_parent) {
    int sz = SIZE(this);
    PROTECT_THIS(Ob);
    PROTECT(new_meta);
    PROTECT(new_parent);
    pOb ob = (pOb)PALLOC(sz);
    obcpy(ob, SELF, sz);
    ob->meta() = new_meta;
    ob->parent() = new_parent;
    ob->updateCnt();
    BASE(new_meta)->addRef();
    if (FOREIGN(SELF)) {
        heap->registerForeignOb(ob);
    }

    return ob;
}


pOb Ob::getLex(int indirect, int level, int offset) {
    pOb p = this;
    while (level--) {
        p = BASE(p->parent());
    }

    if (indirect) {
        if (p->numberOfSlots() <= SLOT_NUM(Actor, extension)) {
            return INVALID;
        } else {
            p = ((Actor*)p)->extension;
        }
    }

    return (offset < p->numberOfSlots() ? p->slot(offset) : INVALID);
}


pOb Ob::setLex(int indirect, int level, int offset, pOb val) {
    pOb p = this;
    while (level--) {
        p = BASE(p->parent());
    }

    if (indirect) {
        if (p->numberOfSlots() <= SLOT_NUM(Actor, extension)) {
            return INVALID;
        } else {
            p = ((Actor*)p)->extension;
        }
    }

    if (offset >= p->numberOfSlots()) {
        return INVALID;
    } else {
        ASSIGN(p, slot(offset), val);
        return val;
    }
}


pOb Ob::getAddr(int indirect, int level, int offset) {
    pOb p = this;
    while (level--) {
        p = BASE(p->parent());
    }

    if (indirect) {
        if (p->numberOfSlots() <= SLOT_NUM(Actor, extension)) {
            return INVALID;
        } else {
            p = ((Actor*)p)->extension;
        }
    }

    if (offset < p->numberOfSlots()) {
        return FIXNUM(ADDR_TO_PRE_FIXNUM(p->slot(offset)));
    } else {
        return INVALID;
    }
}


pOb Ob::setAddr(int indirect, int level, int offset, pOb val) {
    uint32_t addr;
    if (!IS_FIXNUM(val)) {
        return INVALID;
    }

    addr = PRE_FIXNUM_TO_ADDR(FIXVAL(val));
    pOb p = this;
    while (level--) {
        p = BASE(p->parent());
    }

    if (indirect) {
        if (p->numberOfSlots() <= SLOT_NUM(Actor, extension)) {
            return INVALID;
        } else {
            p = ((Actor*)p)->extension;
        }
    }

    if (offset >= p->numberOfSlots()) {
        return INVALID;
    } else {
        ASSIGN(p, slot(offset), (pOb)addr);
        return val;
    }
}


pOb Ob::getField(int indirect, int level, int offset, int span, int sign) {
    long ans;
    pOb p = this;
    while (level--) {
        p = BASE(p->parent());
    }

    if (indirect) {
        p = ((Actor*)p)->extension;
    }

    switch (span) {
    case 8:
        if (sign)
            ans = *(signed char*)((char*)p + (offset / BITS(char)));
        else
            ans = *(unsigned char*)((char*)p + (offset / BITS(char)));
        break;
    case 16:
        if (sign)
            ans = *(signed short*)((short*)p + (offset / BITS(short)));
        else
            ans = *(unsigned short*)((short*)p + (offset / BITS(short)));
        break;
    case 32:
        if (sign)
            ans = *(signed long*)((long*)p + (offset / BITS(long)));
        else
            ans = *(unsigned long*)((long*)p + (offset / BITS(long)));
        break;
    default: {
        // Fields that are not multiples of 8 bits are not expected. Previously,
        // this contained some complex and questionable big-endian dependent
        // code.
        ans = 0;
    }
    }

    return FIXNUM(ans);
}

pOb Ob::setField(int indirect, int level, int offset, int span, uint32_t bits) {
    pOb p = this;
    while (level--) {
        p = BASE(p->parent());
    }

    if (indirect) {
        p = ((Actor*)p)->extension;
    }

    switch (span) {
    case 8:
        *(unsigned char*)((char*)p + (offset / BITS(char))) =
            (unsigned char)bits;
        break;
    case 16:
        *(unsigned short*)((short*)p + (offset / BITS(short))) =
            (unsigned short)bits;
        break;
    case 32:
        *(unsigned long*)((long*)p + (offset / BITS(long))) =
            (unsigned long)bits;
        break;
    default: {
        // Fields that are not multiples of 8 bits are not expected. Previously,
        // this contained some complex and questionable big-endian dependent
        // code.
    }
    }
    return this;
}


/* Object actions */


bool Ob::isSynchronousTrgt() { return true; }

DEF("object-lookup-and-invoke", objectLookupAndInvoke, 2, 2) {
    CHECK(1, Ctxt, ctxt);

    if (debugging_level) {
        printf("\t%s\n", BASE(ARG(0))->asCstring());
    }

    if (ctxt->nargs > 0) {
        return BASE(ctxt->arg(0))->lookupAndInvoke(ctxt);
    } else {
        return BASE(ctxt->trgt)->runtimeError(ctxt, "no argument for dispatch");
    }
}

DEF_OPRN(Sync, "lookupAndInvoke", oprnLookupAndInvoke, objectLookupAndInvoke);

pOb Ob::dispatch(Ctxt* ctxt) {
    PROTECT(ctxt);

    Tuple* av = Tuple::create(2, NIV);

    ASSIGN(av, elem(0), ctxt->trgt);
    ASSIGN(av, elem(1), ctxt);

    pCtxt c = Ctxt::create(oprnLookupAndInvoke, av);
    c->nargs = 2;


    return BASE(c->trgt)->dispatch(c);
}


pOb Ob::invoke(Ctxt* ctxt) {
    if (ctxt->nargs == 1) {
        /*
         * The objective in this case is to achieve the appearance of
         * executing a method that returns "this" value.  In the case of
         * a synchronous operation, the mythical method looks like
         *
         * 	(method [] *me*)
         *
         * and in the case of an asynchronous operation, the mythical
         * method looks like
         *
         * 	(method [] (update!) *me*)
         */

        pOb me = self();
        ctxt->ret(me);
        if (!BASE(ctxt->trgt)->isSynchronousTrgt()) {
            BASE(ctxt->arg(0))->updateNoArgs();
        }

        return me;
    } else {
        return BASE(ctxt->trgt)->runtimeError(ctxt, "bad method");
    }
}


pOb Ob::lookup(pOb key, Ctxt* ctxt) {
    if (interruptPending) {
        return ABSENT;
    }

    pOb me = self();
    pOb result = BASE(meta())->get(me, key, ctxt);
    if (result == ABSENT) {
        return BASE(parent())->lookup(key, ctxt);
    } else {
        return result;
    }
}


pOb Ob::lookupAndInvoke(Ctxt* ctxt) {
    pOb fn = BASE(meta())->lookupOBO(self(), ctxt->trgt, ctxt);

    if (interruptPending) {
        return ABSENT;
    }

    if (fn == ABSENT) {
        PROTECT_THIS(Ob);
        PROTECT(ctxt);
        ctxt->prepare();
        Tuple* new_argvec = Tuple::create(2, INVALID);
        new_argvec->elem(0) = ctxt->trgt;
        new_argvec->elem(1) = ctxt;
        Ctxt* new_ctxt = Ctxt::create(oprnMissingMethod, new_argvec);
        new_ctxt->monitor = vm->systemMonitor;
        return oprnMissingMethod->dispatch(new_ctxt);
    }

    return BASE(fn)->invoke(ctxt);
}


UNIMPLEMENTED(pOb, Ob, nextMsg, (MboxOb*, pOb));
UNIMPLEMENTED(pOb, Ob, receiveMsg, (MboxOb*, Ctxt*));


pOb Ob::receive(Ctxt* ctxt) {
    return BASE(ctxt->arg(0))->lookupAndInvoke(ctxt);
}


bool Ob::accepts(Ctxt*) { return false; }


bool Ob::matches(Ctxt*) { return false; }


pOb Ob::updateNoArgs() { return self(); }


pOb Ob::update(bool enabled_set_provided, Ctxt* ctxt) {
    int key_start = enabled_set_provided ? 1 : 0;
    pOb me = self();
    pOb rslt = me;

    if (ctxt->nargs > key_start) {
        /*
         * We use the (somewhat) funny loop termination condition to make
         * sure that ill-formed argument lists (those that don't have
         * matching pairs of offsets and values) don't cause a bomb.
         */
        for (int i = key_start; i < ctxt->nargs - 1 && rslt == me; i += 2) {
            rslt = BASE(meta())->set(me, ctxt->arg(i), ctxt->arg(i + 1), ctxt);
        }
    }

    return rslt;
}


pOb Ob::updateByLoc(bool enabled_set_provided, Ctxt* ctxt) {
    extern Prim* actorUpdateBang;
    int key_start = enabled_set_provided ? 1 : 0;

    if (ctxt->nargs > key_start) {
        /*
         * We use the (somewhat) funny loop termination condition to make
         * sure that ill-formed argument lists (those that don't have
         * matching pairs of offsets and values) don't cause a bomb.
         */
        for (int i = key_start; i < ctxt->nargs - 1; i += 2) {
            if (!IS(OTlocation, ctxt->arg(i))) {
                return actorUpdateBang->runtimeError(ctxt,
                                                     "bad location descriptor");
            }

            Location loc;
            loc.atom = ctxt->arg(i);
            setValWrt(loc, this, ctxt->arg(i + 1));
        }
    }

    return self();
}


pOb Ob::becomeNew(pOb, Ctxt* ctxt) {
    return runtimeError(ctxt, "cannot become a new object");
}

convertArgReturnPair Ob::convertActualArg(Ctxt*, Ob* actual) {
    cnvArgRetPair.val = (uint32_t)actual;
    cnvArgRetPair.failp = 0;
    return cnvArgRetPair;
}

Ob* Ob::convertActualRslt(Ctxt*, uint32_t rslt) { return ((Ob*)rslt); }

pOb Ob::isNullP() { return RBLFALSE; }


/* Meta actions */

/*
 *  These are the actions performed by an object acting in the role
 *  of another object's meta.  The object on whose behalf the work is
 *  being done is provided as the first parameter.
 */


UNIMPLEMENTED(Tuple*, Ob, keys, (pOb));
UNIMPLEMENTED(Tuple*, Ob, locs, (pOb));
UNIMPLEMENTED(Tuple*, Ob, locContour, (pOb));
UNIMPLEMENTED(Tuple*, Ob, contour, (pOb));
UNIMPLEMENTED(pOb, Ob, get, (pOb, pOb, Ctxt*));
UNIMPLEMENTED(pOb, Ob, add, (pOb, pOb, pOb, Ctxt*));
UNIMPLEMENTED(pOb, Ob, set, (pOb, pOb, pOb, Ctxt*));
UNIMPLEMENTED_VOID(Ob, addRef, ());
UNIMPLEMENTED_VOID(Ob, deleteRef, ());
UNIMPLEMENTED(pOb, Ob, lookupOBO, (pOb, pOb, Ctxt*));

Location Ob::keyLoc(pOb, pOb) {
    NI("Ob::keyLoc");
    return LocLimbo;
}


/* Table actions */

UNIMPLEMENTED(Tuple*, Ob, dumpKeys, ());
UNIMPLEMENTED(Tuple*, Ob, dumpPairs, ());
UNIMPLEMENTED(int, Ob, nPairs, ());
UNIMPLEMENTED(pOb, Ob, getKey, (pOb));
UNIMPLEMENTED(pOb, Ob, addKey, (pOb, pOb));


/* Compiler interface actions */


Pattern* Ob::makePattern() { return ConstPattern::create(self()); }


Template* Ob::makeTemplate() { return (Template*)INVALID; }


AttrNode* Ob::makeAttrNode(bool valueCtxt) {
    return ConstNode::create(self(), valueCtxt);
}


pOb Ob::unquote() { return self(); }


Code* Ob::compileWrt(pOb env, pOb info) {
    PROTECT(env);
    pOb me = self();
    CompilationUnit* cu = CompilationUnit::create(me, info, me);
    cu->atTopLevel();

    Code * cp = cu->compileExpr(env, TopEnv);
    return cp;
}


Location Ob::lex(pOb key, int level) {
    Location loc = BASE(meta())->keyLoc(key, this);
    if (loc != LocLimbo) {
        /*
         * keyLoc will return a LexVar with a level of 0.  We need to add
         * the current nesting level to that value to return a correct
         * location to the requestor.
         */
        assert(GET_GENERIC_TYPE(loc) == LT_LexVariable);
        return level == 0 ? loc
                          : LexVar(GET_LEXVAR_LEVEL(loc) + level,
                                   GET_LEXVAR_OFFSET(loc), GET_LEXVAR_IND(loc));
    } else {
        return BASE(parent())->lex(key, level + 1);
    }
}


extern pOb NILmeta;


pOb Ob::extendWith(pOb keymeta) {
    pOb me = self();
    return (keymeta == NILmeta ? me : StdExtension::create(keymeta, me, 0));
}


pOb Ob::extendWith(pOb keymeta, Tuple* argvec) {
    pOb me = self();
    return (keymeta == NILmeta ? me : argvec->becomeExtension(keymeta, me));
}


pOb Ob::runtimeError(Ctxt* ctxt, const char* msg, ...) {
    PROTECT_THIS(Ob);
    PROTECT(ctxt);

    Tuple* trimmed_argvec = ctxt->argvec->makeSlice(0, ctxt->nargs);
    ASSIGN(ctxt, argvec, trimmed_argvec);

    /*
     * We use our own private buffer here (rather than Ob::stringbuf)
     * because of the likelihood that one of the arguments to the
     * procedure was actually laid down in Ob::stringbuf.
     */

    char buf[1024];
    va_list args;
    va_start(args, msg);
    vsprintf(buf, msg, args);
    va_end(args);

    pOb str = RBLstring::create(buf);
    PROTECT(str);
    Tuple* new_argvec = Tuple::create(3, INVALID);
    PROTECT(new_argvec);
    new_argvec->elem(0) = SELF->self();
    new_argvec->elem(1) = str;
    new_argvec->elem(2) = ctxt;

    Ctxt* new_ctxt = Ctxt::create(oprnRuntimeError, new_argvec);
    new_ctxt->monitor = vm->systemMonitor;

    oprnRuntimeError->dispatch(new_ctxt);
    return DEADTHREAD;
}


pOb Ob::mismatch(Ctxt* ctxt, int argnum, char* type_name) {
    return runtimeError(ctxt, "%d%s argument is not %s %s", argnum + 1,
                        numberSuffix(argnum + 1), properPrep(type_name),
                        type_name);
}


pOb Ob::mismatch(Ctxt* ctxt, int minargs, int maxargs) {
    if (maxargs == MaxArgs) {
        return runtimeError(ctxt, "expected %d or more arguments", minargs);
    } else if (minargs == maxargs) {
        if (minargs == 1) {
            return runtimeError(ctxt, "expected 1 argument");
        } else {
            return runtimeError(ctxt, "expected %d arguments", minargs);
        }
    } else {
        return runtimeError(ctxt, "expected between %d and %d arguments",
                            minargs, maxargs);
    }
}


pOb MboxOb::mailbox() { return mbox; }


pOb MboxOb::setMailbox(pOb new_mbox) {
    ASSIGN(this, mbox, new_mbox);
    return this;
}


pOb MboxOb::receive(Ctxt* ctxt) {
    ASSIGN(ctxt, rcvr, this);
    return mbox->receiveMsg(this, ctxt);
}


void MboxOb::schedule(Ctxt* strand) { vm->scheduleStrand(strand); }


DEF("compile", obCompile, 1, 3) {
    pOb proto = TopEnv;
    pOb info = NIV;

    switch (NARGS) {
    case 3:
        info = ARG(2);
    case 2:
        proto = ARG(1);
    }

    pOb result = BASE(ARG(0))->compileWrt(proto, info);

    if (result == INVALID) {
        return PRIM_ERROR("compilation aborted");
    } else {
        return result;
    }
}


DEF("run", obRun, 1, 2) {
    Monitor* mon = __CTXT__->monitor;
    PROTECT(mon);

    CHECK(0, Code, code);
    if (NARGS == 2) {
        CHECK(1, Monitor, m);
        mon = m;
    }

    Ctxt* newCtxt = Ctxt::create(code, NIL, __CTXT__, 0);
    newCtxt->monitor = mon;
    newCtxt->scheduleStrand();

    return vm->upcall(newCtxt);
}


DEF("run-with-env", obRunWithEnv, 2, 3) {
    Monitor* mon = __CTXT__->monitor;
    PROTECT(mon);

    CHECK(0, Code, code);
    if (NARGS == 3) {
        CHECK(2, Monitor, m);
        mon = m;
    }

    Ctxt* newCtxt = Ctxt::create(code, NIL, __CTXT__, 0);
    newCtxt->monitor = mon;
    newCtxt->env = ARG(1);
    newCtxt->scheduleStrand();

    return vm->upcall(newCtxt);
}


DEF("suicide", obSuicide, 0, 0) { return INVALID; }


DEF("identity", obId, 1, 1) { return ARG(0); }


DEF("niv?", obNivQ, 1, 1) { return RBLBOOL(ARG(0) == NIV); }


DEF("absent?", obAbsentQ, 1, MaxArgs) {
    for (int i = 0; i < NARGS; i++) {
        if (ARG(i) != ABSENT) {
            return RBLFALSE;
        }
    }

    return RBLTRUE;
}


DEF("absent!?", obNotAbsentQ, 1, MaxArgs) {
    for (int i = 0; i < NARGS; i++) {
        if (ARG(i) == ABSENT) {
            return RBLFALSE;
        }
    }

    return RBLTRUE;
}


DEF("same?", obSameQ, 2, 2) { return RBLBOOL(ARG(0) == ARG(1)); }
DEF("same!?", obSameNQ, 2, 2) { return RBLBOOL(ARG(0) != ARG(1)); }
DEF("self", obSelf, 0, 0) { return __CTXT__->self2; }
DEF("home", obHome, 0, 0) { return __CTXT__; }
DEF("clone", obClone, 1, 1) { return BASE(ARG(0))->clone(); }

DEF("clone-to", obCloneTo, 2, 3) {
    pOb base = BASE(ARG(0));
    pOb new_meta = NARGS == 2 ? base->meta() : ARG(1);
    pOb new_parent = ARG(NARGS - 1);
    return base->cloneTo(new_meta, new_parent);
}


DEF("become!", obBecomeNew, 1, 1) {
    return BASE(__CTXT__->self2)->becomeNew(ARG(0), __CTXT__);
}


DEF("add", objectAdd, 2, 3) {
    PROTECT(__CTXT__);
    pOb env = NARGS == 3 ? ARG(2) : GlobalEnv;
    pOb result = BASE(BASE(env)->meta())->add(env, ARG(0), ARG(1), __CTXT__);
    return (result == env ? ARG(0) : result);
}


DEF("set", objectSet, 2, 3) {
    pOb env = __CTXT__->self();
    if (NARGS == 3) {
        env = ARG(2);
    }

    return BASE(BASE(env)->meta())->set(env, ARG(0), ARG(1), __CTXT__);
}


DEF("lookup", objectLookup, 1, 2) {
    pOb env = NARGS == 2 ? ARG(1) : GlobalEnv;
    return BASE(env)->lookup(ARG(0), __CTXT__);
}


DEF("meta", objectMeta, 1, 1) { return BASE(ARG(0))->meta(); }


DEF("meta:", objectSetMeta, 2, 2) {
    ASSIGN(BASE(ARG(0)), meta(), ARG(1));
    return ARG(0);
}


DEF("parent", objectParent, 1, 1) { return BASE(ARG(0))->parent(); }


DEF("parent:", objectSetParent, 2, 2) {
    ASSIGN(BASE(ARG(0)), parent(), ARG(1));
    return ARG(0);
}


DEF("mbox", objectMbox, 1, 1) { return BASE(ARG(0))->mailbox(); }


DEF("mbox:", objectSetMbox, 2, 2) { return BASE(ARG(0))->setMailbox(ARG(1)); }


DEF("prim-size", objectIndexedSize, 1, 1) {
    return BASE(ARG(0))->indexedSize();
}


DEF("prim-nth", objectNth, 2, 2) {
    CHECK_FIXNUM(1, n);
    pOb base = BASE(ARG(0));
    if (0 <= n && n < FIXVAL(base->indexedSize())) {
        return base->nth(n);
    } else {
        return PRIM_ERROR("subscript error");
    }
}


DEF("prim-set-nth", objectSetNth, 3, 3) {
    CHECK_FIXNUM(1, n);
    pOb base = BASE(ARG(0));
    if (0 <= n && n < FIXVAL(base->indexedSize())) {
        return base->setNth(n, ARG(2));
    } else {
        return PRIM_ERROR("subscript error");
    }
}


DEF("prim-sub-object", objectSubObject, 3, 3) {
    CHECK_FIXNUM(1, i);
    CHECK_FIXNUM(2, n);
    pOb base = BASE(ARG(0));
    if (0 <= i && 0 <= n && (i + n) <= FIXVAL(base->indexedSize())) {
        return base->subObject(i, n);
    } else {
        return PRIM_ERROR("subscript error");
    }
}


DEF("object->string", objectToString, 1, MaxArgs) {
    PROTECT(__CTXT__);
    int nChars = 1;
    int n = NARGS;
    int i = 0;
    for (i = 0; i < n; i++) {
        nChars += strlen(BASE(ARG(i))->asCstring());
    }

    RBLstring* str = RBLstring::create(nChars);
    char* buffer = (char*)&str->byte(0);
    buffer[0] = '\0';
    for (i = 0; i < n; i++) {
        strcat(buffer, BASE(ARG(i))->asCstring());
    }

    return str;
}


DEF("object->symbol", objectToSymbol, 1, MaxArgs) {
    int nChars = 1;
    int n = NARGS;
    int i = 0;
    for (i = 0; i < n; i++) {
        nChars += strlen(BASE(ARG(i))->asCstring());
    }

    static char* buffer = 0;
    static int bufsize = 0;
    if (nChars > bufsize) {
        if (buffer) {
            delete buffer;
        }

        bufsize = nChars;
        buffer = new char[bufsize];
    }

    buffer[0] = '\0';
    for (i = 0; i < n; i++) {
        strcat(buffer, BASE(ARG(i))->asCstring());
    }

    return SYMBOL(buffer);
}


DEF("get-field", objectGetField, 5, 5) {
    CHECK_FIXNUM(1, start);
    CHECK_FIXNUM(2, span);
    CHECK_NOVAR(3, RblBool);
    CHECK_NOVAR(4, RblBool);
    pOb rslt = BASE(ARG(0))->getField(ARG(3) == RBLTRUE, 0, start, span,
                                      ARG(4) == RBLTRUE);

    if (INVALID == rslt) {
        return PRIM_ERROR("invalid bit range");
    } else {
        return rslt;
    }
}


DEF("set-field", objectSetField, 5, 5) {
    CHECK_FIXNUM(1, start);
    CHECK_FIXNUM(2, span);
    CHECK(3, RblBool, indirect);
    CHECK_FIXNUM(4, bits);
    pOb rslt = BASE(ARG(0))->setField(BOOLVAL(indirect), 0, start, span,
                                      (uint32_t)bits);

    if (INVALID == rslt) {
        return PRIM_ERROR("invalid bit range");
    } else {
        return rslt;
    }
}


DEF("classname", obClassname, 1, 1) {
    CHECK_FIXNUM(0, index);
    if (0 <= index && index < Base::nClasses) {
        return SYMBOL(Base::classNames[index]);
    } else {
        return PRIM_ERROR("bad class index");
    }
}


DEF("gc", sysGC, 0, 0) {
    heap->gc();
    return NIV;
}


DEF("scavenge", sysScavenge, 0, 0) {
    heap->scavenge();
    return NIV;
}

DEF("sleep", sysSleep, 0, 0) { return SLEEP; }
DEF("sys-reset", sysReset, 0, 0) { return NIV; }

DEF("version", sysVersion, 0, 0) {
    char buf[256];
    sprintf(buf, "Version %d.%d.%d", ROSETTE_VERSION_MAJOR,
            ROSETTE_VERSION_MINOR, ROSETTE_VERSION_PATCH);
    return RBLstring::create(buf);
}


DEF("cwd", sysCwd, 0, 0) {
    char buf[MAXPATHLEN];
    if (getcwd(buf)) {
        return RBLstring::create(buf);
    } else {
        return PRIM_ERROR(buf);
    }
}

// TODO: Swap with something crypotgraphically secure and uniform in length
DEF("random-number-init", randomNumberInit, 0, 0) {
    srand(time(NULL));
    return NIV;
}

DEF("random-number", randomNumber, 0, 0) {
    int random_number = std::rand();
    return FIXNUM(random_number);
}

DEF("now", now, 0, 0) {
    // TODO: Return as epoch time
    return FIXNUM((int)time(NULL));
}

DEF_OPRN(Sync, "expand", oprnExpand, obId);


MODULE_INIT(Ob) {
    /*
     * When Rosette comes up, it expects to be able to find a definition
     * of a zero-argument function "reset", which it will invoke to start
     * the world.  We give it an initial binding here, just to make sure
     * that it is bound to something.  Other boot code will presumably
     * override this definition with more interesting code.
     */
    Define("reset", sysReset);
}


void Define(char* name, pOb val, pOb env) {
    BASE(env->meta())->add(env, SYMBOL(name), val, (Ctxt*)NIV);
}


void Define(pOb key, pOb val, pOb env) {
    BASE(env->meta())->add(env, key, val, (Ctxt*)NIV);
}
