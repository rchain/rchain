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
 */

#ifdef __GNUG__
#pragma implementation
#endif

#include "rosette.h"

#include <stdarg.h>

#include <unistd.h>
#include <sys/param.h>
#include <sys/file.h>
#include <sys/types.h>
#include <fcntl.h>

#ifndef __GNUG__
#ifndef SYSV4
#include <osfcn.h>
#endif
#endif

#ifdef SYSV4
#include <stropts.h>
#endif

#include "RblAtom.h"
#include "CommandLine.h"
#include "Dynload.h"
#include "Expr.h"
#include "ForeignFun.h"
#include "Interrupt.h"
#include "Mbox.h"
#include "Meta.h"
#include "MI.h"
#include "Ob.h"
#include "Operation.h"
#include "Pattern.h"
#include "Prim.h"
#include "Reader.h"
#include "RBLstream.h"
#include "RBLstring.h"
#include "Table.h"
#include "Tuple.h"
/* #include "Unix.h" */
#include "Vm.h"
#include "Cstruct.h"

#include "BuiltinClass.h"
#include "ModuleInit.h"


extern "C" {
int access(const char*, int);
}

convertArgReturnPair cnvArgRetPair;

pMeta TopMeta = (pMeta)INVALID;
pOb TopEnv = (pOb)INVALID;
TblObject* TopSBO = (TblObject*)INVALID;
TblObject* GlobalEnv = (TblObject*)INVALID;
pOb RBLEOF = INVALID;
pOb INCOMPLETE_IO = INVALID;
pOb READ_ERROR = INVALID;
pOb ABSENT = INVALID;
pOb obSBO = INVALID;
RBLstring* protoString = (RBLstring*)INVALID;
Tuple* protoTuple = (Tuple*)INVALID;

GenericDescriptor* obGenericDescriptor = (GenericDescriptor*)INVALID;
NullDescriptor* obNullDescriptor = (NullDescriptor*)INVALID;
AtomicDescriptor* obAtomicDescriptor = (AtomicDescriptor*)INVALID;
CStructure* obCStructure = (CStructure*)INVALID;
CArray* obCArray = (CArray*)INVALID;
CharArray* obCharArray = (CharArray*)INVALID;
CharArray0* obCharArray0 = (CharArray0*)INVALID;
CRef* obCRef = (CRef*)INVALID;
CharRef* obCharRef = (CharRef*)INVALID;
CRef0* obCRef0 = (CRef0*)INVALID;
CharRef0* obCharRef0 = (CharRef0*)INVALID;
AtomicDescriptor* obChar = (AtomicDescriptor*)INVALID;
CUnion* obCUnion = (CUnion*)INVALID;


int BuiltinClass::nClasses = 0;
BuiltinClass* BuiltinClass::root = 0;
uint32_t* BuiltinClass::counts = 0;
char** BuiltinClass::names = 0;


BuiltinClass::BuiltinClass(char* nm, pMeta* cmeta, pSBO* csbo, FIELD_FN ffn)
    : index(nClasses++),
      name(nm),
      clientMeta(cmeta),
      clientSBO(csbo),
      link(root),
      fieldfn(ffn) {
    root = this;
}

uint32_t* Base::obCounts = 0;
char** Base::classNames = {0};
void BuiltinClass::allocBuiltinClasses() {
    counts = new uint32_t[nClasses];
    names = new char*[nClasses];
    for (int i = 0; i < nClasses; i++) {
        counts[i] = 0;
        names[i] = "unknown";
    }

    Base::nClasses = nClasses;
    Base::classNames = names;
    Base::obCounts = counts;

    for (BuiltinClass* p = BuiltinClass::root; p; p = p->link)
        p->alloc();
}


void BuiltinClass::initBuiltinClasses() {
    assert(NIL != INVALID);
    assert(emptyMbox != INVALID);
    assert(lockedMbox != INVALID);

    for (BuiltinClass* p = BuiltinClass::root; p; p = p->link)
        p->init();
}


void BuiltinClass::enterBuiltinClasses() {
    for (BuiltinClass* p = BuiltinClass::root; p; p = p->link)
        p->enter();
}


void BuiltinClass::alloc() {
    BuiltinClass::names[index] = name;

    /*
     * Anything that has an extension vector is deemed to be extensible.
     * At boot time, the following classes are the only ones that
     * qualify; the others, (e.g., RequestExpr, or whatever) have fixed
     * formats and cannot have slots added to them.
     */

    /*
     * This is a stupid way to figure this out, and ought to be replaced
     * with a more automatic method.
     */

    const bool extensible = (clientMeta == &CLASS_META(Actor) ||
                             clientMeta == &CLASS_META(TblObject) ||
                             clientMeta == &CLASS_META(StdMeta) ||
                             clientMeta == &CLASS_META(IndexedMeta) ||
                             clientMeta == &CLASS_META(StdOprn) ||
                             clientMeta == &CLASS_META(MIActor) ||
                             clientMeta == &CLASS_META(Istream));

    /*
     * Pass a NULL map to StdMeta::create in order to postpone allocating
     * the RblTables for the map components of the StdMetas.  These
     * newly-created metas will also have bogus mboxes that need to be
     * properly initialized later.
     */

    if (*clientMeta == INVALID)
        *clientMeta = (pMeta)heap->tenure(
            StdMeta::create(NULL, MAX_FIXNUM, RBLBOOL(extensible)));

    /*
     * Pass INVALIDs for the meta, parents, and extensions of the sbo
     * objects that we are about to allocate, in order that we avoid
     * prematurely allocating other objects that can't yet be properly
     * initialized (because the meta and sbo framework isn't
     * initialized).
     */

    if (*clientSBO == INVALID)
        *clientSBO =
            (pSBO)heap->tenure(Actor::create(INVALID, INVALID, (pExt)INVALID));
}


void BuiltinClass::init() {
    KONST pMeta m = *clientMeta;
    KONST pSBO sbo = *clientSBO;

    /*
     * Because we don't know the relative order of allocation of the
     * CLASS_META and CLASS_SBO objects, some the extension objects for
     * the metas were undoubtedly allocated while
     * CLASS_META(StdExtension) and CLASS_SBO(StdExtension) were still
     * invalid.  We unconditionally patch the necessary fields here to
     * correct that problem.
     */

    /*
     * Notice also that we forego the use of the ASSIGN macro within this
     * routine, since we know that all values being assigned to the
     * various fields have already been tenured into old space.
     */

    m->extension->meta() = CLASS_META(StdExtension);
    m->extension->parent() = CLASS_SBO(StdExtension);

    /*
     * By now, all of the meta and sbo objects have been allocated, but
     * they haven't necessarily been initialized.  Thus, this routine can
     * use any routines that simply refer to CLASS_META and CLASS_SBO
     * objects (such as constructors, which just want to fill in the meta
     * and sbo fields of the objects), but it needs to be careful about
     * using routines that rely on completely initialized objects.
     */

    m->meta() = CLASS_META(StdMeta);
    m->parent() = CLASS_SBO(StdMeta);
    m->mbox = emptyMbox;

    /*
     * Fill in the meta map that describes the slots of objects that are
     * described by the meta we are initializing.  The routines
     * allocateMap and addKey are safe:  they may allocate new objects
     * (allocateMap will allocate a RblTable (which will cause the
     * recursive allocation of a Tuple), and addKey may allocate new
     * Tuples as keys are added), but they don't do anything else tricky.
     *
     */

    m->allocateMap();
    (*fieldfn)(this);

    /*
     * The SBOs were allocated without valid metas, parents, or
     * extensions, so all of those components must now be allocated and
     * initialized.
     */

    pMeta sbo_meta = StdMeta::create(NIL, FIXNUM(1));
    ASSIGN(sbo, meta(), sbo_meta);
    StdExtension* ext = StdExtension::create(0);
    ASSIGN(sbo, extension, ext);
    sbo->mbox = emptyMbox;
    sbo->parent() = TopSBO;
    META(sbo)->parent() = CLASS_SBO(StdMeta);
}


void BuiltinClass::obfield(const char* name, int offset, int indirect) {
    pMeta m = *clientMeta;
    m->map()->addKey(SYMBOL(name), LexVar(0, offset, indirect).atom);
}


void BuiltinClass::addrfield(const char* name, int offset, int indirect) {
    pMeta m = *clientMeta;
    m->map()->addKey(SYMBOL(name), AddrVar(0, offset, indirect).atom);
}


void BuiltinClass::bitfield(const char* name, int offset, int extent,
                            int indirect) {
    pMeta m = *clientMeta;
    m->map()->addKey(SYMBOL(name), BitField(0, offset, extent, indirect).atom);
}


void BuiltinClass::enter() {
    /*
     * Add an entry in the global environment for this sbo.
     */
    KONST pSBO sbo = *clientSBO;
    char buf[128];
    sprintf(buf, "%s-SBO", name);
    Define(buf, (pOb)sbo);
}


class RBLtopenv : public Ob {
   protected:
    RBLtopenv(Ob* meta, Ob* parent) : Ob(sizeof(RBLtopenv), meta, parent) {}

   public:
    static RBLtopenv* create();

    char* typestring() { return "TopEnv"; }
    const char* asCstring() { return "#top"; }
    Ob* invoke(Ctxt*) { return NIV; }
    Ob* lookup(Ob*, Ctxt*) { return ABSENT; }
    Location lex(Ob*, int) { return LocLimbo; }
};


RBLtopenv* RBLtopenv::create() {
    /*
     * This constructor expects to be called *after* sufficient
     * initialization has been performed that it can safely allocate a
     * new StdMeta object.
     */

    pMeta m = StdMeta::create(NIL, FIXNUM(1), RBLFALSE);
    void* loc = PALLOC1(sizeof(RBLtopenv), m);
    return NEW(loc) RBLtopenv(m, INVALID);
}


void InitBuiltinObs() {
    BuiltinClass::allocBuiltinClasses();

    /*
     * It would be nice if these variables could be initialized through
     * the usual module initialization mechanism, but they are used
     * pervasively throughout the allocation and initialization code, and
     * it is easier to allocate them specially than to go back later and
     * try to patch up everything that depends on them.
     */

    NIL = (Tuple*)heap->tenure(Tuple::create());
    NILexpr = (TupleExpr*)heap->tenure(TupleExpr::create());
    NILexpr->rest = NILexpr;
    emptyMbox = heap->tenure(EmptyMbox::create());
    lockedMbox = heap->tenure(LockedMbox::create());
    RBLEOF = heap->tenure(Actor::create());
    INCOMPLETE_IO = heap->tenure(Actor::create());
    READ_ERROR = heap->tenure(Actor::create());
    ABSENT = heap->tenure(Actor::create());

    TopMeta = (pMeta)heap->tenure(StdMeta::create(NIL, MAX_FIXNUM, RBLTRUE));
    TopMeta->meta() = TopMeta;
    TopMeta->map()->addKey(SYMBOL("map"),
                           LexVar(0, STDMETA_MAP_SLOT, INDIRECT).atom);
    TopMeta->map()->addKey(SYMBOL("ref-count"),
                           LexVar(0, STDMETA_REFCOUNT_SLOT, INDIRECT).atom);
    TopMeta->map()->addKey(SYMBOL("extensible"),
                           LexVar(0, STDMETA_EXTENSIBLE_SLOT, INDIRECT).atom);

    CLASS_META(StdMeta)->meta() = TopMeta;
    CLASS_META(StdMeta)->parent() = TopMeta;
    CLASS_META(IndexedMeta)->meta() = TopMeta;
    CLASS_META(IndexedMeta)->parent() = TopMeta;

    TopEnv = heap->tenure(RBLtopenv::create());
    TopEnv->parent() = TopEnv;
    TopSBO = (TblObject*)heap->tenure(TblObject::create());
    GlobalEnv = (TblObject*)heap->tenure(TblObject::create());

    TopSBO->parent() = TopEnv;
    GlobalEnv->parent() = TopSBO;
    RBLEOF->parent() = TopSBO;
    INCOMPLETE_IO->parent() = TopSBO;
    READ_ERROR->parent() = TopSBO;
    ABSENT->parent() = TopSBO;

    BuiltinClass::initBuiltinClasses();
    BuiltinClass::enterBuiltinClasses();

    Define("GlobalEnv", GlobalEnv);
    Define("Top-SBO", TopSBO);
    obSBO = heap->tenure(Actor::create());
    Define("Tuple", protoTuple = (Tuple*)heap->tenure(Tuple::create(1, NIV)));
    Define("String", protoString = (RBLstring*)heap->tenure(
                         RBLstring::create("a string")));
    Define("SBO", obSBO);

    CLASS_META(Tuple)->becomeIndexed(SLOT_NUM(Tuple, elem(0)));
    CLASS_META(TupleExpr)->becomeIndexed(SLOT_NUM(TupleExpr, elem(0)));
    CLASS_META(StdExtension)->becomeIndexed(SLOT_NUM(StdExtension, slot(0)));

    BuiltinPrim::initBuiltinPrims();
    BuiltinOprn::initBuiltinOprns();

    Module::initModules();
}


static void get_path_prefix(char* path, char* dir) {
    char* p = strrchr(path, '/');
    if (p) {
        int n = p - path;
        strncpy(dir, path, n);
        dir[n] = 0;
    }
    else
        strcpy(dir, ".");
}


const char* StandardExtensions[] = {".rbl", 0};


static FILE* FindBootFile() {
    char path[MAXPATHLEN];
    char* RosetteLib = getenv("ROSETTE_LIB");

    if (strcmp(BootFile, "") == 0) {
        if (RosetteLib)
            strcpy(BootDirectory, RosetteLib);
        strcpy(path, BootDirectory);
        strcat(path, "/");
        strcat(path, "boot.rbl");
    }
    else {
        get_path_prefix(BootFile, BootDirectory);
        strcpy(path, BootFile);
    }

    int baselen = strlen(path);
    const char** suffixp = StandardExtensions;

    for (; access(path, R_OK) && *suffixp; suffixp++) {
        path[baselen] = '\0';
        strcat(path, *suffixp);
    }

    if (!(*suffixp))
        suicide("can't find boot file '%s'", path);

    Tuple* loadPaths = Tuple::create(1, RBLstring::create(BootDirectory));
    if (RosetteLib && strcmp(RosetteLib, BootDirectory)) {
        PROTECT(loadPaths);
        RBLstring* temp = RBLstring::create(RosetteLib);
        loadPaths = rcons(loadPaths, temp);
    }
    Define("load-paths", loadPaths);

    return fopen(path, "r");
}


static Tuple* GetArgv(int argc, char** argv) {
    Tuple* RosetteArgv = argc == 0 ? NIL : Tuple::create(argc, NIV);
    PROTECT(RosetteArgv);
    for (int i = 0; i < argc; i++) {
        RBLstring* arg = RBLstring::create(argv[i]);
        ASSIGN(RosetteArgv, elem(i), arg);
    }

    return RosetteArgv;
}

static RblTable* GetEnvp(char** envp) {
    int n = 0;
    for (; envp[n] != 0; n++) {
    }

    RblTable* RosetteEnvp = RblTable::create();
    PROTECT(RosetteEnvp);
    for (int i = 0; i < n; i++) {
        char* cstr = envp[i];
        for (; *cstr != 0; cstr++)
            if (*cstr == '=') {
                *cstr = '\x00';
                Ob* k = SYMBOL(envp[i]);
                PROTECT(k);
                RBLstring* v = RBLstring::create(cstr + 1);
                *cstr = '=';
                RosetteEnvp->addKey(k, v);
                break;
            }
    }

    return RosetteEnvp;
}

static void LoadBootFiles() {
    Reader* reader = Reader::create(FindBootFile());
    PROTECT(reader);

    Ob* expr = INVALID;
    while ((expr = reader->readExpr()) != RBLEOF)
        vm->load(expr);
}


#if defined(MALLOC_DEBUGGING)
extern "C" {
int malloc_debug(int);
}
#endif

extern int restore(const char*, char*);
#if defined(DYNAMIC_LOADING)
extern DynamicLoader* loader;
#endif


int InBigBang = 0;

int BigBang(int argc, char** argv, char** envp) {
    const char* cmdName = argv[0];

    argc = ParseCommandLine(argc, argv);

    InBigBang = TRUE;

    setsid();

    if (RestoringImage) {
/*
 * This stuff must be (re-)initialized *after* the restore, since
 * restore will return them to their values at the time that the
 * image file was created.  This also assumes that the strings in
 * argv actually reside on the stack and are not clobbered by the
 * restore.
 */

#if defined(DYNAMIC_LOADING)
        delete loader;
#endif
        Define("argv", GetArgv(argc, argv));
        Define("envp", GetEnvp(envp));
        vm->resetSignals();

        /*
         * This bit of stream manipulation is necessary to reset the
         * stdin and stdout streams to the values appropriate for this
         * particular run, rather than the ones inherited from the image
         * that was dumped.  In particular, this will properly hook up
         * interactive stdin and stdout for an image that was generated
         * via redirected stdin and stdout.
         */

        *stdin = *fdopen(0, "r");
        *stdout = *fdopen(1, "w");
        *stderr = *fdopen(2, "w");
    }

/*
 * Always reset the malloc_verify stuff to current settings,
 * regardless of whether we are restoring an image.  This permits us
 * maximum checking while building an image, but allows the built
 * image to run with no checking unless specifically overridden with
 * a command-line option.
 */

#if defined(MALLOC_DEBUGGING)
    malloc_debug(ParanoidAboutGC);
#endif

/*
 * Always rebuild the loader so that it gets the current command
 * path, just in case the image has been moved from its birthplace in
 * the file system.
 */

#if defined(DYNAMIC_LOADING)
    loader = new DynamicLoader(cmdName);
#endif

    if (!RestoringImage) {
        heap = new Heap(InfantSpaceSize, SurvivorSpaceSize, OldSpaceChunkSize);

        InitBuiltinObs();

        vm = new VirtualMachine;

        Define("argv", GetArgv(argc, argv));
        Define("envp", GetEnvp(envp));
        LoadBootFiles();

        heap->tenureEverything();
    }

    handleInterrupts();
    InBigBang = FALSE;

    return RestoringImage;
}


void BigCrunch() {
    delete vm;
    delete heap;
#if defined(DYNAMIC_LOADING)
    delete loader;
#endif
}


MODULE_INIT(BigBang) {
    Define("eof", RBLEOF);
    Define("incomplete-io", INCOMPLETE_IO);
    Define("read-error", READ_ERROR);
    Define("min-fixnum", MIN_FIXNUM);
    Define("max-fixnum", MAX_FIXNUM);
}


int asyncHelper(int fd, int desiredState) {
    int flags;
    int result;

#ifdef HANDLE_POLL_WITH_IO
    SET_SIGNAL_POLL_DESIRED(result);
#endif
    SET_SIGNAL_IO_DESIRED(result);

#ifndef HPUX
    if ((flags = fcntl(fd, F_GETFL, 0)) == -1)
        return -1;

#ifdef FCNTL_NONBLOCK
    if (desiredState)
        flags |= FCNTL_NONBLOCK;
    else
        flags &= ~(FCNTL_NONBLOCK);
#else
    DO_BLOCKING
#endif

    result = fcntl(fd, F_SETFL, flags);
#else
    flags = (desiredState ? 1 : 0);
    result = ioctl(fd, FIOSNBIO, &flags);

    if (result == 0) {
        flags = (desiredState ? 1 : 0);
        result = ioctl(fd, FIOASYNC, &flags);
    }
#endif

    if (result == -1) {
        printf("could not set non-block/async on: %d\n", fd);
#ifdef ALLOW_ASYNC_ERRORS
        return 0;
#else
        return -1;
#endif
    }
    else
        return 0;
}


DEF("async", asyncify, 1, 2) {
    FILE* f = stdin;

    CHECK(NARGS - 1, RblBool, b);
    int desiredState = BOOLVAL(ARG(NARGS - 1));
    if (NARGS == 2) {
        CHECK(0, Istream, stream);
        f = stream->reader->file;
    }

    if (asyncHelper(fileno(f), desiredState))
        return RBLstring::create((char*)sys_errmsg());
    else
        return NIV;
}


DEF("fdAsync", asyncify_fd, 2, 2) {
    CHECK_FIXNUM(0, fd);
    CHECK(1, RblBool, b);
    int desiredState = BOOLVAL(b);

    if (asyncHelper(fd, desiredState))
        return RBLstring::create((char*)sys_errmsg());
    else
        return NIV;
}
