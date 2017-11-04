/* Mode: -*- C++ -*- */
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

/*
 * $Header$
 *
 * $Log$
 *
 @EC */

#if !defined(_RBL_Ob_h)
#define _RBL_Ob_h

#ifdef __GNUG__
#pragma interface
#endif

#include "rosette.h"
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#define TWOS_COMPLEMENT

/* #include <generic.h> */

#if !defined(_STRING)  || defined(MIPS_SGI_SYSV)
#if defined(__STDC__) || defined(MIPS_SGI_SYSV)
#define _STRING(name) #name
#else
#define _STRING(name) "name"
#endif
#endif

typedef unsigned char	Byte;
typedef unsigned short	Word16;
typedef unsigned long	Word32;

#if !defined(HAS_BOOL)
typedef int bool;
#endif

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

#ifndef MIPS_SGI_SYSV
#define KONST const
#else 
#define KONST
#endif

#if (defined(__GNUG__) && !defined(GCC27X)) || defined(sun)
#include <values.h>
#define LONG_MAX MAXINT
#define LONG_MIN (~LONG_MAX)
#else
#include <limits.h>
#define BITS(type) (CHAR_BIT*sizeof(type))
#endif


static const int WordSize    = BITS(void*);
static const int TagSize     = 2;
static const int EscTagSize  = 6;

static const unsigned TagMask    = ((unsigned)~0) >> (WordSize-TagSize);
static const unsigned EscTagMask = ((unsigned)~0) >> (WordSize-EscTagSize);

/* to do fix this  wfs*/
#ifdef LITTLE_END
#define BIG_END(x) 
#else
#define BIG_END(x) x
#define LITTLE_END(x)
#endif

#define ESCAPED(n)   (((n)<<TagSize) + OTesc)

/* unused
  #define UNESCAPED(n) (n>>TagSize) */

enum ObTag  { OTptr	= 0,
	      OTsym	= 1,
	      OTfixnum	= 2,
	      OTesc	= 3,

	      /*
	       * The escaped types can't use ESCAPED(0) because of some
	       * encoding and decoding optimizations performed in the
	       * following code.  This shouldn't cramp our style, since
	       * type bits are only needed for atoms; we use the C++
	       * class system to provide an arbitrarily large number of
	       * distinct heap-allocated objects.
	       *
	       * Whenever a new escape type is added, the following steps
	       * MUST be followed:
	       *
	       * 1. Allocate one of the remaining unused OT codes from
	       *    the following list.
	       *
	       * 2. #define a "constructor" for the new type a la the
	       *    already existing ones below.
	       *
	       * 3. Derive a class from Atom (a la RBLbool) in Atom.cc.
	       *    See Atom.cc for examples.
	       *
	       * 4. Declare one instance of the new class (e.g.,
	       *    prototypicalBool) in Atom.cc.  This is the instance
	       *    into which an Ob of this type will be jammed (such as
	       *    on output, for example).
	       *
	       * 5. Add a case to the body of decodeAtom(), which is the
	       *    function that actually does the work when such a cast
	       *    is performed.
	       *
	       * This housekeeping is an unfortunate consequence of C++'s
	       * unwillingness to accommodate short immediate tags on
	       * objects.  Things are much cleaner for the heap-allocated
	       * objects, which fit much more nicely into C++'s model of
	       * long immediate tags.
	       */

	      OTbool         = ESCAPED(1),
	      OTchar         = ESCAPED(2),
	      OTniv          = ESCAPED(3),
	      OTsysval       = ESCAPED(4),
	      OTlocation     = ESCAPED(5),
	      };


/*
 * Pointers are not shifted before being masked (or unmasked).  This
 * means that they had better be zero in their low TagSize bits.
 * Currently, this means that all pointers need to be aligned on longword
 * (4-byte) boundaries.  The stringstore (which produces character
 * strings for symbols) and the heap allocation routines obey this rule.
 */

#define MAKETAGGED(t, p)	((pOb)((unsigned)(p) | (unsigned)(t)))
#define MAKEESCTAGGED(t, p)	((pOb)(((unsigned)(p) << EscTagSize) | (unsigned)(t)))

#define SYMBOL(p)	MAKETAGGED(OTsym, intern(p))
#define FIXNUM(v)	((pOb)(((unsigned)(v) << TagSize) | (unsigned)OTfixnum))

#ifdef TWOS_COMPLEMENT

// This sum macro probably only works on two's complement machines.

#define FIXNUM_SUM(n, i)	((pOb)((int)n + (i<<TagSize)))
#else
#define FIXNUM_SUM(n, i)	FIXNUM(FIXVAL(n) + (i))
#endif

#define MAX_FIXNUM	FIXNUM(LONG_MAX>>TagSize)
#define MIN_FIXNUM	FIXNUM(LONG_MIN>>TagSize)

#define FIXNUM_INC(n)	(n = FIXNUM_SUM(n, 1))
#define FIXNUM_DEC(n)	(n = FIXNUM_SUM(n, -1))

#define RBLBOOL(v)	MAKEESCTAGGED(OTbool, v)
#define RBLCHAR(v)	MAKEESCTAGGED(OTchar, v)


class Actor;
class RblAtom;
class AttrNode;
class BinaryOb;
class BlockExpr;
class BlockNode;
class RblBool;
class ByteVec;
class Char;
class Code;
class CodeBuf;
class CodeVec;
class CompilationUnit;
class ComplexPattern;
class CompoundNode;
class CompoundPattern;
class ConstNode;
class ConstPattern;
class Ctxt;
class EmptyMbox;
class ExpandedLocation;
class Expr;
class Fixnum;
class FixupVec;
class Float;
class ForeignFunction;
class FreeExpr;
class FreeNode;
class GotoExpr;
class GotoNode;
class IdAmperRestPattern;
class IdPattern;
class IdVecPattern;
class IfExpr;
class IfNode;
class IndexedMeta;
class Istream;
class LabelExpr;
class LabelNode;
class LabelTable;
class LetExpr;
class LetNode;
class LetrecExpr;
class LetrecNode;
class LockedMbox;
class MIActor;
class MboxOb;
class MboxQueue;
class MethodExpr;
class MethodNode;
class Monitor;
class MultiMethod;
class Niv;
class NullExpr;
class NullNode;
class Ob;
class ObStk;
class Ostream;
class Pattern;
class Prim;
class Proc;
class ProcExpr;
class ProcNode;
class ProductType;
class PtrCollection;
class Queue;
class QueueMbox;
class QuoteExpr;
class RBLstring;
class RblTable;
class ReadTable;
class ReflectiveMethodExpr;
class ReflectiveMethodNode;
class ReflectiveMthd;
class RequestExpr;
class RequestNode;
class SendExpr;
class SendNode;
class SeqExpr;
class SeqNode;
class SetExpr;
class SetNode;
class Stack;
class StdExtension;
class StdMeta;
class StdMthd;
class StdOprn;
class SumType;
class Symbol;
class SymbolNode;
class Sysval;
class TblObject;
class Template;
class Timer;
class Tuple;
class TupleExpr;
class TupleNode;
class UpcallCtxt;
class VirtualMachine;
class Word16Vec;
class Word32Vec;
class XferNode;


typedef Actor*        pSBO;
typedef Ctxt*         pCtxt;
typedef Ob*           pOb;
typedef StdExtension* pExt;
typedef StdMeta*      pMeta;
typedef Tuple*        pTuple;


extern TupleExpr*	NILexpr;
extern pTuple		NIL;


#define NI(string) notImplemented(string)


extern pOb obcpy (pOb, pOb, int);



extern pOb RBLEOF;
extern pOb INCOMPLETE_IO;
extern pOb READ_ERROR;
extern pOb ABSENT;

extern pMeta MetaTop;
extern TblObject* TopSBO;
extern TblObject* GlobalEnv;
extern pOb emptyMbox;
extern pOb lockedMbox;


enum SysCode { syscodeInvalid, syscodeUpcall, syscodeSuspended, syscodeInterrupt, syscodeSleep, syscodeDeadThread };

#define RBLTRUE  	RBLBOOL(TRUE)
#define RBLFALSE	RBLBOOL(FALSE)
#define NIV		MAKEESCTAGGED(OTniv,    0)
#define INVALID		MAKEESCTAGGED(OTsysval, syscodeInvalid)
#define UPCALL		MAKEESCTAGGED(OTsysval, syscodeUpcall)
#define SUSPENDED	MAKEESCTAGGED(OTsysval, syscodeSuspended)
#define INTERRUPT	MAKEESCTAGGED(OTsysval, syscodeInterrupt)
#define SLEEP		MAKEESCTAGGED(OTsysval, syscodeSleep)
#define DEADTHREAD	MAKEESCTAGGED(OTsysval, syscodeDeadThread)

#include "Bits.h"

#define GET_TAGGED_TAG(x) GET_LF(x,0,TagSize)
#define GET_TAGGED_DATA(x) GET_LF(x,0+TagSize,WordSize-TagSize)

#define GET_ESCTAGGED_TAG(x) GET_LF(x,0,EscTagSize)
#define GET_ESCTAGGED_DATA(x) GET_LF(x,0+EscTagSize,(WordSize-EscTagSize))

union TagExtract
{
    pOb ptr;
    unsigned int locfields;
};


inline
int
TAG (pOb x)
{
    TagExtract te;
    te.ptr = x;
    return GET_TAGGED_TAG(te);
}

inline
int
ESCTAG (pOb x)
{
    TagExtract te;
    te.ptr = x;
    return GET_ESCTAGGED_TAG(te);
}

#define SIGN_EXTEND(a,n) \
  ((((int)a) << (WordSize - (n))) >>(WordSize - (n)))

inline
int
TAGVAL (pOb x)
{
    TagExtract te;
    te.ptr = x;
    return SIGN_EXTEND(GET_TAGGED_DATA(te),WordSize-TagSize);

}

inline
int
ESCVAL (pOb x)
{
    TagExtract te;
    te.ptr = x;
    return SIGN_EXTEND(GET_ESCTAGGED_DATA(te),WordSize-EscTagSize);
}


#define PTR(ob)		((pOb)(ob))
#define NPTR(ob)	((pOb)(ob))
#define SYMPTR(ob)	((char*)((unsigned)PTR(ob) - OTsym))

#define BOOLVAL(ob)	ESCVAL(ob)
#define CHARVAL(ob)	ESCVAL(ob)
#define FIXVAL(ob)	TAGVAL(ob)
#define SYSVAL(ob)	((SysCode)ESCVAL(ob))

#define IS_PTR(ob)	(TAG(ob) == OTptr)
#define IS_SYM(ob)	(TAG(ob) == OTsym)
#define IS_FIXNUM(ob)	(TAG(ob) == OTfixnum)
#define IS(t, ob)	(t < OTesc ? (TAG(ob) == t) : (ESCTAG(ob) == t))


extern pOb decodeAtom (pOb);


inline
pOb
BASE (pOb v)
{
    return TAG(v) == OTptr ? v : decodeAtom(v);
}


/*
 * The three useIfPtr routines are used to conditionally invoke member
 * functions.  If the pOb value provided as the first parameter is not
 * tagged as a valid pointer, nothing is done.  If the pOb value is
 * tagged as pointer, it is invoked with the member function provided as
 * the second argument.
 *
 * The first version is the trickiest; it is used by the scavenging
 * routines and actually needs to modify the pointer passed to it in the
 * first argument.  Because some compilers have problems with reference
 * variables and promotion of derived pointers to base pointers, I have
 * wimped out and simply use a void* formal for this routine.  See Heap.h
 * for comments regarding the robustness of this approach.  This routine
 * returns 1 if the pointer actually changed, 0 otherwise.
 *
 * The second version is a generic function used for counting things.
 * Again, if the pOb value passed as the first argument is not tagged as
 * a pointer, 0 is returned and nothing further happens.  If the value is
 * a pointer, the member function (of type "int (Ob::*) ()) provided as
 * the second argument is invoked and its result is returned.
 *
 * The third version is the same as the second except that it returns no
 * value and expects a second argument of type "void (Ob::*) ()".
 */


typedef pOb  (Ob::*PSOb__PSOb) ();
typedef int  (Ob::*SI__PSOb) ();
typedef void (Ob::*V__PSOb) ();

extern int  useIfPtr (void*, PSOb__PSOb);
extern int  useIfPtr (pOb, SI__PSOb);
extern void useIfPtr (pOb, V__PSOb);

/*
 * Use MF_ADDR(fn) to get the address of a member function.
 */

#define MF_ADDR(fn) &fn

enum GcFlags {
  f_forwarded, // TRUE iff object has been forwarded.
  f_remembered, // TRUE iff object has been remembered.
  f_marked, // TRUE iff object has been marked.
  f_freed, // TRUE iff object is on a free list somewhere.
  f_visited, // TRUE iff object has been visited (auxiliary).
  f_foreign, // TRUE iff object contains foreign pointers
};	  

struct HeaderLayout
{   unsigned char flags;	// See above
    unsigned char  age;		// The number of scavenges the object has survived.
    unsigned short size;	// The total size (in bytes) of the object.
};

union HeaderBits
{
    HeaderLayout 	fields;
    unsigned int	all;

    HeaderBits (HeaderBits&);
    HeaderBits (int);
};

inline
HeaderBits::HeaderBits (HeaderBits& hb)
{
    all = hb.all;
}

inline
HeaderBits::HeaderBits (int sz)
{
    all = 0;
    fields.size = sz;
}

struct convertArgReturnPair
{
  Word32 val;
  int    failp;
};

extern convertArgReturnPair cnvArgRetPair;

/*
 * Ob is the base class for all objects.  The header for each such object
 * contains bits that tell whether it has been forwarded (during
 * scavenging), remembered (i.e., whether it is pointed to from the
 * rememberedSet), or marked (during mark/scan).  The header also holds
 * an age field (the number of scavenges that the object has survived),
 * and a size field (the number of bytes occupied by the object,
 * including header).
 *
 * Obs are currently divided into two classes: those whose bodies consist
 * of sequences of pOb's (the default for Ob), and those whose bodies
 * consist of undifferentiated binary information (BinaryOb).  In the
 * interests of keeping the garbage collector fast (and debuggers
 * simple), it is not possible to mix binary info with Ob's in the body
 * of a derivative of (non-Binary)Ob.  If an object requires such a
 * mixture, it should be described as a derivative of a BinaryOb that
 * supplies its own traversePtrs() routines, or as an ordinary Ob that
 * points to a binary Ob that holds the binary info.
 *
 * ForeignOb's are Ob's that hold pointers to structures in the "outside"
 * world, such as InterViews windows, etc.  ForeignOb's should take care
 * to provide a specific (virtual) destructor to accomplish whatever
 * specific deallocation actions should take place; the garbage collector
 * will invoke that destructor when the object is no longer referenced.
 * In order to avoid introducing multiple inheritance problems,
 * ForeignOb's are *not* represented by a C++ class, but instead by the
 * FOREIGN bit in the header.  The garbage collectors look at this bit
 * and, if it is set, invoke the virtual destructor when the object is
 * freed.
 *
 * The free bit is necessary to avoid an ugly problem with deallocation
 * during garbage collection.  Since the various free lists are linked
 * through the first slot field (slot(0)), we can't blithely indirect
 * through that field to find the destructor for an object during the
 * scan() phase.  The free bit indicates whether an Ob is linked in a
 * free list; if it is (i.e., the bit is TRUE), we don't invoke the
 * destructor.  If the bit is off, then we are actually reclaiming an
 * object that has become garbage (as opposed to simply rediscovering a
 * Ob that was on a free list in the first place), and we can safely
 * invoke the destructor.
 */



#include "Location.h"


class Base
{
  public:

    static char**		classNames;
    static unsigned long*	obCounts;
    static int			nClasses;

    Base (EMPTY);

    virtual char*	typestring (EMPTY);
    virtual void	updateCnt (EMPTY);
    static void		defSlot (const char*, Location);
};


inline Base::Base (EMPTY) { }


/*
 * The InPlace_Constructor type is a dummy type that is used to overload
 * constructors.  This particular type is used to indicate constructors
 * that are called when an object is changed in place, as when we convert
 * an argvec tuple into an environment contour (a StdExtension; see
 * Actor.cc).  Using this specially-created type to identify these
 * constructors eliminates any possibility of confusion with other more
 * useful types.
 */


struct InPlace_Constructor {
    int dummy;
};

#define BuildInPlace ((InPlace_Constructor*)0)


class Ob : public Base
{
  protected:

    static char stringbuf[256];

    Ob (int, pOb, pOb);
    Ob (InPlace_Constructor*);
    Ob (InPlace_Constructor*, int);
    Ob (InPlace_Constructor*, pOb, pOb);

    friend class BuiltinClass;
    friend class OldSpace;

  public:

    HeaderBits	header;
    pOb		_slot[2];

    /*
     * The mandatory meta and parent fields are contained in _slot[0] and
     * _slot[1], respectively.  We declare things as an array to
     * guarantee contiguity, and to make things easier for the scavenging
     * and gc routines that want to regard objects as simple vectors of
     * object pointers.
     */

    virtual ~Ob (EMPTY);

    void* operator new (size_t);
    void* operator new (size_t, void*);
    void  operator delete (void*);

    pOb&	meta (EMPTY);
    pOb&	parent (EMPTY);
    pOb&	slot(int);

    pOb*	endp (EMPTY);
    pOb&	forwardingAddress (EMPTY);
    void	forwardTo (pOb);
    bool	checkStore (pOb);
    bool	reallyCheckStore (pOb);
    int		gcSensitive (EMPTY);
    pOb		relocate (EMPTY);
    bool	suspicious (EMPTY);
    int		numberOfSlots (EMPTY);
    void	clobberVtbl (pOb);

    void	notImplemented (char*);

    void	mark (EMPTY);
    void	check (EMPTY);
    void	checkOb (EMPTY);
    int		size (EMPTY);
    int		obCount (EMPTY);
    void	unvisit (EMPTY);

    virtual pOb	self (EMPTY);

    /* Garbage collector interface */

    virtual int		traversePtrs (PSOb__PSOb);
    virtual int		traversePtrs (SI__PSOb);
    virtual void	traversePtrs (V__PSOb);
    virtual bool	gcFixup (EMPTY);
    virtual bool	scavengeFixup (EMPTY);

    /* Generic runtime actions */

    virtual bool	ConstantP (EMPTY);
    virtual Prim*	InlineablePrimP (EMPTY);
    virtual void	printOn (FILE*);
    virtual void	printQuotedOn (FILE*);
    virtual void	displayOn (FILE* s);
    virtual pOb		container (EMPTY);
    virtual pOb		mailbox (EMPTY);
    virtual pOb		setMailbox (pOb);
    virtual pOb		rcons (pOb);
    virtual int		addSlot (pOb, pOb);
    virtual pOb		dup (EMPTY);
    virtual pOb		clone (EMPTY);
    virtual pOb		cloneTo (pOb, pOb);
    virtual pOb		getLex (int, int, int);
    virtual pOb		setLex (int, int, int, pOb);
    virtual pOb		getAddr (int, int, int);
    virtual pOb		setAddr (int, int, int, pOb);
    virtual pOb		getField (int, int, int, int, int);
    virtual pOb		setField (int, int, int, int, Word32);
    virtual pOb		indexedSize (EMPTY);
    virtual pOb		nth (int);
    virtual pOb		setNth (int, pOb);
    virtual pOb		subObject (int, int);
    virtual const char*	asCstring (EMPTY);
    virtual char*	asPathname (EMPTY);

    /* Object actions */

    virtual bool	isSynchronousTrgt (EMPTY);
    virtual pOb		dispatch (pCtxt);
    virtual pOb		invoke (pCtxt);
    virtual pOb		lookup (pOb, pCtxt);
    virtual pOb		lookupAndInvoke (pCtxt);
    virtual pOb		nextMsg (MboxOb*, pOb);
    virtual pOb		updateNoArgs ();
    virtual pOb		update (bool, pCtxt);
    virtual pOb		updateByLoc (bool, pCtxt);
    virtual pOb		primitiveInitialize (pCtxt);
    virtual pOb		receive (pCtxt);
    virtual pOb		receiveMsg (MboxOb*, pCtxt);
    virtual pOb		becomeNew (pOb, pCtxt);
    virtual bool	accepts (pCtxt);
    virtual bool	matches (pCtxt);

    virtual convertArgReturnPair convertActualArg(Ctxt*, Ob*);
    virtual Ob*         convertActualRslt(Ctxt*, Word32);

    virtual pOb         isNullP(EMPTY);

    /* Type system methods */
    pOb                 typep(pOb);
    virtual bool        hasParentp(pOb);
    virtual bool        compositeCoversp(pOb);
    virtual bool        isCoveredByp(pOb);
    virtual bool        coversp(pOb);
    virtual bool        typeMatchesp(pOb);
    virtual pOb         typeLub(pOb);

    /* Meta actions */

    virtual pTuple	keys (pOb);
    virtual pTuple	locs (pOb);
    virtual Location	keyLoc (pOb, pOb = ABSENT);
    virtual pTuple	locContour (pOb);
    virtual pTuple	contour (pOb);
    virtual pOb		get (pOb, pOb, pCtxt);
    virtual pOb		add (pOb, pOb, pOb, pCtxt);
    virtual pOb		set (pOb, pOb, pOb, pCtxt);
    virtual void	addRef (EMPTY);
    virtual void	deleteRef (EMPTY);
    virtual pOb		lookupOBO (pOb, pOb, pCtxt);

    /* Table actions */

    virtual pTuple	dumpKeys (EMPTY);
    virtual pTuple	dumpPairs (EMPTY);
    virtual int		nPairs (EMPTY);
    virtual pOb		getKey (pOb);
    virtual pOb		addKey (pOb, pOb);

    /* Compiler interface */

    virtual Pattern*	makePattern (EMPTY);
    virtual Template*	makeTemplate (EMPTY);
    virtual AttrNode*	makeAttrNode (bool);
    virtual pOb		unquote (EMPTY);
    virtual Code*	compileWrt (pOb, pOb);
    virtual Location	lex (pOb, int);
    virtual pOb		extendWith (pOb);
    virtual pOb		extendWith (pOb, pTuple);

    /* error handlers */

    virtual pOb		runtimeError (pCtxt, const char*, ...);
    virtual pOb		mismatch (pCtxt, int, char*);
    virtual pOb		mismatch (pCtxt, int, int);
};


#include "Heap.h"

#define HDR_FLAGS(p) ((p)->header.fields.flags)

#define FORWARDED(p)	   (GET_FLAG((p)->header.fields.flags,f_forwarded))
#define REMEMBERED(p)      (GET_FLAG((p)->header.fields.flags,f_remembered))
#define MARKED(p)          (GET_FLAG((p)->header.fields.flags,f_marked))
#define FREED(p)           (GET_FLAG((p)->header.fields.flags,f_freed))
#define VISITED(p)         (GET_FLAG((p)->header.fields.flags,f_visited))
#define FOREIGN(p)         (GET_FLAG((p)->header.fields.flags,f_foreign))
#define AGE(p)             ((p)->header.fields.age)
#define SIZE(p)            ((p)->header.fields.size)

#define SLOT_NUM(type, field) ((int)((pOb*)&((type*)0)->field - (pOb*)&((type*)0)->_slot[2]))
#ifndef offsetof
#define offsetof(type, field) ((int)&((type*)0)->field)
#endif


/*
 * When assigning into a heap-allocated object, we must always check
 * whether
 *
 * 	1) the container object is in old space, and (if so)
 * 	2) the value being written is a pointer into new space.
 *
 * ASSIGN takes care of those checks and records the necessary
 * information.  If there are to be consecutive assignments into fields
 * of the same container, it is a better idea to use the CHECK_STORE/VAL
 * macro combination, since it will generate less setup code.  It takes
 * the form
 *
 * 	container->field1 = val1;
 * 	...
 * 	container->fieldN = valN;
 * 	CHECK_STORE(container, (VAL(val1), ..., VAL(valN)));
 *
 * This is somewhat dangerous, however, in that you *must* guarantee that
 * no scavenges can take place between the first assignment and the
 * invocation of CHECK_STORE.
 */

#define ASSIGN(ptr, field, val) (ptr)->checkStore((ptr)->field = (val))
#define CHECK_STORE(base, vals) if (IS_OLD(base)) { pOb _base = base; vals; }
#define VAL(x) _base->reallyCheckStore(x)


static const int MinObSize = sizeof(Ob);


inline
void*
Ob::operator new (size_t, void* p)
{
    return p;
}


inline pOb& Ob::meta (EMPTY)			{ return _slot[0]; }
inline pOb& Ob::parent (EMPTY)		{ return _slot[1]; }
inline pOb& Ob::slot (int n)		{ return _slot[n+2]; }
inline pOb* Ob::endp (EMPTY)			{ return (pOb*) ((char*)this + SIZE(this)); }
inline pOb& Ob::forwardingAddress (EMPTY)	{ return _slot[0]; }
inline bool Ob::checkStore (pOb v)	{ return IS_OLD(this) && reallyCheckStore(v); }
inline int  Ob::gcSensitive (EMPTY)		{ return IS_NEW(this); }
inline int  Ob::numberOfSlots (EMPTY)	{ return (SIZE(this)-sizeof(Ob))/sizeof(pOb); }


inline
Ob::Ob (InPlace_Constructor*)
    : header(this->header)
{ }


inline
Ob::Ob (InPlace_Constructor*, pOb meta, pOb parent)
    : header(this->header)
{
    ASSIGN(this, meta(), meta);
    ASSIGN(this, parent(), parent);
}

inline
Ob::Ob (int sz, pOb meta, pOb parent)
    : header(sz)
{
    /*
     * WARNING: sz must already be properly aligned.  The individual
     * constructors, which are already responsible for requesting the
     * allocator to give them a chunk of memory, must ensure that they
     * only request chunks that are multiples of the alignment quantum.
     */

    this->meta() = meta;
    this->parent() = parent;
}



class MboxOb : public Ob
{
  protected:

    MboxOb (int, pOb, pOb, pOb);
    MboxOb (InPlace_Constructor*);
  
  public:

    pOb	mbox;

    virtual pOb		mailbox (EMPTY);
    virtual pOb		setMailbox (pOb);
    virtual pOb		receive (pCtxt);
    virtual void	schedule (pCtxt);
};


inline
MboxOb::MboxOb (int sz, pOb meta, pOb parent, pOb mbox)
    : Ob(sz, meta, parent)
{
    this->mbox = mbox;
}


inline
MboxOb::MboxOb (InPlace_Constructor* ipc) : Ob(ipc) { }



class SlotDescriptor;


#define STD_DECLS(classname)						      \
friend class BuiltinClass;						      \
public:									      \
virtual void updateCnt (EMPTY);						      \
virtual char* typestring (EMPTY);						      \
static pMeta name2(classname,Meta);					      \
static pSBO name2(classname,SBO);					      \
static SlotDescriptor* _meta_fields


#define CLASS_META(name) name2(name::name,Meta)
#define CLASS_SBO(name) name2(name::name,SBO)


inline pMeta META (pOb ob) { return (pMeta) ob->meta(); }
inline pSBO SBO (pOb ob) { return (pSBO) ob->parent(); }



class Actor : public MboxOb
{
    STD_DECLS(Actor);

  protected:

    Actor (int, pOb, pOb, pOb, pExt);
    Actor (InPlace_Constructor*);
    Actor (pOb, pOb, pExt);

  public:

    pExt		extension;

    static Actor* create (EMPTY);
    static Actor* create (pOb, pOb, pExt);

    virtual pOb 	container (EMPTY);
    virtual int		addSlot (pOb, pOb);
    virtual pOb		dup (EMPTY);
    virtual pOb		cloneTo (pOb, pOb);
    virtual pOb		updateNoArgs ();
    virtual pOb		update (bool, pCtxt);
    virtual pOb		updateByLoc (bool, pCtxt);
    virtual pOb		primitiveInitialize (pCtxt);
    virtual pOb		becomeNew (pOb, pCtxt);
    virtual pOb		dispatch (pCtxt);
    virtual pOb		lookupAndInvoke (pCtxt);
    virtual void	schedule (pCtxt);
};


inline Actor::Actor (int sz, pOb meta, pOb parent, pOb mbox, pExt ext)
    : MboxOb(sz, meta, parent, mbox), extension(ext)
{ }

inline Actor::Actor (InPlace_Constructor* ipc) : MboxOb(ipc) { }


class StdExtension : public Ob
{
    STD_DECLS(StdExtension);

  protected:

    StdExtension (pOb, pOb, int);
    StdExtension (pOb, pOb);
    StdExtension (int);
    StdExtension (pTuple);

    friend class Tuple;

  public:

    static StdExtension* create (pOb, pOb, int);
    static StdExtension* create (int);
    static StdExtension* create (pTuple);
};



class TblObject : public Actor
{
    STD_DECLS(TblObject);

  protected:

    TblObject (pExt, pOb, pTuple);

  public:

    pOb		validExtent;
    pTuple	keyVec;

    static TblObject* create (EMPTY);

    virtual int	addSlot (pOb, pOb);
    pOb		entry (int);
    pOb		entryKey (int);
};


inline
pOb
TblObject::entry (int n)
{
    return extension->slot(n);
}


class StdMeta : public Actor
{
    STD_DECLS(StdMeta);

  protected:

    StdMeta (int, pOb, pOb, pOb, pExt);
    StdMeta (InPlace_Constructor*);
    StdMeta (EMPTY);
    StdMeta (pExt);
  public:

    static StdMeta* create (EMPTY);
    static StdMeta* create (pTuple, pOb = FIXNUM(0), pOb = RBLTRUE);

    virtual pOb		cloneTo (pOb, pOb);
    virtual pTuple	keys (pOb);
    virtual pTuple	locs (pOb);
    virtual Location	keyLoc (pOb, pOb = ABSENT);
    virtual pTuple	locContour (pOb);
    virtual pTuple	contour (pOb);
    virtual pOb		get (pOb, pOb, pCtxt);
    virtual pOb		add (pOb, pOb, pOb, pCtxt);
    virtual pOb		set (pOb, pOb, pOb, pCtxt);
    virtual void	addRef (EMPTY);
    virtual void	deleteRef (EMPTY);
    virtual pOb		lookupOBO (pOb, pOb, pCtxt);

    void	allocateMap (EMPTY);
    pOb		map (EMPTY);
    bool	isShared (EMPTY);
    bool	clientsAreExtensible (EMPTY);
    void	becomeIndexed (int);
};


static const int STDMETA_MAP_SLOT = 0;
static const int STDMETA_REFCOUNT_SLOT = 1;
static const int STDMETA_EXTENSIBLE_SLOT = 2;

static const int BUILTIN_STDMETA_SLOTS = 3;

inline pOb  StdMeta::map (EMPTY) { return extension->slot(STDMETA_MAP_SLOT); }
inline bool StdMeta::isShared (EMPTY) { return extension->slot(STDMETA_REFCOUNT_SLOT) > FIXNUM(1); }
inline bool StdMeta::clientsAreExtensible (EMPTY) { return BOOLVAL(extension->slot(STDMETA_EXTENSIBLE_SLOT)); }

inline
StdMeta::StdMeta (int sz, pOb meta, pOb parent, pOb mbox, pExt ext)
    : Actor(sz, meta, parent, mbox, ext)
{ }

inline
StdMeta::StdMeta (InPlace_Constructor* ipc)
    : Actor(ipc)
{ }

extern pOb TopEnv;
extern pOb Qanon;

extern int	debugging_level;

#define IS_A(ob, type) (BASE(ob)->meta() == CLASS_META(type))


extern void Define (char*, pOb, Ob* = GlobalEnv);
extern void Define (pOb, pOb, Ob* = GlobalEnv);


/* defined in StringStore.cc */
extern const char* intern (const char*);

#define UNIMPLEMENTED(type,clas,op,args) type clas::op args { NI(_STRING(op)); return((type)INVALID); }
#define UNIMPLEMENTED_VOID(clas,op,args) void clas::op args { NI(_STRING(op)); }

/*
 * These macros are used to synthesize internal names for the functions
 * and initialization structures associated with builtin primitives.
 * They are included here because it is useful to be able to declare
 * primitives as friends of certain classes with including all of the
 * Prim.h baggage.
 */


typedef pOb PRIMFN (Ob*, pCtxt);
#define PRIM_NAME(name) name2(_f_,name)
#define BUILTIN_PRIM(name) pOb PRIM_NAME(name) (Ob* __PRIM__, pCtxt __CTXT__)

#include "misc.h"

#endif
