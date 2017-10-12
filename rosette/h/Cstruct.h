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

#if !defined(_RBL_Cstruct_h)
#define _RBL_Cstruct_h 1

#ifdef __GNUG__
#pragma interface
#endif

#include "rosette.h"

#include "Ob.h"

class GenericDescriptor : public Actor
{
  STD_DECLS(GenericDescriptor);

 protected:

  GenericDescriptor(pExt);
  GenericDescriptor(int, pOb, pOb, pOb, pExt);

  virtual int  traversePtrs( PSOb__PSOb );
  virtual int  traversePtrs( SI__PSOb );
  virtual void traversePtrs( V__PSOb );
  
 public:

  /* This seems to be the base-level protocol provided by descriptors.
   * 
   * (method (S-get base path) ...)
   * (method (S-desc base path) ...)
   * (method (S-deref base path) ...)
   * (method (select base r) ...)
   * (method (S-set base [val & r]) ...)
   * (method (S-tupleSet base [val & r]) ...)
   * (method (nth base [i & path]) ...)
   *    
   */
  
  Word32   _offset, _align_to, _size; /* memory map */
  Ob*      mnemonic;  /* was consed up from rosette heap */
  Ob*      imported;
                      /* was returned by a foreign function or is a */
		      /* substructure of a critter returned by a ff */

  Ob*      freeStructOnGC;

  static GenericDescriptor* create();

  virtual ~GenericDescriptor();
  
  virtual Ob*    sGet( Ctxt* ctxt, Word32 base, Tuple* path, int pindex = 0 ); 
  virtual Ob*    sDesc( Ctxt* ctxt, Word32 base, Tuple* path, int pindex = 0 ); 
  virtual Ob*    sDeref( Ctxt* ctxt, Word32 base, Tuple* path, int pindex = 0 );
  virtual Ob*    select( Ctxt* ctxt, Word32 base, Tuple* path, int pindex = 0 );      
  virtual Ob*    sSet( Ctxt* ctxt, Word32 base, Ob* val, Tuple* path, int pindex = 0 ); 
  virtual Ob*    sTupleSet( Ctxt* ctxt, Word32 base, Tuple* val, Tuple* path, int pindex = 0 ); 
  virtual Ob*    nthBase( Ctxt* ctxt, Word32 base, int i, Tuple* path, int pindex = 0 );
  virtual Ob*    flatten( Ctxt* ctxt, Word32 base, RblTable* );

  virtual convertArgReturnPair convertActualArg(Ctxt*, Ob*);
  virtual Ob* convertActualRslt(Ctxt*, Word32);

  Ob*            nullDescriptor( Ctxt* );
  
  virtual Ob*    oprnSwitch( Ctxt* ctxt, Word32 base, Tuple* path, int pindex = 0 );
  Ob*            sBox( Word32 );

  virtual Word32 absoluteAddress( Word32 base );
  void           setAddrContents( Word32 base, Word32 val );
};

inline
Ob*
GenericDescriptor::sBox( Word32 off )
{
  GenericDescriptor* rslt = (GenericDescriptor*)cloneTo(meta(), parent());
  rslt->mbox = emptyMbox;
  rslt->_offset = off;
  rslt->imported = imported;
  rslt->freeStructOnGC = freeStructOnGC;

  /* should rslt be registered as a foreign ob? */
  return(rslt);
}

class NullDescriptor : public GenericDescriptor
{
  STD_DECLS(NullDescriptor);
  
 protected:
  
  NullDescriptor(pExt);
  
 public:

  static NullDescriptor* create();
  
  virtual Ob*    sGet( Ctxt* ctxt, Word32 base, Tuple* path, int pindex = 0 ); 
  virtual Ob*    sDesc( Ctxt* ctxt, Word32 base, Tuple* path, int pindex = 0 ); 
  virtual Ob*    sDeref( Ctxt* ctxt, Word32 base, Tuple* path, int pindex = 0 ); 
  virtual Ob*    select( Ctxt* ctxt, Word32 base, Tuple* path, int pindex = 0 );	 
  virtual Ob*    sSet( Ctxt* ctxt, Word32 base, Ob* val, Tuple* path, int pindex = 0 ); 
  virtual Ob*    nthBase( Ctxt* ctxt, Word32 base, int i, Tuple* path, int pindex = 0 );
  virtual Ob*    flatten( Ctxt* ctxt, Word32 base, RblTable* );

  virtual Ob*    isNullP();

  virtual Word32 absoluteAddress( Word32 base );
};

class AtomicDescriptor : public GenericDescriptor
{
  STD_DECLS(AtomicDescriptor);
  
 protected:
  
  AtomicDescriptor(RblBool*, pExt);
  AtomicDescriptor(RblBool*, int, pOb, pOb, pOb, pExt);

  virtual int  traversePtrs( PSOb__PSOb );
  virtual int  traversePtrs( SI__PSOb );
  virtual void traversePtrs( V__PSOb );
  
 public:

  RblBool* _signed;

  static AtomicDescriptor* create(RblBool*);
  static AtomicDescriptor* create();
  
  virtual Ob*    sGet( Ctxt* ctxt, Word32 base, Tuple* path, int pindex = 0 ); 
  virtual Ob*    sSet( Ctxt* ctxt, Word32 base, Ob* val, Tuple* path, int pindex = 0 );
  virtual Ob*    flatten( Ctxt* ctxt, Word32 base, RblTable* );

  virtual convertArgReturnPair convertActualArg(Ctxt*, Ob*);
  virtual Ob*         convertActualRslt(Ctxt*, Word32);
  virtual Word32 absoluteAddress( Word32 base );
};

class CStructure : public GenericDescriptor
{
  STD_DECLS(CStructure);
  
 protected:
  
  CStructure(RblTable*, Tuple*, pExt);

  virtual int  traversePtrs( PSOb__PSOb );
  virtual int  traversePtrs( SI__PSOb );
  virtual void traversePtrs( V__PSOb );
  
 public:

  RblTable* _descs;
  Tuple*    _fieldNames;

  static CStructure* create(RblTable*, Tuple*);
  static CStructure* create();
  
  virtual Ob*    select( Ctxt* ctxt, Word32 base, Tuple* path, int pindex = 0 );	 
  virtual Ob*    sTupleSet( Ctxt* ctxt, Word32 base, Tuple* val, Tuple* path, int pindex = 0 );
  virtual Ob*    flatten( Ctxt* ctxt, Word32 base, RblTable* );
};

class CArray : public GenericDescriptor
{
  STD_DECLS(CArray);
  
 protected:
  
  CArray(Word16, GenericDescriptor*, pExt);
  CArray(int s, pOb m, pOb p, pOb mbx, pExt, Word16, GenericDescriptor*);

  virtual int  traversePtrs( PSOb__PSOb );
  virtual int  traversePtrs( SI__PSOb );
  virtual void traversePtrs( V__PSOb );
  
 public:

  Word16              _numElems;
  Word16	      filler_up_please;
  GenericDescriptor*  _elemDesc;

  static CArray* create(Word16, GenericDescriptor*);
  static CArray* create();

  virtual Ob*    sTupleSet( Ctxt* ctxt, Word32 base, Tuple* val, Tuple* path, int pindex = 0 ); 
  virtual Ob*    nthBase( Ctxt* ctxt, Word32 base, int i, Tuple* path, int pindex = 0 );
  virtual Ob*    flatten( Ctxt* ctxt, Word32 base, RblTable* );
};

class CharArray : public CArray
{
  STD_DECLS(CharArray);
  
 protected:
  
  CharArray(Word16, GenericDescriptor*, pExt);
  CharArray(int s, pOb m, pOb p, pOb mbx, pExt, Word16, GenericDescriptor*);

 public:

  static CharArray* create(Word16, GenericDescriptor*);
  static CharArray* create();

  virtual Ob*    sSet( Ctxt* ctxt, Word32 base, Ob* val, Tuple* path, int pindex = 0 );
  virtual Ob*    flatten( Ctxt* ctxt, Word32 base, RblTable* );
};

class CharArray0 : public CharArray
{
  STD_DECLS(CharArray0);
  
 protected:
  
  CharArray0(Word16, GenericDescriptor*, pExt);

 public:

  static CharArray0* create(Word16, GenericDescriptor*);
  static CharArray0* create();

  virtual Ob*    flatten( Ctxt* ctxt, Word32 base, RblTable* );
};

class CRef : public GenericDescriptor
{
  STD_DECLS(CRef);
  
 protected:
  
  CRef(GenericDescriptor*, pExt);
  CRef(GenericDescriptor*, int, pOb, pOb, pOb, pExt);

  virtual int  traversePtrs( PSOb__PSOb );
  virtual int  traversePtrs( SI__PSOb );
  virtual void traversePtrs( V__PSOb );
    
 public:

  GenericDescriptor* _desc;

  static CRef* create(GenericDescriptor*);
  static CRef* create();
  
  virtual Ob*    sDeref( Ctxt* ctxt, Word32 base, Tuple* path, int pindex = 0 ); 
  virtual Ob*    sSet( Ctxt* ctxt, Word32 base, Ob* val, Tuple* path, int pindex = 0 ); 
  virtual Ob*    nthBase( Ctxt* ctxt, Word32 base, int i, Tuple* path, int pindex = 0 );
  virtual Ob*    flatten( Ctxt* ctxt, Word32 base, RblTable* );

  virtual convertArgReturnPair convertActualArg(Ctxt*, Ob*);
  virtual Ob*         convertActualRslt(Ctxt*, Word32);
};

class CharRef : public CRef
{
  STD_DECLS(CharRef);
  
 protected:
  
  CharRef(GenericDescriptor*, pExt);
  CharRef(GenericDescriptor*, int, pOb, pOb, pOb, pExt);
    
 public:

  static CharRef* create(GenericDescriptor*);
  static CharRef* create();
  
  virtual Ob*    sSet( Ctxt* ctxt, Word32 base, Ob* val, Tuple* path, int pindex = 0 ); 

  virtual convertArgReturnPair convertActualArg(Ctxt*, Ob*);
};

class CRef0 : public CRef
{
  STD_DECLS(CRef0);
  
 protected:

  CRef0(GenericDescriptor*, pExt);
  CRef0(GenericDescriptor*, int, pOb, pOb, pOb, pExt);

 public:

  static CRef0* create();
  static CRef0* create(GenericDescriptor*);
  
  virtual Ob*    sGet( Ctxt* ctxt, Word32 base, Tuple* path, int pindex = 0 );
  virtual Ob*    flatten( Ctxt* ctxt, Word32 base, RblTable* );
};


class CharRef0 : public CRef0
{
  STD_DECLS(CharRef0);

 protected:

  CharRef0(pExt);

 public:

  static CharRef0* create();

  virtual Ob*     flatten( Ctxt*, Word32, RblTable* );
  virtual Ob*     sSet( Ctxt* ctxt, Word32 base, Ob* val, Tuple* path, int pindex = 0 );
  virtual convertArgReturnPair convertActualArg(Ctxt*, Ob*);
};

class CUnion : public GenericDescriptor
{
  STD_DECLS(CUnion);
  
 protected:
  
  CUnion(RblTable*, Tuple*, pExt);

  virtual int  traversePtrs( PSOb__PSOb );
  virtual int  traversePtrs( SI__PSOb );
  virtual void traversePtrs( V__PSOb );
  
 public:

  RblTable* _descs;
  Tuple*    _fieldNames;

  static CUnion* create(RblTable*, Tuple*);
  static CUnion* create();
  
  virtual Ob*    select( Ctxt* ctxt, Word32 base, Tuple* path, int pindex = 0 );
  virtual Ob*    flatten( Ctxt* ctxt, Word32 base, RblTable* );
};

#endif
