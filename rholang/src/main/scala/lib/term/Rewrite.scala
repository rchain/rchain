// -*- mode: Scala;-*- 
// Filename:    Rewrite.scala 
// Authors:     luciusmeredith                                                    
// Creation:    Thu Feb  2 12:35:45 2017 
// Copyright:   See site license 
// Description: 
// ------------------------------------------------------------------------

package coop.rchain.lib.term

import coop.rchain.lib.zipper._
import coop.rchain.lib.navigation.{ Right => R, Left => L,_ }

trait TermNavigation[L,V,T] extends ZipperNavigation[Either[T,V]] {
  override def left [A1 >: Either[T,V]] ( location : Location[A1] ) : Location[A1] = {
    location match {
      case Location( _, Top( ) ) => {
        throw new Exception( "left of top" )
      }
      case Location(t, LabeledTreeContext(lbl: L @unchecked, l :: left, up, right)) => {
        Location( l, LabeledTreeContext[L,A1]( lbl, left, up, t :: right ) )
      }
      case Location(t, LabeledTreeContext(lbl: L @unchecked, Nil, up, right)) => {
        throw new Exception( "left of first" )
      }
    }
  }
  override def right [A1 >: Either[T,V]] ( location : Location[A1] ) : Location[A1] = {
    location match {
      case Location( _, Top( ) ) => {
        throw new Exception( "right of top" )
      }
      case Location(t, LabeledTreeContext(lbl: L @unchecked, left, up, r :: right)) => {
        Location( r, LabeledTreeContext[L, A1]( lbl, t :: left, up, right ) )
      }
      case Location( t, _ ) => {
        throw new Exception( "right of last" )
      }
    }
  }
  override def up [A1 >: Either[T,V]]( location : Location[A1] ) : Location[A1] = {
    location match {
      case Location( _, Top( ) ) => {
        throw new Exception( "up of top" )
      }   
      case Location( t : TermCtxt[L, V, T] with Factual, LabeledTreeContext(lbl: L @unchecked, left, up, right)) => {
	( left, right ) match {
	  case (lTerms: List[TermCtxt[L, V, T] with Factual] @unchecked, rTerms: List[TermCtxt[L, V, T] with Factual] @unchecked) => {
	    val rvrsLTerms : List[TermCtxt[L,V,T] with Factual] = lTerms.reverse
            Location[A1]( new TermCtxtBranch[L,V,T]( lbl, rvrsLTerms ::: ( t :: rTerms ) ), up )
	  }
	  case _ => throw new Exception( "unexpected location shape: " + location )
	}
      }
      case Location( t, ctxt ) => {
	/*
	 println(
	  (
	    "/* ------------------------------------------------------- */\n"
	    + "/* method: " + "up" + " */\n"
	    + "/* location: " + location + " */\n"
	    + "/* location.tree: " + location.tree + "; class: " + location.tree.getClass + " */\n"
	    + "/* location.ctxt: " + location.ctxt + "; class: " + location.ctxt.getClass +" */\n"
	    + "/* ------------------------------------------------------- */\n"
	  )
	)
	*/
	throw new Exception( "unmatched location shape: " + location )
      }
    }
  }
  override def down [A1 >: Either[T,V]]( location : Location[A1] ) : Location[A1] = {
    location match {
      case Location( TreeItem( _ ), _ ) => {
        throw new Exception( "down of item" )
      }
      case Location( TreeSection( Nil ), ctxt ) => {
        throw new Exception( "down of empty" )
      }
      case Location(TermCtxtBranch(lbl: L @unchecked, u :: trees), ctxt) => {
        Location( u, LabeledTreeContext[L, A1]( lbl, List[TermCtxt[L,V,T] with Factual](), ctxt, trees ) )
      }
    }
  }
}

trait TermMutation[L,V,T] extends ZipperMutation[Either[T,V]] {
  def update(
    location : Location[Either[T,V]],
    tree : TermCtxt[L,V,T]
  ) : Location[Either[T,V]] = {
    location match {
      case Location( _, ctxt ) =>
	Location( tree, ctxt )
    }
  }
  def insertRight(
    location : Location[Either[T,V]],
    tree : TermCtxt[L,V,T]
  ) : Location[Either[T,V]] = {
    location match {
      case Location( _, Top( ) ) => {
	throw new Exception( "insert of top" )
      }
      case Location(
	curr,
	LabeledTreeContext(lbl: L @unchecked, left, up, right)
      ) => {
	Location(
	  curr,
	  LabeledTreeContext[L,Either[T,V]]( lbl, left, up, tree :: right )
	)	
      }
    }    
  }
  def insertLeft(
    location : Location[Either[T,V]], tree : TermCtxt[L,V,T]
  ) : Location[Either[T,V]] = {
    location match {
      case Location( _, Top( ) ) => {
	throw new Exception( "insert of top" )
      }
      case Location(
	curr,
	LabeledTreeContext(lbl: L @unchecked, left, up, right)
      ) => {
	Location(
	  curr,
	  LabeledTreeContext[L,Either[T,V]]( lbl, tree :: left, up, right )
	)	
      }
    }    
  }
  def insertDown(
    location : Location[Either[T,V]], tree : TermCtxt[L,V,T]
  ) : Location[Either[T,V]] = {
    location match {
      case Location( TreeItem( _ ), _ ) => {
	throw new Exception( "down of item" )
      }
      case Location(
	TermCtxtBranch(lbl: L @unchecked, progeny),
	ctxt@Top()
      ) => {
	Location(
	  tree,
	  LabeledTreeContext[L,Either[T,V]]( lbl, List[TermCtxt[L,V,T] with Factual](), ctxt, progeny )
	)
      }
      case Location(
	TermCtxtBranch(lbl: L @unchecked, progeny),
	ctxt : LabeledTreeContext[L,Either[T,V]]
      ) => {
	Location(
	  tree,
	  LabeledTreeContext[L,Either[T,V]]( lbl, List[TermCtxt[L,V,T] with Factual](), ctxt, progeny )
	)
      }
      case Location(
	t,
	ctxt
      ) => {
	/*
	 println(
	  (
	    "/* ------------------------------------------------------- */\n"
	    + "/* method: " + "insertDown" + " */\n"
	    + "/* location: " + location + " */\n"
	    + "/* location.tree: " + location.tree + "; class: " + location.tree.getClass + " */\n"
	    + "/* location.ctxt: " + location.ctxt + "; class: " + location.ctxt.getClass +" */\n"
	    + "/* tree: " + tree + "; class: " + location.ctxt.getClass + " */\n"
	    + "/* ------------------------------------------------------- */\n"
	  )
	)
	*/
	throw new Exception( "unmatched location shape: " + location )
      }
    }
  }
  def delete(
    location : Location[Either[T,V]], tree : TermCtxt[L,V,T]
  ) : Location[Either[T,V]] = {
    location match {
      case Location( _, Top( ) ) => {
	throw new Exception( "delete of top" )
      }
      case Location(
	_,
	LabeledTreeContext(lbl: L @unchecked, left, up, r :: right)
      ) => {
	Location(
	  r,
	  LabeledTreeContext[L, Either[T,V]]( lbl, left, up, right )
	)
      }
      case Location(
	_,
	LabeledTreeContext(lbl: L @unchecked, l :: left, up, Nil)
      ) => {
	Location(
	  l,
	  LabeledTreeContext[L, Either[T,V]]( lbl, left, up, Nil )
	)
      }
      case Location(
	_,
	LabeledTreeContext(lbl : L @unchecked, Nil, up, Nil)
      ) => {
	Location( new TermCtxtBranch[L,V,T]( lbl, List[TermCtxt[L,V,T] with Factual]() ), up )
      }
    }
  }
}

trait TermZipperComposition[L,V,T] {
  def compose(
    ctxtL : Context[Either[T,V]],
    ctxtR : Context[Either[T,V]]
  ) : Context[Either[T,V]] = {
    ctxtL match {
      case Top() => ctxtR
      case LabeledTreeContext(lbl: L @unchecked, left: List[TermCtxt[L, V, T] with Factual] @unchecked, ctxt: LabeledTreeContext[L, Either[T, V]] @unchecked, right: List[TermCtxt[L, V, T] with Factual] @unchecked) => {
	LabeledTreeContext[L,Either[T,V]](
	  lbl, left, compose( ctxt, ctxtR ), right 
	)
      }
    }
  }
  def compose(
    ctxt : Context[Either[T,V]],
    tree : TermCtxt[L,V,T] with Factual
  ) : TermCtxt[L,V,T] with Factual = {
    ctxt match {
      case Top() => tree
      case LabeledTreeContext(lbl: L @unchecked, left: List[TermCtxt[L, V, T] with Factual] @unchecked, ctxt: LabeledTreeContext[L, Either[T, V]] @unchecked, right: List[TermCtxt[L, V, T] with Factual] @unchecked) => {
	new TermCtxtBranch[L,V,T](
	  lbl, 
	  left ++ ( compose( ctxt, tree ) :: right )
	)
      }
    }
  }
  def decontextualize( location : Location[Either[T,V]] ) : TermCtxt[L,V,T] with Factual = {
    location match {
      case Location( tree : TermCtxt[L,V,T] with Factual, ctxt ) => {
	compose( ctxt, tree )
      }
      case _ => throw new Exception( "unexpected location shape: " + location )
    }
    
  }
}

trait TermSubstitution[L,V,T] {
  def substitute( term : TermCtxt[L,V,T] )(
    bindings : scala.collection.Map[V,TermCtxt[L,V,T] with Factual]
  ) : TermCtxt[L,V,T] with Factual = {
    term match {
      case tLeaf@TermCtxtLeaf( Left( _ ) ) => {
        tLeaf
      }
      case vLeaf@TermCtxtLeaf(Right(v: V @unchecked)) => {
        bindings.get( v ).getOrElse( vLeaf )
      }
      case TermCtxtBranch(fnctr: L @unchecked, actls: List[TermCtxt[L, V, T] with Factual]) => {
        new TermCtxtBranch( fnctr, actls.map( substitute( _ )( bindings ) ) )
      }
    }
  }
}


