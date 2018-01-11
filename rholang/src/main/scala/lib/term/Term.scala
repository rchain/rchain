// -*- mode: Scala;-*- 
// Filename:    Term.scala 
// Authors:     luciusmeredith                                                    
// Creation:    Wed Feb  1 08:55:51 2017 
// Copyright:   See site license
// Description: 
// ------------------------------------------------------------------------

package coop.rchain.lib.term

import coop.rchain.lib.zipper._
import scala.collection.SeqProxy

trait Term[Namespace, /*+*/TagType]
extends Tree[TagType] with SeqProxy[Term[Namespace,TagType]]
  with Serializable {
  // Collects a list of all the tags in the tree.
  def collectTags /* [Tag1 >: Tag] */ ( term : Term[Namespace,TagType] )
   : List[TagType] = {
    term match {
      case TermLeaf( t ) => List( t )
      case TermBranch( ns, lbls ) => {
	( List[TagType]() /: lbls.flatMap( _.self ) )(
	  {
	    ( acc, e ) => {
	      acc ++ collectTags/*[Tag1]*/( e )
	    }
	  }
	)
      }
    }
  }

  def atoms : Seq[TagType] = { this flatMap( collectTags ) }
  def self : List[Term[Namespace,TagType]]
}

class TermLeaf[Namespace,TagType]( val tag : TagType )
extends TreeItem[TagType]( tag )
with Term[Namespace,TagType] {
  override def self = List( this )
}

object TermLeaf extends Serializable {
  def unapply[Namespace,TagType](
    cnxnLeaf : TermLeaf[Namespace,TagType]
  ) : Option[( TagType )] = {
    Some( ( cnxnLeaf.tag ) )
  }
}

trait AbstractTermBranch[Namespace,TagType]
extends Term[Namespace,TagType] {
  def nameSpace : Namespace
  def labels : List[Term[Namespace,TagType]]
  override def self = labels
}

class TermBranch[Namespace,TagType](
  override val nameSpace : Namespace,
  val factuals : List[Term[Namespace,TagType]]
) extends TreeSection[TagType]( factuals )
with AbstractTermBranch[Namespace,TagType]
with Term[Namespace,TagType] {
  override def labels : List[Term[Namespace,TagType]] = {
    factuals
  }
}

object TermBranch extends Serializable {
  def unapply[Namespace,TagType](
    cnxnBranch : TermBranch[Namespace,TagType]
  ) : Option[( Namespace, List[Term[Namespace,TagType]] )] = {
    Some( ( cnxnBranch.nameSpace, cnxnBranch.labels ) )
  }
}

sealed trait TagOrVar[+TagType, +VarType]

final case class Tag[+TagType]( tag : TagType ) extends TagOrVar[TagType, Nothing]
final case class Var[+VarType]( variable : VarType ) extends TagOrVar[Nothing, VarType]

sealed trait NamespaceOrVar[+NSType, +VarType]

final case class NS[+NSType]( ns : NSType ) extends NamespaceOrVar[NSType, Nothing]
final case class NsVar[+VarType]( variable : VarType ) extends NamespaceOrVar[Nothing, VarType]

trait TermCtxt[Namespace,VarType,TagType]
extends Term[NamespaceOrVar[Namespace,VarType],TagOrVar[TagType,VarType]] {
  type U =
    Term[NamespaceOrVar[Namespace,VarType],TagOrVar[TagType,VarType]]

  def names : Seq[TagOrVar[TagType,VarType]] = {
    atoms filter(
      {
	( ctxtLbl ) => {
	  ctxtLbl match { 
	    case Tag( _ ) => false
	    case Var( _ ) => true
	  }
	}
      }
    )
  }

  def show : String = {
    this match {
      case leaf : TermCtxtLeaf[Namespace,VarType,TagType] =>
        leaf.showLeaf
      case branch : TermCtxtBranch[Namespace,VarType,TagType] =>
        branch.showBranch
      case _ => throw new Exception( "unexpected CCL type" )
    }
  }
}

class TermCtxtLeaf[Namespace,VarType,TagType]( val tag : TagOrVar[TagType,VarType] )
extends TreeItem[TagOrVar[TagType,VarType]]( tag )
with TermCtxt[Namespace,VarType,TagType] {
  override def self = List( this )
  override def toString = {
    tag match {
      case Tag( t ) => "" + t + ""
      case Var( v ) => "'" + v
    }
  }
  def showLeaf : String = {
    tag match {
      case Tag( t : String ) => "\"" + t + "\""
      case Tag( t ) => t + ""
      case Var( v ) => v + ""
    }
  }
}

object TermCtxtLeaf extends Serializable {
  def unapply[Namespace,VarType,TagType](
    cnxnCtxtLeaf : TermCtxtLeaf[Namespace,VarType,TagType]
  ) : Option[( TagOrVar[TagType,VarType] )] = {
    Some( ( cnxnCtxtLeaf.tag ) )
  }
}

trait AbstractTermCtxtBranch[Namespace,VarType,TagType]
extends TermCtxt[Namespace,VarType,TagType] {  
  def nameSpace : Namespace
  def labels : List[TermCtxt[Namespace,VarType,TagType]]
  override def self = labels
  override def toString = {
    val lblStr =
      labels match {
	case albl :: rlbls => {
	  ( albl.toString /: rlbls )( 
	    {
	      ( acc, lbl ) => {
		acc + ", " + lbl
	      }
	    }
	  )
	}
	case Nil => ""
      }
    nameSpace + "(" + lblStr + ")"
  }
  def showBranch : String = {
    val lblStr =
      labels match {
	case albl :: rlbls => {
	  ( albl.show /: rlbls )( 
	    {
	      ( acc, lbl ) => {
		acc + ", " + lbl.show
	      }
	    }
	  )
	}
	case Nil => ""
      }
    nameSpace + "(" + lblStr + ")"
  }
}

class TermCtxtBranch[Namespace,VarType,TagType](
  override val nameSpace : Namespace,
  val factuals : List[TermCtxt[Namespace,VarType,TagType]]
) extends TreeSection[TagOrVar[TagType,VarType]]( factuals )
with AbstractTermCtxtBranch[Namespace,VarType,TagType] {
  override def labels : List[TermCtxt[Namespace,VarType,TagType]] = {
    factuals
  }
  override def equals( o : Any ) : Boolean = {
    o match {
      case that : TermCtxtBranch[Namespace,VarType,TagType] => {
	(
	  nameSpace.equals( that.nameSpace )
	  && factuals.equals( that.factuals ) 
	)
      }
      case _ => false
    }
  }
  override def hashCode( ) : Int = {
    (
      ( 37 * nameSpace.hashCode )
      + ( 37 * factuals.hashCode )
    )
  }
}

object TermCtxtBranch extends Serializable {
  def unapply[Namespace,VarType,TagType](
    cnxnCtxtBranch : TermCtxtBranch[Namespace,VarType,TagType]
  ) : Option[
	(
	  Namespace,
	  List[TermCtxt[Namespace,VarType,TagType]]
	)
      ] = {
    Some( ( cnxnCtxtBranch.nameSpace, cnxnCtxtBranch.factuals
 ) )
  }
}

trait TermCtxtInjector[Namespace,VarType,TagType] {
  def injectLabel( cLabel : Term[Namespace,TagType] )
  : TermCtxt[Namespace,VarType,TagType] = {
    cLabel match {
      case cLeaf : TermLeaf[Namespace,TagType] =>
	inject( cLeaf )
      case cBranch : TermBranch[Namespace,TagType] =>
	inject( cBranch )
    }
  }
  def inject( cLabel : TermLeaf[Namespace,TagType] )
  : TermCtxt[Namespace,VarType,TagType] = {
    new TermCtxtLeaf( Tag( cLabel.tag ) )
  }
  def inject( cLabel : TermBranch[Namespace,TagType] )
  : TermCtxt[Namespace,VarType,TagType] = {
    new TermCtxtBranch(
      cLabel.nameSpace,
      cLabel.factuals.map( injectLabel( _ ) )
    )
  }
}
