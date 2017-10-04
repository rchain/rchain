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

trait Term[Namespace, /*+*/Tag]
extends Tree[Tag] with SeqProxy[Either[Tag,Term[Namespace,Tag]]]
  with Serializable {
  def up /* [Tag1 >: Tag] */ ( tOrC : Either[Tag,Term[Namespace,Tag]] )
   : List[Tag] = {
    tOrC match {
      case Left( t ) => List( t )
      case Right( TermBranch( ns, lbls ) ) => {
	( List[Tag]() /: lbls.flatMap( _.self ) )(
	  {
	    ( acc, e ) => {
	      acc ++ up/*[Tag1]*/( e )
	    }
	  }
	)
      }
    }
  }

  def atoms : Seq[Tag] = { this flatMap( up ) }
}

trait OntologicalStatus
trait Factual extends OntologicalStatus
trait Hypothetical extends OntologicalStatus
trait Theoretical extends OntologicalStatus

class TermLeaf[Namespace,Tag]( val tag : Tag )
extends TreeItem[Tag]( tag )
with Term[Namespace,Tag]
with Factual {
  override def self = List( Left( tag ) )
}

object TermLeaf extends Serializable {
  def unapply[Namespace,Tag](
    cnxnLeaf : TermLeaf[Namespace,Tag]
  ) : Option[( Tag )] = {
    Some( ( cnxnLeaf.tag ) )
  }
}

trait AbstractTermBranch[Namespace,Tag]
extends Term[Namespace,Tag] {
  def nameSpace : Namespace
  def labels : List[Term[Namespace,Tag]]
  override def self = labels.map( Right( _ ) )
}

class TermBranch[Namespace,Tag](
  override val nameSpace : Namespace,
  val factuals : List[Term[Namespace,Tag] with Factual]
) extends TreeSection[Tag]( factuals )
with AbstractTermBranch[Namespace,Tag]
with Term[Namespace,Tag]
with Factual {
  override def labels : List[Term[Namespace,Tag]] = {
    factuals
  }
}

object TermBranch extends Serializable {
  def unapply[Namespace,Tag](
    cnxnBranch : TermBranch[Namespace,Tag]
  ) : Option[( Namespace, List[Term[Namespace,Tag]] )] = {
    Some( ( cnxnBranch.nameSpace, cnxnBranch.labels ) )
  }
}

trait TermCtxt[Namespace,Var,/*+*/Tag]
extends Term[Either[Namespace,Var],Either[Tag,Var]] {
  type U/*[Tag1]*/ =
    Either[
      Either[Tag,Var],
      Term[Either[Namespace,Var],Either[Tag,Var]]
    ]
  override def up /*[Tag1 >: Tag]*/ ( tOrC : U/*[Tag1]*/ )
   : List[Either[Tag,Var]] = {
    tOrC match {
      case Left( t ) => List( t )
      case Right( TermCtxtLeaf( tOrV ) ) => List( tOrV )
      case Right( TermCtxtBranch( ns, lbls ) ) => {
	val selves : List[U/*[Tag1]*/] = lbls.flatMap( _.self )
	( List[Either[Tag,Var]]() /: selves )(
	  {
	    ( acc, e ) => {
	      acc ++ up/*[Tag1]*/( e )
	    }
	  }
	)
      }
    }
  }  

  def names : Seq[Either[Tag,Var]] = {
    atoms filter(
      {
	( ctxtLbl ) => {
	  ctxtLbl match { 
	    case Left( _ ) => false
	    case Right( _ ) => true
	  }
	}
      }
    )
  }

  def show : String = {
    this match {
      case leaf : TermCtxtLeaf[Namespace,Var,Tag] =>
        leaf.showLeaf
      case branch : TermCtxtBranch[Namespace,Var,Tag] =>
        branch.showBranch
      case _ => throw new Exception( "unexpected CCL type" )
    }
  }
}

class TermCtxtLeaf[Namespace,Var,Tag]( val tag : Either[Tag,Var] )
extends TreeItem[Either[Tag,Var]]( tag )
with TermCtxt[Namespace,Var,Tag]
with Factual {
  def this() = { this( null.asInstanceOf[Either[Tag,Var]] ) }
  override def self = List( Left( tag ) )
  override def toString = {
    tag match {
      case Left( t ) => "" + t + ""
      case Right( v ) => "'" + v
    }
  }
  def showLeaf : String = {
    tag match {
      case Left( t : String ) => "\"" + t + "\""
      case Left( t ) => t + ""
      case Right( v ) => v + ""
    }
  }
}

object TermCtxtLeaf extends Serializable {
  def unapply[Namespace,Var,Tag](
    cnxnCtxtLeaf : TermCtxtLeaf[Namespace,Var,Tag]
  ) : Option[( Either[Tag,Var] )] = {
    Some( ( cnxnCtxtLeaf.tag ) )
  }
}

trait AbstractTermCtxtBranch[Namespace,Var,Tag]
extends TermCtxt[Namespace,Var,Tag] {  
  override def self = labels.map( Right( _ ) )
  def nameSpace : Namespace
  def labels : List[TermCtxt[Namespace,Var,Tag]]
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

class TermCtxtBranch[Namespace,Var,Tag](
  override val nameSpace : Namespace,
  val factuals : List[TermCtxt[Namespace,Var,Tag] with Factual]
) extends TreeSection[Either[Tag,Var]]( factuals )
with AbstractTermCtxtBranch[Namespace,Var,Tag]
with Factual {
  def this() = { this( null.asInstanceOf[Namespace], Nil ) }
  override def labels : List[TermCtxt[Namespace,Var,Tag]] = {
    factuals
  }
  override def equals( o : Any ) : Boolean = {
    o match {
      case that : TermCtxtBranch[Namespace,Var,Tag] => {
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
  def unapply[Namespace,Var,Tag](
    cnxnCtxtBranch : TermCtxtBranch[Namespace,Var,Tag]
  ) : Option[
	(
	  Namespace,
	  List[TermCtxt[Namespace,Var,Tag] with Factual]
	)
      ] = {
    Some( ( cnxnCtxtBranch.nameSpace, cnxnCtxtBranch.factuals
 ) )
  }
}

trait TermCtxtInjector[Namespace,Var,Tag] {
  def injectLabel( cLabel : Term[Namespace,Tag] )
  : TermCtxt[Namespace,Var,Tag] with Factual = {
    cLabel match {
      case cLeaf : TermLeaf[Namespace,Tag] =>
	inject( cLeaf )
      case cBranch : TermBranch[Namespace,Tag] =>
	inject( cBranch )
    }
  }
  def inject( cLabel : TermLeaf[Namespace,Tag] )
  : TermCtxt[Namespace,Var,Tag] with Factual = {
    new TermCtxtLeaf( Left( cLabel.tag ) )
  }
  def inject( cLabel : TermBranch[Namespace,Tag] )
  : TermCtxt[Namespace,Var,Tag] with Factual = {
    new TermCtxtBranch(
      cLabel.nameSpace,
      cLabel.factuals.map( injectLabel( _ ) )
    )
  }
}

trait TwoCell[Src,+Label,Trgt] {
  def src   : Src
  def label : Label
  def trgt  : Trgt
}
class CTwoCell[Src,+Label,Trgt](
  override val src : Src,
  override val label : Label,
  override val trgt : Trgt
) extends TwoCell[Src,Label,Trgt]

object CTwoCell extends Serializable {
  def apply[Src,Label,Trgt](
    src : Src, lbl : Label, trgt : Trgt 
  ) : CTwoCell[Src,Label,Trgt] = {
    new CTwoCell[Src,Label,Trgt]( src, lbl, trgt )
  }
  def unapply[Src,Label,Trgt](
    cnxn : CTwoCell[Src,Label,Trgt]
  ) : Option[(Src,Label,Trgt)] = {
    Some( ( cnxn.src, cnxn.label, cnxn.trgt ) )
  }  
}

case class StrTermLf( override val tag : String )
     extends TermLeaf[String,String]( tag ) with Factual

case class StrTermBr(
  override val nameSpace : String,
  override val labels : List[Term[String,String] with Factual]
) extends TermBranch[String,String]( nameSpace, labels ) with Factual

case class StrTermCtxtLf( override val tag : Either[String,String] )
     extends TermCtxtLeaf[String,String,String]( tag ) with Factual

case class StrTermCtxtBr(
  override val nameSpace : String,
  override val labels : List[TermCtxt[String,String,String] with Factual]
) extends TermCtxtBranch[String,String,String]( nameSpace, labels ) with Factual
