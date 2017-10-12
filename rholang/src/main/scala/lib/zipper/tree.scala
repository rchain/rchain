// -*- mode: Scala;-*- 
// Filename:    tree.scala 
// Authors:     luciusmeredith                                                    
// Creation:    Wed Feb  1 09:13:01 2017 
// Copyright:   See site license 
// Description: 
// ------------------------------------------------------------------------

package coop.rchain.lib.zipper

trait Tree[+A] extends Serializable

case object TZero
extends Tree[Nothing]

class TreeItem[+A]( val item : A ) extends Tree[A] {
  override def equals( o : Any ) : Boolean = {
    o match {
      case that : TreeItem[A] => {
        item.equals( that.item )
      }
      case _ => {
        false
      }
    }
  }
  override def hashCode( ) : Int = {
    37 * item.hashCode
  }
}
object TreeItem {
  def apply[A]( item : A ) = { new TreeItem( item ) }
  def unapply[A]( tree : TreeItem[A] )
  : Option[( A )] = {
    Some( ( tree.item ) )
  }
}
case class CTreeItem[+A]( val item : A ) extends Tree[A]
class TreeSection[+A](
  val section: List[Tree[A]]
) extends Tree[A] {
  override def equals( o : Any ) : Boolean = {
    o match {
      case that : TreeSection[A] => {
        section.equals( that.section )
      }
      case _ => {
        false
      }
    }
  }
  override def hashCode( ) : Int = {
    37 * section.hashCode
  }
}
object TreeSection {
  def apply[A]( section : List[Tree[A]] ) = { new TreeSection( section ) }
  def unapply[A]( tree : TreeSection[A] )
  : Option[( List[Tree[A]] )] = {
    Some( ( tree.section ) )
  }
}
case class CTreeSection[+A](
  val section: List[Tree[A]]
) extends Tree[A]
