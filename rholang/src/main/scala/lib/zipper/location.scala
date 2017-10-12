// -*- mode: Scala;-*- 
// Filename:    location.scala 
// Authors:     luciusmeredith                                                    
// Creation:    Wed Feb  1 09:11:33 2017 
// Copyright:   See site license 
// Description: 
// ------------------------------------------------------------------------

package coop.rchain.lib.zipper

class Location[+A](
  val tree : Tree[A],
  val ctxt : Context[A]
) {
  override def toString() : String = {
    ctxt + "." + tree
  }
}
object Location {
  def apply[A]( tree : Tree[A], ctxt : Context[A] ) = {
    new Location( tree, ctxt )
  }
  def unapply[A]( loc : Location[A] )
  : Option[( Tree[A], Context[A] )] = {
    Some( ( loc.tree, loc.ctxt ) )
  }
}
