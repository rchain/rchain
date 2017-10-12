// -*- mode: Scala;-*- 
// Filename:    ast.scala 
// Authors:     luciusmeredith                                                    
// Creation:    Wed Feb  1 09:10:22 2017 
// Copyright:   See site license 
// Description: 
// ------------------------------------------------------------------------

package coop.rchain.lib.zipper

case class Token[A](
  override val item : A
) extends TreeItem[A]( item )
case class AST[A](
  override val section : List[Tree[A]]
) extends TreeSection[A]( section )


