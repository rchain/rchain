// -*- mode: Scala;-*- 
// Filename:    path.scala 
// Authors:     luciusmeredith                                                    
// Creation:    Wed Feb  1 09:12:27 2017 
// Copyright:   See site license 
// Description: 
// ------------------------------------------------------------------------

package coop.rchain.lib.navigation

import coop.rchain.lib.zipper._
import scala.collection.SeqProxy

trait Path[+A]

trait Direction
case object Up extends Path[Nothing] with Direction
case object Down extends Path[Nothing] with Direction
case object Left extends Path[Nothing] with Direction
case object Right extends Path[Nothing] with Direction

case class RelativePath[+A](
  route : List[Path[A] with Direction]
) extends Path[A] with Direction
     with SeqProxy[Path[A] with Direction]
{
  override def self = route
}

trait ZipperPathNavigation[+A] 
extends ZipperNavigation[A] {
  def move [A1 >: A] (
    location : Location[A1],
    pd : Path[A1] with Direction
  ) : Location[A1] = {
    pd match {
      case Left => left( location )
      case Right => right( location )
      case Up => up( location )
      case Down => down( location )
    }
  }
  def move [A1 >: A] (
    location : Location[A1],
    rpath : RelativePath[A1]
  ) : Location[A1] = {
    ( location /: rpath )(
      {
        ( acc, e ) => {
          move[A1]( acc, e )
        }
      }
    )
  }
}

class EndPoint[+A](
  override val tree : Tree[A],
  override val ctxt : Context[A]
) extends Location[A]( tree, ctxt )
with Path[A]
with ZipperPathNavigation[A] {
  def move [A1 >: A] (
    location : EndPoint[A1],
    pd : Path[A1] with Direction
  ) : EndPoint[A1] = {
    pd match {
      case Left => left( location )
      case Right => right( location )
      case Up => up( location )
      case Down => down( location )
    }
  }
  def move [A1 >: A] (
    location : EndPoint[A1],
    rpath : RelativePath[A1]
  ) : EndPoint[A1] = {
    ( location /: rpath )(
      {
        ( acc, e ) => {
          move[A1]( acc, e )
        }
      }
    )
  }
  def / [A1 >: A] ( pd : Path[A1] with Direction ) = {
    move( this, pd )
  }
  def left [A1 >: A] ( location : EndPoint[A1] ) : EndPoint[A1] = {
    location match {
      case EndPoint( _, Top( ) ) => {
        throw new Exception( "left of top" )
      }
      case EndPoint( t, TreeContext( l :: left, up, right ) ) => {
        EndPoint( l, TreeContext( left, up, t :: right ) )
      }
      case EndPoint( t, TreeContext( Nil, up, right ) ) => {
        throw new Exception( "left of first" )
      }
    }
  }
  def right [A1 >: A] ( location : EndPoint[A1] ) : EndPoint[A1] = {
    location match {
      case EndPoint( _, Top( ) ) => {
        throw new Exception( "right of top" )
      }
      case EndPoint( t, TreeContext( left, up, r :: right ) ) => {
        EndPoint( r, TreeContext( t :: left, up, right ) )
      }
      case EndPoint( t, _ ) => {
        throw new Exception( "right of last" )
      }
    }
  }
  def up [A1 >: A]( location : EndPoint[A1] ) : EndPoint[A1] = {
    location match {
      case EndPoint( _, Top( ) ) => {
        throw new Exception( "up of top" )
      }   
      case EndPoint( t, TreeContext( left, up, right ) ) => {
        EndPoint( TreeSection( left.reverse ::: ( t :: right ) ),
                  up )
      }
    }
  }
  def down [A1 >: A]( location : EndPoint[A1] ) : EndPoint[A1] = {
    location match {
      case EndPoint( TreeItem( _ ), _ ) => {
        throw new Exception( "down of item" )
      }
      case EndPoint( TreeSection( Nil ), ctxt ) => {
        throw new Exception( "down of empty" )
      }
      case EndPoint( TreeSection( u :: trees ), ctxt ) => {
        EndPoint( u, TreeContext( Nil, ctxt, trees ) )
      }
    }
  }
}

object EndPoint {
  def apply[A]( tree : Tree[A], ctxt : Context[A] ) = {
    new EndPoint( tree, ctxt )
  }
  def unapply[A]( loc : EndPoint[A] )
  : Option[( Tree[A], Context[A] )] = {
    Some( ( loc.tree, loc.ctxt ) )
  }
}

case class AbsolutePath[+A](
  root : EndPoint[A],
  route : RelativePath[A]
) extends Path[A]

object NavDSL {
  def / [A]( root : Tree[A] ) : EndPoint[A] = {
    EndPoint( root, Top( ) )
  }  
}
