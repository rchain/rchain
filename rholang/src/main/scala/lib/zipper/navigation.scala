// -*- mode: Scala;-*- 
// Filename:    navigation.scala 
// Authors:     luciusmeredith                                                    
// Creation:    Wed Feb  1 09:12:11 2017 
// Copyright:   See site license 
// Description: 
// ------------------------------------------------------------------------

package coop.rchain.lib.navigation

import coop.rchain.lib.zipper._

trait ZipperNavigation[+A] {
  def left [A1 >: A] ( location : Location[A1] ) : Location[A1] = {
    location match {
      case Location( _, Top( ) ) => {
        throw new Exception( "left of top" )
      }
      case Location( t, TreeContext( l :: left, up, right ) ) => {
        Location( l, TreeContext( left, up, t :: right ) )
      }
      case Location( t, TreeContext( Nil, up, right ) ) => {
        throw new Exception( "left of first" )
      }
    }
  }
  def right [A1 >: A] ( location : Location[A1] ) : Location[A1] = {
    location match {
      case Location( _, Top( ) ) => {
        throw new Exception( "right of top" )
      }
      case Location( t, TreeContext( left, up, r :: right ) ) => {
        Location( r, TreeContext( t :: left, up, right ) )
      }
      case Location( t, _ ) => {
        throw new Exception( "right of last" )
      }
    }
  }
  def up [A1 >: A]( location : Location[A1] ) : Location[A1] = {
    location match {
      case Location( _, Top( ) ) => {
        throw new Exception( "up of top" )
      }   
      case Location( t, TreeContext( left, up, right ) ) => {
        Location( TreeSection( left.reverse ::: ( t :: right ) ),
                  up )
      }
    }
  }
  def down [A1 >: A]( location : Location[A1] ) : Location[A1] = {
    location match {
      case Location( TreeItem( _ ), _ ) => {
        throw new Exception( "down of item" )
      }
      case Location( TreeSection( Nil ), ctxt ) => {
        throw new Exception( "down of empty" )
      }
      case Location( TreeSection( u :: trees ), ctxt ) => {
        Location( u, TreeContext( Nil, ctxt, trees ) )
      }
    }
  }
}


