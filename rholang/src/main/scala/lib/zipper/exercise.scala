// -*- mode: Scala;-*- 
// Filename:    exercise.scala 
// Authors:     luciusmeredith                                                    
// Creation:    Wed Feb  1 09:11:17 2017 
// Copyright:   See site license 
// Description: 
// ------------------------------------------------------------------------

package coop.rchain.lib.zipper
import coop.rchain.lib.navigation.ZipperNavigation

object Exercise extends ZipperNavigation[String]
with ZipperMutation[String]{
  val arithmeticExpr1 =
    AST[String](
      List(
        AST[String](
          List(
            Token[String]( "a" ),
            Token[String]( "*" ),
            Token[String]( "b" )
          )
        ),
        Token[String]( "+" ),
        AST[String](
          List(
            Token[String]( "c" ),
            Token[String]( "*" ),
            Token[String]( "d" )
          )
        )
      )
    )
  val arithmeticExpr2 =
    AST[String](
      List(
        Token[String]( "c1" ),
        Token[String]( "-" ),
        Token[String]( "c2" )
      )
    )
  val locationOf2ndMult =
    Location[String](
      Token[String]( "*" ),
      TreeContext[String](
        List( Token[String]( "c" ) ),
        TreeContext[String](
          List(
            Token[String]( "+" ),
            AST[String](
              List(
                Token[String]( "a" ),
                Token[String]( "*" ),
                Token[String]( "b" )
              )
            )
          ),
          Top( ),
          List( )
        ),
        List( Token[String]( "d" ) )
      )
    )
  def show( depth : Int )( tree : Tree[String] ) : Unit = {
    tree match {
      case TreeItem( item : String ) => {
        val indent =
          ( "" /: (1 to depth) )( { ( acc, d ) => acc + " " } )
        println( indent + "Leaf : " + item )
      }
      case TreeSection( section : List[Tree[String]] ) => {
        for( t <- section ){ show( depth + 2 )( t ) }
      }
    }
  }
  def framedShow( depth : Int )( tree : Tree[String] ) : Unit = {
    println( "***********************************" )
    show( depth )( tree )
    println( "***********************************" )
  }
  def pushUps( f : Tree[String] => Unit )( location : Location[String] ) : Unit = {
    location match {
      case Location( tree, Top( ) ) => {
        f( tree )
        //println( "at the top" )
      }
      case Location( tree, ctxt ) => {
        f( tree )
        pushUps( f )( up( location ) )
      }
    }
  }
  def doPushUps() = {
    val f = framedShow( 0 ) _
    pushUps( f )( locationOf2ndMult )
  }
  def doMutation() = {
    val f = framedShow( 0 ) _
    val nLoc = update( left( locationOf2ndMult ), arithmeticExpr2 )
    pushUps( f )( nLoc )
  }
}
