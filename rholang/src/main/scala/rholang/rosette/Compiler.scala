// -*- mode: Scala;-*- 
// Filename:    Compiler.scala 
// Authors:     luciusmeredith                                                    
// Creation:    Wed Mar  8 09:51:33 2017 
// Copyright:   See site license 
// Description: Basic loading of source from a stream and running it
// through the source-to-source translation.
// ------------------------------------------------------------------------

package coop.rchain.rho2rose

import coop.rchain.lib.term._
import coop.rchain.lib.zipper._
import coop.rchain.syntax.rholang._
import coop.rchain.syntax.rholang.Absyn._

import java_cup.runtime._
import java.io._

trait Rholang2RosetteCompilerT {
  self : RholangASTToTerm =>
  def reader( fileName : String ) : FileReader
  def lexer( fileReader : FileReader ) : Yylex
  def parser( lexer : Yylex ) : parser
  def serialize( ast : VisitorTypes.R ) : String

  def compile( fileName : String ) : VisitorTypes.R
}

object StrTermZipr extends StrTermNavigation
    with StrTermMutation with StrTermZipperComposition

object Rholang2RosetteCompiler extends RholangASTToTerm
    with Rholang2RosetteCompilerT
{
     // Members declared in coop.rchain.rho2rose.RholangASTToTerm
  def theTupleSpaceVar : String = s"""TupleSpaceVar${Fresh()}"""

  def visit(p: CPattern,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(b: Bind,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: Chan,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: Proc,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
   
   // Members declared in coop.rchain.rho2rose.StrFoldCtxtVisitor
  def theCtxtVar: String = s"""ContextVar${Fresh()}"""
  def zipr: StrTermNavigation with StrTermMutation with StrTermZipperComposition = 
    StrTermZipr

  override def reader( fileName : String ) : FileReader = { new FileReader( fileName ) }
  override def lexer( fileReader : FileReader ) : Yylex = { new Yylex( fileReader ) }
  override def parser( lexer : Yylex ) : parser = { new parser( lexer ) }
  override def serialize( ast : VisitorTypes.R ) : String = {
    ast match {
      case Some(Location(term: StrTermCtorAbbrevs.StrTermCtxt, _)) =>
        term.rosetteSerializeOperation + term.rosetteSerialize
      case _ => "Not a StrTermCtxt"
    }
  }

  override def compile( fileName : String ) : VisitorTypes.R = {
    try {
      val rdr = reader( fileName )
      val lxr = lexer( rdr )
      val prsr = parser( lxr )
      val ast = prsr.pContr()
      visit( ast, Here() )
    }
    catch {
      case e : FileNotFoundException => {
        System.err.println(s"""Error: File not found: ${fileName}""")
        None
      }
      case t : Throwable => {
        System.err.println(s"""Unexpect error compiling: ${fileName}""")
        None
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val result = compile(args(0))
    println(serialize(result))
  }
}
