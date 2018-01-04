// -*- mode: Scala;-*- 
// Filename:    Compiler.scala 
// Authors:     luciusmeredith                                                    
// Creation:    Wed Mar  8 09:51:33 2017 
// Copyright:   See site license 
// Description: Basic loading of source from a stream and running it
// through the source-to-source translation.
// ------------------------------------------------------------------------

package coop.rchain.rho2rose

import coop.rchain.syntax.rholang._
import coop.rchain.syntax.rholang.Absyn._

import java.io._

trait Rholang2RosetteCompilerT {
  self : RholangASTToTerm =>
  def reader( fileName : String ) : FileReader
  def lexer( fileReader : FileReader ) : Yylex
  def parser( lexer : Yylex ) : parser
  def serialize( ast : VisitorTypes.R ) : String

  def compile( fileName : String ) : VisitorTypes.R
}

object Rholang2RosetteCompiler extends RholangASTToTerm
    with Rholang2RosetteCompilerT
{
     // Members declared in coop.rchain.rho2rose.RholangASTToTerm
  def theTupleSpaceVar : String = s"""TupleSpaceVar${Fresh()}"""

  def visit(p: CPattern,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(b: Bind,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: Chan,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: Proc,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
   
  override def reader( fileName : String ) : FileReader = { new FileReader( fileName ) }
  override def lexer( fileReader : FileReader ) : Yylex = { new Yylex( fileReader ) }
  override def parser( lexer : Yylex ) : parser = { new parser( lexer ) }
  override def serialize( ast : VisitorTypes.R ) : String = {
    ast match {
      case Some(term: StrTermCtorAbbrevs.StrTermCtxt @unchecked) =>
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
      visit( ast, null )
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
    args match {
      case Array(fileName) => {
        compile(fileName) match {
          case result@Some(_) => {
            val rbl: String = serialize(result)
            val rblFileName = fileName.replaceAll(".rho$", "") + ".rbl"
            new java.io.PrintWriter(rblFileName) { write(rbl); close }
            System.err.println(s"compiled $fileName to $rblFileName")
          }
          case None => System.exit(1)
        }
      }
      case _ => {
        System.err.println("no input file?")
        System.exit(1)
      }
    }
  }
}
