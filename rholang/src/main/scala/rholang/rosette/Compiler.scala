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

import java.io._

trait Rholang2RosetteCompilerT {
  self : RholangASTToTerm =>
  def reader( fileName : String ) : FileReader
  def lexer( fileReader : FileReader ) : Yylex
  def parser( lexer : Yylex ) : parser
  def serialize( ast : VisitorTypes.R ) : String

  def compile( fileName : String ) : Option[VisitorTypes.R]
}

object Rholang2RosetteCompiler extends RholangASTToTerm
    with Rholang2RosetteCompilerT
{
     // Members declared in coop.rchain.rho2rose.RholangASTToTerm
  def theTupleSpaceVar : String = s"""TupleSpaceVar${Fresh()}"""

  override def reader( fileName : String ) : FileReader = { new FileReader( fileName ) }
  override def lexer( fileReader : FileReader ) : Yylex = { new Yylex( fileReader ) }
  override def parser( lexer : Yylex ) : parser = { new parser( lexer, lexer.getSymbolFactory() ) }
  override def serialize( ast : VisitorTypes.R ) : String = {
    val term = ast._1
    term.rosetteSerializeOperation + term.rosetteSerialize
  }

  override def compile( fileName : String ) : Option[VisitorTypes.R] = {
    try {
      val rdr = reader( fileName )
      val lxr = lexer( rdr )
      val prsr = parser( lxr )
      val ast = prsr.pContr()
      Some(visit(ast, Set[String]()))
    }
    catch {
      case e : FileNotFoundException => {
        System.err.println(s"""Error: File not found: ${fileName}""")
        None
      }
      case e : CompilerExceptions.CompilerException => {
        System.err.println(s"""Error while compiling: ${fileName}\n${e.toString()}""")
        None
      }
      case t : Throwable => {
        System.err.println(s"""Error while compiling: ${fileName}\n${t.toString()}""")
        None
      }
    }
  }

  def main(args: Array[String]): Unit = {
    args match {
      case Array(fileName) => {
        compile(fileName) match {
          case Some(term) => {
            val rbl: String = serialize(term)
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
