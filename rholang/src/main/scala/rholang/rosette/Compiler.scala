// -*- mode: Scala;-*- 
// Filename:    Compiler.scala 
// Authors:     luciusmeredith                                                    
// Creation:    Wed Mar  8 09:51:33 2017 
// Copyright:   See site license 
// Description: Basic loading of source from a stream and running it
// through the source-to-source translation.
// ------------------------------------------------------------------------

package coop.rchain.rho2rose

import coop.rchain.lib.zipper._
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

  //  To Do, fix stub implementations.
  // Members declared in coop.rchain.syntax.rholang.Absyn.CPQuantity.Visitor
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPQ,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  // Members declared in coop.rchain.syntax.rholang.Absyn.RhoBool.Visitor
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPVerifyEd25519,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPVerifySecp256k1,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  // Members declared in coop.rchain.syntax.rholang.Absyn.Curve25519Nonce.Visitor
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPBytesToNonceCurve25519,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPNonceCurve25519,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  // Members declared in coop.rchain.syntax.rholang.Absyn.Ed25519PrvKey.Visitor
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPBytesToPrvKeyEd25519,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPPrvKeyEd25519,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  // Members declared in coop.rchain.syntax.rholang.Absyn.CPVar.Visitor
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPV,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  // Members declared in coop.rchain.syntax.rholang.Absyn.CPrim.Visitor
  def visit(p: coop.rchain.syntax.rholang.Absyn.QText,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.QByteString,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  // Members declared in coop.rchain.syntax.rholang.Absyn.Text.Visitor
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPBase64encode,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPBase58encode,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPBase16encode,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPText,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  // Members declared in coop.rchain.syntax.rholang.Absyn.ByteString.Visitor
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPBase64decode,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPBase58decode,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPBase16decode,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPBlake2b256,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPKeccak256,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPSha256,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPBytesFromNonceCurve25519,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPBytesFromPrvKeyCurve25519,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPBytesFromPubKeyCurve25519,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPDecryptCurve25519,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPEncryptCurve25519,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPBytesFromPrvKeyEd25519,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPBytesFromPubKeyEd25519,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPSignEd25519,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPBytesFromPrvKeySecp256k1,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPBytesFromPubKeySecp256k1,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPSignSecp256k1,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPByteString,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  // Members declared in coop.rchain.syntax.rholang.Absyn.Secp256k1PubKey.Visitor
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPBytesToPubKeySecp256k1,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPPubKeySecp256k1,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  // Members declared in coop.rchain.syntax.rholang.Absyn.Curve25519PubKey.Visitor
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPBytesToPubKeyCurve25519,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPPubKeyCurve25519,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  // Members declared in coop.rchain.syntax.rholang.Absyn.Quantity.Visitor
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPNewNonce,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPNewKeyPairCurve25519,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPNewKeyPairEd25519,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPNewKeyPairSecp256k1,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.QCPrim,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  // Members declared in coop.rchain.syntax.rholang.Absyn.Secp256k1PrvKey.Visitor
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPBytesToPrvKeySecp256k1,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPPrvKeySecp256k1,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  // Members declared in coop.rchain.syntax.rholang.Absyn.Curve25519PrvKey.Visitor
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPBytesToPrvKeyCurve25519,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPPrvKeyCurve25519,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  // Members declared in coop.rchain.syntax.rholang.Absyn.Ed25519PubKey.Visitor
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPBytesToPubKeyEd25519,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???
  def visit(p: coop.rchain.syntax.rholang.Absyn.CPPubKeyEd25519,arg: coop.rchain.rho2rose.VisitorTypes.A): coop.rchain.rho2rose.VisitorTypes.R = ???

   // Members declared in coop.rchain.rho2rose.StrFoldCtxtVisitor
  def theCtxtVar: String = s"""ContextVar${Fresh()}"""
  def zipr: StrTermNavigation with StrTermMutation with StrTermZipperComposition = 
    StrTermZipr

  override def reader( fileName : String ) : FileReader = { new FileReader( fileName ) }
  override def lexer( fileReader : FileReader ) : Yylex = { new Yylex( fileReader ) }
  override def parser( lexer : Yylex ) : parser = { new parser( lexer ) }
  override def serialize( ast : VisitorTypes.R ) : String = {
    ast match {
      case Some(Location(term: StrTermCtorAbbrevs.StrTermCtxt @unchecked, _)) =>
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
